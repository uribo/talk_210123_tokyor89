#####################################
# 2021-01-23 Tokyo.R
# 都道府県地価調査
# https://www.mlit.go.jp/totikensangyo/totikensangyo_fr4_000044.html
#####################################
# library(kuniumi)
library(sf)
library(tibble)
library(ggplot2)
library(dplyr)
library(mapview)
library(forcats)
library(stringr)
library(gghighlight)
library(kableExtra)
library(conflicted)
conflict_prefer("filter", winner = "dplyr")
ggplot2::theme_set(new = theme_bw(base_size = 16, base_family = "HiraginoSans-W4"))

ksj_l02_selected <- function(data) {
  d_mod <-
    data %>%
    # dplyr::select(seq.int(53), `調査価格_2019`) %>%
    # dplyr::select(!c(
    #   tidyselect::starts_with("属性移動"),
    #   tidyselect::starts_with("供給施設有無"),
    #   tidyselect::starts_with("前年度基準地コード"))) %>%
    # dplyr::select(2, 4, 5, 6, 7, 24, 25, 26, 28, 35) %>%
    dplyr::filter(stringr::str_detect(基準地市区町村名称,
                                               "（林）",
                                               negate = TRUE)) %>%
    dplyr::filter(`調査価格_2019` != 0)
  d <-
    d_mod %>%
    dplyr::select(
      code = `基準地コード_一連番号`,
      address = `住居表示`,
      price = `調査価格`,
      price_2019 = `調査価格_2019`,
      city_code = `基準地行政区域コード`,
      # nearest_st = `駅名`,
      # st_distance = `駅からの距離`,
      land_divide = `用途区分`,
      urban_divide = `都市計画区分`,
      starts_with("調査価格"))
  d %>%
    st_drop_geometry() %>%
    tidyr::nest(price_vars = starts_with("調査価格")) %>%
    mutate(geometry = st_geometry(d_mod)) %>%
    sf::st_sf()
}

ksj_l02_volatility <- function(data, price, prev_price) {
  data %>%
    dplyr::mutate(
      diff = {{ price }} - {{ prev_price }},
      # 変動率
      volatility = round((({{ price }} / {{ prev_price }}) * 100) - 100, digits = 1)
    )
}

ksj_l02_separate_address <- function(data, address) {
  cbind(
    data,
    data %>%
      st_drop_geometry() %>%
      select({{ address }}) %>%
      mutate(address_elem = zipangu::separate_address({{ address }})) %>%
      tidyr::hoist(address_elem,
                   prefecture = "prefecture",
                   city = "city",
                   street = "street") %>%
      select(- {{ address }})
  ) %>%
    select(everything(), geometry) %>%
    select(- {{ address }}) %>%
    mutate(across(c(prefecture, city, street),
                  .fns = stringr::str_trim)) %>%
    mutate(street = stringi::stri_trans_nfkc(street))
}

if (file.exists("simple_japan.geojson") == FALSE) {
  d_pref <-
    kuniumi::read_ksj_n03("~/Documents/resources/国土数値情報/N03/N03-20200101_GML/N03-20_200101.geojson") %>%
    group_by(prefectureName) %>%
    summarise() %>%
    st_simplify(dTolerance = 0.005)

  d_pref %>%
    st_write("simple_japan.geojson")
} else {
  d_pref <-
    sf::st_read("simple_japan.geojson")
}

# 全国の傾向 -------------------------------------------------------------------
if (file.exists("l02_2020.rds") == FALSE) {
  # 3分クッキングよろしく、加工したデータを使います
  # sf::st_read("~/Documents/resources/国土数値情報/L02/L02-20_GML/L02-20.geojson")

  df_l02_2020_raw <-
    kuniumi::read_ksj_l02("~/Documents/resources/国土数値情報/L02/L02-20_GML/L02-20.geojson")
  identical(as.integer(21519 - 12),
            nrow(df_l02_2020_raw))
  df_l02_2020 <-
    df_l02_2020_raw %>%
    ksj_l02_selected() %>%
    ksj_l02_volatility(price = price, prev_price = price_2019) %>%
    ksj_l02_separate_address(address) %>%
    relocate(prefecture, city_code, city, street, .after = code) %>%
    tibble::new_tibble(class = "sf", nrow = nrow(.))
  df_l02_2020 %>%
    readr::write_rds("l02_2020.rds")
} else {
  df_l02_2020 <-
    readr::read_rds("l02_2020.rds")
}

# 住宅地
# プラス: 15 -> 5 (宮城県、東京都、 福岡県、大分県、沖縄県)
# マイナス: 32 -> 42
d_map <-
  d_pref %>%
  left_join(
    df_l02_2020 %>%
      st_drop_geometry() %>%
      filter(land_divide %in% c("1低専", "2低専", "1中専", "2中専", "準住居", "1住居", "2住居")) %>%
      group_by(prefecture) %>%
      summarise(volatility = mean(volatility)),
    by = c("prefectureName" = "prefecture")
  ) %>%
  mutate(volatility = case_when(
    volatility > 5.0 ~ "5.0%以上",
    between(volatility, 2.0, 5.0) ~ "2.0~5.0未満",
    between(volatility, 1.0, 2.0) ~ "1.0~2.0未満",
    between(volatility, 0.1, 1.0) ~ "0.1~1.0未満",
    volatility == 0.0 ~ "0.0",
    between(volatility, -0.1, -1.0) ~ "-0.1~-1.0未満",
    between(volatility, -1.0, -2.0) ~ "-1.0~-2.0未満",
    volatility > -2.0 ~ "-2.0%以上",
  ) %>%
    forcats::fct_relevel("5.0%以上",
                         #"2.0~5.0未満",
                         "1.0~2.0未満",
                         "0.1~1.0未満",
                         # "0",
                         #"-0.1~-1.0未満",
                         #"-1.0~-2.0未満",
                         "-2.0%以上"))

mapview(d_map %>%
          st_transform(crs = 4326), zcol = "volatility")


p <-
  ggplot() +
  geom_sf(data = d_map,
          aes(fill = volatility),
          color = "transparent") +
  scale_fill_manual(values = c("#ff9903", "#ffd202", "#ffff99", "#587fbe")) +
  theme_void(base_size = 16, base_family = "HiraginoSans-W4") +
  labs(title = "令和2年 都道府県別地価変動率(住宅地)",
       fill = "変動率") +
  theme(plot.title = element_text(size = 12),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_line(color = NA),
        panel.grid.major = element_line(color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave(filename = "images/令和2年_都道府県別地価変動率_住宅地.png",
       plot = p,
       width = 10,
       height = 8,
       bg = "transparent")


x <-
  df_l02_2020 %>%
  st_drop_geometry() %>%
  filter(is.na(land_divide) | land_divide %in% c("1低専", "2低専", "1中専", "2中専", "準住居", "1住居", "2住居")) %>%
  slice_max(n = 10, order_by = volatility) %>%
  tidyr::unnest(cols = price_vars) %>%
  select(prefecture, city, street, price, price_2019, volatility) %>%
  mutate(rank = row_number(),
         volatility = purrr::pmap_chr(.,
                                      ~ paste(c(as.character(fontawesome::fa(name = "arrow-circle-up",
                                                                              fill = "#00FF7F"))),
                                               ..6,
                                               collapse = "&nbsp;"))) %>%
  relocate(rank, .before = 1) %>%
  knitr::kable(format = "html",
               escape = FALSE,
               caption = "変動率上昇率順位表 （全国・住宅地）")

x <-
  df_l02_2020 %>%
  st_drop_geometry() %>%
  filter(is.na(land_divide) | land_divide %in% c("1低専", "2低専", "1中専", "2中専", "準住居", "1住居", "2住居")) %>%
  slice_min(n = 10, order_by = volatility) %>%
  tidyr::unnest(cols = price_vars) %>%
  select(prefecture, city, street, price, price_2019, volatility) %>%
  mutate(rank = row_number(),
         volatility = purrr::pmap_chr(.,
                                      ~ paste(c(as.character(fontawesome::fa(name = "arrow-circle-down",
                                                                             fill = "#ff7f00"))),
                                              ..6,
                                              collapse = "&nbsp;"))) %>%
  relocate(rank, .before = 1) %>%
  knitr::kable(format = "html",
               escape = FALSE,
               caption = "変動率下落率順位表 （全国・住宅地）")

x %>%
  kableExtra::kable_minimal() %>%
  kableExtra::column_spec(7,
                          width = "20em",
                          color = "#eeeeee",
                          bold = TRUE)


# 下落率順位表（全国、東京圏）
p <-
  df_l02_2020 %>%
  filter(prefecture %in% c("東京都", "神奈川県", "千葉県", "茨城県", "埼玉県"),
         land_divide %in% c("1低専", "2低専",
                            "1中専", "2中専",
                            "1住居", "2住居",
                            "準住居")) %>%
  st_drop_geometry() %>%
  # filter(is.na(land_divide) &　urban_divide == "都計外" | land_divide %in% c("近商", "準住居", "商業", "準工")) %>%
  select(2, 4:7, 10, 11) %>%
  slice_min(n = 10, order_by = volatility) %>%
  mutate(pref_city = paste(city, street)) %>%
  ggplot(aes(forcats::fct_rev(forcats::fct_reorder(pref_city, volatility)),
             volatility)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab(NULL) +
  ylab("変動率") +
  labs(title = "下落率順位上位10地点",
       subtitle = "住宅地・東京圏") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave(filename = "images/令和2年_都道府県別地価変動率_下落率順位上位_住宅地_東京圏.png",
       plot = p,
       width = 10,
       height = 8,
       bg = "transparent")

df_l02_2020 %>%
  st_drop_geometry() %>%
  filter(diff > 0) %>%
  group_by(prefecture,
           city_code,
           city) %>%
  summarise(diff = mean(diff, na.rm = TRUE),
            .groups = "drop") %>%
  slice_max(n = 30, order_by = diff) %>%
  mutate(pref_city = paste(prefecture, city)) %>%
  ggplot(aes(forcats::fct_reorder(pref_city, diff),
             diff)) +
  geom_bar(stat = "identity") +
  coord_flip()


# 東京都
df_l02_2020 %>%
  filter(diff < 0,
         prefecture == "東京都",
         stringr::str_detect(city_code, paste0("^13",
                                               c("(361|362|363|364|381|382|401|402|421|900)")),
                             negate = TRUE)) %>%
  mapview(zcol = "diff", map = kuniezu::gsi_tiles$pale)

# 茨城県 ---------------------------------------------------------------------
d_tgt <-
  df_l02_2020 %>%
  filter(prefecture == "茨城県") %>%
  filter(land_divide %in% c("1低専", "2低専", "1中専", "2中専",
                            "1住居", "2住居", "準住居"))

candidate_cities <-
  c("つくば市", "守谷市",
    "つくばみらい市", "牛久市",
    "小美玉市", "石岡市")

p <-
  d_tgt %>%
  st_drop_geometry() %>%
  filter(city %in% candidate_cities) %>%
  ggplot(aes(city, price)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  xlab(NULL) +
  ylab("基準地価価格(円/m^2)") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave(filename = "images/pref08_landprice2020_boxplot.png",
       plot = p,
       width = 8,
       height = 8,
       bg = "transparent")
p <-
  d_tgt %>%
  st_drop_geometry() %>%
  group_by(city) %>%
  summarise(price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(forcats::fct_reorder(city, price), price)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "令和2年茨城県地価調査") +
  theme(plot.margin= unit(c(1, 2, 1, 1), "lines")) +
  xlab(NULL) +
  ylab("基準地価価格(円/m^2)") +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

p <-
  p +
  gghighlight(city %in% candidate_cities)

ggsave(filename = "images/pref08_landprice2020.png",
       plot = p,
       width = 8, height = 10,
       bg = "transparent")

# 守谷、つくばはそんなに変わっていない
d_tgt %>%
  st_drop_geometry() %>%
  group_by(city) %>%
  summarise(volatility = mean(volatility, na.rm = TRUE)) %>%
  ggplot(aes(forcats::fct_reorder(city, volatility), volatility)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  gghighlight(city %in% candidate_cities)

p <-
  d_tgt %>%
  st_drop_geometry() %>%
  filter(city %in% candidate_cities[!candidate_cities %in% c("石岡市", "小美玉市")]) %>%
  tidyr::unnest(cols = price_vars) %>%
  select(city, starts_with("調査価格")) %>%
  tidyr::pivot_longer(cols = starts_with("調査価格"),
                      names_to = c("a", "year"),
                      names_pattern = "(.*)_(.*)") %>%
  filter(value != 0) %>%
  group_by(year, city) %>%
  summarise(value = mean(value),.groups = "drop") %>%
  ggplot(aes(year, value)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ city, ncol = 1) +
  xlab(NULL) +
  ylab("基準地価価格(円/m^2)") +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(size = 12),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave(filename = "images/pref08_landprice2020_historical.png",
       plot = p,
       width = 20,
       height = 8,
       bg = "transparent")

# みらい平らへんが良さそう??
d_tgt %>%
  select(-price_vars) %>%
  filter(city == "つくばみらい市") %>%
  mapview(zcol = "price")
# つくばだったら...みどりの、谷田部（今の居住地に近くて良さげ）
d_tgt %>%
  select(-price_vars) %>%
  filter(city == "つくば市") %>%
  mapview(zcol = "price")
