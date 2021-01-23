####################################
####################################
library(gtrendsR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gghighlight)

res_20 <-
  gtrends(c("引っ越し", "引越し"), geo = c("JP"), time = "2020-01-01 2020-12-31", gprop = "web")
res_19 <-
  gtrends(c("引っ越し", "引越し"), geo = c("JP"), time = "2019-01-01 2019-12-31", gprop = "web")
df_trends <-
  res_20 %>%
  purrr::pluck("interest_over_time") %>%
  dplyr::bind_rows(
    res_19 %>%
      purrr::pluck("interest_over_time")
    ) %>%
  select(date, hits, keyword)

df_trends %>%
  readr::write_rds("trends.rds")

df_trends <-
  readr::read_rds("trends.rds")

d <-
  tibble::tibble(
  date = seq(make_date(2020, 1, 1),
             make_date(2020, 12, 31), by = 1),
  m = lubridate::month(date, label = TRUE, abbr = FALSE),
  doy = lubridate::yday(date))

df_trends %>%
  dplyr::mutate(year = lubridate::year(date),
                doy = lubridate::yday(date)) %>%
  dplyr::left_join(d, by = "doy") %>%
  filter(year %in% c(2020, 2019)) %>%
  ggplot(aes(as_date(doy, origin = as_date("2020-01-01")), hits)) +
  geom_line(aes(group = as.character(year), color = as.character(year))) +
  scale_x_date(date_labels = "%b月") +
  # gghighlight(year == 2020) +
  facet_wrap(~ keyword, ncol = 1) +
  xlab(NULL) +
  theme_bw(base_size = 18, base_family = "HiraginoSans-W6") +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        panel.grid.minor = element_line(color = NA),
        panel.grid.major = element_line(color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  labs(color = "検索キーワード",
       caption = "Google Trendsのデータを利用")

ggsave(filename = "images/google_trends.png",
       plot = last_plot(),
       width = 8, height = 10,
       bg = "transparent")
