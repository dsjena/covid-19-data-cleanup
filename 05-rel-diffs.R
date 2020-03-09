# some ideas

library(tidyverse)

ts_combined <- readRDS(
  here::here("data/covid-19_ts_combined.rds")
)

na_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}

ts_diffs <- ts_combined %>%
  as_tibble() %>%
  group_by(
    continent,
    iso3c,
    country_region,
    ts
  ) %>%
  summarise(
    confirmed = na_zero(sum(confirmed,na.rm = TRUE)),
    deaths = na_zero(sum(deaths, na.rm = TRUE)),
    recovered = na_zero(sum(recovered, na.rm = TRUE))
  ) %>%
  mutate(
    # differences
    diff_confirmed = confirmed - lag(confirmed),
    diff_deaths = deaths - lag(deaths),
    diff_recovered = recovered - lag(recovered),
    # ratios
    ratio_confirmed = lead(diff_confirmed) / diff_confirmed
  )


ts_confirmed_max <- ts_combined %>%
  as_tibble() %>%
  group_by(
    continent,
    iso3c,
    country_region,
    ts
  ) %>%
  summarise(
    confirmed = sum(confirmed, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(
    continent,
    iso3c,
    country_region
  ) %>%
  summarise(
    max_confirmed = max(confirmed)
  ) %>%
  arrange(desc(max_confirmed)) %>%
  filter(
    max_confirmed > 100
  )

ggplot(ts_diffs %>%
         filter(iso3c %in% ts_confirmed_max$iso3c),
       aes(x = ts, y = confirmed,
           group = iso3c, color = iso3c)) +
  geom_point(show.legend = FALSE, size = .6, alpha = .5) +
  geom_smooth(se = FALSE, method = "lm",
              show.legend = FALSE) +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  facet_wrap(~country_region, scales = "free_y")

library(ggpubr)
ggscatter(ts_diffs %>%
         filter(iso3c %in% ts_confirmed_max$iso3c),
       x = "ts",
       y = "confirmed",
       size = .6,
       alpha = .5,
       color = "iso3c") +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  facet_wrap(~country_region, scales = "free_y") +
  #stat_cor() +
  #stat_regline_equation(label.y.npc = .8) +
  theme(
    legend.position = "none"
  )


