library(tidyverse)

ts_combined <- readRDS("data/covid-19_ts_combined.rds")

pe <- ts_combined %>%
  filter(iso3c == "PER" & confirmed > 0) %>%
  select(country_region, iso3c, ts, confirmed, deaths, recovered) %>%
  mutate(
    y = log10(confirmed),
    confirmed_diff = lead(confirmed) - confirmed,
    confirmed_diff2 = lead(confirmed_diff) - confirmed_diff
  )

logmod1 <- lm(y ~ ts, pe)
summary(logmod1)

last_ts <- max(pe$ts)

# siguientes 7 dias
future <- data.frame(
  ts = seq(from = last_ts + 1, by = 1, length.out = 7)
)

res <- predict(logmod1, newdata = future)
future$casos_futuros <- floor(10^res)
future
