library(tidyverse)

ts_combined <- readRDS("data/covid-19_ts_combined.rds")

#Noticia de hace unos minutos: https://rpp.pe/peru/actualidad/coronavirus-en-peru-minuto-a-minuto-43-casos-de-covid-19-y-gobierno-suspende-la-recepcion-de-cruceros-a-puertos-del-pais-live-163

pe <- ts_combined %>%
  filter(iso3c == "PER" & confirmed > 0) %>%
  select(country_region, iso3c, ts, confirmed) %>%
  add_row(
    country_region = "Peru",
    iso3c = "PER",
    ts = as.Date("2020-03-15"),
    confirmed = 71 # noticia de hace unos minutos
  ) %>%
  mutate(
    y = log10(confirmed),
    confirmed_diff = lead(confirmed) - confirmed,
    confirmed_diff2 = lead(confirmed_diff) - confirmed_diff
  )

# modelo lineal asumiendo crecimiento exponencial del número de casos confirmados
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

# gráfico de modelo y valores:
ggplot(pe, aes(x = ts, y = y)) +
  geom_point() +
  geom_line(aes(y = predict(logmod1)), color = "red") +
  labs(
    y = "log10(casos)",
    x = "",
    title = "Casos confirmados en Perú (al 2020-03-14)"
  ) +
  theme_bw(18)
ggsave(
  filename = "casos-peru-modelo-exp.png"
)

ggplot(pe, aes(x = ts, y = confirmed)) +
  geom_point() +
  geom_line(aes(y = 10^predict(logmod1)), color = "red") +
  labs(
    y = "Casos",
    x = "",
    title = "Casos confirmados en Perú (al 2020-03-14)"
  ) +
  theme_bw(18)
ggsave(
  filename = "casos-peru-modelo-exp2.png"
)

