rm(list = ls())
library(tidyverse)
library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(ggformula)
library(bsts)
library(cmdstanr)
library(posterior)
library(ggtext)

# Lectura de la base desde Internet
ile_data         <- read_csv(c("datasets/ile-2016-2018.csv", "datasets/ile-2019-2021.csv",
                               "datasets/ile-2022-final.csv"), show_col_types = FALSE)
ile_data         <- ile_data %>% filter(!is.na(fingreso))

fechas <- tibble(
  Fecha = seq(ymd("2015/01/01"), today(), by = "1 day")
) %>%
  mutate(epiweek = epiweek(Fecha)) %>%
  mutate(epiyear = epiyear(Fecha)) %>%
  distinct(epiweek, epiyear, .keep_all = T)

ile_data <- ile_data %>%
  rename(Fecha = fingreso) %>%
  group_by(Fecha) %>%
  tally() %>%
  rename(ILE = n)

ile_data <- ile_data %>%
  mutate(epiweek = epiweek(Fecha)) %>%
  mutate(epiyear = epiyear(Fecha)) %>%
  group_by(epiweek, epiyear) %>%
  summarise(ILE = sum(ILE))

ile_data <- ile_data %>%
  left_join(fechas) %>%
  arrange(Fecha)


ile_data %>%
  filter(Fecha > "2016-01-03" ) %>%
  ggplot() +
  geom_rect(aes(xmin = ymd("2020/03/23"), ymin = -100,
                xmax = ymd("2020/05/31"), ymax = 500),
            data = NULL, alpha = 0.5, fill = "gray75") +
  geom_vline(aes(xintercept = ymd("2020/02/28")), linetype = "solid", color = "gray75") +
  geom_point(aes(x = Fecha,  y = ILE), color = "forestgreen", size = 2) +
  geom_point(aes(x = Fecha,  y = ILE), color = "white", size = 1) +
  geom_spline(aes(x = Fecha, y = ILE), color = "black", df = 20, size = 1) +
  geom_label(aes(x = ymd("2020/03/23") +  (ymd("2020/05/31") - ymd("2020/03/23"))/2),
             y = 400, label = "Jornada Nacional\nde Sana Distancia", size = 3) +
  annotate("text", x = ymd("2020/02/28") - days(5), y = 100, hjust = 0.5, angle = 90, vjust = 0,
           label = "[Report of first COVID-19 case in Mexico]", size = 2.5, color = "gray50") +
  theme_bw() +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  labs(
    x        = "",
    y        = "Weekly number of <span style='color:forestgreen'>**ILE**</span>",
    title    = "Total number of <span style='color:forestgreen'>**legal interruptions of pregnancy (ILE)**</span> per week registered in Mexico City's public health services",
    subtitle = "_Trend line corresponds to spline smoothing_",
    caption  = "**Source:** Secretaría de Salud de la Ciudad de México"
  ) +
  coord_cartesian(ylim = c(0, 450)) +
  theme(
    plot.title    = element_markdown(),
    plot.caption  = element_markdown(),
    plot.subtitle = element_markdown(),
    axis.title.y  = element_markdown(),
  )
ggsave("images/ILE_Mexico_City.pdf", width = 10, height = 5)