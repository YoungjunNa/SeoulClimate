library(tidyverse)
library(lubridate)
library(janitor)

df_seoul <- readxl::read_excel("data/ASOS.xlsx") %>% filter(지점명 == "서울")
df_seoul <- df_seoul %>% clean_names(ascii = FALSE)
df_seoul <- df_seoul %>% select(일시, 평균기온_c)

df_seoul_before_1940 <- df_seoul %>% filter(일시 < 1940) %>% pull(평균기온_c) %>% mean(na.rm = TRUE)
df_seoul_before_1940

df_seoul_after_1940 <- df_seoul %>% filter(일시 >= 1940) %>%
  filter(일시 < 2024) %>%
  mutate(increases = 평균기온_c - df_seoul_before_1940)

df_seoul_after_1940 %>% arrange(desc(일시))

df_seoul_after_1940 %>%
  ggplot(aes(일시, increases, fill = increases)) +
  geom_line(size = 1.3, color = "white") +
  geom_area(stat = "identity", fill = "#EE4E4E") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1940, 2020, 20)) +
  scale_y_continuous(
    breaks = seq(-2, 3, 1),  # Customize the breaks as needed
    labels = function(y) paste0(ifelse(y > 0, "+", ""), sprintf("%.1f", y), "°C")  # Add + sign and °C with one decimal place
  ) +
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "#203A4D"),
        panel.border = element_rect(color = "#203A4D", fill = NA),
        plot.background = element_rect(fill = "#203A4D"),
        panel.grid.major = element_line(color = "#2D5D6B"),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white"),
        plot.caption = element_text(color = "grey"),
        plot.title = element_text(color = "white", size = 20),
        plot.subtitle = element_text(color = "grey"),
        ) +
  labs(title = "Seoul surface temperature increase above pre-industrial",
       subtitle = "12-month running mean anomalies relative to the 1907-1939 average \nData: Korea Meteorological Administration, ASOS data.",
       caption = "", x = "", y = "")
