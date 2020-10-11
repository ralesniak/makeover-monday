library(tidyverse)
library(lubridate)
library(showtext)
library(ggtext)


# Fonts -------------------------------------------------------------------

font_add_google(name = "Spartan", family = "spartan")
font_add_google(name = "Roboto", family = "roboto")
showtext_auto()

# Colors ------------------------------------------------------------------

bg_gray <- "#f4f4f4"
axis_line_gray <- "#B8B8B8"

highlight_1 <- "#1d3557"
highlight_2 <- "#e63946" 

# FHWA Data ---------------------------------------------------------------

## data source
## https://www.fhwa.dot.gov/policyinformation/travel_monitoring/20jultvt/20jultvt.pdf
## data in BILLIONS

vmt_data <- tibble::tribble(
               ~category, ~year, ~JAN, ~FEB, ~MAR,  ~APR,  ~MAY, ~JUN,  ~JUL, ~AUG, ~SEP,  ~OCT, ~NOV, ~DEC,
      "Rural Interstate", 2019L, 18.7, 16.8, 20.9,    22,  23.1,   23,  25.4, 24.2, 21.5,  22.4, 20.5, 21.7,
  "Rural Other Arterial", 2019L, 27.6, 25.7,   31,  32.3,  33.6, 33.8,  36.7, 35.2, 32.9,  33.9, 30.3, 31.2,
           "Other Rural", 2019L, 25.1, 22.8,   28,    30,  30.7, 30.5,  32.7, 31.4, 29.2,  30.3, 26.8, 27.5,
      "Urban Interstate", 2019L, 43.9, 39.8, 48.1,  48.9,  50.3, 50.5,  49.9, 49.8, 47.9,  49.3, 47.1, 49.7,
  "Urban Other Arterial", 2019L, 90.4,   83, 97.8, 100.6, 100.5, 97.4, 102.5, 99.9, 95.8, 101.8, 92.5, 98.3,
           "Other Urban", 2019L, 42.5, 38.7, 45.7,  47.6,  47.8, 45.8,  48.4, 46.2, 44.5,  46.1, 43.3, 45.7,
      "Rural Interstate", 2020L, 19.2, 17.4, 16.8,  12.2,  16.9, 19.5,  22.2,   NA,   NA,    NA,   NA,   NA,
  "Rural Other Arterial", 2020L, 28.3, 26.4, 25.8,  20.4,  26.4, 30.6,  33.5,   NA,   NA,    NA,   NA,   NA,
           "Other Rural", 2020L, 25.6, 23.3, 23.5,    20,  24.6, 27.9,  30.6,   NA,   NA,    NA,   NA,   NA,
      "Urban Interstate", 2020L, 44.9, 40.7, 37.8,  26.9,    35, 41.8,  42.5,   NA,   NA,    NA,   NA,   NA,
  "Urban Other Arterial", 2020L,   92, 84.6, 78.6,  59.5,  73.7, 83.8,  90.4,   NA,   NA,    NA,   NA,   NA,
           "Other Urban", 2020L, 43.6, 39.5, 37.6,  29.4,  36.1, 40.3,  43.2,   NA,   NA,    NA,   NA,   NA
  )

#reshape from wide to tidy
vmt_reshaped <- vmt_data %>%
  pivot_longer(cols = -c("category", "year"), names_to = "month_char") %>%
  mutate(year_month = ymd(glue::glue("{year}-{month_char}-01"))) %>%
  filter(!is.na(value)) %>%
  #create a general category to summarize
  mutate(general_category = if_else(str_detect(category, "Rural"), "Rural", "Urban"))

vmt_chart <- vmt_reshaped %>%
  #split out into two lines to compare year-to-year
  group_by(year_month) %>%
  summarize(value = sum(value)) %>%
  mutate(month = month(year_month, label = TRUE),
         year = as.character(year(year_month)),
         chart_color = if_else(year == "2019", highlight_1, highlight_2)) %>%
  ggplot(aes(x = month, y = value, color = chart_color, group = year)) +
  geom_line(size = 1) +
  scale_color_identity() +
  scale_y_continuous(limits = c(100, NA),
    labels = scales::number_format(suffix = "B")) +
  labs(title = "Traffic is stabilizing after COVID-related travel dropped",
       subtitle = glue::glue("Monthly vehicle miles (urban and rural), 
       <b style='color:{highlight_1}'>2019</b> compared to <b style='color:{highlight_2}'>2020</b>"),
       caption = "Source: Federal Highway Administration, July 2020") +
  theme_classic(base_size = 16) +
  theme(
    #align title left
    plot.title.position = "plot",
    #colors and fonts
    text = element_text(family = "roboto"),
    plot.background = element_rect(fill = bg_gray),
    panel.background = element_rect(fill = bg_gray,
                                    colour = bg_gray),
    axis.line = element_line(color = axis_line_gray),
    plot.title = element_text(family = "spartan", face = "bold"),
    plot.subtitle = element_markdown(family = "spartan"),
    axis.text.y = element_text(),
    #no annoying stuff
    axis.title = element_blank(),
    legend.position = "none"
  )


# Export in Twitter Size --------------------------------------------------

png(filename = "vmt_chart.png",
    width = 600,
    height = 335)
vmt_chart
dev.off()