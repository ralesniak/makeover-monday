library(tidyverse)
library(showtext)
library(ggtext)


# Fonts -------------------------------------------------------------------

font_add_google(name = "Roboto", family = "roboto")
font_add_google(name = "Rubik", family = "rubik") #rubik light
showtext_auto()


# APTA Data ---------------------------------------------------------------

#original data from https://www.apta.com/wp-content/uploads/APTA-Brief-Agency-Survey-Sept-2020.pdf

apta_data <- tibble::tribble(
                                    ~category, ~value,
                           "Eliminate Routes",   0.38,
                     "Reduce Days of Service",   0.20,
                                "Cut Service",   0.61,
                             "Furlough Staff",   0.23,
                              "Lay Off Staff",   0.31,
                             "Increase Fares",   0.17,
  "Delay, Defer, or Cancel Vehicle Purchases",   0.33,
   "Delay, Defer, or Cancel Capital Projects",   0.45
  )


# Plot --------------------------------------------------------------------

bg_gray <- "#F5F5F5"
axis_line_gray <- "#B8B8B8"
other_gray <- "#474747"

plot_data <- apta_data %>%
  mutate(
    highlight = case_when(
      category == "Cut Service" ~ "#650533",
      str_detect(category, "Capital") ~ "#094279",
      TRUE ~ 	other_gray
    ),
    category_label = glue::glue("<span style ='color:{highlight}'>{category}</span>"),
    category_label = fct_reorder(category_label, value)
  )

new_plot <- plot_data %>%
  ggplot(aes(x = value, y = category_label, fill = highlight)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_identity() +
  labs(title = "Without emergency funding, your transit agency might<br><span style='color:#650533'>cut service</span> or <span style='color:#094279'>delay infrastructure projects</span>",
       subtitle = "Percent of transit agencies that said they were considering the following actions",
       caption = "n = 128<br>Source: APTA.com, Sep 2020") +
  theme_classic(base_size = 12) +
  theme(
    plot.background = element_rect(fill = bg_gray),
    panel.background = element_rect(fill = bg_gray,
                                    colour = bg_gray),
    text = element_text(family = "rubik"),
    axis.line = element_line(color = axis_line_gray),
    plot.title.position = "plot",
    plot.title = element_markdown(family = "roboto", face = "bold"),
    plot.subtitle = element_markdown(family = "roboto"),
    plot.caption = element_markdown(), 
    axis.title = element_blank(),
    axis.text.y = element_markdown(),
    legend.position = "none"
    )


# Export Plot -------------------------------------------------------------

png(filename = "new_plot.png",
    width = 600,
    height = 335)
new_plot
dev.off()