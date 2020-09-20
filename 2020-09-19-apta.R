library(tidyverse)
library(showtext)
library(ggtext)


# Fonts -------------------------------------------------------------------

font_add_google(name = "Roboto", family = "roboto")
font_add_google(name = "Rubik", family = "rubik")
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

# colors
bg_gray <- "#F5F5F5"
axis_line_gray <- "#B8B8B8"
other_gray <- "#474747"

highlight_1 <- "#650533" #apta maroon
highlight_2 <- "#094279" #apta blue

#add in new columns re: color and labels

plot_data <- apta_data %>%
  mutate(
    highlight = case_when(
      category == "Cut Service" ~ highlight_1,
      str_detect(category, "Capital") ~ highlight_2,
      TRUE ~ 	other_gray
    ),
    category_label = glue::glue("<span style ='color:{highlight}'>{category}</span>"),
    category_label = fct_reorder(category_label, value)
  )

# the chart!
new_plot <- plot_data %>%
  ggplot(aes(x = value, y = category_label, fill = highlight)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_identity() +
  labs(title = glue::glue("Without emergency funding from Congress, your transit 
                          agency<br>might <span style='color:{highlight_1}'>cut 
                          service</span> or <span style='color:{highlight_2}'>delay 
                          infrastructure projects</span>"),
       subtitle = "Percent of transit agencies that said they were considering the following actions",
       caption = "n = 128<br>Source: APTA.com, Sep 2020") +
  theme_classic(base_size = 12) +
  theme(
    #align title left
    plot.title.position = "plot",
    #colors and fonts
    text = element_text(family = "rubik"),
    plot.background = element_rect(fill = bg_gray),
    panel.background = element_rect(fill = bg_gray,
                                    colour = bg_gray),
    axis.line = element_line(color = axis_line_gray),
    plot.title = element_markdown(family = "roboto", face = "bold"),
    plot.subtitle = element_markdown(family = "roboto"),
    plot.caption = element_markdown(), 
    axis.text.y = element_markdown(),
    #no annoyting stuff
    axis.title = element_blank(),
    legend.position = "none"
    )


# Export Plot -------------------------------------------------------------

# {showtext} requires these steps, no ggsave available

png(filename = "new_plot.png",
    width = 600,
    height = 335)
new_plot
dev.off()