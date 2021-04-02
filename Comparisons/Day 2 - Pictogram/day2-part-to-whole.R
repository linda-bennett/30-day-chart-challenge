library(dplyr)
library(extrafont)
library(waffle)
library(patchwork)

days_climb <- data.frame(
  date = seq(as.Date("2020-12-20"), as.Date("2021-04-12"), by="days")
) %>% 
  mutate(label =
           case_when(date <= Sys.Date() ~ "Days I've spent waiting to climb again",
                     TRUE ~ "Days left until I can climb again")
         ) %>%
  group_by(label) %>%
  summarise(count = n()) %>%
  mutate(label =  factor(label, levels = c(
    "Days left until I can climb again",
    "Days I've spent waiting to climb again")))


picto <- days_climb %>%
  arrange(label) %>%
  ggplot(aes(label = label, values = count, color = label)) +
  geom_pictogram(n_rows = 10,
                 family = "Font Awesome 5 Free Solid",
                 flip = TRUE) +
  scale_color_manual(
    name = NULL,
    labels = c(
      "DAYS LEFT UNTIL I CAN CLIMB AGAIN",
      "DAYS I'VE SPENT WAITING TO CLIMB AGAIN"
    ),
    values = c(
      "#2F3C7E",
      "#979ebf"
    )) +
  scale_label_pictogram(
    name = NULL,
    labels = c(
               "DAYS LEFT UNTIL I CAN CLIMB AGAIN",
               "DAYS I'VE SPENT WAITING TO CLIMB AGAIN"
               ),
    values = c(
      "calendar-alt",
      "calendar-alt"
    )
  ) +
  coord_equal() +
  theme_enhance_waffle() +
  theme(legend.key.height = unit(2, "line"),
        legend.text = element_text(size = 8, hjust = 0, vjust = 0.75, family = "Verdana"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "#FBEAEB"),
        plot.background = element_rect(fill = "#FBEAEB"),
        legend.background = element_rect(fill = "#FBEAEB"),
        legend.position = "bottom",
        legend.direction = "vertical")


header <- ggdraw() +
  draw_text("LIFTING LOCKDOWN", x= 0.5, y = 0.8, size = 20, family = "Verdana") +
  draw_text("11 days until climbing centrEs and gyms reopen in England (hopefully)", 
            x = 0.5, y = 0.6, size = 12, family = "Verdana Pro Light") +
  theme(panel.background = element_rect(fill = "#FBEAEB"),
        plot.background = element_rect(fill = "#FBEAEB"))


final_picto <- picto + plot_annotation(
  title = "LIFTING LOCKDOWN",
  subtitle = "10 days until climbing centres and gyms reopen in England (hopefully)",
  caption = "Data: self collected (based on date London went into Tier 4) | #30DayChartChallenge") &
  theme(rect = element_rect(fill = "#FBEAEB"),
        panel.background = element_rect(fill = "#FBEAEB", color = "#FBEAEB"),
        plot.background = element_rect(fill = "#FBEAEB", color = "#FBEAEB"),
        text = element_text(family = "Verdana"),
        plot.title = element_text(color = "#2F3C7E", face = "bold", size = 22, hjust = .5),
        plot.subtitle = element_text(color = "#2F3C7E", size = 11, hjust = .5),
        plot.caption = element_text(color = "#2F3C7E", size = 9, hjust = .5),
        plot.margin = margin(t = 15, b = 15, l = 15, r = 15),
  )


ggsave(
  "day2-pictogram.png", final_picto, width = 6, height=9, dpi = 300, limitsize = FALSE)
