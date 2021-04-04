library(rtweet)
library(dplyr)
library(extrafont)
library(ggplot2)
library(waffle)

tweets_d <- search_tweets("#Dumbledore",
                          n = 10000) %>%
  mutate(wizard = "DUMBLEDORE",
         count = 1) %>%
  filter(created_at >= "2021-03-28",
         created_at <= "2021-04-03")

tweets_g <- search_tweets("#Gandalf",
                          n = 10000) %>%
  mutate(wizard = "GANDALF",
         count = 1) %>%
  filter(created_at >= "2021-03-28",
         created_at <= "2021-04-03")


tweets_all <-
  tweets_d %>%
  select(wizard, count) %>%
  rbind(
    tweets_g %>%
      select(wizard, count)
  ) %>%
  group_by(wizard) %>%
  summarize(sum = sum(count)/10)


wizard_plot <- tweets_all %>%
  ggplot(aes(label = wizard, values = sum, color = wizard)) +
  geom_pictogram(n_rows = 10,
                 family = "Font Awesome 5 Free Solid",
                 flip = TRUE,
                 size = 6) +
  scale_color_manual(
    name = NULL,
    labels = c(
      "DUMBLEDORE",
      "GANDALF"
    ),
    values = c(
      "#BBC2C2",
      "#BBC2C2"
    )) +
  scale_label_pictogram(
    name = NULL,
    labels = c(
      "DUMBLEDORE",
      "GANDALF"
    ),
    values = c(
      "quidditch",
      "hat-wizard"
    )
  ) +
  facet_wrap(~wizard) +
  coord_equal() +
  theme_enhance_waffle() +
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "#09143C"),
        plot.background = element_rect(fill = "#09143C"),
        legend.background = element_rect(fill = "#09143C"),
        legend.key = element_rect(fill = "#09143C"),
        strip.background = element_rect(fill = "#09143C"),
        legend.position = "none",
        strip.text = element_text(size = 20, hjust = 0.5, vjust = 1, family = 'Felix Titling', color = "#BBC2C2",
                                  margin = margin(b = 30))) + 
  plot_annotation(
    title = "THE GREAT WIZARD SHOWDOWN",
    subtitle = "Number of times #Dumbledore and #Gandalf hashtags used on twitter in the last week. Each icon represents 10 tweets.",
    caption = "Data: Twitter | #30DayChartChallenge") &
  theme(rect = element_rect(fill = "#09143C"),
        panel.background = element_rect(fill = "#09143C", color = "#09143C"),
        plot.background = element_rect(fill = "#09143C", color = "#09143C"),
        text = element_text(family = 'Felix Titling'),
        plot.title = element_text(color = "#BBC2C2", face = "bold", size = 22, hjust = .5),
        plot.subtitle = element_text(color = "#BBC2C2", size = 14, hjust = .5, family = "Vivaldi"),
        plot.caption = element_text(color = "#BBC2C2", size = 9, hjust = .5)
  )


ggsave(
  "day4-magic.png", wizard_plot, width = 8, height=6, dpi = 300, limitsize = FALSE)
