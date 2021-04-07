library(emojifont)
library(showtext)
library(readxl)
library(ggtext)
library(dplyr)
library(ggplot2)
library(ggridges)
library(ggExtra)
library(ggrepel)
library(patchwork)

font_add_google("Vollkorn", "Volkorn")
font_add_google("Raleway", "Raleway")

private_space <- read_excel("Distributions/Day 7 - Physical/osprivateoutdoorspacereferencetables.xlsx", sheet = "LAD gardens")

public_space <- read_excel("Distributions/Day 7 - Physical/ospublicgreenspacereferencetables.xlsx", sheet = "LAD Parks and Playing Fields")


cols <- private_space[1,20:24]
colnames(cols) <- cols[1,]
cols <- private_space[1, 5] %>%
  cbind(cols)


private_space_total <- 
  private_space[2:NROW(private_space), c(5, 20:24)]

colnames(private_space_total) <- colnames(cols)


public_private_space <-
  public_space %>%
  left_join(
    private_space_total,
    by = "LAD code"
  ) %>%
  filter(!`Region name` %in% c("Scotland", "Wales"),
         is.na(`Region name`) == FALSE) %>%
  mutate(`Percentage of adresses with private outdoor space` = as.numeric(`Percentage of adresses with private outdoor space`))
  

public_private_space %>%
  ggplot() +
  geom_density_ridges(
    aes(x=`Percentage of adresses with private outdoor space`,
        y = `Region name`,
        group = `Region name`))

public_private_space %>%
  ggplot() +
  geom_density_ridges(
    aes(x=`Average distance to nearest Park, Public Garden, or Playing Field (m)`,
        y = `Region name`,
        group = `Region name`))


public_private_space %>%
  ggplot() +
  geom_point(aes(x=`Percentage of adresses with private outdoor space`, y=`Average distance to nearest Park, Public Garden, or Playing Field (m)`)) +
  facet_wrap(~`Region name`)

p <- public_private_space %>%
  ggplot(aes(x=`Percentage of adresses with private outdoor space`,
             y=`Average distance to nearest Park, Public Garden, or Playing Field (m)`,
             color=ifelse(`Region name` == "London", "#F7FCF5", "#74C476"),
             label=`LAD name`)) +
  geom_point(alpha = 0.5) +
  # geom_text(data = annotations, aes(x=x, y=y, label=label), nudge_x=0.1, nudge_y=100) +
  geom_text_repel(
    data = subset(public_private_space, `Percentage of adresses with private outdoor space`<0.5),
    family = "Raleway"
  )  +
  # geom_text(data = annotations, aes(x=x, y=y, label=label), nudge_x=0.1, nudge_y=100) +
  geom_text_repel(
    data = subset(public_private_space, `Average distance to nearest Park, Public Garden, or Playing Field (m)`>800),
    family = "Raleway"
  ) + 
  # labs(title = "Households in <span style='color:#F7FCF5;'>London local authorities</span> are more likely to be without a private garden than households in <span style='color:#74C476;'>other local authorities</span>") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    plot.background = element_rect(fill="#005A32",
                                   colour = "#005A32"),
    axis.text = element_text(family = "Raleway",
                             color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Raleway")
  ) +
  scale_color_identity() +
  scale_x_continuous(labels = scales::percent_format()) +
  ylab("Average distance to nearest Park, Public Garden,\n or Playing Field (m)")


p_gg <- ggMarginal(p, type = "histogram", fill = "#C7E9C0")

text <- ggplot() +
  geom_fontawesome("fa-tree", x=1.7, y=2, vjust=1, color='green', size = 10) +
  geom_fontawesome("fa-tree", x=1.75, y=2, vjust=0.9, color='lightgreen', size = 8) +
  geom_fontawesome("fa-tree", x=1.65, y=1.95, vjust=1.1, color='lightgreen', size = 6) +
  geom_fontawesome("fa-tree", x=1.62, y=2, vjust=1.1, color='green', size = 4) +
  geom_fontawesome("fa-tree", x=1.58, y=2, vjust=1.1, color='#74C476', size = 12) +
  geom_text(aes(label = "WHERE'S THE GREEN SPACE AT?", x=0, y=2, hjust=0, vjust=1), color = "#E5F5E0", size = 10, family = "Volkorn", fontface = "bold") +
  geom_fontawesome("fa-tree", x=0.03, y=1.8, color='#238B45', size = 12) +
  geom_fontawesome("fa-tree", x=0.1, y=1.75, color='#74C476', size = 10) +
  geom_fontawesome("fa-tree", x=0.15, y=1.8, color='lightgreen', size = 8) +
  geom_text(aes(label = "Access to parks & private outdoor spaces across England local authorities",
                x=0.2, y=1.8, hjust=0, vjust=1), family = "Raleway", color="#238B45", fontface="bold") +
  geom_richtext(aes(
    label = "Households in <span style='color:#F7FCF5;'>London local authorities</span> are close to parks, but are more likely to be without a private garden<br> than households in <span style='color:#74C476;'>other local authorities</span>",
    x=0, y=1.6, hjust=0, vjust=1), fontface = "plain", family = "Raleway",
    # remove label background and outline
    fill = NA, label.color = NA,
    # remove label padding, since we have removed the label outline
    label.padding = grid::unit(rep(0, 4), "pt")) +
  xlim(c(0,2)) +
  ylim(c(1.4,2)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#005A32",
                                       color = "#005A32"))

final_p <- text/p_gg + 
  plot_annotation(
    caption = "Data: ONS | #30DayChartChallenge"
  ) +
  plot_layout(heights = c(2,4)) &
  theme(plot.background = element_rect(fill = "#005A32",
                                       color = "#005A32"),
        text = element_text(family = "Raleway"))
