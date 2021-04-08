library(dplyr)
library(tidyr)
library(showtext)
library(ggfx)
library(ggplot2)

font_add_google("Alegreya", "Aleg")
font_add_google("Bree Serif", "Bree")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load(2018, week = 24)

cats_dogs <- tuesdata$cats_vs_dogs %>%
  mutate(cat_dog_pop = dog_population + cat_population) %>%
  arrange(cat_dog_pop) %>%
  mutate(state = toupper(state))

state_factor <- cats_dogs$state
  
cats_dogs$cat_population <- cats_dogs$cat_population*-1


cats_dogs_long <- cats_dogs %>%
  select(state, dog_population, cat_population) %>%
  pivot_longer(
    cols=-state,
    names_to = "animal",
    values_to = "population"
  ) %>%
  mutate(state = factor(state, levels = state_factor))


p <- cats_dogs_long %>%
  ggplot(aes(x = population, y = state, fill = animal)) +
  as_reference(
  geom_bar(data = subset(cats_dogs_long, animal == "cat_population"), fill="#E94F64", stat = "identity", width=1),
  id="Cat")+ 
  geom_bar(data = subset(cats_dogs_long, animal == "dog_population"), fill="#46BCDE", stat = "identity", width=1) +
  with_blend(
    geom_text(aes(x=0, hjust=0.5, label = state), size=2, color="#E94F64", fontface="bold", family="Aleg"),
    bg_layer="Cat",
    blend_type="xor"
    ) +
  geom_text(aes(x=-6000, y=51, label="CATS"), color="#E94F64", size=24, family="Aleg") +
  geom_text(aes(x=5400, y=51, label="DOGS"), color="#46BCDE", size=24, family="Aleg") +
  geom_text(aes(x=-6800, y=54.5, label=format(sum(cats_dogs$cat_population)*-1, big.mark = ",", scientific=FALSE)), color="#E94F64", size=18, family="Aleg") +
  geom_text(aes(x=6400, y=54.5, label=format(sum(cats_dogs$dog_population), big.mark = ",", scientific=FALSE)), color="#46BCDE", size=18, family="Aleg") +
  geom_text(aes(x=-7200, y=65, hjust=0, label="CAT AND DOG POPULATIONS"), color="#D6DBDE", size=20, family="Bree") +
  geom_text(aes(x=2800, y=62, label="ACROSS AMERICAN STATES"), color="#D6DBDE", size=20, family="Bree") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = 'none',
    plot.background = element_rect(fill="#333333"),
    plot.caption = element_text(family="Aleg", color="#D6DBDE", size=16)
  ) +
  coord_cartesian(ylim=c(0,65)) +
  labs(caption = "Data: #TidyTuesday/data.world | #30DayChartChallenge")


ggsave(
  "day8-animals.png", p, width=6, height=4, dpi = 300, limitsize = FALSE)


# d=data.frame(x=c(0,0,1, 0,1,1),
#              y=c(0,1,1, 0,0,1), 
#              t=c('a', 'a', 'a',  'b', 'b', 'b'), 
#              r=c(1,2,3, 4,5,6))
# 
# 
# ggplot() +
#   geom_text(aes(label="VS", x=0.5, y=0.5),
#             fontface="bold", size=24) +
#   geom_text(aes(label="LAND", x=0, y=1),
#             fontface="bold", size=24, hjust=0, vjust=1) +
#   geom_text(aes(label="SEA", x=1, y=0),
#             fontface="bold", size=24, hjust=1, vjust=0) +
#   geom_polygon(data=d, mapping=aes(x=x, y=y, group=t, colour=t), alpha = 0.3) +
#   theme_void()
