library(dplyr)
library(ggplot)
library(ggtext)
library(treemapify)
library(packcircles)
library(ggraph)
library(igraph)
library(patchwork)
library(cowplot)
library(extrafont)
library(ggpubr)

board_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")


top_100_games <- board_games %>%
  arrange(-average_rating) %>%
  head(100) %>%
  mutate(value = 1) %>%
  arrange(year_published) %>%
  mutate(year_published = factor(year_published))


waffle <-
  top_100_games %>%
  ggplot(aes(fill = year_published, values = value)) +
  coord_equal() +
  geom_waffle(flip = TRUE, size = 1, colour = "white") +
  theme_void() +
  ggtitle("WAFFLE") +
  scale_fill_viridis(discrete=TRUE)

leg <- get_legend(waffle + guides(fill=guide_legend(title="Year Published", direction = "horizontal", title.position = "top",
                                                    label.position="bottom", nrow = 1, label.hjust = 0.5, label.vjust = 0.5))) %>%
  as_ggplot()

waffle <- waffle +
  theme(legend.position="none",
        text=element_text(size=12,  family="Verdana Pro Light"))


pie <-
  top_100_games %>%
  ggplot(aes(x="", y=value, fill=year_published)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  ggtitle("PIE") +
  scale_fill_viridis(discrete=TRUE) +
  theme(legend.position="none",
        text=element_text(size=12,  family="Verdana Pro Light")) 


grouped_data <-
  top_100_games %>%
  group_by(year_published) %>%
  summarise(count = n()) %>%
  mutate(fraction = count/sum(count),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax,n=-1)))



donut <-
  grouped_data %>%
  ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=year_published)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(2,4)) +
  theme_void() +
  ggtitle("DONUT") +
  scale_fill_viridis(discrete=TRUE) +
  theme(legend.position="none",
        text=element_text(size=12,  family="Verdana Pro Light"))

treemap <-
  grouped_data %>%
  ggplot(aes(area = count, fill = year_published)) +
  geom_treemap() +
  ggtitle("TREEMAP") +
  scale_fill_viridis(discrete=TRUE) +
  theme(legend.position="none",
        text=element_text(size=12,  family="Verdana Pro Light"))


stacked_bar <-
  grouped_data %>%
  mutate(x = 1) %>%
  ggplot(aes(x = x, y=fraction, fill=year_published)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete=TRUE) +
  theme_void() +
  ggtitle("STACKED BAR") +
  theme(legend.position="none",
        text=element_text(size=12,  family="Verdana Pro Light"))


packing <- circleProgressiveLayout(grouped_data$count)
circle_data <- cbind(grouped_data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

circle_plot <-
  dat.gg %>%
  ggplot() +
  geom_polygon(aes(x,y,group=id, fill=as.factor(id)), colour="black", alpha =0.6) +
  # geom_text(data = circle_data, aes(x, y, size=8, label=year_published)) +
  scale_size_continuous(range=c(1,4)) +
  theme_void() +
  theme(legend.position="none",
        text=element_text(size=12,  family="Verdana Pro Light")) +
  coord_equal() +
  ggtitle("CIRCULAR PACKING") +
  scale_fill_viridis(discrete=TRUE) +
  theme(legend.position="none")
  

edge_list1 <- top_100_games %>%
  select(value, year_published) %>%
  mutate(year_published = as.character(year_published)) %>%
  unique() %>%
  rename(from=value, to=year_published)

edge_list2 <- top_100_games %>%
  select(year_published, game_id) %>%
  unique() %>%
  rename(from=year_published, to=game_id)

edge_list=rbind(edge_list1, edge_list2)

name <- unique(c(as.character(edge_list$from), as.character(edge_list$to)))

vertices <-
  data.frame(
    name = name
  ) %>%
  left_join(
    top_100_games %>%
      select(game_id, year_published) %>%
      mutate(game_id = as.character(game_id)),
      by=c("name" = "game_id")
  )

mygraph <- graph_from_data_frame(edge_list, vertices=vertices)
dendrogram <- ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point(aes(filter=leaf, color=year_published), alpha=0.6) +
  scale_color_viridis(discrete=TRUE) +
  theme_void() +
  theme(legend.position="none",
        text=element_text(size=12,  family="Verdana Pro Light")) +
  ggtitle("DENDROGRAM")


header <- ggdraw() +
  draw_text("COMPARISONS: PART-TO-WHOLE", x= 0, y = 0.8, hjust=0, size = 20, family = "Verdana") +
  draw_text("Publication year of top 100 rated board games on BoardGameGeek",
            x = 0, y = 0.4, hjust=0, size = 10, family = "Verdana Pro Light")

footer <- ggdraw() +
  draw_text("#TidyTuesday BGG data | #30DayChartChallenge", x= 1, y = 0, hjust=1, size = 10, family = "Verdana Pro Light") 


header / (dendrogram | treemap | circle_plot) / (pie | donut | waffle | stacked_bar) / (leg | footer) + plot_layout(height = c(0.5, 2, 2, 1))
