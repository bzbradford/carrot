library(tidyverse)
library(stats)
library(Cairo)


# read diseased carrot locations
carrots =
  read_csv("in/carrot locs wtm.csv") %>%
  filter(AY == 1) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(x = WTM_X, y = WTM_Y)

# front and back stake locations
stakes =
  read_csv("in/stakes.csv") %>%
  mutate_if(is.character, as.factor)

# read AY incidence data
ay =
  read_csv("in/incidence.csv") %>%
  mutate_if(is.character, as.factor)



# plot carrot locations
ay.pts =
  carrots %>%
  mutate(x = WTM_X - min(WTM_X),
         y = WTM_Y - min(WTM_Y)) %>%
  ggplot(aes(x = x, y = y, color = Density)) +
  geom_point() +
  labs(title = "AY+ Carrot Locations",
       x = "Meters",
       y = "Meters")
ay.pts
ggsave("ay pts.png", ay.pts)



# mean AY incidence by density and plot location
ay.summary =
  ay %>%
  group_by(Date, Density, Location) %>%
  summarise(n = n(),
            mean = mean(Incidence),
            sd = sd(Incidence),
            se = sd(Incidence)/sqrt(n()))


# bar plot of incidence over time by groups
ay.barplot =
  ay.summary %>%
  ggplot(aes(x = Date,
             y = mean,
             ymin = mean - se,
             ymax = mean + se,
             fill = Location)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity",
           color = "black",
           width = 10,
           position = "dodge") +
  geom_linerange(position = position_dodge(10)) +
  facet_grid(. ~ Density) +
  labs(y = "Mean AY incidence", x = "")
ay.barplot
ggsave("ay barplot.png", ay.barplot)


# disease incidence over time, line plot
ay.lineplot =
  ay.summary %>%
  ggplot(aes(x = Date,
             y = mean,
             ymin = mean - se,
             ymax = mean + se,
             group = paste(Density, Location))) +
  geom_line(aes(linetype = Density), size = .5) +
  geom_point(aes(shape = Location), size = 2.5) +
  geom_errorbar(width = 3, size = .25) +
  scale_shape_discrete() +
  scale_color_manual(values = c("black", "darkgrey")) +
  labs(y = "Mean AY incidence per plot", x = "Sampling date")
ay.lineplot
ggsave("ay lineplot.png", ay.lineplot)



#### kmeans grouping ####

# carrot loc chart
km = kmeans(cbind(carrots$WTM_X, carrots$WTM_Y), centers = 20)
carrots %>%
  ggplot(aes(x = WTM_X, y = WTM_Y)) +
  geom_point(aes(color = as.factor(km$cluster)))



#### distmap images ####

# define WTM coordinate extents for carrot field
fieldExtent =
  data.frame(x = c(556040, 556107),
             y = c(405070, 405119))

# create ppp object from carrot locations and field extent
carrot.ppp =
  carrots %>%
  ppp(x = .$x,
      y = .$y,
      xrange = fieldExtent$x,
      yrange = fieldExtent$y)

# generate distmap for whole field
carrot.distmap = distmap(carrot.ppp)
png("distmaps/field.png")
image(ay.distmap, main = "AY distmap")
dev.off()

# generate distmaps for each plot
for (plot in levels(carrots$PlotID)) {
  image =
    carrots %>%
    filter(PlotID == plot) %>%
    ppp(x = .$x, y = .$y,
        xrange = range(.$x) + c(-1, 1),
        yrange = range(.$y) + c(-1, 1)) %>%
    distmap()
  png(paste0("distmaps/", plot, ".png"))
  image(image, main = plot)
  dev.off()
}
