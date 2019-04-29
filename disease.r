# packages ----
library(stats)
library(gridExtra)
library(tidyverse)



# read data ----

# diseased carrot locations
carrots =
  read_csv("data/carrot locs wtm.csv") %>%
  filter(AY == 1) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(x = WTM_X, y = WTM_Y)

# front and back stake locations
stakes =
  read_csv("data/stakes.csv") %>%
  mutate_if(is.character, as.factor)

# read AY incidence data
ay =
  read_csv("data/incidence.csv") %>%
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



# disease incidence ----

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



# subgroup plots ----

subgroups = read_csv("data/subgroups.csv") %>%
  mutate_if(is.character, as.factor)
str(subgroups)

# cumulative subgroups over time
subgroup_count = subgroups %>%
  group_by(Date, Subgroup) %>%
  summarise(n = n()) %>%
  spread(Subgroup, n, fill = 0) %>%
  gather(Subgroup, n, `1A`:`1B`) %>%
  ungroup() %>%
  arrange(Subgroup) %>%
  group_by(Subgroup) %>%
  mutate(cumn = cumsum(n))
subgroup_count

library(gridExtra)
p = subgroup_count %>%
  ggplot(aes(x = Date, y = cumn, fill = Subgroup)) +
  coord_cartesian(expand = F)
grid.arrange(p + geom_area() +
               labs(title = "Total AY+ carrots by subgroup",
                    y = "Number of samples"),
             p + geom_area(position = "fill") +
               labs(y = "Proportion"))

