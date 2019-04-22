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



# subgroups ----

subgroups = read_csv("in/subgroups.csv") %>%
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



# effectors ----

effectors = read_csv("in/effectors.csv")

# gather SAPs
eff_long = effectors %>%
  gather(Effector, Reads, SAP05:SAP68) %>%
  mutate(CopyNumber = Reads / R16)

# bar plot overall
eff_long %>%
  group_by(Subgroup, Effector) %>%
  summarise(CopyNumber.mean = mean(CopyNumber),
            CopyNumber.sd = sd(CopyNumber)) %>%
  ggplot(aes(x = Effector,
             y = CopyNumber.mean,
             fill = Subgroup)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Effector abundance by subgroup",
       y = "Number of copies per R16 read",
       x = "Collection date")

eff_long %>% left_join(subgroup_count)



# bar plot by date
p = eff_long %>%
  filter(Subgroup != "1A1B") %>%
  group_by(Date, Subgroup, Effector) %>%
  summarise(CopyNumber.mean = mean(CopyNumber),
            CopyNumber.sd = sd(CopyNumber)) %>%
  ggplot(aes(x = factor(Date,
                        labels = unique(format(Date, "%b %d")),
                        ordered = T),
             y = CopyNumber.mean,
             fill = Subgroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p + geom_col(position = "stack") +
  facet_wrap(~Effector, scales = "free_y") +
  labs(title = "Effector abundance by subgroup",
       x = "Collection date",
       y = "Number of copies per R16 read")
p + geom_col(position = "fill") +
  facet_wrap(~Effector) +
  labs(title = "Effector balance by subgroup",
       x = "Collection date",
       y = "Ratio of 1A:1B copy number")



eff_long %>%
  group_by(Date, Subgroup, Effector) %>%
  summarise(CopyNumber.mean = mean(CopyNumber),
            CopyNumber.sd = sd(CopyNumber)) %>%
  ggplot(aes(x = Date,
             y = CopyNumber.mean,
             fill = Subgroup)) +
  geom_area() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Effector, scales = "free_y")


# generate log copy number transformations
# Logit analysis/chi2 tests and save results
# produce summary table