#### packages ####

library(tidyverse)
library(stats)
library(spatstat)
library(Cairo)


#### read data ####
carrots =
  read_csv("in/carrot locs wtm.csv") %>%
  filter(AY == 1) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(x = WTM_X, y = WTM_Y)

stakes =
  read_csv("in/stakes.csv") %>%
  mutate_if(is.character, as.factor)

ay =
  read_csv("in/incidence.csv") %>%
  mutate_if(is.character, as.factor)



#### disease incidence stuff ####
# mean AY incidence by density and plot location
ay.summary =
  ay %>%
  group_by(Date, Density, Location) %>%
  summarise(n = n(),
            mean = mean(Incidence),
            sd = sd(Incidence),
            se = sd(Incidence)/sqrt(n()))

ay.barplot =
  ay.summary %>%
  ggplot(aes(x = Date, y = mean, ymin = mean-se, ymax = mean+se, fill = Location)) +
  geom_hline(yintercept = 0) +
  geom_bar(stat = "identity", color = "black", width = 10, position = "dodge") +
  geom_linerange(position = position_dodge(10)) +
  facet_grid(. ~ Density) +
  labs(y = "Mean AY incidence", x = "") +
  theme_pubr()

ay.lineplot =
  ay.summary %>%
  ggplot(aes(x = Date,
             y = mean,
             ymin = mean-se,
             ymax = mean+se,
             group = paste(Density, Location))) +
  geom_line(aes(linetype = Density), size = .5) +
  geom_point(aes(shape = Location), size = 2.5) +
  geom_errorbar(width = 3, size = .25) +
  scale_shape_discrete() +
  scale_color_manual(values = c("black", "darkgrey")) +
  labs(y = "Mean AY incidence per plot", x = "Sampling date") +
  theme_classic2()
tiff("out/incidence.tif", type="cairo", w=5, h=3, u="in", res=300)
ay.lineplot
dev.off()



#### plots ####
carrots %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = as.factor(AY)))


carrots %>%
  ggplot(aes(x = WTM_X, y = WTM_Y)) +
  geom_point(data = stakes) +
  geom_point(aes(color = PlotID))



#### kmeans test ####

# carrot loc chart
km = kmeans(cbind(carrots$WTM_X, carrots$WTM_Y), centers = 20)
carrots %>%
  ggplot(aes(x = WTM_X, y = WTM_Y)) +
  geom_point(aes(color = as.factor(km$cluster)))



#### clustering stuff ####
library(spatstat)

# define WTM coordinate extents for carrot field
fieldExtent =
  data.frame(x = c(556040, 556107),
             y = c(405070, 405119))

# create ppp object from carrot locations and field extent
carrot.ppp = carrots %>%
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

# monte carlo sims
envelope(carrot.ppp, Kest, nsim = 39)




#### random pts ####

clust_in =
  read.csv("in/carrot rel locs.csv") %>%
  mutate_if(is.character, as.factor) %>%
  filter(AY == 1)

clust_in %>%
  ggplot(aes(x = Row, y = NewLocDecFt)) +
  geom_point()
  
clust =
  clust_in %>%
  mutate(Loc = NewLocDecFt) %>%
  select(c(Density, PlotID, Row, Loc))


genpts =
  function(df, ...) {
    groups = enquos(...)
    df %>%
      group_by(!!!groups) %>%
      do(data.frame(
        Row = sample(1:3, nrow(.), replace = T),
        Loc = runif(nrow(.), 0, 22),
        n = nrow(.)
      ))
  }

genptsppp =
  function(df, ...) {
    df %>%
      do(data.frame(
        Row = sample(1:3, nrow(.), replace = T),
        Loc = runif(nrow(.), 0, 22),
        n = nrow(.)
      )) %>%
      ppp(x = .$Row,
          y = .$Loc,
          xrange = c(1, 3),
          yrange = c(0, 22))
  }

genpts(clust, PlotID)
genptsppp(clust)


clust %>%
  group_by(PlotID) %>%
  do(genpts(.))


# look at random pts
rand_pts %>%
  ggplot(aes(x = as.factor(Row), y = Loc, color = Density)) +
  geom_point() +
  facet_wrap(~PlotID)


clust %>%
  filter(PlotID == "H-102") %>%
  ppp(x = .$Row, y = .$Loc, xrange = c(1, 3), yrange = c(0, 22)) %>%
  envelope(., Kest, nsims = 10)


clust %>%
  group_by(PlotID) %>%
  ppp(x = .$Row, y = .$Loc, xrange = c(1, 3), yrange = c(0, 22)) %>%
  envelope(., Kest, nsims = 10)

clust.ppp =
  clust %>%
  ppp(x = .$Row,
      y = .$Loc,
      xrange = c(1, 3),
      yrange = c(0, 22))

clust.kenv = envelope(clust.ppp, Kest, nsims = 100)

plot(clust.kenv)
plot(clust.ppp)
