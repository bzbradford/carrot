library(tidyverse)
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

# generate a number of random carrots matching number of AY carrots
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

# same as above but returns a ppp object. Also doesn't take grouping vars
genptsppp =
  function(df) {
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

rand_pts = genpts(clust, Density, PlotID)
rand_pts_ppp = genptsppp(clust)


genptsppp(clust)



# generate random pts by plot
clust %>%
  group_by(PlotID) %>%
  do(genpts(.))


# look at random pts
rand_pts %>%
  ggplot(aes(x = as.factor(Row), y = Loc, color = Density)) +
  geom_point() +
  facet_wrap(~PlotID)



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

set.seed(422)

rand1 =
  clust %>%
  filter(PlotID == "H-115") %>%
  genptsppp()
rand2 =
  clust %>%
  filter(PlotID == "H-115") %>%
  genptsppp()

plot(rand1)
plot(rand2)

rand_env1 = envelope(rand1, Gest)
rand_env2 = envelope(rand2, Gest)

plot(rand_env1$r ~ rand_env1$obs, type = "s")
plot(rand_env2$r ~ rand_env2$obs, type = "s")

