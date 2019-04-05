library(tidyverse)
library(spatstat)


#### load and prep data ####
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


#### random carrot generation (non-ppp) ####
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

# randomize ay locs within plots
rand_pts = genpts(clust, Density, PlotID)
rand_pts

# look at random pts
rand_pts %>%
  ggplot(aes(x = as.factor(Row), y = Loc, color = Density)) +
  geom_point() +
  facet_wrap(~PlotID)


#### random carrot generation (ppp) ####
# same as above but returns a ppp object. Also doesn't take grouping vars
genptsppp =
  function(df,
           xmin = 1,
           xmax = 3,
           ymin = 0,
           ymax = 22) {
    df %>%
      do(data.frame(
        Row = sample(xmin:xmax, nrow(.), replace = T),
        Loc = runif(nrow(.), ymin, ymax),
        n = nrow(.)
      )) %>%
      ppp(x = .$Row,
          y = .$Loc,
          xrange = c(xmin, xmax),
          yrange = c(ymin, ymax))
  }

randpts = genptsppp(clust)


# clustering for whole field (don't use)
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


levels(clust$PlotID)

clust %>%
  filter(PlotID == "H-102") %>%
  genptsppp() %>%
  plot()

####
for (p in levels(clust$PlotID)) {
  print(p)
}


testplot =
  clust %>%
  filter(PlotID == levels(PlotID)[4])

testppp =
  testplot %>%
  ppp(
    x = .$Row,
    y = .$Loc,
    xrange = c(1, 3),
    yrange = c(0, 22)
  )

for (s in 1:20) {
  randppp = genptsppp(testplot)
  env = envelope(randppp, Kest)
  if (s == 1) {obs = NULL}
  obs = cbind(obs, env$obs)
}
sim =
  data.frame(
  r = env$r,
  obs = envelope(testppp, Kest)$obs,
  min = apply(obs, 1, min),
  max = apply(obs, 1, max)
)
sim %>%
  ggplot(aes(x = r,
             y = obs,
             ymin = min,
             ymax = max)) +
  geom_area(alpha = .5) +
  geom_line(color = "red")




### full loop
for (p in levels(clust$PlotID)) {
  print(p)
  testplot =
    clust %>%
    filter(PlotID == p)
  testppp =
    testplot %>%
    ppp(
      x = .$Row,
      y = .$Loc,
      xrange = c(1, 3),
      yrange = c(0, 22)
    )
  
  for (s in 1:20) {
    randppp = genptsppp(testplot)
    env = envelope(randppp, Kest)
    if (s == 1) {
      obs = NULL
    }
    obs = cbind(obs, env$obs)
  }
  sim =
    data.frame(
      r = env$r,
      obs = envelope(testppp, Kest)$obs,
      min = apply(obs, 1, min),
      max = apply(obs, 1, max)
    )
}





genptsppp2 =
  function(ppp) {
    require(spatstat)
    xrange = ppp$window$xrange
    yrange = ppp$window$yrange
    ppp$x = sample(xrange[1]:xrange[2], ppp$n, replace = T)
    ppp$y = runif(ppp$n, yrange[1], yrange[2])
    return(ppp)
  }

plot(testppp)
plot(genptsppp2(testppp))
