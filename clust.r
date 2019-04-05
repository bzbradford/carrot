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



#### define function ####

genptsppp =
  function(ppp) {
    require(spatstat)
    xrange = ppp$window$xrange
    yrange = ppp$window$yrange
    ppp$x = sample(xrange[1]:xrange[2], ppp$n, replace = T)
    ppp$y = runif(ppp$n, yrange[1], yrange[2])
    return(ppp)
  }


#### deprecated ####
nsims = 99
for (p in levels(clust$PlotID)) {
  print(p)
  testplot =
    clust %>%
    filter(PlotID == p) %>%
    ppp(
      x = .$Row,
      y = .$Loc,
      xrange = c(1, 3),
      yrange = c(0, 22)
    )
  actual.env = envelope(testplot, Kest, verbose = F)
  for (s in 1:nsims) {
    randppp = genptsppp(testplot)
    sim.env = envelope(randppp, Kest, verbose = F)
    if (s == 1) {
      sim.obs = NULL
    }
    sim.obs = cbind(obs, sim.env$obs)
  }
  if (p == levels(clust$PlotID)[1]) {
    sim = NULL
  }
  sim =
    rbind(sim,
          data.frame(
            plot = p,
            r = env$r,
            obs = actual.env$obs,
            min = apply(sim.obs, 1, min),
            max = apply(sim.obs, 1, max)
          ))
}

plot(actual.env)
str(actual.env)

sim %>%
  ggplot(aes(x = r,
             y = obs,
             ymin = min,
             ymax = max)) +
  geom_area(alpha = .5) +
  geom_line(color = "red") +
  facet_wrap(~plot)



#### Best code right now ####
nsims = 1000
for (p in levels(clust$PlotID)) {
  testplot =
    clust %>%
    filter(PlotID == p) %>%
    ppp(
      x = .$Row,
      y = .$Loc,
      xrange = c(1, 3),
      yrange = c(0, 22)
    )
  print(paste0("Running ",
               nsims,
               " sims of ",
               p,
               " (n = ",
               testplot$n,
               ")"))
  env =
    envelope(testplot,
             Kest,
             verbose = F,
             nsim = nsims,
             simulate = expression(genptsppp(testplot)))
  if (p == levels(clust$PlotID)[1]) {
    sim = NULL
  }
  sim =
    rbind(sim,
          data.frame(
            plot = p,
            r = env$r,
            obs = env$obs,
            min = env$lo,
            max = env$hi
          ))
}

# plot k-function simulations
sim %>%
  ggplot(aes(x = r,
             y = obs,
             ymin = min,
             ymax = max)) +
  geom_ribbon(alpha = .5) +
  geom_line(color = "red") +
  facet_wrap(~plot) +
  scale_y_sqrt()

# plot all carrot locs
clust %>%
  ggplot(aes(x = Row,
             y = Loc)) +
  geom_point() +
  facet_wrap(~PlotID)



# whats up with H-306 and L-305
testplot =
  clust %>%
  filter(PlotID == "H-306") %>%
  ppp(x = .$Row,
      y = .$Loc,
      xrange = c(1,3),
      yrange = c(0,22))

testenv = envelope(testplot, Kest, simulate = expression(genptsppp(testplot)))
envelope(testplot, Kest)
testenv = envelope(genptsppp(testplot), Kest)

plot(testplot)
plot(genptsppp(testplot))
plot(testenv)
view(testplot)

testk = Kest(testplot)
plot(testk)

testplot$x
testplot$y = testplot$y*2
