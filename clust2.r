# Jun's code --------------------------------------------------------------

library(tidyverse)
library(spatstat)



# load data ---------------------------------------------------------------

# load in data, subset to AY==1
clust_in =
  read.csv("carrot rel locs.csv") %>%
  #converting characters to factors
  mutate_if(is.character, as.factor) %>%
  # subset where AY ==1
  filter(AY == 1)


# explore data ------------------------------------------------------------

#Table - Plot ID by Row
with(clust_in, table(PlotID, factor(Row)))

#scatter plot
clust_in %>%
  ggplot(aes(x = Row, y = NewLocDecFt)) +
  geom_point(alpha = 0.5)

#boxplot
clust_in %>%
  ggplot(aes(
    x = factor(Row),
    y = NewLocDecFt,
    fill = factor(Row)
  )) +
  geom_boxplot()

#create new cleaned dataframe, clust
clust =
  clust_in %>%
  #Rename NewLocDecFt to Loc
  mutate(Loc = NewLocDecFt) %>%
  #select variables Density, PlotID, Row, Loc
  select(c(Density, PlotID, Row, Loc))

### random carrot generation (non-ppp) ###
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



### simulate ###

#clust: dataframe consisting of density, PlotID, Row, and Loc
idplots = levels(clust$PlotID)

nplots = length(idplots)

#nsims: number of sims
nsims = 1000 #1000 #number of sims for each plot

#513 is the default number of r in envelope() function (see: http://spatstat.org/FAQ.html)
nr = 513 #number of r's (default)

#create empty matrix and substitute in values (more computationally-efficient)
#ncol=4 for Kest output for data (r, iso) and simu (min, max)
matouter = matrix(rep(NA, 4 * nr * nplots), ncol = 4 * nplots) #matrix
set.seed(1234)

iplot = 1

for (p in idplots) {
  #for each plot in plot ID
  testplot = clust %>%
    #create testplot, which is subset for the given plotID a
    #create ppp object
    filter(PlotID == p) %>%
    ppp(
      x = .$Row,
      y = .$Loc,
      xrange = c(.5, 3.5),
      yrange = c(-.5, 22.5)
    )
  #testplot is now ppp object
  print(paste0("Running ", nsims, " sims of ", p, " (n = ", testplot$n, ")"))
  testKest = Kest(testplot)
  
  #ncol=3 because we are returning plot p, r, obs
  matout <- matrix(rep(NA, 4 * nr), ncol = 4) #matrix
  matout[, 1] = testKest$r
  matout[, 2] = testKest$iso
  
  matinner <- matrix(rep(NA, nsims * nr), ncol = nsims) #matrix
  
  for (i in 1:nsims) {
    simuplot = genpts(data.frame(testplot$x, testplot$y))[, 1:2] %>%
      ppp(
        x = .$Row,
        y = .$Loc,
        xrange = c(.5, 3.5),
        yrange = c(-.5, 22.5)
      )
    #plot(simuplot)
    simuKest = Kest(simuplot)
    matinner[, i] = simuKest$iso
  }
  matout[, 3] = apply(matinner, 1, min)
  matout[, 4] = apply(matinner, 1, max)
  matouter[, (iplot - 1) * 4 + 1:4] = matout
  
  iplot = iplot + 1
  
}


# plot everything
par(mfrow = c(4, 5))
for (i in 1:20) {
  matout = matouter[, (i - 1) * 4 + 1:4]
  
  plot(
    matout[, 1],
    matout[, 2],
    type = "s",
    main = idplots[i],
    xlab = "r",
    ylab = "K(r)"
  ) #obs for plot i
  lines(matout[, 1], matout[, 3], type = "s", lty = 2) #min
  lines(matout[, 1], matout[, 4], type = "s", lty = 3) #max
}


# plot in groups of 4
par(mfrow = c(2, 2))
for (i in 1:4) {
  matout = matouter[, (i - 1) * 4 + 1:4]
  
  plot(
    matout[, 1],
    matout[, 2],
    type = "s",
    main = idplots[i],
    xlab = "r",
    ylab = "K(r)"
  ) #obs for plot i
  lines(matout[, 1], matout[, 3], type = "s", lty = 2) #min
  lines(matout[, 1], matout[, 4], type = "s", lty = 3) #max
}

par(mfrow = c(2, 2))
for (i in 5:8) {
  matout = matouter[, (i - 1) * 4 + 1:4]
  plot(
    matout[, 1],
    matout[, 2],
    type = "s",
    main = idplots[i],
    xlab = "r",
    ylab = "K(r)"
  ) #obs for plot i
  lines(matout[, 1], matout[, 3], type = "s", lty = 2) #min
  lines(matout[, 1], matout[, 4], type = "s", lty = 3) #max
}

par(mfrow = c(2, 2))
for (i in 9:12) {
  matout = matouter[, (i - 1) * 4 + 1:4]
  plot(
    matout[, 1],
    matout[, 2],
    type = "s",
    main = idplots[i],
    xlab = "r",
    ylab = "K(r)"
  ) #obs for plot i
  lines(matout[, 1], matout[, 3], type = "s", lty = 2) #min
  lines(matout[, 1], matout[, 4], type = "s", lty = 3) #max
}

par(mfrow = c(2, 2))
for (i in 13:16) {
  matout = matouter[, (i - 1) * 4 + 1:4]
  
  plot(
    matout[, 1],
    matout[, 2],
    type = "s",
    main = idplots[i],
    xlab = "r",
    ylab = "K(r)"
  ) #obs for plot i
  lines(matout[, 1], matout[, 3], type = "s", lty = 2) #min
  lines(matout[, 1], matout[, 4], type = "s", lty = 3) #max
}

par(mfrow = c(2, 2))
for (i in 17:20) {
  matout = matouter[, (i - 1) * 4 + 1:4]
  
  plot(
    matout[, 1],
    matout[, 2],
    type = "s",
    main = idplots[i],
    xlab = "r",
    ylab = "K(r)"
  ) #obs for plot i
  lines(matout[, 1], matout[, 3], type = "s", lty = 2) #min
  lines(matout[, 1], matout[, 4], type = "s", lty = 3) #max
}