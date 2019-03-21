#### packages ####

library(tidyverse)
library(stats)
library(spatstat)



#### read data ####
carrots =
  read_csv("in/carrots.csv") %>%
  filter(AY == 1) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(x = WTM_X, y = WTM_Y)

stakes =
  read_csv("in/stakes.csv") %>%
  mutate_if(is.character, as.factor)




#### disease incidence stuff ####

ay %>%
  group_by(Date, Density, PlotType) %>%
  summarise(n = n(),
            mean.inc = mean(Incidence),
            se.inc = sd(Incidence)/sqrt(n())) %>%
  ggplot(aes(x = Date, y = mean.inc)) +
  geom_point() +
  geom_line() +
  facet_grid(Density ~ PlotType)

ay.cum =
  ay %>%
  arrange(Date) %>%
  group_by(PlotID) %>%
  mutate(cumAY = cumsum(AYPos),
         cumInc = cumAY / Stand) %>%
  ungroup()

ay.cum %>%
  group_by(Density, PlotType, Date) %>%
  summarise(n = n(),
            meanInc = mean(cumInc),
            seInc = sd(cumInc)/sqrt(n())) %>%
  ggplot(aes(x = Date, y = meanInc)) +
  geom_hline(yintercept = 0) +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = meanInc - seInc, ymax = meanInc + seInc), width = .1) +
  facet_grid(Density ~ PlotType)



#### plots ####

carrots %>%
  ggplot(aes(x = WTM_X, y = WTM_Y)) +
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

ay = carrots %>%
  ppp(x = .$x, y = .$y, xrange = range(.$x), yrange = range(.$y))


# distmap for whole field
ay.distmap =
  carrots %>%
  ppp(
    x = .$x,
    y = .$y,
    xrange = range(.$x) + c(-1, 1),
    yrange = range(.$y) + c(-1, 1)
  ) %>%
  do(distmap())
png("distmaps/field.png")
image(ay.distmap, main = "AY distmap")
dev.off()


# distmaps for each plot
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



#### random pts ####

for (plot in levels(carrots$PlotID)) {
  print(plot)
}

ncarrots =
  carrots %>%
  group_by(PlotID) %>%
  summarise(n = n())

generatePoints =
  function(fstake = c(0, 0), bstake = c(0, 0), n = 0) {
    require(stats)
    if (n == 0) {
      return()
    }
    else {
      cbind(
        runif(n, fstake[1], fstake[2]),
        runif(n, bstake[1], bstake[2])
      )
# 
#       xdif = BX - FX
#       xinc = xdif / n
#       ydif = BY - FY
#       yinc = ydif / n
#       
      # data.frame(
      #   X = seq(
      #     from = FX + xinc / 2,
      #     to = BX - xinc / 2,
      #     by = xinc
      #   ),
      #   Y = seq(
      #     from = FY + yinc / 2,
      #     to = BY - yinc / 2,
      #     by = yinc
      #   )
      # )
    }
  }

# need to figure out how to pass a list of variables (maybe)
stakes %>%
  group_by(RowID) %>%
  do(generatePoints(.$FX, .$FY, .$BX, .$BY, .$n))


test <- stakes %>%
  unite(merge, FX, FY, BX, BY, sep = ",")
  
stakes %>%
  left_join(ncarrots) %>%
  group_by(PlotID, RowID) %>%
  do(data.frame(count = 0:.$n, pos = c(.$FX, .$FY)))

stakes %>%
  left_join(ncarrots) %>%
  group_by(PlotID, RowID) %>%
  do(generatePoints(.$FX,.$FY,.$BX,.$BY,.$n))

test.f = c(556043.2876, 405117.6877)
test.b = c(556043.1761, 405111.2948)
generatePoints(testf, testb, 15)


n = 15
testpts =
  data.frame(
    x = runif(n, min(test.f), max(test.f)),
    y = runif(n, min(test.b), max(test.b)))
testpts


library(spatstat)

rpoint(15, f = 1, win = matrix(testpts, nrow = 2))


# stakes %>%
#   left_join(ncarrots) %>%
#   mutate(WTM = paste(WTM_X, WTM_Y, sep = ","),
#          WTM_X = NULL, WTM_Y = NULL, StakeID = NULL) %>%
#   spread(key = Stake, value = WTM)


# test = stakes %>%
#   left_join(ncarrots) %>%
#   unite(WTM, WTM_X, WTM_Y) %>%
#   mutate(WTM_X = NULL,
#          WTM_Y = NULL,
#          StakeID = NULL) %>%
#   spread(key = Stake, value = WTM) %>%
#   unite(WTM, F, B) %>%
#   mutate(WTM = strsplit(WTM, "_", fixed = T))





# continue this idea, join Stake and point columns to generate variable names, then spread
# test = stakes %>%
#   left_join(ncarrots) %>%
#   gather(WTM_X, WTM_Y, key = "point", value = "value") %>%
#   mutate(point = str_replace(point, "WTM_", ""))
# test
