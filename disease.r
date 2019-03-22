#### packages ####

library(tidyverse)
library(stats)
library(ggpubr)
library(Cairo)


#### read data ####
carrots =
  read_csv("in/carrots.csv") %>%
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
library(stats)

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
