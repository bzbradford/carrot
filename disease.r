library(tidyverse)


# read data ----
carrots =
  read_csv("in/carrots.csv") %>%
  filter(AY == 1) %>%
  mutate_if(is.character, as.factor)

stakes =
  read_csv("in/stakes.csv") %>%
  mutate_if(is.character, as.factor)


carrots %>%
  ggplot(aes(x = WTM_X, y = WTM_Y)) +
  geom_point(data = stakes) +
  geom_point(aes(color = as.factor(AY)))


carrots %>%
  ggplot(aes(x = WTM_X, y = WTM_Y)) +
  geom_point(data = stakes) +
  geom_point(aes(color = PlotID))

for (plot in levels(carrots$PlotID)) {
  print(plot)
}

ncarrots =
  carrots %>%
  group_by(PlotID) %>%
  summarise(n = n())

generatePoints =
  function(FX, FY, BX, BY, n) {
    xdif = BX - FX
    xinc = xdif/n
    ydif = BY - FY
    yinc = xdif/n
    data.frame(WTM_X = seq(FX + xinc/2, BX - xinc/2, by = xinc),
               WTM_Y = seq(FY + yinc/2, BY - yinc/2, by = yinc))
  }

stakes %>%
  left_join(ncarrots) %>%
  mutate(WTM = paste(WTM_X, WTM_Y, sep = ","),
         WTM_X = NULL, WTM_Y = NULL, StakeID = NULL) %>%
  spread(key = Stake, value = WTM)


test = stakes %>%
  left_join(ncarrots) %>%
  unite(WTM, WTM_X, WTM_Y) %>%
  mutate(WTM_X = NULL,
         WTM_Y = NULL,
         StakeID = NULL) %>%
  spread(key = Stake, value = WTM) %>%
  unite(WTM, F, B) %>%
  mutate(WTM = strsplit(WTM, "_", fixed = T))

# need to figure out how to pass a list of variables (maybe)
  group_by(PlotID) %>%
  do(generatePoints(.$WTM, .$n))


# continue this idea, join Stake and point columns to generate variable names, then spread
test = stakes %>%
  left_join(ncarrots) %>%
  gather(WTM_X, WTM_Y, key = "point", value = "value") %>%
  mutate(point = str_replace(point, "WTM_", ""))
test
