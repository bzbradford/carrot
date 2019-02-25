library(tidyverse)
library(openxlsx)
library(Cairo)

dat =
  read.xlsx(file.choose()) %>%
  gather(6:ncol(.), key = "wav", value = "ref") %>%
  mutate(wav = round(as.numeric(wav),5)) %>%
  mutate_at(c("Mask", "Density", "Plot", "AY"),~ as.factor(.))

gaps =
  data.frame(wav = c(1.4, 1.9),
             ref = c(NA, NA))


spex20180822 = dat
spex20180910 = dat


p1 =
  spex20180822 %>%
  group_by(AY, wav) %>%
  summarise(
    meanref = mean(ref),
    minref = min(ref),
    maxref = max(ref)
  ) %>%
  ggplot(aes(x = wav, y = meanref, color = AY)) +
  geom_ribbon(aes(ymin = minref, ymax = maxref), alpha = .5) +
  geom_line() +
  labs(title = "Spectral irradiance of aster-yellows infected carrot: 2018-08-22",
       x = "Wavelength",
       y = "Mean reflectance")
CairoPNG("spex20180822.png", h=500, w=800); p1; dev.off()


p2 =
  spex20180910 %>%
  group_by(AY, wav) %>%
  summarise(
    meanref = mean(ref),
    minref = min(ref),
    maxref = max(ref)
  ) %>%
  ggplot(aes(x = wav, y = meanref, color = AY)) +
  geom_ribbon(aes(ymin = minref, ymax = maxref), alpha = .5) +
  geom_line() +
  labs(title = "Spectral irradiance of aster-yellows infected carrot: 2018-09-10",
       x = "Wavelength",
       y = "Mean reflectance")
CairoPNG("spex20180910.png", h=500, w=800); p2; dev.off()
