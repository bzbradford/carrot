library(tidyverse)
library(openxlsx)
library(Cairo)

dat =
  read.xlsx(file.choose()) %>%
  gather(6:ncol(.), key = "wav", value = "ref") %>%
  mutate(wav = round(as.numeric(wav),5)) %>%
  mutate_at(c("Mask", "Density", "Plot", "AY"),~ as.factor(.))

spex20180822 = dat
spex20180910 = dat

spex =
  rbind(
    mutate(spex20180822, Date = 20180822),
    mutate(spex20180910, Date = 20180910)
  )

spex_comp =
  spex %>%
  group_by(Date, AY, wav) %>%
  summarise(
    meanref = mean(ref),
    minref = min(ref),
    maxref = max(ref)
  )

spex_diff =
  spex %>%
  group_by(Date, AY, wav) %>%
  summarise(meanref = mean(ref)) %>%
  spread(AY, meanref) %>%
  mutate(diff = log(NEG) - log(POS))


p1 =
  spex_comp %>%
  ggplot(aes(x = wav, y = meanref, color = AY)) +
  facet_grid(Date ~ .) +
  geom_ribbon(aes(ymin = minref, ymax = maxref), alpha = .5) +
  geom_line() +
  geom_hline(yintercept = 0) +
  labs(title = "Spectral irradiance of aster-yellows infected carrot",
       x = "Wavelength",
       y = "Mean reflectance")
p1
CairoPNG("spex20180822.png", h = 500, w = 800);p1;dev.off()


p2 =
  spex_diff %>%
  ggplot(aes(x = wav*1000)) +
  facet_grid(Date ~ .) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = 0, ymax = diff)) +
  labs(x = "Wavelength",
       y = "Log difference in means")
p2

  
