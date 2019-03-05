library(tidyverse)
library(openxlsx)
library(Cairo)

# read sheet
indat =
  read.xlsx(file.choose(), sheet = 1) %>%
  gather(4:ncol(.), key = "wav", value = "refl") %>%
  mutate(wav = round(as.numeric(wav),5)) %>%
  mutate_at(c("PlotID", "AY"),~ as.factor(.))

# save to correct name
spex0822with0821 = indat
spex0910with0906 = indat
spex0910with0918 = indat
rm(indat)

# merge datasets
spex =
  rbind(
    mutate(spex0822with0821, Date = 20180822),
    mutate(spex0910with0906, Date = 20180910)
  )

spex_comp =
  spex %>%
  group_by(Date, AY, wav) %>%
  summarise(
    meanref = mean(refl),
    minref = min(refl),
    maxref = max(refl)
  )

spex_diff =
  spex %>%
  group_by(Date, AY, wav) %>%
  summarise(meanref = mean(refl)) %>%
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

  
