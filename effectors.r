# For analyzing effector proportions

# load packages ----
library(stats,
        gridExtra,
        tidyverse)



# read data ----

mplx = read_csv("data/monsterplex_all.csv") %>%
  mutate_if(is.character, as.factor)

# generate relative copy numbers for effectors
effdat = mplx %>%
  filter(Year_Type == "2018_C",
         Subgroup != "1A1B") %>%
  gather(Effector, Reads, SAP05:SAP68) %>%
  mutate(CopyNum = Reads / R16) %>%
  mutate(LogCopyNum = log(CopyNum + 1)) %>%
  mutate(Effector = as.factor(Effector)) %>%
  droplevels()
str(effdat)


# summary of copy number by effector
effdat %>%
  group_by(Subgroup, Effector) %>%
  summarise(n = n(),
            CopyNum.mean = mean(CopyNum),
            LogCopyNum.mean = mean(LogCopyNum))


# plots ----
# bar plots, no date
p = effdat %>%
  filter(Subgroup != "1A1B") %>%
  group_by(Subgroup, Effector) %>%
  summarise(CopyNum.mean = mean(CopyNum),
            CopyNum.sd = sd(CopyNum)) %>%
  arrange(CopyNum.mean) %>%
  ggplot(aes(x = Effector,
             y = CopyNum.mean,
             fill = Subgroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1 = p + geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste0("Effector abundance by subgroup, data = ",
                      levels(effdat$Year_Type), ", n = ",
                      length(levels(effdat$SampleID))),
       y = "Number of copies per R16 read",
       x = "Effector")

p2 = p + geom_bar(stat = "identity", position = "fill") +
  geom_hline(yintercept = 0.5, linetype = 2) +
  labs(title = "Relative effector frequency",
       y = "Copy number ratio",
       x = "Effector")

grid.arrange(p1, p2)
grid.arrange(p1, p3, p2, p4)


# set up bar plot by date
p = effdat %>%
  filter(Subgroup != "1A1B") %>%
  group_by(Date, Subgroup, Effector) %>%
  summarise(CopyNum.mean = mean(CopyNum),
            CopyNum.sd = sd(CopyNum)) %>%
  ggplot(aes(x = factor(Date,
                        labels = unique(format(Date, "%b %d")),
                        ordered = T),
             y = CopyNum.mean,
             fill = Subgroup)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p + geom_col(position = "stack") +
  facet_wrap(~Effector) +
  labs(title = "Effector abundance by subgroup and date",
       x = "Collection date",
       y = "Number of copies per R16 read")

p + geom_col(position = "fill") +
  facet_wrap(~Effector) +
  labs(title = "Effector balance by subgroup and date",
       x = "Collection date",
       y = "Ratio of 1A:1B copy number")



# effector analysis ----

# # sap analysis test code
# test = effdat %>%
#   filter(Effector == "SAP54") %>%
#   droplevels()
# str(test)
# test.fit = test %>%
#   glm(Subgroup ~ LogCopyNum,
#       data = .,
#       family = binomial(link = "logit"))
# test.fit
# test.anova = anova(test.fit, test = "Chisq")
# test.anova

saptest = function(df) {
  df = mutate_if(df, is.character, as.factor)
  fit = glm(Subgroup ~ LogCopyNum,
            data = df,
            family = binomial(link = "logit"))
  anova = anova(fit, test = "Chisq")
  p = anova$"Pr(>Chi)"[2]
  data.frame(
    pval = case_when(p < .0001 ~ 0,
                     TRUE ~ p),
    sig = case_when(
      p < .0001 ~ "****",
      p < .001 ~ "***",
      p < .01 ~ "**",
      p < .05 ~ "*",
      TRUE ~ "NS"
    )
  )
}

# generate mean copy number summary table
effdat.means = effdat %>%
  select(Subgroup, Effector, CopyNum) %>%
  group_by(Subgroup, Effector) %>%
  summarise(MeanCopyNum = mean(CopyNum)) %>%
  spread(Subgroup, MeanCopyNum)

# run logistic regression and return p values
effdat.pvals = effdat %>%
  group_by(Effector) %>%
  do(saptest(.))

# output summary table
eff.out = left_join(effdat.means, effdat.pvals)
eff.out
write.csv(eff.out, "eff_out.csv")


