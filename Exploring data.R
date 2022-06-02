# Explore data file and contents#
View(nutrition)
dim(nutrition)
names(nutrition)
head(nutrition)

#Explore data within file#
summary(nutrition)

library(magrittr)
library(dplyr)

#Explore data distribution for beta-carotene and retinol#

boxplot(nutrition$BetaPlasma, range = 1.7, horizontal = TRUE)

boxplot(nutrition$RetinolPlasma, range = 1.7, horizontal = TRUE)

smo %>%
  dplyr::select(Alcohol, RetinolPlasma) %>%
  plot(main = "Smoker Alcohol - RetinolPlasma", las = 2)
nutrition %>%
  dplyr::select(RetinolPlasma, BetaPlasma) %>%
  lm() %>%
  abline()

#Testing for correlation#

nutrition %>%
  dplyr::select(Age, BetaPlasma) %>%
  cor(method = "pearson")

nutrition %>%
  dplyr::filter(VitaminUse=="Regular") %>%
  select(BetaPlasma, RetinolPlasma) %>%
  cor(method = "spearman")

#Exploring columns with categorical values as tables#

nutrition %>%
  select(Smoke) %>%
  table() %>%
  proportions()

nutrition %>%
  dplyr::select(Gender, BetaPlasma) %>%
  boxplot(formula = BetaPlasma ~ Gender, data = ., horizontal = TRUE)

nutrition %>%
  dplyr::select(Gender, RetinolPlasma) %>%
  boxplot(formula = RetinolPlasma ~ Gender, data = ., horizontal = TRUE)

nutrition %>%
  select(VitaminUse) %>%
  table() %>%
  proportions()

nutrition %>%
  dplyr::select(Vitamin, BetaPlasma) %>%
  boxplot(formula = BetaPlasma ~ Vitamin, data = ., horizontal = TRUE)
  
nutrition %>%
  dplyr::select(Vitamin, RetinolPlasma) %>%
  boxplot(formula = RetinolPlasma ~ Vitamin, data = ., horizontal = TRUE)


nutrition %>%
  select(Smoke) %>%
  table() %>%
  proportions()

nutrition %>%
  dplyr::select(Smoke, BetaPlasma) %>%
  boxplot(formula = BetaPlasma ~ Smoke, data = ., horizontal = TRUE)

nutrition %>%
  dplyr::select(Smoke, RetinolPlasma) %>%
  boxplot(formula = RetinolPlasma ~ Smoke, data = ., horizontal = TRUE)

nutrition %>%
  select(Smoke) %>%
  table() %>%
  proportions()

nutrition %>%
  dplyr::select(PriorSmoke, BetaPlasma) %>%
  boxplot(formula = BetaPlasma ~ PriorSmoke, data = ., horizontal = TRUE)

nutrition %>%
  dplyr::select(PriorSmoke, RetinolPlasma) %>%
  boxplot(formula = RetinolPlasma ~ PriorSmoke, data = ., horizontal = TRUE)

nutrition %>%
  dplyr::select(Smoke, Age) %>%
  boxplot(formula = Age ~ Smoke, data = ., horizontal = TRUE)

nutrition %>%
  dplyr::select(Vitamin, Age) %>%
  boxplot(formula = Age ~ Vitamin, data = ., horizontal = TRUE)
