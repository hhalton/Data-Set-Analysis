#16/5/22
#By Hazel
#Evaluating 2 sets of data file

library(dplyr)
library(magrittr)

#Creating separate sets of data for comparisons#

#Gender
fem <- nutrition[ which(nutrition$Gender=='Female'), ]
mal<- nutrition[ which(nutrition$Gender=='Male'), ]

#Smoking#
smo <- nutrition[ which(nutrition$Smoke=='Yes'), ]
nosmo <- nutrition[ which(nutrition$Smoke=='No'), ]

#Vitamin Use#
vit <- nutrition[ which(nutrition$VitaminUse=='Regular'), ]
occvit <- nutrition[ which(nutrition$VitaminUse=='Occasional'), ]
novit <- nutrition[ which(nutrition$VitaminUse=='No'), ]

#Exploring sample statistics#

hist(fem$Age, col = "palegreen", breaks = pretty(19:83, n = 26), 
     main = "Ages", xlab = "")
hist(mal$Age, col = "lightpink1", breaks = pretty(19:83, n = 26), add = TRUE)

range(fem$Age)

range(mal$Age)

fem %>%
  dplyr:: filter(Age>=33) %>%
  summarize(Avgfem = mean(RetinolPlasma, trim = 0.1))
  
mal %>%
  dplyr:: summarize(Avg = mean(RetinolPlasma, trim = 0.1))

range(nosmo$Age)

range(smo$Age)

nosmo %>%
  dplyr:: filter(Age>=25 & Age<=74) %>%
  summarize(Avgnosmo = mean(BetaPlasma, trim = 0.1))

smo %>%
  dplyr:: summarize(Avgsmo = mean(BetaPlasma, trim = 0.1))

range(vit$Age)

range(novit$Age)

vit %>%
  dplyr:: filter(Age>=22 & Age<=74) %>%
  summarize(Avgvit = mean(BetaPlasma, trim = 0.1))

novit %>%
  dplyr:: summarize(Avgnovit = mean(BetaPlasma, trim = 0.1))


#Comparing beta-carotene and retinol blood levels#
fem %>%
  dplyr::select(RetinolPlasma, BetaPlasma) %>%
  plot(main = "Vitamin A forms in blood plasma", las = 2, col= "turquoise4")

mal %>%
  dplyr::select(RetinolPlasma, BetaPlasma) %>%
  points(pch = 16, col="purple4")
legend("topright", legend = c("Female", "Male"), col=c("turquoise4", "purple4"), 
                         pch = 16, cex = 0.8)


#Scatter Plots of Variables against Beta-carotene and Retinol blood plasma levels#
nutrition %>%
  dplyr::select(RetinolDiet, BetaPlasma) %>%
  plot(main = "RetinolDiet and Beta Blood Plasma Levels", las = 2, col= "purple4")

nutrition %>%
  dplyr::select(RetinolDiet, RetinolPlasma) %>%
  plot(main = "RetinolDiet and Retinol Blood Plasma Levels", las = 2, col= "purple4")


nosmo %>%
  dplyr:: select(RetinolPlasma, BetaPlasma) %>%
  plot(main = "Vitamin A forms in blood plasma", las = 2, col= "turquoise4")

smo %>%
  dplyr:: select(RetinolPlasma, BetaPlasma) %>%
  points(pch = 16, col="purple4")
legend("topright", legend = c("Smoker", "Non-smoker"), col=c("turquoise4", "purple4"), 
       pch = 16, cex = 0.8)

vit %>%
  dplyr::select(RetinolPlasma, BetaPlasma) %>%
  plot(main = "Vitamin A form for different Vitamin Usage", las = 2, col= "red")

occvit %>%
  dplyr::select(RetinolPlasma, BetaPlasma) %>%
  points(col= "blue")

novit %>%
  dplyr::select(RetinolPlasma, BetaPlasma) %>%
  points(col="green")
legend("topright", legend = c("Regular", "No"), 
       col=c("red", "blue"), pch = 16, cex = 0.8)

novit %>%
  dplyr::select(RetinolPlasma, BetaPlasma) %>%
  plot(main = "Vitamin A data", las = 2, col= "red")





