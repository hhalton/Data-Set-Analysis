#15/5/22
#By Hazel
#Testing Hypotheses for categorical variables of data file

#Vitamin Usage - comparing trimmed means for retinol
nutrition_sample <- nutrition[ which(nutrition$VitaminUse=="Regular" | nutrition$VitaminUse=="No"), ]
nutrition_sample$VitaminUse <- "Regular"
nutrition_sample$VitaminUse[sample(232, 110)] <- "No"
nutrition_sample$VitaminUse


#Create function
calc_xbar_diff <- function(){
  nutrition_sample <- nutrition[ which(nutrition$VitaminUse=="Regular" | nutrition$VitaminUse=="No"), ]
  nutrition_sample$VitaminUse <- "Regular"
  nutrition_sample$VitaminUse[sample(232, 110)] <- "No"
  diff<-mean(nutrition_sample[nutrition_sample$VitaminUse=="Regular",]$RetinolPlasma, trim = 0.1) - mean(nutrition_sample[nutrition_sample$VitaminUse=="No",]$RetinolPlasma, trim = 0.1)
  return(diff)
}

calc_xbar_diff()

set.seed(1)
rand_reg <- replicate(10000, calc_xbar_diff())

hist(rand_reg, main = "Random Distribution regular vitamins/none retinol levels")

sum(rand_reg>=12)/10000
