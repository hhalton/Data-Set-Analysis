#15/5/22
#By Hazel
#Testing Hypotheses for categorical variables of data file

#Smoke - comparing trimmed means for beta-carotene 
nutrition_sample<-nutrition
nutrition_sample$Smoke <- "No"
nutrition_sample$Smoke[sample(314, 42)] <- "Yes"
nutrition_sample$Smoke

#Create function
calc_xbar_diff <- function(){
  nutrition_sample<-nutrition
  nutrition_sample$Smoke <- "No"
  nutrition_sample$Smoke[sample(314, 42)] <- "Yes"
  diff<-mean(nutrition_sample[nutrition_sample$Smoke=="No",]$BetaPlasma, trim = 0.1) - mean(nutrition_sample[nutrition_sample$Smoke=="Yes",]$BetaPlasma, trim = 0.1)
  return(diff)
}

calc_xbar_diff()

set.seed(1)
rand_no <- replicate(10000, calc_xbar_diff())

hist(rand_no, main = "Random Distribution non-smokers/smokers beta-carotene levels")

sum(rand_no>=51)/10000
