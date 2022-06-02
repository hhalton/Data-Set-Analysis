#15/5/22
#By Hazel
#Testing Hypotheses for categorical variables of data file

#Gender - comparing trimmed means for beta-carotene and retinol
nutrition_sample<-nutrition
nutrition_sample$Gender <- "Female"
nutrition_sample$Gender[sample(314, 41)] <- "Male"
nutrition_sample$Gender

#Check if age difference has changed.
nutrition_sample %>%
  dplyr::select(Gender, Age) %>%
  boxplot(formula = Age ~ Gender, data = ., horizontal = TRUE)

#Create function
calc_xbar_diff <- function(){
  nutrition_sample<-nutrition
  nutrition_sample$Gender <- "Female"
  nutrition_sample$Gender[sample(314, 41)] <- "Male"
  diff<-mean(nutrition_sample[nutrition_sample$Gender=="Female",]$BetaPlasma, trim = 0.1) - mean(nutrition_sample[nutrition_sample$Gender=="Male",]$BetaPlasma, trim = 0.1)
  return(diff)
}

calc_xbar_diff()

set.seed(1)
rand_female <- replicate(10000, calc_xbar_diff())

hist(rand_female, main = "Random Distribution female/male beta levels")

sum(rand_female>= 42)/10000
