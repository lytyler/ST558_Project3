
library(plumber)
library(tidyverse)
library(tidymodels)

#* @apiTitle Diabetes Model
#* @apiDescription NCSU ST558 Project 3: API for Diabetes Model

#read in model
diabetes_model <- readRDS("final_model.rds")

#read in data
diabetes_modeling_data <- read_csv("diabetes_modeling_data.csv")

#transform data
diabetes_modeling_data <- diabetes_modeling_data |> mutate(
  dia_ind = factor(dia_ind, levels = c("No Diabetes", "Diabetes"), 
                   labels = c("No Diabetes", "Diabetes")), 
  smoker = factor(smoker, levels = c("Non-Smoker", "Smoker"),
                  labels = c("Non-Smoker", "Smoker")),
  phys_act = factor(phys_act, levels = c("No Physical Activity", "Physical Activity"),
                    labels = c("No Physical Activity", "Physical Activity")), 
  fruits = factor(fruits, levels = c("No Fruit", "Fruit"),
                  labels = c("No Fruit", "Fruit")), 
  veggies = factor(veggies, levels = c("No Veggies", "Veggies"),
                   labels = c("No Veggies", "Veggies")),     
  alcohol = factor(alcohol, levels = c("Not Heavy Drinker", "Heavy Drinker"),
                   labels = c("Not Heavy Drinker", "Heavy Drinker")),    
  sex = factor(sex, levels = c("Female", "Male"),
               labels = c("Female", "Male")),
  age = factor(age, levels = c("Ages 18 to 24", 
                               "Ages 25 to 29", 
                               "Ages 30 to 34", 
                               "Ages 35 to 39",
                               "Ages 40 to 44", 
                               "Ages 45 to 49", 
                               "Ages 50 to 54", 
                               "Ages 55 to 59",
                               "Ages 60 to 64",
                               "Ages 65 to 69",
                               "Ages 70 to 74",
                               "Ages 75 to 79",
                               "Ages 80 or older"),
               labels = c("Ages 18 to 24", 
                          "Ages 25 to 29", 
                          "Ages 30 to 34", 
                          "Ages 35 to 39",
                          "Ages 40 to 44", 
                          "Ages 45 to 49", 
                          "Ages 50 to 54", 
                          "Ages 55 to 59",
                          "Ages 60 to 64",
                          "Ages 65 to 69",
                          "Ages 70 to 74",
                          "Ages 75 to 79",
                          "Ages 80 or older")))


#--------------------------------------------------------------------------------
#* Request a Prediction
#* @param bmi
#* @param smoker Smoker or Non-Smoker
#* @param phys_act Physical Activity or No Physical Activity
#* @param fruits Fruit or No Fruit
#* @param veggies Veggies or No Veggies
#* @param alcohol Heavy Drinker or Not Heavy Drinker
#* @param sex Male or Female
#* @param age Ages 18 to 24...
#* @get /pred
function(bmi = 28.38236, smoker = "Non-Smoker", 
         phys_act = "Physical Activity", fruits = "Fruit", 
         veggies = "Veggies", alcohol = "Not Heavy Drinker", 
         sex = "Female", age = "Ages 60 to 64") {
  observation <- tibble("bmi" = as.numeric(bmi), "smoker" = smoker, 
                        "phys_act" = phys_act, "fruits" = fruits, 
                        "veggies" = veggies, "alcohol" = alcohol, 
                        "sex" = sex, "age" = age)
  predict(diabetes_model, observation, type = "class")
  
}

# to see prediction for default values: http://localhost:PORT/pred

# sample query 1: http://localhost:PORT/pred?bmi=63&smoker=Smoker&phys_act=No%20Physical%20Activity&fruits=No%20Fruit&veggies=No%20Veggies&alcohol=Not%20Heavy%20Drinker&sex=Male

# sample query 2: http://localhost:PORT/pred?bmi=28&smoker=Non-Smoker&phys_act=Physical%20Activity&fruits=Fruit&veggies=Veggies&alcohol=Not%20Heavy%20Drinker&sex=Female

# sample query 3: http://localhost:PORT/pred?bmi=80&smoker=Smoker&phys_act=No%20Physical%20Activity&fruits=No%20Fruit&veggies=No%20Veggies&alcohol=Not%20Heavy%20Drinker&sex=Male


#--------------------------------------------------------------------------------

#* Get More Information
#* @get /info
function(){
  "Lanette Tyler   https://lytyler.github.io/ST558_Project3/EDA.html"
}

#http://localhost:PORT/info

#--------------------------------------------------------------------------------
#* Plot a confusion matrix
#* @serializer png
#* @get /confusion
function() {
  #add model predictions to data
  new_column <- predict(diabetes_model, diabetes_modeling_data, type = "class")
  diabetes_modeling_data2 <- mutate(diabetes_modeling_data, 
                                   estimate = new_column$.pred_class)
  
  #calculate confusion matrix
  cm <- conf_mat(diabetes_modeling_data2, dia_ind, estimate)
  
  #plot confusion matrix
  fourfoldplot(cm$table)
}

#http://localhost:PORT/confusion


