#Project 1 - Final Submission
#Class: CPSC 375
#Group Members: Siddharth Chauhan - 02, Sarthak Gajjar - 01, Parthiv Desai - 02

#Question 1a
library(tidyverse)
library(modelr)
covid_d <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
demographics_d <- read_csv("demographics.csv")

#Question 1b
covid_d <- covid_d %>% filter(nchar(iso_code) == 3)

#Question 1c
dg1_data <- demographics_d %>% mutate(YR2015 = as.numeric(YR2015)) %>% filter(!(is.na(YR2015)))
dg1_data$YR2015 <- floor(dg1_data$YR2015)
dg1_data <- dg1_data %>% filter(YR2015 >= 1000000)

#Question 1d
c2_data <- covid_d
names(c2_data)[names(c2_data) == 'new_deaths_smoothed'] <- 'xxx'
c2_data <- c2_data %>% select(-starts_with("excess"), -contains(("deaths")))
names(c2_data)[names(c2_data) == 'xxx'] <- 'new_deaths_smoothed'

#Question 1e
c3_data <- c2_data %>%mutate(date = as.Date(date)) %>%group_by(iso_code) %>%mutate(new_deaths_smoothed_2wk = lead(new_deaths_smoothed, 14)) %>%ungroup()


#Question 1f
dg1_data <- dg1_data %>% unite(SERIES, `Series Name`, `Series Code`, sep = " _ ")
demographics_tidy <- dg1_data %>%
  pivot_wider(names_from = SERIES, values_from = YR2015)



#Question 1g
demographics_tidy <- dg1_data %>% 
  rename(iso_code = `Country Code`)
merged_data <- c3_data %>%
  inner_join(demographics_tidy, by = "iso_code")

reg_model <- merged_data %>%select(date, iso_code, location, new_deaths_smoothed, new_deaths_smoothed_2wk, new_cases, people_vaccinated, people_fully_vaccinated, hospital_beds_per_thousand, aged_65_older, diabetes_prevalence, total_cases, total_vaccinations, `population`, cardiovasc_death_rate,new_cases_smoothed,icu_patients,gdp_per_capita)

#Question 2a
remove_cols <- c("new_deaths_smoothed", "new_deaths_smoothed_2wk", "location", "iso_code", "date", "date_2wk")
question2a <- setdiff(colnames(reg_model), remove_cols)
question2a

#Question 2b
reg_model <- reg_model %>%mutate(cardiovasc_deaths = cardiovasc_death_rate * total_cases,diabetes_cases = `population` * total_cases, all_vaccinations = total_vaccinations * total_cases)

#Question 2c
reg_model_train_d <- reg_model %>% filter(date >= as.Date("2022-01-01"), date < as.Date("2023-01-01"))
reg_model_test_d <- reg_model %>% filter(date >= as.Date("2023-01-01"))

#Question 2d
reg_model_1 <- lm(new_deaths_smoothed_2wk ~ total_vaccinations + diabetes_prevalence + icu_patients + new_cases_smoothed, data = reg_model_train_d)
reg_model_2 <- lm(new_deaths_smoothed_2wk ~ total_vaccinations + gdp_per_capita + diabetes_prevalence + icu_patients + cardiovasc_death_rate, data = reg_model_train_d)
reg_model_3 <- lm(new_deaths_smoothed_2wk ~ total_vaccinations + cardiovasc_death_rate + population + icu_patients + new_cases_smoothed, data = reg_model_train_d)
reg_model_4 <- lm(new_deaths_smoothed_2wk ~ total_vaccinations + gdp_per_capita + diabetes_prevalence + icu_patients, data = reg_model_train_d)
reg_model_5 <- lm(new_deaths_smoothed_2wk ~ total_vaccinations + diabetes_prevalence + cardiovasc_death_rate + icu_patients + new_cases_smoothed, data = reg_model_train_d)

#Question 3a

reg_model_test_d <- reg_model_test_d %>% add_predictions(reg_model_1, var = 'Prediction_1')
reg_model_test_d <- reg_model_test_d %>% add_predictions(reg_model_2, var = 'Prediction_2')
reg_model_test_d <- reg_model_test_d %>% add_predictions(reg_model_3, var = 'Prediction_3')
reg_model_test_d <- reg_model_test_d %>% add_predictions(reg_model_4, var = 'Prediction_4')
reg_model_test_d <- reg_model_test_d %>% add_predictions(reg_model_5, var = 'Prediction_5')


#Question 3b
#Could not use rmse() function because of an error in Rstudio.
rmse1 <- sqrt(mean((reg_model_test_d$new_deaths_smoothed_2wk - reg_model_test_d$Prediction_1)^2, na.rm = TRUE))

rmse2 <- sqrt(mean((reg_model_test_d$new_deaths_smoothed_2wk - reg_model_test_d$Prediction_2)^2, na.rm = TRUE))

rmse3 <- sqrt(mean((reg_model_test_d$new_deaths_smoothed_2wk - reg_model_test_d$Prediction_3)^2, na.rm = TRUE))

rmse4 <- sqrt(mean((reg_model_test_d$new_deaths_smoothed_2wk - reg_model_test_d$Prediction_4)^2, na.rm = TRUE))

rmse5 <- sqrt(mean((reg_model_test_d$new_deaths_smoothed_2wk - reg_model_test_d$Prediction_5)^2, na.rm = TRUE))


reg_model_test_d <- reg_model_test_d %>%add_predictions(reg_model_4, var = "best_prediction")

country_rmse <- reg_model_test_d %>%group_by(location, population) %>%summarise(rmse = sqrt(mean((new_deaths_smoothed_2wk - best_prediction)^2, na.rm = TRUE))) %>% arrange(desc(population))
