#install packages
install.packages("haven") 
install.packages("tidyverse")
install.packages("impumsr")
library(haven) 
library(tidyverse)
library(ipumsr)

#set working directory
getwd()

#load in data and define in global environment 
aca_data <- read_dta("aca_data_march_cps.dta")  

#clean variables#
## The year and the age in the data set refers to the year that the survey was taken, but the insurance information refers to the year before.
##Therefore, the year and the age of the respondents that the data is referring to is the give year minus 1, also known as the study_year. 
##Since the objective is to understand how the ACA and the included parental health care coverage for young adults under 26, adjusting the age is very important to the study. 
aca_data <- aca_data |> 
  mutate(study_year = (year - 1), age = as.numeric(age), study_age = (age-1)) |>
  relocate(study_year) |>
  relocate(study_age, .after = "study_year")

# add pre and post ACA dummy variable. post is defined as study year after 2010.  
aca_data <- aca_data |> 
  mutate(post_aca = ifelse(study_year >= 2011, yes = 1, no = 0)) |> 
  relocate(post_aca, .after = "study_age")

# add treatment and control dummy. 
##the control group is adults where study_age = 26-29, where the ACA parental health care coverage would not have an impact. 
##the treatment group is young adults where study_age = 19-25, where ACA would have an impact on levels of health care coverage. 
aca_data <- aca_data |> 
  mutate(young_adult = ifelse(study_age >= 26, yes = 0, no = 1 )) |> 
  relocate(young_adult, .after = "post_aca")

#visualizations#
#creating a line graph to show the general trends for the proportion of young adults from 19 to 29 years old who have any insurance.
#added vertical line indicating when the ACA was fully implemented, regardless of which month the survey was taken. 
aca_data |> 
  group_by(study_year) |> 
  mutate(proportion_insurance_level = mean(any_insurance)) |> 
  ggplot(aes(x = study_year, y = proportion_insurance_level)) +
  geom_line() +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "red", size = 0.75)+
  labs(
    title = "Young Adults with Health Insurance in the United States",
    subtitle = "Ages represented: 19-29",
    x = "Year",
    y = "Proportion of Adults with Health Insurance")

#creating a line graph to show the trends of health care coverage.
##proportion on whether young adults (19-25) have health care coverage (covered_ya) by the year of the study.
young_adults <- aca_data |>
  filter(young_adult == 1) |>
  group_by(study_year) |>
  summarize(covered_ya = mean(any_insurance))
##proportion on whether non young adults (26-29) have health care coverage (covered_nya) by the year of the study
non_young_adults <- aca_data |>
  filter(young_adult == 0) |>
  group_by(study_year) |>
  summarize(covered_nya = mean(any_insurance))
#line graph with two lines, comparing how health care coverage changed between young adults and non young adults between survey year 2004 and 2012.  
ggplot() +
  geom_line(data = non_young_adults, aes(x = study_year, y = covered_nya, color = "Non-Young Adults")) +
  geom_line(data = young_adults, aes(x = study_year, y = covered_ya, color = "Young Adults")) +
  scale_color_manual(
    values = c("Non-Young Adults" = "blue", "Young Adults" = "red"),
    name = "Age Groups"
  ) +
  labs(
    title = "Young Adults with Health Insurance in the United States",
    subtitle = "Comparison between Young Adults (19-25) and Non-Young Adults (26-29)",
    x = "Year",
    y = "Proportion of Adults with Health Insurance",
    color = "Age Groups")

#analysis#
#using a 2x2 difference in difference
#determine the impact of how being a young adult (the treatment group) and after the implementation of the ACA on health insurance coverage
#focusing on the interaction term 
model_1 <- aca_data |> 
  lm(formula = any_insurance ~ young_adult + post_aca + post_aca*young_adult)
summary(model_1)

print(aca_data)
model_2 <- aca_data |> 
  lm(formula = pvt_insurance ~ young_adult + post_aca + post_aca*young_adult)
summary(model_2)

print(aca_data)
model_3 <- aca_data |> 
  lm(formula = medicaid ~ young_adult + post_aca + post_aca*young_adult)
summary(model_3)