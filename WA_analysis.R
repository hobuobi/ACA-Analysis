install.packages(c("haven","dplyr","tidyverse"))
library(haven)
library(dplyr)
library(tidyverse)
data <- read_dta("aca_data_march_cps.dta")
ageSeries <- summarize(
  select(
    group_by(data,year,age),
    any_insurance, pvt_insurance, emp_prov_ins, medicaid
  ),
  any = mean(any_insurance, na.rm = TRUE),
  pvt = mean(pvt_insurance, na.rm = TRUE),
  emp = mean(emp_prov_ins, na.rm = TRUE),
  medicaid = mean(medicaid, na.rm = TRUE),
)

ggplot(
  mutate(
    select(ageSeries, year, age, medicaid),
    year = as.character(year)
  ),
  aes(
    x = age, 
    y = medicaid,
    color = year
  )
) + geom_line()

ggplot(
  mutate(
    select(ageSeries, year, age, medicaid),
    age = as.character(age)
  ),
  aes(
    x = year, 
    y = medicaid,
    color = age
  )
) + geom_line()

#plt_anyOT <- 
ggplot(
  mutate(
    select(filter(ageSeries, age == 21), year, medicaid),
    year = as.character(year)
  ),
  aes(year,medicaid)
) + geom_col() + ylim(0,0.2)

