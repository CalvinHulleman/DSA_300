library(haven)
library(forecast)
library(tseries)
library(vrtest)
library(ggplot2)
library(fracdiff)
library(TSA)
data5 <- read.csv("Inc,age,sex,edu,race/J332923.csv")

remove_columns <- c(seq(1, ncol(data5), by = 7))
subset_data5 <- data5[, -remove_columns]

years <- c(1975:1997, seq(1999, 2021, by = 2))
years2 <- c(1975:1984)
years3 <- c(1985:1997, seq(1999, 2021, by = 2))

names <- names(subset_data5)
indices <- seq(1, length(names), by = 6)
names[indices] <- paste0("interview_number", years)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(2, length(names), by = 6)
names[indices] <- paste0("age", years)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(3, length(names), by = 6)
names[indices] <- paste0("sex", years)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(4, 58, by = 6)
names[indices] <- paste0("degree", years2)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(5, 59, by = 6)
names[indices] <- paste0("income", years2)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(6, 60, by = 6)
names[indices] <- paste0("race", years2)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(64, length(names), by = 6)
names[indices] <- paste0("race", years3)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(65, length(names), by = 6)
names[indices] <- paste0("degree", years3)
names(subset_data5) <- names

names <- names(subset_data5)
indices <- seq(66, length(names), by = 6)
names[indices] <- paste0("income", years3)
names(subset_data5) <- names

subset_data5[, seq(1, ncol(subset_data5), by = 6)] <- row.names(subset_data5)

library(dplyr)
library(tidyr)

final_data <- subset_data5 |> 
  mutate(household_id = row_number()) |>  
  pivot_longer(cols = starts_with(c("interview_number", "age", "sex", "degree", "income", "race")), 
               names_to = c(".value", "year"), 
               names_pattern = "(\\D+)(\\d+)",  
               values_drop_na = TRUE) |> 
  arrange(household_id, year) |> na.omit()

final_data <- final_data[(final_data$year >= 1997 & final_data$age <= 120) | final_data$year < 1997 & final_data$age <= 97 & final_data$age > 0, ]
final_data <- final_data[final_data$income < 9999999, ]
final_data$race <- ifelse(final_data$race > 3 & final_data$race <= 8, 3, final_data$race)
final_data$race <- ifelse(final_data$race == 0, 3, final_data$race)
final_data <- subset(final_data, race != 9)
final_data$degree <- ifelse(final_data$degree == 5, 2, final_data$degree)
final_data <- subset(final_data, degree != 9 & degree != 0)
final_data <- subset(final_data, year > 1992)
final_data$sex <- ifelse(final_data$sex == 1, 0, 1)
final_data$degree <- ifelse(final_data$degree == 1, 0, 1)


###################################
time_series <- ts(final_data$income, start = 1975, end = 2021)
plot(time_series)
time_series

ggtsdisplay(time_series, plot.type = "partial", points = FALSE, lag.max = 20, smooth = TRUE,
            main = "Household income",
            xlab = "Year",
            ylab = "Income",
)

adf.test(time_series,k=0)
adf.test(time_series)
kpss.test(time_series)
# 2 out of 3 test conclude that the data is stationary
time_series %>% diff() %>% ggtsdisplay(main="")

set.seed(1)
time_series %>% auto.arima(trace = TRUE, max.d = 1)
#####################################
library(dplyr)

# Count the number of observations per household
household_counts <- final_data %>% 
  group_by(household_id) %>% 
  summarise(obs_count = n())

# Filter the dataset to keep households with more than one observation
final_data_filtered <- final_data %>%
  inner_join(household_counts, by = "household_id") %>%
  filter(obs_count > 1) %>%
  select(-obs_count)


library(plm)

final_data$age2 <- (final_data$age)^2

income_diff <- c(NA, diff(final_data_filtered$income))
final_data_filtered$diff_income <- income_diff
final_data_filtered$age2 <- (final_data_filtered$age)^2

model2 <- plm(diff_income ~ factor(year) + age + age2 + degree + sex + race + + age*degree + sex*degree + degree*race, 
             data = final_data_filtered,
             model = "within")
summary(model2)
model3 <- plm(diff_income ~ factor(year) + age2 + degree + race + + age*degree, 
              data = final_data_filtered,
              model = "within")
summary(model3)
model1 <- plm(diff_income ~ age + age2 + degree + sex + race + age*degree + sex*degree + degree*race, 
             data = final_data_filtered)

library(stargazer)
stargazer(model1, model2, type = "text")
# PLOT ALL REALTIONSHIPS
plot(final_data_filtered$age, final_data_filtered$income/1000, xlab = "Age", ylab = "Income (hundreds of thousands)")
#plot(final_data_filtered$degree, final_data_filtered$income)
#plot(final_data_filtered$sex, final_data_filtered$income)
#plot(final_data_filtered$race, final_data_filtered$income)

