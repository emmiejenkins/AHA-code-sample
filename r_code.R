library(readxl)
library(tidyverse)
library(glue)

# data cleaning and ggplots with COVID data

covid_data <- read_xlsx("Downloads/texas-covid-data.xlsx", range = cell_limits(c(3, 1), c(258, NA))) 
# selecting first 5 variables and their first 5 observations to get a view of the data
covid_data %>% select(1:5) %>% slice_head(n = 5) %>% glimpse()

# creating a function to change variable names
cases <- function(covid_case) {
  month <- str_extract_all(covid_case, "(.+(?=-))") # extracting first digit (month)
  day <- str_extract_all(covid_case, "(?<=-).+") # extracting second digit 
  glue("cases_{month}_{day}") # using extracted digits and glueing them into a new variable name
}

# renaming variables
covid_data <- covid_data %>% 
  rename(county_name = 'County Name', population = Population)  %>% 
  rename_with(cases, contains("ases")) 

# pivoting the data to include the "cases..." variables to one variable, "date", and their values to "count"
covid_data <- covid_data %>%
  pivot_longer(cases_03_04:cases_05_22, names_to = "date", values_to = "count")

# creating a funtion to extract date to create a date variable with a proper format
case_to_date <- function(case_date) {
  # extracting last two characters in the string to get day
  day <- str_sub(case_date, -2, -1) 
  # extracting the two characters that represent the month in the string
  month <- str_sub(case_date, -5, -4)
  glue("2020-{month}-{day}") 
}

covid_data <- mutate(covid_data, date = parse_date(case_to_date(date))) 
covid_data

# creating a data set that contains the counties in order from largest to smallest population
mini_covid <- covid_data %>% 
  group_by(county_name) %>%
  slice(1) %>% # taking first observation from each grouping 
  arrange(desc(population))
mini_covid

top_counties <- mini_covid[2:13, "county_name"] # removing "total" and only including 12 largest counties

top_counties_data <- covid_data %>%
  filter(county_name %in% top_counties$county_name) # using only data from the top 12 counties

# graphing Cumulative Cases over Time in the Largest Texas Counties seperately 
ggplot(top_counties_data, aes(date, count)) +
  geom_line() +
  facet_wrap(~county_name, scales = "free_y") +
  labs(title = "Cumulative Cases over Time in the Largest Texas Counties", x = NULL, y = NULL)

# creating a data set that provides info on the new cases over time for largest counties
new_cases_data <- top_counties_data %>% group_by(county_name) %>%
  mutate(diff = count - lag(count))

# bar chart of new cases over time for each of the largest counties
ggplot(new_cases_data, aes(date, diff)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~county_name, scales = "free_y") +
  labs(title = "New Cases over Time in the Largest Texas Counties", x = NULL, y = NULL)

# regression for cars data (not built-in cars data in R)

# exploratory data analysis
cars <- cars %>% mutate(discount = ((MSRP - Invoice) / MSRP) * 100, # creating new variable, "discount"
                        MPG_overall = .5 * MPG_Highway + .5 * MPG_City) # creating new variable for MPG overall
cars[1:4] <- lapply(cars[1:4], factor) # specifying the first 4 variables as factor variables

# correlation of the numeric variables of the car data set with each other
round(cor(cars[6:17]), 2)

# histograms and correlations of character variables
pairs.panels(cars[1:4],
             method = "pearson",
             density = TRUE,
             ellipses = TRUE)

# Fitting the full model for stepwise regression using MPG_overall as the response variable
# omitting mpg_city, mpg_highway, model, and length due to hogh correlation with other variables
full_model <- lm(MPG_overall ~ Make + Type + Origin + DriveTrain + MSRP + Invoice + EngineSize + Cylinders
                 + Horsepower + Weight + Wheelbase + discount, data = cars)

# creating a step model for the data
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
summary(step_model)

# summary of step model 
summary(lm(MPG_overall ~ Type + Horsepower + Weight + discount, data = cars))

# creating a new model with significant variables found above
summary(lm(MPG_overall ~ Type + Horsepower + Weight, data = cars))

# creating a variable that determines if the discount meets criteria to be a "high" discount
cars <- cars %>% mutate(high_discount = discount >= 10)


# LASSO regression with high_discount as response variable is initially used to shrink any of the variables with collinearity 
# down to zero. From there, the glm function was used to determine which predictor variables were significant. It was concluded that 
# Origin and Weight were the two main predictor variables for high_discount. Using the Origin and Weight for the Ford Supercab Lariat 
# from the data, we predict the truck to have a high_discount with a probability of .358.

X <- model.matrix(high_discount ~ MPG_overall + Make + Type + Origin + DriveTrain + MSRP + Invoice + EngineSize +
                    Cylinders + Horsepower + Weight + Wheelbase, data = cars)
Y <- unlist(cars[,"high_discount"])
Y <- Y[1:426]

# pulling out best lambda
cv_lambda_lasso <- cv.glmnet(X, Y, alpha = 1)

# getting coefs from lambda
l_lasso_min <- cv_lambda_lasso$lambda.min
lasso_model <- glmnet(X, Y, family = c("binomial"), alpha = 1, lambda = l_lasso_min)
lasso_model$beta #finding beta coefficients 

# fitting generalized linear model with high discount as response variable
glm(formula = high_discount ~ Type + Origin + DriveTrain + Horsepower +
       Cylinders + Wheelbase + Weight, data = cars)

glm_cars <- glm(high_discount ~ Origin + Weight, data = cars) # using significant variables
glm_cars %>% summary()




