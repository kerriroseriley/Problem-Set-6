# Problem Set 6
# Author: Kerri Rose Riley


# Import modules
library(tidyverse)
library(haven)

# Read the GSS data
gss <- read_dta("~/Documents/PAI741/Problem Set 6/Data/gss7224_r1.dta")

# Variables for Analysis

# Checking variables labels and values - Example
gss$spkath 

# Variables and Recoding
# Recode to 0 if respondent agreed, Recode 1 if respondent disagreed, Recode "Don't Know" to 1


# spkath, colath, libath

# 
# Take the gss dataset and save changes back into gss
gss <- gss|>
  # Modify the spkath variable
  mutate(spkath = case_when(
    # If spkath equals 1, change it to 0
    spkath == 1 ~ 0,
    # If spkath equals 2, change it to 1
    spkath == 2 ~ 1,
    # All other responses are coded as missing
    TRUE ~ NA
  ))

# Now Repeat for other variables in Score

gss <- gss|>
  mutate(colath = case_when(
    colath == 4 ~ 0,
    colath == 5 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(libath = case_when(
    libath == 1 ~ 0,
    libath == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))


# Racists
# spkrac, colrac, librac
gss <- gss|>
  mutate(spkrac = case_when(
    spkrac == 1 ~ 0,
    spkrac == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(colrac = case_when(
    colrac == 4 ~ 0,
    colrac == 5 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(librac = case_when(
    librac == 1 ~ 0,
    librac == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))


# Communists
# spkcom, colcom, libcom
gss <- gss|>
  mutate(spkcom = case_when(
    spkcom == 1 ~ 0,
    spkcom == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(colcom = case_when(
    colcom == 4 ~ 0,
    colcom == 5 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(libcom = case_when(
    libcom == 1 ~ 0,
    libcom == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))


# militarists
# spkmil, colmil, libmil
gss <- gss|>
  mutate(spkmil = case_when(
    spkmil == 1 ~ 0,
    spkmil == 2 ~ 1,
   # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(colmil = case_when(
    colmil == 4 ~ 0,
    colmil == 5 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(libmil = case_when(
    libmil == 1 ~ 0,
    libmil == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))


# Homosexuals
# spkhomo, colhomo, libhomo
gss <- gss|>
  mutate(spkhomo = case_when(
    spkhomo == 1 ~ 0,
    spkhomo == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(colhomo = case_when(
    colhomo == 4 ~ 0,
    colhomo == 5 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

gss <- gss|>
  mutate(libhomo = case_when(
    libhomo == 1 ~ 0,
    libhomo == 2 ~ 1,
    # The rest is missing
    TRUE ~ NA
  ))

# Replicate figures
# Figure 1: 0-15 GSS Modified Stouffer Tolerance Battery, 1976-2024

# Replicate figures
# Figure 1: 0-15 GSS Modified Stouffer Tolerance Battery, 1976-2024

# Create intolerance score by adding together the sum of all the values for the respondent.

# Take the gss dataset and save the changes back into gss (|> = then)
gss <- gss |>
  # Create a new variable called intolerance_score
  mutate(intolerance_score = spkath + colath + libath +
           spkrac + colrac + librac +
           spkcom + colcom + libcom +
           spkmil + colmil + libmil +
           spkhomo + colhomo + libhomo)

# Take the mean score of all respondents by year

# Take gss dataset
figure1 <- gss |>
  # group the oberservations by year
  group_by(year) |>
  # For each year, calculate the average intolerance score
  # na.rm = TRUE tells R to ignore missing values when calculating the mean
  summarize(mean_intolerance = mean(intolerance_score, na.rm = TRUE))

# Take figure1 and filter
figure1 <- figure1 |>
  # Keep only rows where mean_intolerance is not NaN
  filter(!is.nan(mean_intolerance))

# Show table of data for figure1
figure1


# Create plot 1
plot(figure1$year, figure1$mean_intolerance,
     # Setting theplot type to a line plot
     type = "l",
     # Main title of figure
     main = "The 0-15 GSS Modified Stouffer Tolerance Battery, 1976-2024",
     # X-axis label
     xlab = "Year",
     # Y-axis label
     ylab = "Mean Intolerance Score",
     # Parameters for y-axis
     ylim = c(0,15)
)



# Figure 2: Intolerance Towards Racists and Homosexuals, 1976-2024
# Figure 2: Intolerance Towards Racists and Homosexuals, 1976-2024

# Take the gss dataset and save the changes back into gss
gss <- gss |>
  # Create two new variables
  mutate(
    # Create a racist_score by adding the three intolerance measures
    racist_score = spkrac + colrac + librac,
    # Create a homosexual_score by adding the three intolerance measures
    homosexual_score = spkhomo + colhomo + libhomo
  )

figure2 <- gss |>
  group_by(year) |>
  # Calculate the average racist and homosexual intolerance scores for each year
  summarize(
    # Calculate the mean racist score for each year, ignoring missing values
    racist_mean = mean(racist_score, na.rm = TRUE),
    # Calculate the mean homosexual score for each year, ignoring missing values
    homosexual_mean = mean(homosexual_score, na.rm = TRUE)
  )

# Remove years without data
# Take the figure2 dataset and save the filtered results back into figure2
figure2 <- figure2 |>
  # Keep only rows where both mean scores are not NaN
  filter(!is.nan(racist_mean) & !is.nan(homosexual_mean))


# Create plot
plot(figure2$year, figure2$racist_mean,
     type = "l",
     ylim = c(0, 3),
     main = "Intolerance Towards Racists and Homosexuals, 1976-2024",
     xlab = "Year",
     ylab = "Mean Intolerance Score")

# Add homosexual intolerance line
lines(figure2$year, figure2$homosexual_mean,
      lty = 2)

# Add legend
legend("topright",
       legend = c("Racists", "Homosexuals"),
       lty = c(1, 2))



# Figure 5: A Dichotomous Measure of Tolerance and Intolerance, 1976-2024
# If a respondent gives at least one intolerant response among the 15 items, they are classified as intolerant. (=1)

# Figure 5: A Dichotomous Measure of Tolerance and Intolerance, 1976-2024
# If a respondent gives at least one intolerant response among the 15 items, they are classified as intolerant. (=1)


# Create dichotomous measure

gss <- gss |>
  mutate(intolerant = case_when(
    intolerance_score == 0 ~ 0,
    intolerance_score > 0 ~ 1,
    # If there is a missing value for intolerance_score, keep it missing
    TRUE ~ NA
  ))

# Calculate proportion intolerant by year
figure5 <- gss |>
  group_by(year) |>
  # Calculate the proportion of respondents who are intolerant for each year
  # Ignore missing values when calculating the mean
  summarize(
    proportion_intolerant = mean(intolerant, na.rm = TRUE)
  ) |> 
  # Remove years with no data
  filter(!is.nan(proportion_intolerant))

# Create plot
plot(figure5$year, figure5$proportion_intolerant,
     type = "l",
     ylim = c(0, 1),
     main = "A Dichotomous Measure of Tolerance and Intolerance, 1976-2024",
     xlab = "Year",
     ylab = "Proportion Intolerant")



# Original Work: Explore the code book for the GSS. Find two interesting variables and create compelling univariate graphs to illustrate their central tendency, distribution, and spread.

# HEALTH

gss <- gss |>
  mutate(health = case_when(
    health == 1 ~ 4,
    health == 2 ~ 3,
    health == 3 ~ 2,
    health == 4 ~ 1,
    # The rest is missing
    TRUE ~ NA_real_
  ))

# Central tendency and spread

# Calculate the average self-reported health score, ignoring missing values
mean(gss$health, na.rm = TRUE)
# Calculate the standard deviation of self-reported health scores, ignoring missing values
sd(gss$health, na.rm = TRUE)

# Distribution of Health Responses
barplot(table(gss$health),
        main = "Distribution of Self-Reported Health",
        xlab = "Health",
        ylab = "Number of Respondents",
        names.arg = c("Poor", "Fair", "Good", "Excellent"))


# HAPPINESS

gss <- gss |>
  mutate(happy = case_when(
    happy == 1 ~ 3,
    happy == 2 ~ 2,
    happy == 3 ~ 1,
    # The rest is missing
    TRUE ~ NA_real_
  ))

# Central tendency and spread
mean(gss$happy, na.rm = TRUE)
sd(gss$happy, na.rm = TRUE)

# Distribution of Happiness Responses
barplot(table(gss$happy),
        main = "Distribution of Self-Reported Happiness",
        xlab = "Happiness",
        ylab = "Number of Respondents",
        names.arg = c("Not Too Happy", "Pretty Happy", "Very Happy"))



