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
gss <- gss|>
  mutate(spkath = case_when(
    spkath %in% 1 ~ 0,
    spkath %in% 2 ~ 1,
    is.na(spkath) ~ NA_real_
  ))

gss <- gss|>
  mutate(colath = case_when(
    colath %in% 4 ~ 0,
    colath %in% 5 ~ 1,
    is.na(colath) ~ NA_real_
  ))

gss <- gss|>
  mutate(libath = case_when(
    libath %in% 1 ~ 0,
    libath %in% 2 ~ 1,
    is.na(libath) ~ NA_real_
  ))


# Racists
# Recode dont know as 1
# spkrac, colrac, libracgss
gss <- gss|>
  mutate(spkrac = case_when(
    spkrac %in% 1 ~ 0,
    spkrac %in% 2 ~ 1,
    is.na(spkrac) ~ NA_real_
  ))

gss <- gss|>
  mutate(colrac = case_when(
    colrac %in% 4 ~ 0,
    colrac %in% 5 ~ 1,
    is.na(colrac) ~ NA_real_
  ))

gss <- gss|>
  mutate(librac = case_when(
    librac %in% 1 ~ 0,
    librac %in% 2 ~ 1,
    is.na(librac) ~ NA_real_
  ))


# Communists
# spkcom, colcom, libcom
gss <- gss|>
  mutate(spkcom = case_when(
    spkcom %in% 1 ~ 0,
    spkcom %in% 2 ~ 1,
    is.na(spkcom) ~ NA_real_
  ))

gss <- gss|>
  mutate(colcom = case_when(
    colcom %in% 4 ~ 0,
    colcom %in% 5 ~ 1,
    is.na(colcom) ~ NA_real_
  ))

gss <- gss|>
  mutate(libcom = case_when(
    libcom %in% 1 ~ 0,
    libcom %in% 2 ~ 1,
    is.na(libcom) ~ NA_real_
  ))


# militarists
# spkmil, colmil, libmil
gss <- gss|>
  mutate(spkmil = case_when(
    spkmil %in% 1 ~ 0,
    spkmil %in% 2 ~ 1,
    is.na(spkmil) ~ NA_real_
  ))

gss <- gss|>
  mutate(colmil = case_when(
    colmil %in% 4 ~ 0,
    colmil %in% 5 ~ 1,
    is.na(colmil) ~ NA_real_
  ))

gss <- gss|>
  mutate(libmil = case_when(
    libmil %in% 1 ~ 0,
    libmil %in% 2 ~ 1,
    is.na(libmil) ~ NA_real_
  ))


# Homosexuals
# spkhomo, colhomo, libhomo
gss <- gss|>
  mutate(spkhomo = case_when(
    spkhomo %in% 1 ~ 0,
    spkhomo %in% 2 ~ 1,
    is.na(spkhomo) ~ NA_real_
  ))

gss <- gss|>
  mutate(colhomo = case_when(
    colhomo %in% 4 ~ 0,
    colhomo %in% 5 ~ 1,
    is.na(colhomo) ~ NA_real_
  ))

gss <- gss|>
  mutate(libhomo = case_when(
    libhomo %in% 1 ~ 0,
    libhomo %in% 2 ~ 1,
    is.na(libhomo) ~ NA_real_
  ))

# Replicate figures
# Figure 1: 0-15 GSS Modified Stouffer Tolerance Battery, 1976-2024

gss <- gss |>
  mutate(intolerance_score = spkath + colath + libath +
           spkrac + colrac + librac +
           spkcom + colcom + libcom +
           spkmil + colmil + libmil +
           spkhomo + colhomo + libhomo)

# Take the mean score of all respondents by year'

figure1 <- gss |>
  group_by(year) |>
  summarize(mean_intolerance = mean(intolerance_score, na.rm = TRUE))

figure1 <- figure1 |>
  filter(!is.nan(mean_intolerance))

figure1


# Create plot
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

gss <- gss |>
  mutate(
    racist_score = spkrac + colrac + librac,
    homosexual_score = spkhomo + colhomo + libhomo
  )

figure2 <- gss |>
  group_by(year) |>
  summarize(
    racist_mean = mean(racist_score, na.rm = TRUE),
    homosexual_mean = mean(homosexual_score, na.rm = TRUE)
  )

# Remove years without data
figure2 <- figure2 |>
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

# Create dichotomous measure
gss <- gss |>
  mutate(intolerant = case_when(
    intolerance_score == 0 ~ 0,
    intolerance_score > 0 ~ 1,
    is.na(intolerance_score) ~ NA_real_
  ))

# Calculate proportion intolerant by year
figure5 <- gss |>
  group_by(year) |>
  summarize(
    proportion_intolerant = mean(intolerant, na.rm = TRUE)
  )

# Remove years with no data
figure5 <- figure5 |>
  filter(!is.nan(proportion_intolerant))
 
# Create plot
plot(figure5$year, figure5$proportion_intolerant,
     type = "l",
     ylim = c(0, 1),
     main = "A Dichotomous Measure of Tolerance and Intolerance, 1976-2024",
     xlab = "Year",
     ylab = "Proportion Intolerant")



# Original Work: explore the code book for the GSS. Find two interesting variables and create compelling univariate graphs to illustrate their central tendency, distribution, and spread.


