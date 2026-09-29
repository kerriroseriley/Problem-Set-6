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
    spkath == 1 ~ 0,
    spkath == 2 ~ 1,
    is.na(spkath) ~ 1
  ))

gss <- gss|>
  mutate(colath = case_when(
    colath == 4 ~ 0,
    colath == 5 ~ 1,
    is.na(colath) ~ NA
  ))

gss <- gss|>
  mutate(libath = case_when(
    libath == 1 ~ 0,
    libath == 2 ~ 1,
    is.na(libath) ~ NA
  ))


# Racists
# Recode dont know as 1
# spkrac, colrac, librac
gss <- gss|>
  mutate(spkrac = case_when(
    spkrac == 1 ~ 0,
    spkrac == 2 ~ 1,
    is.na(spkrac) ~ NA
  ))

gss <- gss|>
  mutate(colrac = case_when(
    colrac == 4 ~ 0,
    colrac == 5 ~ 1,
    is.na(colrac) ~ NA
  ))

gss <- gss|>
  mutate(librac = case_when(
    librac == 1 ~ 0,
    librac == 2 ~ 1,
    is.na(librac) ~ NA
  ))


# Communists
# spkcom, colcom, libcom
gss <- gss|>
  mutate(spkcom = case_when(
    spkcom == 1 ~ 0,
    spkcom == 2 ~ 1,
    is.na(spkcom) ~ NA
  ))

gss <- gss|>
  mutate(colcom = case_when(
    colcom == 4 ~ 0,
    colcom == 5 ~ 1,
    is.na(colcom) ~ NA
  ))

gss <- gss|>
  mutate(libcom = case_when(
    libcom == 1 ~ 0,
    libcom == 2 ~ 1,
    is.na(libcom) ~ NA
  ))


# militarists
# spkmil, colmil, libmil
gss <- gss|>
  mutate(spkmil = case_when(
    spkmil == 1 ~ 0,
    spkmil == 2 ~ 1,
    is.na(spkmil) ~ NA
  ))

gss <- gss|>
  mutate(colmil = case_when(
    colmil == 4 ~ 0,
    colmil ==% 5 ~ 1,
    is.na(colmil) ~ NA
  ))

gss <- gss|>
  mutate(libmil = case_when(
    libmil == 1 ~ 0,
    libmil == 2 ~ 1,
    is.na(libmil) ~ NA
  ))


# Homosexuals
# spkhomo, colhomo, libhomo
gss <- gss|>
  mutate(spkhomo = case_when(
    spkhomo == 1 ~ 0,
    spkhomo == 2 ~ 1,
    is.na(spkhomo) ~ NA
  ))

gss <- gss|>
  mutate(colhomo = case_when(
    colhomo == 4 ~ 0,
    colhomo == 5 ~ 1,
    is.na(colhomo) ~ NA
  ))

gss <- gss|>
  mutate(libhomo = case_when(
    libhomo == 1 ~ 0,
    libhomo == 2 ~ 1,
    is.na(libhomo) ~ NA
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


