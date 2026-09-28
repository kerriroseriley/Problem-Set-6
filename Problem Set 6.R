
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
    is_tagged_na(spkath) & na_tag(spkath) == "d" ~ 1,
    is.na(spkath) ~ NA_real_
  ))

gss <- gss|>
  mutate(colath = case_when(
    colath %in% 4 ~ 0,
    colath %in% 5 ~ 1,
    is_tagged_na(colath) & na_tag(colath) == "d" ~ 1,
    is.na(colath) ~ NA_real_
  ))

gss <- gss|>
  mutate(libath = case_when(
    libath %in% 1 ~ 0,
    libath %in% 2 ~ 1,
    is_tagged_na(libath) & na_tag(libath) == "d" ~ 1,
    is.na(libath) ~ NA_real_
  ))


# Racists
# Recode dont know as 1
# spkrac, colrac, libracgss
gss <- gss|>
  mutate(spkrac = case_when(
    spkrac %in% 1 ~ 0,
    spkrac %in% 2 ~ 1,
    is_tagged_na(spkrac) & na_tag(spkrac) == "d" ~ 1,
    is.na(spkrac) ~ NA_real_
  ))

gss <- gss|>
  mutate(colrac = case_when(
    colrac %in% 4 ~ 0,
    colrac %in% 5 ~ 1,
    is_tagged_na(colrac) & na_tag(colrac) == "d" ~ 1,
    is.na(colrac) ~ 1
  ))

gss <- gss|>
  mutate(librac = case_when(
    librac %in% 1 ~ 0,
    librac %in% 2 ~ 1,
    is_tagged_na(librac) & na_tag(librac) == "d" ~ 1,
    is.na(librac) ~ 1
  ))


# Communists
# spkcom, colcom, libcom
gss <- gss|>
  mutate(spkcom = case_when(
    spkcom %in% 1 ~ 0,
    spkcom %in% 2 ~ 1,
    is_tagged_na(spkcom) & na_tag(spkcom) == "d" ~ 1,
    is.na(spkcom) ~ NA_real_
  ))

gss <- gss|>
  mutate(colcom = case_when(
    colcom %in% 4 ~ 0,
    colcom %in% 5 ~ 1,
    is_tagged_na(colcom) & na_tag(colcom) == "d" ~ 1,
    is.na(colcom) ~ NA_real_
  ))

gss <- gss|>
  mutate(libcom = case_when(
    libcom %in% 1 ~ 0,
    libcom %in% 2 ~ 1,
    is_tagged_na(libcom) & na_tag(libcom) == "d" ~ 1,
    is.na(libcom) ~ NA_real_
  ))


# militarists
# spkmil, colmil, libmil
gss <- gss|>
  mutate(spkmil = case_when(
    spkmil %in% 1 ~ 0,
    spkmil %in% 2 ~ 1,
    is_tagged_na(cpkmil) & na_tag(spkmil) == "d" ~ 1,
    is.na(spkmil) ~ NA_real_
  ))

gss <- gss|>
  mutate(colmil = case_when(
    colmil %in% 4 ~ 0,
    colmil %in% 5 ~ 1,
    is_tagged_na(colmil) & na_tag(colmil) == "d" ~ 1,
    is.na(colmil) ~ NA_real_
  ))

gss <- gss|>
  mutate(libmil = case_when(
    libmil %in% 1 ~ 0,
    libmil %in% 2 ~ 1,
    is_tagged_na(libmil) & na_tag(libmil) == "d" ~ 1,
    is.na(libmil) ~ NA_real_
  ))


# Homosexuals
# spkhomo, colhomo, libhomo
gss <- gss|>
  mutate(spkhomo = case_when(
    spkhomo %in% 1 ~ 0,
    spkhomo %in% 2 ~ 1,
    is_tagged_na(spkhomo) & na_tag(spkhomo) == "d" ~ 1,
    is.na(spkhomo) ~ NA_real_
  ))

gss <- gss|>
  mutate(colhomo = case_when(
    colhomo %in% 4 ~ 0,
    colhomo %in% 5 ~ 1,
    is_tagged_na(colhomo) & na_tag(colhomo) == "d" ~ 1,
    is.na(colhomo) ~ NA_real_
  ))

gss <- gss|>
  mutate(libhomo = case_when(
    libhomo %in% 1 ~ 0,
    libhomo %in% 2 ~ 1,
    is_tagged_na(libhomo) & na_tag(libhomo) == "d" ~ 1,
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

# Take the mean score of all respondents by year
figure1 <- gss |>
  group_by(year) |>
  summarize(mean_intolerance = mean(intolerance_score, na.rm = TRUE))

figure1


  
  


# Create plot
plot(year, y,
     # Setting the plot type to a line plot
     type = "l",
     # title of the figure
     main = "0-15 GSS Modified Stouffer Tolerance Battery, 1976-2024",
     # x-axis label
     xlab = "Year",
     # y-axis label
     ylab = "Tolerance Battery")


# Figure 2: Intolerance Towards Racists and Homosexuals, 1976-2024
plot(year, y,
     type = "l",
     main = "Intolerance Towards Racists and Homosexuals, 1976-2024",
     xlab = "Year",
     ylab = "Tolerance")

# Figure 5: A dichotomous Measure of Tolerance and Intolerance, 1976-2024
plot(year, y,
     type = "l",
     main = "A Dichotomous Measure of Tolerance and Intolerance, 1976-2024",
     xlab = "Year",
     ylab = "Proportion Intolerant")


# Original Work: explore the code book for the GSS. Find two interesting variables and create compelling univariate graphs to illustrate their central tendency, distribution, and spread.


