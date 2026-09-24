
# Read the GSS data
gss <- read_dta("~/Documents/PAI741/Problem Set 6/Data/gss7224_r1.dta")

# Figure 1: 0-15 GSS Modified Stouffer Tolerance Battery, 1976-2024

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


