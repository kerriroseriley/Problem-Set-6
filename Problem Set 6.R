
# Read the GSS data
gss <- read_dta("~/Documents/PAI741/Problem Set 6/Data/gss7224_r1.dta")

# Figure 1: 0-15 GSS Modified Stouffer Tolerance Battery, 1976-2024
plot(x, y,
     type = "l",
     xlab = "X-axis",
     ylab = "Y-axis",
     main = "Line Plot")

# Figure 2: Intolerance Towards Racists and Homosexuals, 1976-1998
plot(x, y,
     type = "l",
     xlab = "X-axis",
     ylab = "Y-axis",
     main = "Line Plot")

# Figure 5: A dichotomous Measure of Tolerance and Intolerance, 1976-2024
plot(x, y,
     type = "l",
     xlab = "X-axis",
     ylab = "Y-axis",
     main = "Line Plot")



# Original Work: explore the code book for the GSS. Find two interesting variables and create compelling univariate graphs to illustrate their central tendency, distribution, and spread.