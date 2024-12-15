##################################################
# ECON 418-518 Exam 3
# Michael Gunderson
# The University of Arizona
# michaelgunderson@arizona.edu 
# 15 December 2024
###################################################

#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)

#####################
# Question 3
#####################

##############
# Part (i)
##############

library(data.table)
data <- fread(file.choose())

# total_emp_it = β0 + β1 * state_i + β2 * time_t + β3 * (state_i * time_t) + ε_it

# This model includes state fixed effects via the state_i variable.
# Including state fixed effects accounts for time-invariant differences between New Jersey and Pennsylvania,
# such as economic conditions or industry characteristics.

##############
# Part (ii)
##############

# Creating new indicator columns
data[, time_nov := ifelse(time_period == "November", 1, 0)]
data[, state_nj := ifelse(state == 1, 1, 0)]

# mean total employment for each state and time period
mean_employment <- data[, .(mean_total_emp = mean(total_emp, na.rm = TRUE)), by = .(state, time_period)]

# View answers
print(mean_employment)

# 23.38000, 21.09667, 20.43058, 25.89725

##############
# Part (iii)
##############

# Calculate mean employment for each group
mean_NJ_Post <- mean(data[state == 1 & time_period == "Nov", total_emp], na.rm = TRUE)
mean_NJ_Pre <- mean(data[state == 1 & time_period == "Feb", total_emp], na.rm = TRUE)
mean_PA_Post <- mean(data[state == 0 & time_period == "Nov", total_emp], na.rm = TRUE)
mean_PA_Pre <- mean(data[state == 0 & time_period == "Feb", total_emp], na.rm = TRUE)

# Compute the DiD estimate
DiD_estimate <- (mean_NJ_Post - mean_NJ_Pre) - (mean_PA_Post - mean_PA_Pre)

# Print 
cat("DiD Estimate:", DiD_estimate, "\n")

library(ggplot2)

# Prepare data for plotting
plot_data <- data[, .(mean_total_emp = mean(total_emp, na.rm = TRUE)), by = .(state, time_period)]
plot_data$state <- factor(plot_data$state, labels = c("Pennsylvania", "New Jersey"))

# Create the DiD plot
ggplot(plot_data, aes(x = time_period, y = mean_total_emp, color = state, group = state)) +
  geom_line(size = 1) +                      # Connect points with lines
  geom_point(size = 3) +                     # Show points for each group
  labs(
    title = "Difference-in-Differences Plot",
    x = "Time Period",
    y = "Mean Total Employment",
    color = "State"
  ) +
  theme_minimal()

##############
# Part (iv)
##############

# Estimate the DiD model using lm()
DiD_model <- lm(total_emp ~ state + time_nov + state:time_nov, data = data)

# Viewing the model summary to get coefficients and standard errors
summary(DiD_model)

# Compute confidence interval
beta_hat <- 7.75
SE <- 1.25
z_value <- 1.96

lower_bound <- beta_hat - z_value * SE
upper_bound <- beta_hat + z_value * SE

cat("95% Confidence Interval: [", lower_bound, ",", upper_bound, "]\n")

# Compute confidence interval
confint(DiD_model, level = 0.95)

t_value <- (beta_hat - 5) / SE
p_value <- 2 * (1 - pt(abs(t_value), df = nrow(data) - length(DiD_model$coefficients)))
cat("P-value:", p_value, "\n")

##############
# Part (v)
##############

# # Test the parallel trends assumption by using pre-treatment data.
# Step 1: Plot mean employment over time for both states to visually assess trends.
# Step 2: Run a regression with a state-time interaction term using pre-treatment data.
# Step 3: If the interaction term is not significant, the parallel trends assumption holds.

##########################
# (vi) 
##########################

# The release of the study in April 1992 is an example of a confounding event,
# also called an external shock, which coincides with the policy change.
# This could bias the Difference-in-Differences (DiD) estimate by violating
# the parallel trends assumption.

# Step 1: Assess potential changes in employment trends caused by the event.
# We can compare the pre-treatment and post-treatment trends for both states.

# Step 2: Add an indicator variable for the external shock (after April 1992).
data[, shock := ifelse(time_period == "Nov", 1, 0)]

# Step 3: Modify the DiD regression to include the external shock.
DiD_with_shock <- lm(total_emp ~ state + time_nov + state:time_nov + shock, data = data)

# Step 4: Check if the external shock significantly impacts employment trends.
summary(DiD_with_shock)

# Step 5: Interpretation:
# If the coefficient for the "shock" variable is significant, the external event
# impacts employment. This means the DiD estimate is biased because the
# observed changes in employment may not be solely attributable to the minimum wage.
# If the "shock" variable is insignificant, the event is likely irrelevant to the analysis.

##########################
# (vii) 
##########################


# lm() treats fixed effects implicitly by creating indicator variables for each `restaurant_id`.
DiD_fixed_effects <- lm(total_emp ~ state + time_nov + state:time_nov + factor(restaurant_id), data = data)

# Step 2: View the summary of the new model.
summary(DiD_fixed_effects)

# Original DiD model:
summary(DiD_model)


# If the DiD estimate changes, this would happens 
# because restaurant fixed effects control for unobserved, 
# time-uninvolved characteristics
# specific to each restaurant
# If the DiD estimate doesn't change
# this suggests that unobserved, time-invariant differences 
# between restaurants are not significant.
# the original DiD estimate is robust even without controlling 
# for restaurant-level factors.

##########################
# (viii) 
##########################

# I would trust the estimate if the parallel trends assumption holds,
# verified in pre-treatment data analysis. And if there are no significant 
# events affecting employment trends differently between states.

##########################
# (ix) 
##########################

# One way to estimate the effect non-parametrically is by using Matching.
# This pairs treated restaurants (New Jersey) with control restaurants (Pennsylvania)
# that are similar based on things like pre-treatment employment or restaurant size.
# Then, you compare the difference in employment after the treatment.

# What's unique about this method is it doesn't assume any specific model setup,
# so it avoids assumptions. It focuses more on direct comparisons 
# between similar restaurants, making the treatment effect feel more local

