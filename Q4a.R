##Question 2##

# Loading the necessary library
install.packages("dplyr")
library(dplyr)

# Setting the number of friends and the seed for reproducibility
num_friends <- 20
set.seed(0)

# Generating independent true heights for each person's measurements
edward_true_heights <- rnorm(num_friends, 170, 10)
hugo_true_heights <- rnorm(num_friends, 170, 10)
lucy_true_heights <- rnorm(num_friends, 170, 10)

# Edward, Hugo, and Lucy's measurements with respective error distributions
edward_measurements <- edward_true_heights + rnorm(num_friends, 0, 2)
hugo_measurements <- hugo_true_heights + rnorm(num_friends, 0, 3)
lucy_measurements <- lucy_true_heights + rnorm(num_friends, 0, 1.5)

# Creating a data frame for the independent scenario
independent_data <- data.frame(
  Edward_True_Height = edward_true_heights,
  Edward_Measured = edward_measurements,
  Hugo_True_Height = hugo_true_heights,
  Hugo_Measured = hugo_measurements,
  Lucy_True_Height = lucy_true_heights,
  Lucy_Measured = lucy_measurements
)

# Performing three statistical tests
# Test 1: T-test comparing Edward's true heights and his measurements
ttest_edward <- t.test(independent_data$Edward_True_Height, independent_data$Edward_Measured, paired = TRUE)

# Test 2: T-test comparing Hugo's true heights and his measurements
ttest_hugo <- t.test(independent_data$Hugo_True_Height, independent_data$Hugo_Measured, paired = TRUE)

# Test 3: T-test comparing Lucy's true heights and her measurements
ttest_lucy <- t.test(independent_data$Lucy_True_Height, independent_data$Lucy_Measured, paired = TRUE)

# Outputting the independent data and test results
print(independent_data)

# Compiling and displaying test results
test_results <- data.frame(
  Test = c("Edward", "Hugo", "Lucy"),
  T_Statistic = c(ttest_edward$statistic, ttest_hugo$statistic, ttest_lucy$statistic),
  P_Value = c(ttest_edward$p.value, ttest_hugo$p.value, ttest_lucy$p.value)
)

print(test_results)


##Question 4##

install.packages("ggplot2")
install.packages("reshape2")

library(ggplot2)
library(reshape2)

# Assuming 'independent_data' is the DataFrame created in the previous R code
# Convert it to long format for easier plotting with ggplot2
long_data <- melt(independent_data, id.vars = c("Edward_True_Height", "Hugo_True_Height", "Lucy_True_Height"), 
                  variable.name = "Measurement", value.name = "Height")

# Plotting
ggplot(long_data, aes(x = Height, fill = Measurement)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribution of Heights Measured by Edward, Hugo, and Lucy",
       x = "Height (cm)", y = "Frequency") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")
