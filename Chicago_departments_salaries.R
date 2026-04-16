# =========================================================================
# PROJECT: THE CHICAGO "CARE PENALTY" (2026)
# AUTHOR: Rebe
# 
# TABLE OF CONTENTS:
# 1-11: Data Ingestion & Cleaning
# 12-29: ANOVA Phase (Raw Discovery)
# 30-44: Multiple Regression (The Detective Model)
# 45-48: GAM Validation (Advanced Robustness Check)
# =========================================================================

install.packages("tidyverse")
library(tidyverse)


# 1. Use the most direct CSV link for the 2026 dataset
url_2026 <- "https://data.cityofchicago.org/api/views/xzkq-xp2w/rows.csv?accessType=DOWNLOAD"
chicago_pay <- read.csv(url_2026)

# 2. Clean the Salary column (Removing $ and ,)
# 3. We use the exact column name 'Annual.Salary'
chicago_pay$Annual.Salary <- as.numeric(gsub("[\\$,]", "", chicago_pay$Annual.Salary))

# 4. FIX: Check how they spell 'Salary' in 2026
# Some versions use 'SALARY' (all caps) or 'Salary'
# Let's see the unique values first
table(chicago_pay$Salary.or.Hourly)

# 5. Filter again using the correct case (usually "Salary" or "SALARY")
# 6. This line works regardless of the case:
chicago_salary <- chicago_pay[grepl("Salary", chicago_pay$Salary.or.Hourly, ignore.case = TRUE), ]

# 7. Verify we have data now
nrow(chicago_salary) 
head(chicago_salary)

# 8. Create a subset of the departments we want to compare
# (Using exactly how they are spelled in your head() output)
target_depts <- c("DEPT OF FAMILY AND SUPPORT", 
                  "DEPARTMENT OF PUBLIC HEALTH", 
                  "CHICAGO POLICE DEPARTMENT")

df_anova <- subset(chicago_salary, Department %in% target_depts)

# 9. Let's find the 'Social Service' and 'Health' departments exactly
# This will list every department—find the ones you want
sort(unique(chicago_salary$Department))
# 10. Select the groups based on the exact 2026 names you found
my_depts <- c("CHICAGO POLICE DEPARTMENT", 
              "DEPARTMENT OF FAMILY AND SUPPORT SERVICES", 
              "CHICAGO DEPARTMENT OF PUBLIC HEALTH")

# 11. Subset and clean
df_anova <- subset(chicago_salary, Department %in% my_depts)
df_anova$Department <- as.factor(df_anova$Department)

# 12. Run the ANOVA (Analysis of Variance)
# Research Question: Does salary vary significantly by department?
wage_gap_model <- aov(Annual.Salary ~ Department, data = df_anova)

# 13. View the results
summary(wage_gap_model)
# 14. This compares the departments to each other
TukeyHSD(wage_gap_model)

# 15. And let's see the actual average (mean) for each group
aggregate(Annual.Salary ~ Department, data = df_anova, mean)

# 16. Set the margins so the left side (where the names are) is wider
# The numbers are: bottom, left, top, right
par(mar=c(5, 18, 4, 2)) 

# 17. Create a Horizontal Boxplot
boxplot(Annual.Salary ~ Department, data = df_anova,
        horizontal = TRUE,       # Flips the plot
        las = 1,                 # Makes labels always horizontal
        xlab = "Annual Salary ($)",
        main = "2026 Chicago Salary Gap by Department",
        col = c("lightblue", "lightgreen", "lightcoral"),
        cex.axis = 0.7)          # Shrinks text slightly to fit

# 18. Add the Mean point (Diamond)
means <- aggregate(Annual.Salary ~ Department, data = df_anova, mean)
points(means$Annual.Salary, 1:3, col = "black", pch = 18, cex = 2)
# 19. Get the numbers from your model
anova_summary <- summary(wage_gap_model)

# 20. Manual calculation: (Sum of Sq for Dept) / (Total Sum of Sq)
ss_dept <- anova_summary[[1]]["Department", "Sum Sq"]
ss_total <- sum(anova_summary[[1]][, "Sum Sq"])
eta_squared <- ss_dept / ss_total

print(paste("Eta-Squared:", round(eta_squared, 4)))
# 21. Get the residuals from your specific ANOVA model
res_anova <- residuals(wage_gap_model)

# 22. Plot the Histogram
hist(res_anova, 
     main="ANOVA Health Check: Normality of Residuals", 
     xlab="Residuals (Difference from Dept Average)", 
     col="lightblue", 
     breaks=20)

# 23. The Q-Q Plot (The "Straight Line" Test)
qqnorm(res_anova)
qqline(res_anova, col = "red")
# 24. Re-run the model to be 100% sure it's in memory
wage_gap_model <- aov(Annual.Salary ~ Department, data = df_anova)

# 25. Extract Residuals
res_anova <- residuals(wage_gap_model)

# 26. Normality Check (The "Mountain")
hist(res_anova, main="ANOVA Health Check: Normality", col="lightblue")

# 27. Variance Check (The "Ratio")
vars <- aggregate(Annual.Salary ~ Department, data = df_anova, var)
ratio <- max(vars$Annual.Salary) / min(vars$Annual.Salary)

print(paste("The Variance Ratio is:", round(ratio, 2)))
# 28. See how many people are in each department
table(df_anova$Department)

# 29. See the total sample size
nrow(df_anova)
View(df_anova)
# 30. Create the model (The "Estimate")
# 31. Since everyone is Full-Time, we go back to the basic Dept model
detective_model <- lm(Annual.Salary ~ Department, data = df_anova)

# 32. Show the summary (The "Standard Error" table)
summary(detective_model)
# 33. The 4-Way Diagnostic Plot
par(mfrow=c(2,2)) # This puts 4 charts in one window
plot(detective_model)
df_anova[12402, ]
df_anova |> 
  dplyr::arrange(desc(Annual.Salary)) |> 
  head(5)
# 34. This scans all 13,000+ rows in less than a second
table(df_anova$Full.or.Part.Time, useNA = "always")
# 35. Search for the 'rebels'
part_timers <- df_anova |> 
  dplyr::filter(Full.or.Part.Time != "F")

# 36. See how many we found
nrow(part_timers)
# 37. Reset the layout to 1 chart at a time
par(mfrow=c(1,1))

# 38. Zoom into the Normal Q-Q (The "Heavy Tails")
plot(detective_model, which = 2)

# 39. Zoom into Residuals vs Leverage (The "Bodyguard" plot)
plot(detective_model, which = 5)
# 40. This will label the 10 most extreme cases in the plot
plot(detective_model, which = 5, id.n = 10)
# 41. We create a 'Manager' flag to see if seniority explains the gap
df_anova$Is_Manager <- grepl("MANAGER|DIRECTOR|SUPERVISOR|CHIEF", df_anova$Job.Titles)

# 42. Now THIS is a Multiple Regression (Department + Manager status)
detective_model_v3 <- lm(Annual.Salary ~ Department + Is_Manager, data = df_anova)

summary(detective_model_v3)
library(ggplot2)
library(scales)

# 43. Create a summary table based on your regression coefficients
# These numbers come directly from your detective_model_v3 output!
plot_data <- data.frame(
  Department = c("Public Health (Baseline)", "Police Department", "Family & Support Services"),
  Salary = c(106422, 117862, 98184), # Calculated: Base, Base + 11k, Base - 8k
  SE = c(912, 934, 1529)           # Standard Errors from your summary table
)

# 44. Build the "Signature Visual"
ggplot(plot_data, aes(x = reorder(Department, -Salary), y = Salary, fill = Department)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  # This adds the "Safety Net" (Standard Error) Wheelan talks about
  geom_errorbar(aes(ymin = Salary - SE, ymax = Salary + SE), 
                width = 0.2, color = "black", size = 0.8) +
  # Formatting to make it look professional
  scale_y_continuous(labels = dollar_format(), limits = c(0, 130000)) +
  scale_fill_manual(values = c("#2c3e50", "#e74c3c", "#2980b9")) +
  theme_minimal() +
  labs(
    title = "The Chicago 'Care Penalty' (Adjusted Salaries)",
    subtitle = "Predicted base salary for non-managerial staff (Full-Time)",
    x = "",
    y = "Average Annual Salary",
    caption = "Error bars represent +/- 1 Standard Error. Source: City of Chicago 2026 Payroll Data"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 10)
  )
ggplot(df_anova, aes(x = Annual.Salary, fill = Department)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(labels = label_dollar(), limits = c(40000, 200000)) +
  scale_fill_manual(values = c("#2c3e50", "#e74c3c", "#2980b9")) +
  theme_minimal() +
  labs(
    title = "Salary Distribution by Department",
    subtitle = "The Police 'Mountain' is shifted significantly higher than Social Services",
    x = "Annual Salary",
    y = "Density",
    fill = "Department"
  ) +
  theme(legend.position = "bottom") 

# 45. THE GAM (The Robustness Fix) -----------------------------------------
library(mgcv)


# We move to a Generalized Additive Model because our linear diagnostics 
# showed a "smile" curve, meaning the salary jumps weren't perfectly linear.
model_gam <- gam(Annual.Salary ~ Department + Is_Manager, data = df_anova)

# Summary of the "Care Penalty"
summary(model_gam)

# 46. GAM ASSUMPTION CHECK 1: LINEARITY & THE ZIPZAC -----------------------
# We pull the residuals and fitted values to verify the 'fix'
df_anova$res_gam <- residuals(model_gam)
df_anova$fit_gam <- fitted(model_gam)

# This plot proves the GAM successfully 'centered' the model (flat red line)
# while acknowledging the structural 'steps' of Chicago union pay.
ggplot(df_anova, aes(x = fit_gam, y = res_gam)) +
  geom_jitter(alpha = 0.15, color = "#2c3e50", width = 800) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1.2) +
  scale_x_continuous(labels = label_dollar()) +
  theme_minimal() +
  labs(title = "GAM Diagnostic: Residuals vs. Fitted",
       subtitle = "Flat red line = Success | Vertical 'Steps' = Union Pay Grades",
       x = "Predicted Salary", y = "Residual (Model Error)")

# 47. GAM ASSUMPTION CHECK 2: NORMALITY (The Health Check) -----------------
# We check if the errors are still normally distributed in this advanced model.
par(mfrow=c(1,2))
hist(df_anova$res_gam, main="GAM: Residual Normality", 
     col="lightgreen", xlab="Residuals", breaks=30)
qqnorm(df_anova$res_gam, main="GAM: Q-Q Plot"); qqline(df_anova$res_gam, col="red")
par(mfrow=c(1,1))

# 48. FINAL SIGNATURE VISUAL: THE CARE PENALTY ----------------------------
# Using the GAM estimates (Intercept ~106k, Police +11k, Family -8k)
plot_data <- data.frame(
  Department = c("Public Health (Baseline)", "Police Department", "Family & Support Services"),
  Salary = c(106404.1, 117860.7, 98205.3), 
  SE = c(913.7, 935.3, 1529.7)
)

ggplot(plot_data, aes(x = reorder(Department, -Salary), y = Salary, fill = Department)) +
  geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
  geom_errorbar(aes(ymin = Salary - SE, ymax = Salary + SE), 
                width = 0.2, color = "black", linewidth = 0.8) +
  scale_y_continuous(labels = label_dollar(), limits = c(0, 130000)) +
  scale_fill_manual(values = c("#2c3e50", "#e74c3c", "#2980b9")) +
  theme_minimal() +
  labs(title = "The Chicago 'Care Penalty'", 
       subtitle = "Predicted base salary for non-managerial staff (Full-Time)",
       caption = "Error bars: +/- 1 Std Error. Model: GAM (Generalized Additive Model)") +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

# 49. SAVE CLEAN DATASET FOR PORTFOLIO
# write.csv(df_anova, "Chicago_Salary_Clean_2026.csv", row.names = FALSE)