# Policy Evaluation & Impact Analysis | Master in Data Science
# Gianmarco Santoro


# ----
# Set the working folder
setwd("/Users/GianmarcoSantoro/miniconda3/Projects/6_Policy-Rungi/Project_Impact")

# Load file with data
load("Greek_Elections.Rdata")

# Data
summary(muni)


# ----
# First analysis considering only values in 2016, after treatment
   
post_treat_data <- muni[muni$year == 2016,]
post_treat_model <- lm(gdvote ~ treatment, data = post_treat_data)
summary(post_treat_model)

# Call:
#   lm(formula = gdvote ~ treatment, data = post_treat_data)
# 
# Residuals:
#    Min     1Q Median     3Q    Max
# -5.659 -1.904 -0.091  1.426  9.109
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
#   (Intercept)   5.6591     0.2513  22.517  < 2e-16 ***
#   treatment     2.7448     0.7109   3.861 0.000207 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.303 on 94 degrees of freedom
# Multiple R-squared:  0.1369,	Adjusted R-squared:  0.1277
# F-statistic: 14.91 on 1 and 94 DF,  p-value: 0.000207


# ----
# Calc diff-in-diff between 2015 and 2016, between treatment and control groups

# Difference in means between treatment and control, PRE-treatment
pre_diff <- mean(muni$gdvote[muni$ever_treated == T & muni$year == 2015]) - 
            mean(muni$gdvote[muni$ever_treated == F & muni$year == 2015])
pre_diff

# [1] 0.6212737

# Difference in means between treatment and control, POST-treatment
post_diff <- mean(muni$gdvote[muni$ever_treated == T & muni$year == 2016]) - 
             mean(muni$gdvote[muni$ever_treated == F & muni$year == 2016])
post_diff

# [1] 2.744838

# diff-in-diff
diff_in_diff <- post_diff - pre_diff
diff_in_diff

# [1] 2.123564


# ----
# Linear regression to estimate the difference-in-differences with an interaction term

# Subset considering 2015 and 2016 records
muni_from_2015 <- muni[muni$year >= 2015,]

# If year is 2016 is considered post treatment, so new variable to take it into account
muni_from_2015$post_treat <- muni_from_2015$year == 2016

# Calculate the difference-in-differences
interaction_model <- lm(gdvote ~ ever_treated * post_treat, data = muni_from_2015)
summary(interaction_model)

# Call:
#   lm(formula = gdvote ~ ever_treated * post_treat, data = muni_from_2015)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.6591 -1.6784 -0.2008  1.3713  9.1088 
# 
# Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)                       4.3854     0.2415  18.162  < 2e-16 ***
#   ever_treatedTRUE                  0.6213     0.6829   0.910 0.364147    
#   post_treatTRUE                    1.2737     0.3415   3.730 0.000253 ***
#   ever_treatedTRUE:post_treatTRUE   2.1236     0.9658   2.199 0.029120 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.213 on 188 degrees of freedom
# Multiple R-squared:  0.1762,	Adjusted R-squared:  0.163 
# F-statistic:  13.4 on 3 and 188 DF,  p-value: 5.795e-08


# ----
# Assess parallel trends assumption by plotting the evolution of outcome variable

# Calc gdvote mean variable by year and whether the unit was ever_treated
# Aggregate by outcome variable and calc means:
vote_means <- aggregate(x = muni$gdvote, 
                        by = list(muni$year, muni$ever_treated), 
                        FUN = mean)

names(vote_means) <- c("year", "treated", "gdvote")

plot(x    = vote_means$year,
     y    = vote_means$gdvote,
     col  = ifelse(vote_means$treated, "navy", "lightblue"),
     pch  = 15,
     xlab = "Year",
     ylab = "% GD Vote",
     main = "Trends")

lines(x   = vote_means$year[vote_means$treated == T], 
      y   = vote_means$gdvote[vote_means$treated == T], 
      col = "navy")

lines(x   = vote_means$year[vote_means$treated ==  F], 
      y   = vote_means$gdvote[vote_means$treated == F], 
      col = "lightblue")

legend("topleft", legend = c("Treated", "Control"), col = c("navy", "lightblue"), pch = 15)


# ----
# Fixed-effects regression to estimate the diff-in-diff

fixed_eff_model <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + treatment, 
                      data = muni)
summary(fixed_eff_model)

# Considering only treatment coef, the one intresting
summary(fixed_eff_model)$coefficients['treatment',]

#     Estimate   Std. Error      t value     Pr(>|t|) 
# 2.087002e+00 3.932824e-01 5.306624e+00 2.252636e-07 


# ----
# Changin treatment variable for the trarrprop variable

fixed_eff_model_Prop <- lm(gdvote ~ as.factor(municipality) + as.factor(year) + trarrprop, 
                           data  = muni)

summary(fixed_eff_model_Prop)
summary(fixed_eff_model_Prop)$coefficients['trarrprop',]

#    Estimate   Std. Error      t value     Pr(>|t|) 
# 6.061088e-01 1.324367e-01 4.576591e+00 7.082297e-06 
