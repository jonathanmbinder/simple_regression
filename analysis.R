# load the data ----------------------------

df_possum <- read.csv("data/possum_data.csv", header=TRUE) # nine morphometric measurements on each of 
                                                    # 104 mountain brushtail possums, trapped at 
                                                    # seven Australian sites from Southern Victoria 
                                                    # to central Queensland

str(df_possum) # check the structure of the data frame

# visualize the data ----------------------------

## plot with base R -----------------------------

plot(df_possum$total_length, df_possum$head_length,               # plot with base R
  xlab="Total Length", 
  ylab="Head Length",
  main="Head Length vs Total Length")

# plot with ggplot2 ------------------------------

library(ggplot2) 

ggplot(df_possum, aes(x=total_length, y=head_length)) + 
  geom_point() + 
  labs(x="Total Length", y="Head Length", title="Head Length vs Total Length") 

# fit a linear model ----------------------------

model_possum <- lm(head_length ~ total_length, data=df_possum) # fit a linear model with head length as the 
                                                 # response variable and total length as the 
                                                 # predictor variable

str(model_possum) # check the structure of the model object

# the model object contains several components, including the coefficients, residuals, 
# fitted values, and more. We can access these components using the $ operator. 

model_possum$coefficients # get the coefficients of the model
model_possum$residuals # get the residuals of the model 
model_possum$fitted.values # get the fitted values of the model

summary(model_possum) # get a summary of the model

# regression is a linear equation of the form y = mx + b, where m is the slope and b is the intercept. 
# In this case, the slope (m) is 0.57 and the intercept (b) is 42.7. This means that 
# for every unit increase in total length, head length increases by 0.57 units

# check the assumptions of the model ----------------------------

# we need to check the assumptions of the linear regression model, which include linearity,
#  homoscedasticity, normality of residuals, and independence of residuals.

plot(model_possum$fitted.values, model_possum$residuals,  # plot the residuals against the fitted values to check for 
  xlab="Fitted Values",                     # homoscedasticity (constant variance of residuals)
  ylab="Residuals", 
  main="Residuals vs Fitted Values")

# we see no obvious pattern in the residuals, which suggests that the assumption of homoscedasticity 
# is met. 
 
qqnorm(model_possum$residuals)         # create a Q-Q plot to check for normality of residuals 
qqline(model_possum$residuals)        # add a reference line to the Q-Q plot 

# the points in the Q-Q plot roughly follow the reference line, which suggests that the 
# residuals are approximately normally distributed. 

library(performance) # load the performance package to check for independence of residuals
check_model(model_possum)
?check_heteroscedasticity
check_heteroscedasticity(model_possum)
check_normality(model_possum)

# publication-quality plots and tables ----------------------------
## plot model with ggplot2 ---------------------------- 

# you can plot the regression line on top of the scatter plot using ggplot2 by adding a geom_smooth() 
# layer with method="lm".

ggplot(df_possum, aes(x=total_length, y=head_length)) + 
  geom_point(fill="white", shape=21) + 
  geom_smooth(method="lm", se=FALSE, color="black") + 
  labs(x="Total Length (cm)", y="Head Length (cm)") +
theme_classic()

# save regression plot with ggsave()

ggsave("figures/possum_regression_plot.svg", width=6, height=4) # save the plot as a SVG file

# SVG plots are infinitely scalable and can be edited in vector graphics software like 
# Adobe Illustrator or Inkscape (free)

# you can also save as png or pdf if you prefer.
ggsave("figures/possum_regression_plot.png", width=6, height=4, dpi=600) # save the plot as a PNG file
ggsave("figures/possum_regression_plot.pdf", width=6, height=4, dpi=600) # save the plot as a PDF file

# use ggpubr package to display regression equation and R-squared value on the plot 

library(ggpubr) # load the ggpubr package to add regression equation and R-squared value to the plot 

ggplot(df_possum, aes(x=total_length, y=head_length)) + 
  geom_point(fill="white", shape=21) + 
  geom_smooth(method="lm", se=FALSE, color="black") + 
  labs(x="Total Length (cm)", y="Head Length (cm)") + 
  stat_regline_equation(
   label.y=102
  ) + # add regression equation to the plot 
  stat_cor(
  aes(label = paste(..rr.label.., ..p.label.., sep = "~")), # add R-squared value to the plot 
  label.y=101
  ) + 
  theme_classic()

## use package gtsummary to create a summary table of regression model ---------------------------

library(gtsummary) # load the gtsummary package to create a summary table of the regression model 

tbl_regression(model_possum, label= list(total_length="total length (cm)")) # create a summary table of the regression model

tbl_regression(model_possum,
label= list(total_length="total length (cm)"))  |> 
  as_flex_table() |> 
  flextable::save_as_image("tables/regression_table.png") # create a summary table of the regression model and save it as a PNG file

# example of model diagnostics with the cars dataset ----------------------------

?cars # the cars dataset contains the speed of cars and the distances taken to stop.

model_cars <- lm(dist ~ speed, data=cars)
# fit a linear model with expense as the response variable and income as the predictor variable 

summary(model_cars) # get a summary of the model for expense data
# for every increase in unit speed, the stopping distance increases by 3.93 units. 
# The R-squared value of 0.65 indicates that 
# 65% of the variation in stopping distance can be explained by the speed of the car.

plot(model_cars$fitted.values, model_cars$residuals, # plot the residuals against the fitted values to check for 
  xlab="Fitted Values",                              # homoscedasticity (constant variance of residuals) 
  ylab="Residuals", main="Residuals vs Fitted Values") 

qqnorm(model_cars$residuals) # create a Q-Q plot to check for normality of residuals 
qqline(model_cars$residuals) # add a reference line to the Q-Q plot 
check_model(model_cars) # check model visually
check_heteroscedasticity(model_cars) # check for homoscedasticity 
check_normality(model_cars) # check for normality of residuals

