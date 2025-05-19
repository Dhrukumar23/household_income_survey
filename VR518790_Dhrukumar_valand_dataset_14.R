# Set working directory and load data
setwd("C:/Users/Nikita/OneDrive/Desktop/eco_proj")
data <- read.csv("shiw_14.csv")
View(data)
summary(data)

# Load necessary libraries
library(lmtest)
library(car)
library(sandwich)
library(ivreg) 
library(AER)

# Exercise 1a: OLS regression
data$age <- 2014 - data$ancostr
data$bath2 <- ifelse(data$bagni == 2, 1, 0)  # Dummy for two bathrooms
model1a <- lm(valabit ~ age + bath2 + m2, data = data)
summary(model1a)

# Exercise 1b: Log-linear model
model1b <- lm(log(valabit) ~ age + bath2 + m2, data = data)
summary(model1b)

# Exercise 1c: Add log(impacq)
model1c <- lm(log(valabit) ~ age + bath2 + m2 + log(impacq), data = data)
summary(model1c)

# Exercise 1d: Hypothesis tests
linearHypothesis(model1c, "age = m2")
summary(model1c)$coefficients["bath2", ]

# Exercise 1e: Add m2 squared
model1e <- lm(log(valabit) ~ age + bath2 + m2 + I(m2^2) + log(impacq), data = data)
effect_50 <- coef(model1e)["m2"] + 2 * coef(model1e)["I(m2^2)"] * 50
effect_200 <- coef(model1e)["m2"] + 2 * coef(model1e)["I(m2^2)"] * 200
print(c(effect_50, effect_200))

# Exercise 2a: White test
white_test <- bptest(model1e, ~ fitted(model1e) + I(fitted(model1e)^2), data = data)
white_test

# Exercise 2b: RESET test
reset_test <- resettest(model1e)
reset_test

# Exercise 2c: Chow test (North vs Center/South)
# Chow test for structural break by area3 (North vs Other)
data$north_dummy <- ifelse(data$area3 == 1, 1, 0)  # 1=North, 0=Center/South
chow_model <- lm(log(valabit) ~ (age + bath2 + m2 + I(m2^2) + log(impacq)) * north_dummy,data = data)
#ftest
lht(chow_model, c("north_dummy = 0","age:north_dummy = 0","bath2:north_dummy = 0","m2:north_dummy = 0","I(m2^2):north_dummy = 0","log(impacq):north_dummy = 0"),test="Chisq")




# Exercise 2d: Add North/South dummies
data$north <- ifelse(data$area3 == 1, 1, 0)
data$south <- ifelse(data$area3 == 3, 1, 0)
model2d <- lm(log(valabit) ~ age + bath2 + m2 + I(m2^2) + log(impacq) + north + south, data = data)
summary(model2d)

# Exercise 2e: Test area effects
linearHypothesis(model2d, c("north = 0", "south = 0"))

# Exercise 3a: Return regression
data$return <- (data$valabit / data$impacq) - 1
model3a <- lm(return ~ age + bath2 + m2 + I(m2^2) + log(impacq) + north + south, data = data)
summary(model3a)

# Exercise 3b: IV regression
iv_model <- ivreg(return ~ age + bath2 + m2 + I(m2^2) + north + south + log(impacq) |
                    age + bath2 + m2 + I(m2^2) + north + south + cpi1 + cpi2, 
                  data = data)
summary(iv_model)


# 3c) Test for the relevance of additional instruments
newdata <- subset(data, select = c("impacq","cpi1","cpi2"))
cor(newdata, use = "complete.obs")

# First stage regression
firstReg <- lm(log(impacq) ~ cpi1 + cpi2 + age + bath2 + m2 + I(m2^2) + north + south, data = data)
summary(firstReg)  # Correct object name here

# Test if the additional instruments (cpi1 and cpi2) are significant
lht(firstReg, c("cpi1=0", "cpi2=0"))

# Alternative: Check diagnostics from IV model summary
reg2ivDiag <- summary(iv_model, diagnostics = TRUE)
data.frame(reg2ivDiag$diagnostics)[1,]  # First diagnostic usually the Weak instruments test

#3d) Test for the validity of instruments (Sargan test) and endogeneity (Wu-Hausman test).
data.frame(reg2ivDiag$diagnostics)[3,]
#wu-hausman test 
data.frame(reg2ivDiag$diagnostics)[2,]

#3e
iv_model$coefficients['bath2']
lht(iv_model, c("bath2=0"), test = "Chisq")


# Exercise 4a: Linear Probability Model
data$rendneg <- ifelse(data$varvalabit == 3, 1, 0)
model4a <- lm(rendneg ~ age + bath2 + m2 + I(m2^2) + log(impacq) + north + south, data = data)
summary(model4a)  # Coefficient on north

# Exercise 4b: Probit model
probit_model <- glm(rendneg ~ age + bath2 + m2 + I(m2^2) + log(impacq) + north + south,
                    family = binomial(link = "probit"), data = data)
summary(probit_model)  # Marginal effect for north

# Exercise 4c: Wald test for m2
linearHypothesis(probit_model, c("m2 = 0", "I(m2^2) = 0"))

# Exercise 4d: Test North vs South
linearHypothesis(probit_model, "north = south")

# Exercise 4e: Classification table
predicted <- ifelse(predict(probit_model, type = "response") > 0.5, 1, 0)
table(data$rendneg, predicted)
mean(predicted == data$rendneg)

