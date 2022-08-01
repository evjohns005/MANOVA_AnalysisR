library('mvnormtest')
library('car')
library('readr')

heartAttacks <- read.csv("C:/Users/evjoh/Downloads/heartAttacks.csv")
View(heartAttacks)

# How does gender (sex) influence some of the heart attack predictors like resting blood pressure (trestbps) and cholesterol (chol)?

# Data Wrangling to test assumptions:
str(heartAttacks$trestbps)
str(heartAttacks$chol)

heartAttacks$trestbps <- as.numeric(heartAttacks$trestbps)
heartAttacks$chol <- as.numeric(heartAttacks$chol)
heartAttacks$sex <- as.factor(heartAttacks$sex)

keeps <- c("trestbps", "chol")
heartAttacks1 <- heartAttacks[keeps]
heartAttacks2 <- as.matrix(heartAttacks1)

# Testing Assumptions:
# Sample size= 303 (Assumption met)
# Multivariate Normality=
heartAttacks2 <- na.omit(heartAttacks2)
mshapiro.test(t(heartAttacks2)) # (Assumption failed)

### Would stop here, but for practice purposes, will continue with setting up MANOVA

# Homogeneity of Variance
leveneTest(heartAttacks$trestbps, heartAttacks$sex, data=heartAttacks) #(Assumption met)

leveneTest(heartAttacks$chol, heartAttacks$sex, data=heartAttacks) #(Assumption failed)

# Absense of Multicollinearity

cor.test(heartAttacks$trestbps, heartAttacks$chol, method="pearson", use="complete.obs") #(Assumption met; value 0.12)

# Run Analysis:

MANOVA <- manova(cbind(trestbps, chol) ~ sex, data=heartAttacks)
summary(MANOVA)

### There is a significance shown for sex's influence on resting blood pressure and cholesterol.  To see which DV?

# Post Hocs

summary.aov(MANOVA, test="wilks")

### This analysis shows that sex's influence on resting blood pressure is not significant as a predictor for heart attacks, but sex's influence on cholesterol as a predictor of heart attacks is significant.


library(IDPmisc)
NaRV.omit(heartAttacks)
