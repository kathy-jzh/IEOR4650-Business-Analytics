setwd("/Users/kathy/Desktop/Columbia University/2019Spring/Business Analytics/HW1")

# 1(a) Load the data and print a summary of the variables.
eggdata = read.csv("EggProduction.csv")
summary(eggdata)

# 1(b) Run a regression of eggs on feed and interpret the result.
attach(eggdata)
linReg1 = lm(eggs~feed) # 0.1755
summary(linReg1)

# 1(c) Run a regression of eggs on feed and temperature and interpret the result.
linReg2 = lm(eggs~feed+temperature) # 0.1751
summary(linReg2)

# 1(d) Plot feed against temperature
plot(feed, temperature)
dummytem = factor(ifelse(0<=temperature & temperature<=35, 1, 0))
summary(dummytem)

# 1(e) Regress eggs on feed, temperature, and the new variable you created.
linReg3 = lm(eggs~feed+temperature+dummytem) # 0.2341
summary(linReg3) 

# 1(g) a 95% confidence interval for the regression coeffi- cients.
confint(linReg3, level=0.95)

# 1(h) what is a 99% prediction interval if the feed was 26 and the temperature was -2
newdata = data.frame(feed = 26, temperature=-2, dummytem = factor(1))
output = predict(linReg3, newdata, interval = "prediction", level = 0.99)
output


# 2  Specifically we want to look at quotes for new cars, 60 month term, FICO between 675 and 725, and amount between $30,000 and $40,000.
eCarData = read.csv("NomisB_e-Car_Data.csv")
eCarData$Approve_Date = as.Date(eCarData$Approve_Date, "%m/%d/%Y")
eCarData$Tier = factor(eCarData$Tier)
eCarData$Car_Type = factor(eCarData$Car_Type)
eCarData$Partner_Bin = factor(eCarData$Partner_Bin)

attach(eCarData)
ind = FICO >= 675 & FICO <= 725 & Term == 60 &  Amount >= 30000 & Amount <= 40000
cardata = eCarData[ind,] 
remove(ind)
detach(eCarData)
attach(cardata)

# 2(a) Find a linear regression model to predict the probability of the quote being accepted as a function of the APR offered.
linReg3  = lm(Outcome~Rate)
summary(linReg3) # 0.0004393

# 2(b) logistic regression
logitReg = glm(Outcome ~ Rate, family = binomial)
summary(logitReg) # r^2 = 1 - (Residual Deviance/Null Deviance) = 0.00144

# 2(c) adding the competitor rate to your linear model
linReg4 = lm(Outcome~Rate+Competition_Rate)
summary(linReg4) # 0.0133

# 2(d) compute the RSE of the predictions for customers in Partner Bin 1
bindata = cardata[Partner_Bin==1,]
detach(cardata)
attach(bindata)
pred = predict(linReg4, bindata)
RSS = sum((pred-Outcome)**2)
RSE = sqrt(SSE/(length(pred)-1-2))
