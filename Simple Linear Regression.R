info = read.csv("prostate_cancer.csv")

boxplot((info[[2]]))
boxplot((info[[3]]))
boxplot((info[[4]]))
boxplot((info[[5]]))
boxplot((info[[6]]))
boxplot((info[[7]]))

boxplot(log(info[[2]]))
boxplot(log(info[[3]]))
boxplot(log(info[[4]]))
boxplot(log(info[[5]])) 
boxplot(sqrt(info[[7]]))

PSA = log(info[[2]])
cancervol = log(info[[3]])
weight = log(info[[4]])
age = log(info[[5]])
benpros = info[[6]]
capspen = sqrt(info[[7]])
vesinv = info[[8]]
gleason = info[[9]]

#Regressing PSA on cancervol
fit1 = lm(PSA ~ cancervol)
summary(fit1)

#Regressing PSA on weight
fit2 = lm(PSA ~ weight)
summary(fit2)

#Regressing PSA on age
fit3 = lm(PSA ~ age)
summary(fit3)

#Regressing PSA on benpros
fit4 = lm(PSA ~ benpros)
summary(fit4)

#Regressing PSA on capspen
fit5 = lm(PSA ~ capspen)
summary(fit5)

#Regressing PSA on vesinv
fit6 = lm(PSA ~ vesinv)
summary(fit6)

#Regressing PSA on gleason
fit7 = lm(PSA ~ gleason)
summary(fit7)

#Regressing PSA on cancervol, capspen, vesinv and gleason
model1 = lm(PSA ~ cancervol + capspen + vesinv + gleason)
summary(model1)

#Regressing PSA on cancervol, vesinv and gleason
model2 = lm(PSA ~ cancervol + vesinv + gleason)
summary(model2)

#Comparing the 2 models - model1 and model2
anova(model2,model1)

#Regressing PSA on cancervol and vesinv
model3 = lm(PSA ~ cancervol + vesinv)
summary(model3)

#Comparing the 2 models - model2 and model3
anova(model3,model2)

#Regressing PSA on cancervol and gleason
model4 = lm(PSA ~ cancervol + gleason)
summary(model4)

#Comparing the 2 models - model2 and model4
anova(model4,model2)

#Regressing PSA on every predictor
model5 = lm(PSA ~ cancervol + weight + age + benpros + capspen + vesinv + gleason)
summary(model5)

#Comparing the 2 models - model2 and model5
anova(model2,model5)

#Regressing PSA on cancervol, weight, vesinv, gleason
model6 = lm(PSA ~ cancervol + weight + vesinv + gleason)
summary(model6)

#Comparing the 2 models - model6 and model5
anova(model6,model5)

#Regressing PSA on cancervol, weight, vesinv
model7 = lm(PSA ~ cancervol + weight + vesinv)
summary(model7)

#Comparing the 2 models - model6 and model7
anova(model7,model6)

#Plotting the model6
plot(fitted(model6), (resid(model6)))
qqnorm(resid(model6))
qqline(resid(model6))




 