library(MASS)
library(mixtools)
food_table = read.csv("datasets/food_table.csv")
dat_diet = read.csv("datasets/Dietterichetalcropnutritiondata.csv")

## Rice
dat_rice0 = dat_diet[dat_diet$Crop == "Rice", ]
co2_rice = c()
for (i in 1:nrow(dat_rice0)) {
  if (dat_rice0[i, "CO2_Treatment"] == "aCO2") {
    co2_rice[i] = dat_rice0[i, "Ambient_CO2_ppm"] 
  } else{
    co2_rice[i] = dat_rice0[i, "Elevated_CO2_Achieved_ppm"] 
  }
}
dat_rice0$CO2 = co2_rice
dat_rice = dat_rice0[dat_rice0$Temperature_Qualitative == "Ambient", ]
aggregate(Zn_ppm ~ CO2_Treatment, data = dat_rice, mean)
(26.12857 - 27.02411) / 10
aggregate(Fe_ppm ~ CO2_Treatment, data = dat_rice, mean)
(10.56429 - 11.27321) / 10

# Check equal vcov
dat_rice1 = dat_rice[dat_rice$CO2_Treatment == "aCO2", ]
dat_rice2 = dat_rice[dat_rice$CO2_Treatment == "eCO2", ]
mean(dat_rice1$Zn_ppm)/10
mean(dat_rice1$Fe_ppm)/10
mean(dat_rice2$Zn_ppm)/10
mean(dat_rice2$Fe_ppm)/10
dat_rice1 = dat_rice1[dat_rice1$Fe_ppm < 20, ]
dat_rice2 = dat_rice2[dat_rice2$Fe_ppm < 20, ]

cor(dat_rice1$Fe_ppm, dat_rice1$Zn_ppm)
cor(dat_rice2$Fe_ppm, dat_rice2$Zn_ppm)

aggregate(Zn_ppm ~ CO2_Treatment, data = dat_rice, sd)
aggregate(Fe_ppm ~ CO2_Treatment, data = dat_rice, sd)
library(biotools)
dat_rice0 = dat_rice[dat_rice$Fe_ppm < 20, ]
boxM(dat_rice0[, c("Zn_ppm", "Fe_ppm")], dat_rice0[, "CO2_Treatment"])
sigma_aCO2 = cov(dat_rice1[, c("Zn_ppm", "Fe_ppm")])
sigma_eCO2 = cov(dat_rice2[, c("Zn_ppm", "Fe_ppm")])

plot(dat_rice1$Fe_ppm, dat_rice1$Zn_ppm, xlab = "Fe, ppm", ylab = "Zn, ppm", xlim = c(0,25), ylim = c(17,40))
points(dat_rice2$Fe_ppm, dat_rice2$Zn_ppm, col = "red")
legend(x = "bottomright", legend = c("aCO2", "eCO2"), pch = 1, col = c(1, 2), cex = 0.8)  
ellipse(c(mean(dat_rice1$Fe_ppm), mean(dat_rice1$Zn_ppm)), sigma_aCO2, newplot = F, type = "l")
ellipse(c(mean(dat_rice2$Fe_ppm), mean(dat_rice2$Zn_ppm)), sigma_eCO2, newplot = F, type = "l", col = 2)

# model fitting
fit_rice1 = lm(cbind(Zn_ppm, Fe_ppm) ~ CO2 + Nitrogen_Application_Quantitative + 
                 Phosphorus_Application_Quantitative + Cultivar, data = dat_rice)
null = lm(Zn_ppm ~ 1, data = dat_rice)
full = lm(Zn_ppm ~ CO2 + Nitrogen_Application_Quantitative + 
            Phosphorus_Application_Quantitative + Cultivar + Potassium_Application_Quantitative + Year, data = dat_rice)
fit1 = step(null, scope = list(lower=null,upper=full), direction="both", k = log(224))
summary(fit1)

fit_rice_res = fit_rice1$residuals
shapiro.test(fit_rice_res[,1])
qqnorm(fit_rice_res[,1], main = "Rice, Zn")
qqline(fit_rice_res[,1])
qqnorm(fit_rice_res[,2], main = "Rice, Fe")
qqline(fit_rice_res[,2])
fit_sum1 = summary(fit_rice1)

Sigma = vcov(fit_rice1)[c("Zn_ppm:CO2", "Fe_ppm:CO2"), c("Zn_ppm:CO2", "Fe_ppm:CO2")]
Sigma[2,1] / sqrt(Sigma[1,1] * Sigma[2,2])
mu_rice = c(fit_sum1$`Response Zn_ppm`$coefficients[2,1], fit_sum1$`Response Fe_ppm`$coefficients[2,1])
delta_CO2 = 200 / 10
a_b = c(delta_CO2, delta_CO2)
Sigma_ab_rice = matrix(c(Sigma[1,1] * a_b[1]^2, Sigma[1,2] * a_b[1] * a_b[2], 
                    Sigma[2,1] * a_b[1] * a_b[2], Sigma[2,2] * a_b[2]^2), nrow = 2, byrow = T)
par(mfrow=c(1,2))
ellipse(mu_rice * a_b, Sigma_ab_rice, newplot = T, type = "l", ylim = c(-0.4, 0),
        xlab = "Zinc change, mg/100 g of rice", ylab = "Iron change, mg/100 g of rice", main = "Rice")
points((mu_rice * a_b)[1], (mu_rice * a_b)[2], pch = 16)

test_dat = read.csv("/Users/lijinghui/Desktop/Human_nutrition/Papers/2014 Loladze dataset/co2df.csv")
dat_rice_test = test_dat[test_dat$name == "rice", ]
dat_rice_test = dat_rice_test[dat_rice_test$tissue == "grain", ]
dat_rice_fe = dat_rice_test[dat_rice_test$element == "Fe", ]
dat_rice_zn = dat_rice_test[dat_rice_test$element == "Zn", ]
points(dat_rice_zn$delta[c(5:10)], dat_rice_fe$delta[c(5:10)], col = 'red')


# Wheat
dat_wheat0 = dat_diet[dat_diet$Crop == "Wheat", ]
co2_wheat = c()
for (i in 1:nrow(dat_wheat0)) {
  if (dat_wheat0[i, "CO2_Treatment"] == "aCO2") {
    co2_wheat[i] = dat_wheat0[i, "Ambient_CO2_ppm"] 
  } else{
    co2_wheat[i] = dat_wheat0[i, "Elevated_CO2_Target_ppm"]
  }
}
dat_wheat0$CO2 = co2_wheat
dat_wheat = dat_wheat0[dat_wheat0$Temperature_Qualitative == "Ambient", ]
aggregate(Zn_ppm ~ CO2_Treatment, data = dat_wheat, mean)
(28.41887 - 31.40332) / 10
aggregate(Fe_ppm ~ CO2_Treatment, data = dat_wheat, mean)
(38.83090 - 40.95457) / 10
fit_wheat = lm(cbind(Zn_ppm, Fe_ppm) ~ CO2 + Nitrogen_Application_Quantitative + 
                 Phosphorus_Application_Quantitative, data = dat_wheat)
null = lm(Fe_ppm ~ 1, data = dat_wheat)
full = lm(Fe_ppm ~ CO2 + Nitrogen_Application_Quantitative + 
            Phosphorus_Application_Quantitative + Cultivar + Potassium_Application_Quantitative + Year, data = dat_wheat)
fit2 = step(null, scope = list(lower=null,upper=full), direction="both", k = log(512))
summary(fit2)

fit_sum2 = summary(fit_wheat)
Sigma = vcov(fit_wheat)[c("Zn_ppm:CO2", "Fe_ppm:CO2"), c("Zn_ppm:CO2", "Fe_ppm:CO2")]
Sigma[2,1] / sqrt(Sigma[1,1] * Sigma[2,2])
mu_wheat = c(fit_sum2$`Response Zn_ppm`$coefficients[2,1], fit_sum2$`Response Fe_ppm`$coefficients[2,1])
a_b = c(delta_CO2, delta_CO2)
Sigma_ab_wheat = matrix(c(Sigma[1,1] * a_b[1]^2, Sigma[1,2] * a_b[1] * a_b[2], 
                    Sigma[2,1] * a_b[1] * a_b[2], Sigma[2,2] * a_b[2]^2), nrow = 2, byrow = T)
ellipse(mu_wheat * a_b, Sigma_ab_wheat, newplot = T, type = "l", ylim = c(-0.4, 0),
        xlab = "Zinc change, mg/100 g of wheat", ylab = "Iron change, mg/100 g of wheat", main = "Wheat")
points((mu_wheat * a_b)[1], (mu_wheat * a_b)[2], pch = 16)

dat_wheat_test = test_dat[test_dat$name == "wheat", ]
dat_wheat_test = dat_wheat_test[dat_wheat_test$tissue == "grain", ]
dat_wheat_fe = dat_wheat_test[dat_wheat_test$element == "Fe", ]
dat_wheat_zn = dat_wheat_test[dat_wheat_test$element == "Zn", ]
points(dat_wheat_zn$delta[c(2, 7:9)], dat_wheat_fe$delta[c(2, 9:11)], col = 'red')
table(test_dat$name)


# Corn
dat_corn0 = dat_diet[dat_diet$Crop == "Corn", ]
co2_corn = c()
for (i in 1:nrow(dat_corn0)) {
  if (dat_corn0[i, "CO2_Treatment"] == "aCO2") {
    co2_corn[i] = dat_corn0[i, "Ambient_CO2_ppm"] 
  } else{
    co2_corn[i] = dat_corn0[i, "Elevated_CO2_Target_ppm"]
  }
}
dat_corn0$CO2 = co2_corn
dat_corn = dat_corn0[dat_corn0$Temperature_Qualitative == "Ambient", ]

fit_corn = lm(cbind(Zn_ppm, Fe_ppm) ~ CO2 + Nitrogen_Application_Quantitative + 
                   Cultivar, data = dat_corn)
fit_sum = summary(fit_corn)
anova(fit_corn)
qqnorm(fit_corn$residuals[,2])
qqline(fit_corn$residuals[,2])

Sigma = vcov(fit_corn)[c("Zn_ppm:CO2", "Fe_ppm:CO2"), c("Zn_ppm:CO2", "Fe_ppm:CO2")]
mu = c(fit_sum$`Response Zn_ppm`$coefficients[2,1], fit_sum$`Response Fe_ppm`$coefficients[2,1])



