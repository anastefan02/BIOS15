library(MASS)
dat = read.csv(file = "D:/Desktop/Master/Uni courses/Statistics/Exercises/Eulaema.csv", encoding = "latin1")
str(dat)
head(dat)

#Eulaema_nigrita: count of bees
#forest.: proportion forest cover (0–1)
#MAP: mean annual precipitation
#MAT, Tseason, Pseason, effort, method, etc. also available

par(mfrow = c(1, 3))
plot(dat$forest., dat$Eulaema_nigrita,
     xlab = "Forest cover",
     ylab = "Bee abundance",
     las  = 1,
     pch  = 16,
     main = "Bees vs forest cover")

plot(dat$MAP, dat$Eulaema_nigrita,
     xlab = "Mean annual precipitation (MAP)",
     ylab = "Bee abundance",
     las  = 1,
     pch  = 16,
     main = "Bees vs MAP")

plot(dat$effort, dat$Eulaema_nigrita,
     xlab = "Sampling effort (log hours)",
     ylab = "Bee abundance",
     las  = 1,
     pch  = 16,
     main = "Bees vs effort")

par(mfrow = c(1, 1))

#Poisson GLM and checking for overdispersion

m_pois = glm(Eulaema_nigrita ~ MAP + forest.,
              family = poisson,
              data   = dat)
summary(m_pois)
dispersion_pois = m_pois$deviance / m_pois$df.residual
dispersion_pois

#Strong overdispersion, so Poisson is not appropriate, moving to negative binomial
#Mean-centering MAP to make the intercept interpretable
dat$mcMAP = dat$MAP - mean(dat$MAP, na.rm = TRUE)
m_nb = glm.nb(Eulaema_nigrita ~ mcMAP + forest., data = dat)
summary(m_nb)

#Pseudo-R^2
pseudo_r2 = 1 - m_nb$deviance / m_nb$null.deviance
pseudo_r2
coefs = summary(m_nb)$coef
coefs

#Intercept: expected bee count at forest=0, MAP = mean
pred_unforested = exp(coefs[1,1])

#Predicted count at forest=1, MAP = mean
pred_forest = exp(coefs[1,1] + coefs[3,1])
pred_unforested
pred_forest

#Plotting bee abundance vs forest cover with fitted curves
par(mfrow = c(1, 1))
plot(dat$forest., dat$Eulaema_nigrita,
     xlab = "Forest cover",
     ylab = expression(paste(italic("Eulaema nigrita"), " abundance")),
     las  = 1,
     pch  = 16,
     main = "Bee abundance along forest-cover gradient")
newforest = seq(min(dat$forest., na.rm = TRUE),
                 max(dat$forest., na.rm = TRUE),
                 length.out = 200)
mcMAP_mean = 0
mcMAP_sd = sd(dat$mcMAP, na.rm = TRUE)

newMAP_mean = rep(mcMAP_mean, length(newforest))
newMAP_high = rep(mcMAP_mean + mcMAP_sd, length(newforest))
newMAP_low  = rep(mcMAP_mean - mcMAP_sd, length(newforest))


#Predicted abundances for the three rainfall levels
y_hat_mean = predict(m_nb,
                      newdata = list(mcMAP = newMAP_mean,
                                     forest. = newforest),
                      type = "response")

y_hat_high = predict(m_nb,
                      newdata = list(mcMAP = newMAP_high,
                                     forest. = newforest),
                      type = "response")

y_hat_low = predict(m_nb,
                      newdata = list(mcMAP = newMAP_low,
                                     forest. = newforest),
                      type = "response")
lines(newforest, y_hat_mean, lwd = 2)
lines(newforest, y_hat_high, lwd = 2, col = "blue3")
lines(newforest, y_hat_low,  lwd = 2, col = "firebrick")
legend("topleft",
       legend = c("MAP = mean",
                  "MAP = mean + SD (wetter)",
                  "MAP = mean - SD (drier)"),
       lty    = 1,
       lwd    = 2,
       col    = c("black", "blue3", "firebrick"),
       bty    = "n")
abline(h = pred_unforested, lty = 2)
abline(h = pred_forest,     lty = 2)
