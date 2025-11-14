birds = read.csv("D:/Desktop/Master/Uni courses/Statistics/Exercises/bird_allometry.csv")
length(unique(birds$Genus_Species))
head(birds)
males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]
mm = lm(log(brain_mass)~log(body_mass), data = males)
mf = lm(log(brain_mass)~log(body_mass), data = females)
hist(residuals(mm))
hist(residuals(mf))
summary(mm)
summary(mf)
plot(log(brain_mass) ~ log(body_mass), data = birds,
     col = ifelse(Sex == "f", "red", "blue"),
     pch = 16,
     las = 1,
     xlab = "Body mass (log g)",
     ylab = "Brain mass (log g)")
abline(mm, col = "blue", lwd = 2)

abline(mf, col = "red", lwd = 2)
