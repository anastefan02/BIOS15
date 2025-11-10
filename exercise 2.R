birds = read.csv("D:/Desktop/Master/Uni courses/Statistics/Exercises/bird_allometry.csv")
head(birds)
males = birds[birds$Sex=="m",]
females = birds[birds$Sex=="f",]
mm = lm(log(brain_mass)~log(body_mass), data=males)
mf = lm(log(brain_mass)~log(body_mass), data=females)
hist(residuals(mm))
summary(mm)
summary(mf)
plot(log(brain_mass)~log(body_mass), data=males, las=1,
     ylab="Brain mass (log g)",
     xlab="Body mass (log g)")
abline(mm)
plot(log(brain_mass)~log(body_mass), data=females, las=1,
     ylab="Brain mass (log g)",
     xlab="Body mass (log g)")
abline(mf)