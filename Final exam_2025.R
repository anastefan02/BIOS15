dat = read.table(file = "D:/Desktop/Master/Uni courses/Statistics/Final exam/penstemon-1.txt", header = TRUE)
str(dat)
summary(dat)
names(dat)
dat$Pop = factor(dat$Pop)
dat$Block = factor(dat$Block)
levels(dat$Pop)
levels(dat$Block)

#missing data check
colSums(is.na(dat))

#Do populations differ in fitness?

hist(dat$fitness, main = "Fitness distribution", xlab = "Fitness")

boxplot(fitness ~ Pop, data = dat,
        xlab = "Population", ylab = "Fitness",
        main = "Fitness by population")

boxplot(fitness ~ Block, data = dat,
        xlab = "Block", ylab = "Fitness",
        main = "Fitness by block")
dat1 = dat[complete.cases(dat$fitness, dat$Pop, dat$Block), ]

m1_full = lm(fitness ~ Pop + Block, data = dat1)
summary(m1_full)
par(mfrow = c(2,2))
plot(m1_full)
par(mfrow = c(1,1))
drop1(m1_full, test = "F")

#since block is clearly non-significant, I simplified
m1_pop = lm(fitness ~ Pop, data = dat1)
anova(m1_pop, m1_full)
m1_final = lm(fitness ~ 1, data = dat1)
summary(m1_final)

#Which traits predict fitness? flowers + flwsize + height (+ Block as control)
par(mfrow = c(1,3))
plot(dat$flowers, dat$fitness, xlab = "Number of flowers", ylab = "Fitness",
     main = "Fitness vs flowers")
plot(dat$flwsize, dat$fitness, xlab = "Flower size", ylab = "Fitness",
     main = "Fitness vs flower size")
plot(dat$height, dat$fitness, xlab = "Height", ylab = "Fitness",
     main = "Fitness vs height")
par(mfrow = c(1,1))
dat2 = dat[complete.cases(dat$fitness, dat$flowers, dat$flwsize, dat$height, dat$Block), ]
m2_full = lm(fitness ~ flowers + flwsize + height + Block, data = dat2)
summary(m2_full)
par(mfrow = c(2,2))
plot(m2_full)
par(mfrow = c(1,1))
drop1(m2_full, test = "F")

#Reduced model removing flwsize and height, keeping Block
m2_red1 = lm(fitness ~ flowers + Block, data = dat2)
summary(m2_red1)
drop1(m2_red1, test = "F")

#Final model: flowers only
m2_final = lm(fitness ~ flowers, data = dat2)
summary(m2_final)

#Diagnostics for final model
par(mfrow = c(2, 2))
plot(m2_final)
par(mfrow = c(1, 1))

#Plot fitted relationship
plot(dat2$flowers, dat2$fitness,
     xlab = "Number of flowers",
     ylab = "Fitness",
     main = "Final model: fitness vs flowers")
abline(m2_final, lwd = 2)
