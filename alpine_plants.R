plants = read.csv(file = "D:/Desktop/Master/Uni courses/Statistics/Exercises/alpineplants.csv")
head(plants)
#defining the variables
x1 = plants$mean_T_winter
x2 = plants$mean_T_summer
l = plants$light
s = plants$snow
m = plants$soil_moist
a = plants$altitude
y1 = plants$Carex.bigelowii
y2 = plants$Thalictrum.alpinum
#trying to see if the winter and summer temperatures have an influence of the species
tm1 = lm(y1~x1+x2)
coefs = summary(tm1)$coef
summary(tm1)
tm2 = lm(y2~x1+x2)
coefs = summary(tm2)$coef
summary(tm2)
#since the R2 is quite small, it means that it doesn't really impact the distributions
#I will now try to see if the snow and the soil moisture affect the distributions of the species
sm1 = lm(y1~s+m)
coef = summary(sm1)$coef
summary(sm1)
sm2 = lm(y2~s+m)
coef = summary(sm2)$coef
summary(sm2)
#the answer is definitely no (in a statistically significant way) so I will now test light and winter and summer temperatures
lm1 = lm(y1~l+x1+x2)
coef = summary(lm1)$coef
summary(lm1)
lm2 = lm(y2~l+x1+x2)
coef = summary(lm2)$coef
summary(lm2)
#for the second species it looks like it is a bit significant
#let's test with moisture, light and altitude
mm1 = lm(y1~m+l+a)
coef = summary(mm1)$coef
summary(mm1)
mm2 = lm(y2~m+l+a)
coef = summary(mm2)$coef
summary(mm2)
#not really
#going for temperatures and altitudes
ta1 = lm(y1~x1+x2+a)
coef = summary(ta1)$coef
summary(ta1)
ta2 = lm(y2~x1+x2+a)
coef = summary(ta2)$coef
summary(ta2)
#looks like it does have a stronger effect of the second species
#now running a model with everything for each species
pairs(plants[,c(1,2,3,6,9,10,11,12)], panel=panel.smooth)
all1 = lm(y1~x1+x2+l+s+m+a)
coef = summary(all1)$coef
summary(all1)
all2 = lm(y2~x1+x2+l+s+m+a)
coef = summary(all2)$coef
summary(all2)
