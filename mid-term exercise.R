dat = read.csv(file = "D:/Desktop/Master/Uni courses/Statistics/exam2023_data-2.csv", check.names = FALSE)
str(dat)
summary(dat)

#Create total Eucalyptus seedlings variable
dat$euc_total = dat$`euc_sdlgs0_50cm` +
  dat$`euc_sdlgs50cm-2m` +
  dat$`euc_sdlgs>2m`

#Create presence / absence variable (binary)
dat$euc_presence = ifelse(dat$euc_total > 0, 1, 0)

#Convert relevant columns to factor
dat$Season = factor(dat$Season)
dat$Property = factor(dat$Property)   # only if used later
dat$Aspect = factor(dat$Aspect)       # only if used later
dat$`Landscape position` = factor(dat$`Landscape position`)

#Distribution of bare ground cover
hist(dat$BareGround_cover,
     main = "Histogram of bare ground cover",
     xlab = "Bare ground cover (%)")

#Seedling presence vs distance
plot(dat$`Distance_to_Eucalypt_canopy(m)`, dat$euc_presence,
     xlab = "Distance to Eucalyptus canopy (m)",
     ylab = "Seedling presence (0 = none, 1 = some)",
     main = "Seedling presence vs distance")

#Bare ground vs Eucalyptus canopy cover
plot(dat$Euc_canopy_cover, dat$BareGround_cover,
     xlab = "Eucalyptus canopy cover (%)",
     ylab = "Bare ground cover (%)",
     main = "Bare ground vs canopy cover")

#GLM model (with Season, distance, and canopy cover)
mod1_full = glm(euc_presence ~ Season +
                   `Distance_to_Eucalypt_canopy(m)` +
                   Euc_canopy_cover,
                 family = binomial,
                 data = dat)
summary(mod1_full)

#Test significance of terms (drop-test)
drop1(mod1_full, test = "Chisq")

#Non-significant terms, so modelling only distance
mod1_simple = glm(euc_presence ~ `Distance_to_Eucalypt_canopy(m)`,
                   family = binomial,
                   data = dat)
summary(mod1_simple)
#make sure both models use the SAME observations
dist = grep("Distance_to_Eucalypt", names(dat), value = TRUE)[1]
dist

vars_full = c("euc_presence", dist, "Euc_canopy_cover", "Season")
vars_full

dat_glm = dat[complete.cases(dat[, vars_full]), ]
dat_glm = dat[complete.cases(dat$euc_presence,
                              dat[[dist]],
                              dat$Euc_canopy_cover,
                              dat$Season), ]

#GLM: Seedling presence vs distance (+ canopy cover + season)

#Simple model: distance only
mod1_simple = glm(euc_presence ~ dat_glm[[dist]],
                   family = binomial,
                   data = dat_glm)

#Full model: distance + canopy cover + season
mod1_full = glm(euc_presence ~ dat_glm[[dist]] +
                   Euc_canopy_cover +
                   Season,
                 family = binomial,
                 data = dat_glm)

#Check that models use identical N (must match for anova)
nobs(mod1_simple)
nobs(mod1_full)

#Model summaries
summary(mod1_simple)
summary(mod1_full)

#Compare models (Likelihood Ratio Test) -----------------------------
anova(mod1_simple, mod1_full, test = "Chisq")

lrt = anova(mod1_simple, mod1_full, test = "Chisq")
lrt

p_lrt = lrt$`Pr(>Chi)`[2]
p_lrt

if (!is.na(p_lrt) && p_lrt < 0.05) {
  mod_final = mod1_full
} else {
  mod_final = mod1_simple
}

summary(mod_final)


#Diagnostics
#Overdispersion check (values ~1 are fine; much >1 suggests overdispersion)
deviance(mod_final) / df.residual(mod_final)

#Plot fitted relationship (probability vs distance)
#Distance sequence across observed range
dist_seq = seq(min(dat_glm[[dist]], na.rm = TRUE),
                max(dat_glm[[dist]], na.rm = TRUE),
                length = 100)

#Prediction dataset:must contain the distance column with the SAME NAME as in dat_glm
newdat = dat_glm[rep(1, length(dist_seq)), ]

#Replace distance with sequence
newdat[[dist]] = dist_seq

#If the final model includes Season, set it to a reference level
if ("Season" %in% names(model.frame(mod_final))) {
  newdat$Season = levels(dat_glm$Season)[1]
}

#If the final model includes Euc_canopy_cover, set it to its mean
if ("Euc_canopy_cover" %in% names(model.frame(mod_final))) {
  newdat$Euc_canopy_cover = mean(dat_glm$Euc_canopy_cover, na.rm = TRUE)
}

#Predict probabilities
pred_prob = predict(mod_final, newdata = newdat, type = "response")

#Plot observed (jittered) and fitted curve
x = dat_glm[[dist]][keep_plot]
y = dat_glm$euc_presence[keep_plot]

plot(x,
     jitter(y, amount = 0.05),
     xlab = "Distance to Eucalyptus canopy (m)",
     ylab = "Seedling presence (0/1; jittered)",
     main = "GLM: Seedling presence vs distance")
