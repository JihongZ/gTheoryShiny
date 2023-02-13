dat <- read.csv("Rajaratnam.2.new.withCovariate.csv")
head(dat)

## Simulate two covariate: trait1 and trait2
set.seed(20230212)
# dat$trait1 = rnorm(nrow(dat))
# dat$trait2 = rnorm(nrow(dat))
# head(dat)
library(lme4)
m1_form <- as.formula("Score ~ trait1 + (1 | Person) + (1 | Subtest) + (1 | Item) + (1 | Rater) + (1 | Person:Subtest) + (1 | Ocasion:Subtest) + (1 | Ocasion)")
m1 <- lmer(m1_form, dat)
summary(m1)
VarCorr(m1)

library(glmmTMB)
m2_form <- as.formula("Score ~  1 + trait1 + (1|Subtest) + (1|Item) + (1|Rater) + (1|Ocasion/Subtest) + (1|Person/Subtest)")
m2_mGT <- as.formula("Score ~  1 + trait1 + (1|Subtest) + (1|Item) + (1|Rater) + us(Ocasion|Subtest) + (1|Person/Subtest)")
m2 <- glmmTMB::glmmTMB(m2_form, dat, family = gaussian)
m2_mGT <- glmmTMB::glmmTMB(m2_mGT, dat, family = gaussian)
summary(m2)
VarCorr(m2)
VarCorr(m2_mGT)


fixef(m1)
ranef(m1)



m1 <- relmatLmer(Score ~ AGE + SEX + (1|Rater) + (1|Score), dat, 
                 relmat = list(ID = kin2))
