library(tidyverse)
set.seed(20230212)

dat <- data.frame(
  Person_ID = rep(1:100, each=10L),
  Item_ID = rep(1:10, 100),
  Score = rnorm(1:1000)
) %>% 
  mutate(Subtest = factor(ifelse(Item_ID %in% 1:5, 1, 2)))
## Simulate two covariate: trait1 and trait2
# dat$trait1 = rnorm(nrow(dat))
# dat$trait2 = rnorm(nrow(dat))
# head(dat)

lme4::lmer("Score ~ Subtest + (1|Person_ID) +  (1|Item_ID)", data = dat)

library(glmmTMB)
m2_mGT <- as.formula("Score ~ Subtest + (0+Subtest|Person_ID) +  us(0+Subtest|Item_ID)")
m2_mGT_fit <- glmmTMB::glmmTMB(m2_mGT, dat, family = gaussian)
m2_mGT_fit
cor(residuals(m2_mGT_fit, "response")[dat$Subtest == "1"], 
    residuals(m2_mGT_fit, "response")[dat$Subtest == "2"])


dat2 = dat
dat2$res = residuals(m2_mGT_fit, "response")
m2_mGT2 <- as.formula("Score ~ Subtest + us(0+Subtest|Person_ID) +  us(0+Subtest|Item_ID) + us(0+Subtest|res)")
m2_mGT_fit2 <- glmmTMB::glmmTMB(m2_mGT2, dat2, family = gaussian, dispformula =~0)
m2_mGT_fit2




# gstudy(m2)
VarCorr(m2)
VarCorr(m2_mGT_fit2)
ranef(m2_mGT_fit2)

fixef(m2_mGT_fit2)

ranef(m1)




