library(tidyverse)
library(MASS)
set.seed(20230212)
source("advGtheoryFunctions.R")

n_Person = 50
n_Item = 20
n_Facet = 4

## Set prior parameters
### For Item and Person
sd_person = 1.5
sigma_Person_Facet = matrix(NA, byrow = TRUE, n_Facet, n_Facet)
diag(sigma_Person_Facet) <- sd_person^2
sigma_Person_Facet[lower.tri(sigma_Person_Facet)] <- 0.5 # covariance
sigma_Person_Facet[upper.tri(sigma_Person_Facet)] <- 0.5
sigma_Item_Facet = sigma_Person_Facet
sigma_Item_Facet
cov2cor(sigma_Item_Facet)

### For residual
sd_residual = 0.5
sigma_residual = matrix(0, byrow = TRUE, n_Facet, n_Facet)
diag(sigma_residual) <- sd_residual^2

## Simulate a full-crossed data: Person X Item X Dimension
conditions = expand_grid(Person_ID = 1:n_Person,
                         Item_ID = 1:n_Item,
                         Dimension = 1:n_Facet)
ran_Person_facet = mvrnorm(n_Person, mu = rep(0, n_Facet), Sigma = sigma_Person_Facet)
ran_Item_facet = mvrnorm(n_Item, mu = rep(0, n_Facet), Sigma = sigma_Item_Facet)
ran_residual = mvrnorm(nrow(conditions), mu = rep(0, n_Facet), Sigma = sigma_residual)

## Generate data
dat <- conditions %>% 
  mutate(case = row_number()) |> 
  rowwise() |> 
  mutate(
    Score = ran_residual[case, Dimension] + ran_Person_facet[Person_ID, Dimension] + ran_Item_facet[Item_ID, Dimension]
  ) |> 
  ungroup() |> 
  mutate(
    Person_ID = factor(Person_ID, levels = 1:n_Person),
    Item_ID = factor(Item_ID, levels = 1:n_Item),
    Dimension = factor(Dimension, levels = 1:n_Facet))

glimpse(dat)
# write.csv(dat, "data/mGtheoryData.csv", row.names = FALSE)
# write.csv(dat, "data/mGtheoryDataShort.csv", row.names = FALSE)
library(glmmTMB)
library(lme4)



# first run ---------------------------------------------------------------
if (1) {
  
  m2_mGT <- as.formula("Score ~ us(Dimension + 0 | Person_ID) +  us( Dimension + 0 | Item_ID) ")
  suppressWarnings(m2_mGT_fit <- glmmTMB::glmmTMB(m2_mGT, dat, family = gaussian, dispformula =~0))
  m2_mGT_fit
  lme4::VarCorr(m2_mGT_fit)
  
  ## extract residual var-cov matrix
  residuals_Person <- cbind(residuals = residuals(m2_mGT_fit, "response"), 
                            dat[c("Person_ID", "Item_ID", "Dimension")]) %>% 
    pivot_wider(names_from = Dimension, values_from = residuals, names_prefix = "facet") %>% 
    ungroup()
  
  residual_cor = cor(residuals_Person |> dplyr::select(starts_with("facet")))
  residual_cor
  
  # second run  -------------------------------------------------------------
  dat2 = dat
  dat2$Residual = residuals(m2_mGT_fit, "response")
  glimpse(dat2)
  m2_mGT2 <- as.formula("Score ~ 0 + us(Dimension + 0 | Person_ID) +  us( Dimension + 0 | Item_ID) + diag(Dimension + 0 | Residual)")
  suppressWarnings(m2_mGT_fit2 <- glmmTMB::glmmTMB(m2_mGT2, dat2, family = gaussian, dispformula = ~0))
  m2_mGT_fit2
  res <- lme4::VarCorr(m2_mGT_fit2)
  res
  
  resDat <- extract.VarCorr.glmmTMB(x = res$cond, residCor = residual_cor)
  print(resDat)
}



# Test dstudy plot --------------------------------------------------------
res <- glmer(Score ~ (1 | Person_ID) +  ( 1 | Item_ID) + (1 | Dimension), dat = dat)

(gstudy.res <- gstudy(res))
gstudy.res |> class()

## gstudyResultBoot()
boot.gstudy <-
  lme4::bootMer(
    res, # lme4 object
    gstudy.forboot,
    nsim = 200,
    use.u = FALSE,
    type = "parametric",
    parallel = "snow",
    ncpus = 2
)
t(boot.gstudy$t)

##
# calculate bootstrap CI
gstudy.res.CI <- t(apply(boot.gstudy$t, 2, \(x) {quantile(x, probs = c(.025, .975))}))

cbind(gstudy.res_boot, gstudy.res.CI)

# Run dstudy
dstudy(x = gstudy.res, n = list(Dimension = 100, Item_ID = 100), unit = "Person_ID") |> str()

###### --- 
# Alternative way of mG-theory following Jiang (2022)
###### ---
# read.table("https://alabama.box.com/shared/static/9omab5aadtp2ofvkmcqamtv1c2vw8sqq.txt")
datDummy <- dat |> 
  fastDummies::dummy_cols(select_columns = "Dimension", remove_first_dummy = FALSE) |> 
  rename(D1 = Dimension_1, D2 = Dimension_2, D3 = Dimension_3, D4 = Dimension_4)
glimpse(datDummy)

###### --- 
# G-theory
###### ---
m1_GT <- as.formula("Score ~ D2+D3+D4+(1 | Person_ID) +  ( 1 | Item_ID) ")
suppressWarnings(m1_GT_fit <- lmer(m1_GT, datDummy))
m1_GT_fit
summary(m1_GT_fit)$coefficients$cond

###### --- 
# mG-theory
###### ---
m1_mGT <- as.formula("Score ~ D2+D3+D4+us(0+D1+D2+D3+D4| Person_ID) +  us( 0+D1+D2+D3+D4 | Item_ID) ")
suppressWarnings(m1_mGT_fit <- glmmTMB::glmmTMB(m1_mGT, datDummy, family = gaussian, dispformula =~0))
m1_mGT_fit
gstudy(m1_mGT_fit)

# fixed estimates
extractFixedCoefsmG(m1_mGT_fit)

## G-study bootstrap
gstudy.forboot(m1_mGT_fit)
boot.gstudy <-
  lme4::bootMer(
    m1_mGT_fit,
    gstudy.forboot,
    nsim = 100,
    use.u = FALSE,
    type = "parametric",
    parallel = "snow",
    ncpus = 3
  )



## extract residual var-cov matrix
residuals_Person <- cbind(residuals = residuals(m1_mGT_fit, "response"), 
                          dat[c("Person_ID", "Item_ID", "Dimension")]) %>% 
  ungroup() |> 
  pivot_wider(names_from = Dimension, values_from = residuals, names_prefix = "D")

residual_cov = cov(residuals_Person |> dplyr::select(starts_with("D")))
residual_cov

gCoef_mGTheory(nDimension = 4, glmmTMBObj = m1_mGT_fit, residual_cov = residual_cov,
               dat = dat[c("Person_ID", "Item_ID", "Dimension")])
extract.VarCorr.glmmTMB(x = m1_mGT_fit)

res <- lme4::VarCorr(m1_mGT_fit)
(resVarCor <- extract.VarCorr.glmmTMB(x = res$cond, residCor = residual_cor))
resVarCor |> 
  mutate(
    across(any_of(colnames(resVarCor)[-(1:2)]), \(x) round(as.numeric(x),3))
  )
