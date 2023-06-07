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


## Using Simulated RCG example -----

###### --- 
# Alternative way of mG-theory following Jiang (2022)
# Refer to Brennan (2001a, Chap. 9)
###### ---
# read.table("https://alabama.box.com/shared/static/9omab5aadtp2ofvkmcqamtv1c2vw8sqq.txt")
rm(list = ls())

library(dplyr)
library(gtheory)

#------------#
# Data simulation
#------------#

p.effect.cov <- matrix(c(
  1.5714, 1.4821, 0.5,
  1.4821, 2.7857, 0.9464,
  0.5,    0.9464, 1.8571
), 3 , 3, byrow = TRUE)

i.effect.cov <- matrix(c(
  0.4286,    0,         0,
  0,         0.1994,    0,
  0,         0,         0.4464
), 3,3, byrow = TRUE)

pi.effect.cov <- matrix(c(
  0.5714,    0,         0,
  0,         1.0714,    0,
  0,         0,         0.4286
), 3,3, byrow = TRUE)

N.i = 50
N.p = 100
N.h = 3
grand.mean = c(40, 50, 60)


# For Person Effect; 100 X 300
# 100 X 3
set.seed(221203)
temp.p <- rmvnorm(N.p, grand.mean, p.effect.cov)
TEMP.P <- NULL
for(v in 1:N.h) {
  for (i in 1:N.i) {
    TEMP.P <- cbind(TEMP.P, temp.p[, v])
  }
}	

#For Item Effect; 100:200
temp.i <- rmvnorm(N.i, rep(0, N.h), i.effect.cov)
TEMP.I <- NULL
for (p in 1:N.p) {
  TEMP.I.OneRow = NULL
  for (v in 1:N.h) {
    TEMP.I.OneRow = c(TEMP.I.OneRow, t(temp.i)[v, ])
  }
  TEMP.I <- rbind(TEMP.I,  TEMP.I.OneRow)
}

#For Error Effect
temp.e <- rmvnorm(N.p * N.i, rep(0, N.h), pi.effect.cov)
TEMP.E <- matrix(temp.e, N.p, N.i * N.h)
Dat1 <- TEMP.P + TEMP.I + TEMP.E
rownames(Dat1) <- NULL

Dat1 <- Dat1 |>
  as.data.frame() |> 
  rownames_to_column("Person")

Dat_out <- rbind(
    c("Person", rep(paste0("I", 1:N.i), N.h)),
    c("Person", rep(paste0("V", 1:N.h), each = N.i)),
    Dat1
)

write.csv(Dat_out, file = "ExampleCode/Zhehan/RCGI100P100.csv", row.names = FALSE)
#------------#
# End
#------------#
head(dat)
tail(dat, 10)

cor2cov <- function(R, S) { # 
  sweep(sweep(R, 1, S, "*"), 2, S, "*")
}

cov_component <- function(obj, source, mat_type = "us") {
  res = VarCorr(obj)
  if (mat_type == "diag") {
    cor2cov(S = attr(res$cond[[source]], "stddev"), R = diag(1, length(attr(res$cond[[source]], "stddev"))))
  }else if(mat_type == "us"){
    sd_facet  <- attr(res$cond[[source]], "stddev")
    cor_facet <- attr(res$cond[[source]], "correlation")
    cov_facet <- cor2cov(R = cor_facet, S = sd_facet)
    cov_facet
  }
}

###### --- 
# lmer method
###### ---
m1_GT <- as.formula("Score~0+Subtest + (0+Subtest|Person) + (0+Subtest|Item)")
suppressWarnings(m1_GT_fit <- lmer(m1_GT, dat, REML = FALSE))
m1_GT_fit

datlmer = dat
datlmer$pi = residuals(m1_GT_fit)
m2_GT <- as.formula("Score~0+Subtest+(Subtest|Person)+(Subtest|Item)+(Subtest|pi)")
suppressWarnings(m2_GT_fit <- lmer(m2_GT, datlmer))
m2_GT_fit

###### --- 
# mG-theory
###### ---
m1_mGT <- as.formula("Score~ Subtest + us(0+Subtest|Person) + diag(0+Subtest|Item)")
suppressWarnings(m1_mGT_fit <- glmmTMB::glmmTMB(m1_mGT, dat, family = gaussian, dispformula =~0))
m1_mGT_fit

cov_component(obj = m1_mGT_fit, source = "Person", mat_type = "us")
cov_component(obj = m1_mGT_fit, source = "Item", mat_type = "us")

### second run  -------------------------------------------------------------
dat2 = dat
dat2$Residual = residuals(m1_mGT_fit)
glimpse(dat2)
m2_mGT <- as.formula("Score~0+Subtest+us(Subtest+0|Person)+diag(Subtest+0|Item)+diag(Subtest+0|Residual)")
suppressWarnings(m2_mGT_fit <- glmmTMB::glmmTMB(m2_mGT, dat2, family = gaussian, dispformula =~0, REML = FALSE))
m2_mGT_fit

res <- lme4::VarCorr(m2_mGT_fit)
res

## residual
var_residual <-
  cov_component(obj = m2_mGT_fit,
                source = "Residual",
                mat_type = "diag")
round(var_residual, 2)

## item
var_item <-
  cov_component(obj = m2_mGT_fit,
                source = "Item",
                mat_type = "diag")
round(var_item, 2)

## person
cov_person <-
  cov_component(obj = m2_mGT_fit,
                source = "Person",
                mat_type = "us")
round(cov_person, 2)

list(
  Person = round(cov_person, 3),
  Item = round(var_item, 3),
  Residual = round(var_residual, 3)
)
  

## not work: G-study bootstrap
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
