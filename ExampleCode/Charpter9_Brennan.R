library(tidyverse)
table9.3 <- tribble(
~Person,~v1_i1,~v1_i2,~v1_i3,~v1_i4,~v1_i5,~v1_i6,~v2_i1,~v2_i2,~v2_i3,~v2_i4,~v2_i5,~v2_i6,
1, 6,4,3,5,4,4,6,4,5,6,4,5,
2, 3,2,2,4,5,5,6,4,5,6,2,5,
3, 6,5,7,5,4,3,6,5,8,4,6,3,
4, 4,2,2,3,3,5,5,4,2,4,3,5,
5, 4,4,3,5,4,6,4,5,5,3,3,7,
6, 8,5,4,7,5,4,9,6,6,5,7,7,
7, 5,4,5,7,4,4,4,5,6,7,4,5,
8, 4,5,3,4,5,6,6,7,6,6,5,6,
9, 7,5,4,6,6,5,6,7,3,5,5,6,
10,5,3,3,7,4,5,6,5,4,6,2,6
)

table9.3

(Sum = colSums(table9.3[,-1]))


table9.3Output <- rbind(
  c("Person", rep("v1", 6), rep("v2", 6)),
  c("Person", paste0("i", 1:6), paste0("i", 1:6)),
  table9.3
)

write_csv(table9.3Output, file = "data/Brennan.9.3.csv")

## glmm Method -----
library(glmmTMB)
library("effects")
library("lmerTest")
source("advGtheoryFunctions.R")
dat <- table9.3 |> 
  pivot_longer(-Person, names_to = "Item", values_to = "Score") |> 
  separate(Item, into = c("Occasion", "Item"))

model1 <- as.formula("Score~0+Occasion+us(Occasion+0|Item)+us(Occasion+0|Person)")
model1_fit <- glmmTMB(formula = model1, data = dat, dispformula = ~0, control = glmmTMBControl(parallel = 3))
model1_fit
confint(model1_fit)

dat <- cbind(dat, Residual= residuals(model1_fit))
model2 <- as.formula("Score~0+Occasion+us(Occasion+0|Item)+us(Occasion+0|Person)+us(Occasion+0|Residual)")
model2_fit <- glmmTMB(formula = model2, data = dat, dispformula = ~0, control = glmmTMBControl(parallel = 3))
model2_fit
confint(model2_fit)

## get adjust residual var-cov
residual_cor <- dat |> 
  mutate(Residual = residuals(model1_fit)) |> 
  dplyr::select(-Score) |> 
  pivot_wider(names_from = Occasion, values_from = Residual) |> 
  dplyr::select(starts_with("v")) |> 
  cor()

residual_cov


###### --- 
# dstudy of mgtheory
###### ---
n = data.frame(
  "Item" = 10
)

extract.VarCorr.glmmTMB(x = lme4::VarCorr(model2_fit)$cond, residCor = residual_cor, facetName = "Occasion")

dtheoryVarCov <- gtheoryVarCov / n[["Item"]]

residuals_Person <- cbind(residuals = residuals(model1_fit, "response"),
                          datG[c(selectedID(), selectedFacet())]) %>%
  pivot_wider(names_from = selectedFixedFacet(), 
              values_from = residuals, 
              names_prefix = "facet") %>%
  ungroup()
residual_cor = cor(residuals_Person |> dplyr::select(starts_with("facet")),
                   use = "pairwise.complete.obs")
residual_cov = cov(residuals_Person |> dplyr::select(starts_with("facet")),
                   use = "pairwise.complete.obs")

###### --- 
# run second time
###### ---
dat2 = datG
dat2$Residual = residuals(lmmFit0, "response")



