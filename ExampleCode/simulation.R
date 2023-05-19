library(tidyverse)
library(MASS)
set.seed(20230212)

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

## Simulate a full-crossed data: Person X Item X Occasion
conditions = expand_grid(Person_ID = 1:n_Person,
                         Item_ID = 1:n_Item,
                         Occasion = 1:n_Facet)
ran_Person_facet = mvrnorm(n_Person, mu = rep(0, n_Facet), Sigma = sigma_Person_Facet)
ran_Item_facet = mvrnorm(n_Item, mu = rep(0, n_Facet), Sigma = sigma_Item_Facet)
ran_residual = mvrnorm(nrow(conditions), mu = rep(0, n_Facet), Sigma = sigma_residual)

## Generate data
dat <- conditions %>% 
  mutate(case = row_number()) |> 
  rowwise() |> 
  mutate(
    Score = ran_residual[case, Occasion] + ran_Person_facet[Person_ID, Occasion] + ran_Item_facet[Item_ID, Occasion]
  ) |> 
  ungroup() |> 
  mutate(
    Person_ID = factor(Person_ID, levels = 1:n_Person),
    Item_ID = factor(Item_ID, levels = 1:n_Item),
    Occasion = factor(Occasion, levels = 1:n_Facet)) |> 
  select(-case)

glimpse(dat)
# write.csv(dat, "data/mGtheoryData.csv", row.names = FALSE)
write.csv(dat, "data/mGtheoryDataShort.csv", row.names = FALSE)
library(glmmTMB)
library(lme4)

# first run ---------------------------------------------------------------
m2_mGT <- as.formula("Score ~ us(Occasion + 0 | Person_ID) +  us( Occasion + 0 | Item_ID) ")
m2_mGT_fit <- glmmTMB::glmmTMB(m2_mGT, dat, family = gaussian, dispformula =~0)
m2_mGT_fit
lme4::VarCorr(m2_mGT_fit)

## extract residual var-cov matrix
residuals_Person <- cbind(residuals = residuals(m2_mGT_fit, "response"), 
                          dat[c("Person_ID", "Item_ID", "Occasion")]) %>% 
  pivot_wider(names_from = Occasion, values_from = residuals, names_prefix = "facet") %>% 
  ungroup()

residual_cor = cor(residuals_Person |> dplyr::select(starts_with("facet")))
residual_cor

# second run  -------------------------------------------------------------
dat2 = dat
dat2$Residual = residuals(m2_mGT_fit, "response")
glimpse(dat2)
m2_mGT2 <- as.formula("Score ~ 0 + us(Occasion + 0 | Person_ID) +  us( Occasion + 0 | Item_ID) + diag(Occasion + 0 | Residual)")
m2_mGT_fit2 <- glmmTMB::glmmTMB(m2_mGT2, dat2, family = gaussian, dispformula = ~0)
m2_mGT_fit2
res <- lme4::VarCorr(m2_mGT_fit2)
res

extract.VarCorr.glmmTMB <- function (x, row.names = NULL, optional = FALSE, 
                                     order = c("cov.last", "lower.tri"), residCor) 
{
  order <- match.arg(order)
  tmpf <- function(v, grp) {
    lt.v <- lower.tri(v, diag = FALSE)
    # vcov <- c(diag(v), v[lt.v <- lower.tri(v, diag = FALSE)])
    sdcor <- c(attr(v, "stddev"), attr(v, "correlation")[lt.v])
    nm <- rownames(v)
    n <- nrow(v)
    dd <- data.frame(grp = grp, 
                     var1 = nm[c(seq(n), col(v)[lt.v])], 
                     var2 = c(rep(NA, n), nm[row(v)[lt.v]]), 
                     sdcor, 
                     stringsAsFactors = FALSE)
    if (order == "lower.tri") {
      m <- matrix(NA, n, n)
      diag(m) <- seq(n)
      m[lower.tri(m)] <- (n + 1):(n * (n + 1)/2)
      dd <- dd[m[lower.tri(m, diag = TRUE)], ]
    }
    dd
  }
  r <- do.call(rbind, c(mapply(tmpf, x, names(x), SIMPLIFY = FALSE), 
                        deparse.level = 0))
  if (attr(x, "useSc")) {
    ss <- attr(x, "sc")
    r <- rbind(r, data.frame(grp = "Residual", var1 = NA, 
                             var2 = NA, vcov = ss^2, sdcor = ss), deparse.level = 0)
  }
  rownames(r) <- NULL
  r$sdcor[r$sdcor == 0] = residCor[lower.tri(residCor)]
  
  ## adjust table
  resTable <- r |> 
    mutate(var2 = ifelse(is.na(var2), "Std.Dev", var2)) |> 
    pivot_wider(names_from = var2, values_from = sdcor)
  resTable
}

resDat <- extract.VarCorr.glmmTMB(x = res$cond, residCor = residual_cor)
print(resDat)
