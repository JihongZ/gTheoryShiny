library(tidyverse)
library(lme4)

## Functions
SumSquare <- function(x) {
  sum((x)^2)
}

# TABLE 2.2. Synthetic Data Set No. 1 and the p x i Design
dat <- tribble(
  ~Person, ~I1, ~I2, ~I3, ~I4, ~I5, ~I6, ~I7, ~I8, ~I9, ~I10, ~I11, ~I12,
  1,1,0,1,0,0,0,0,0,0,0,0,0,
  2,1,1,1,0,0,1,0,0,0,0,0,0,
  3,1,1,1,1,1,0,0,0,0,0,0,0,
  4,1,1,0,1,1,0,0,1,0,0,0,0,
  5,1,1,1,1,1,0,1,0,0,0,0,0,
  6,1,1,1,0,1,1,1,0,0,0,0,0,
  7,1,1,1,1,1,1,1,0,0,0,0,0,
  8,1,1,1,1,0,1,1,1,1,1,0,0,
  9,1,1,1,1,1,1,1,1,1,1,1,0,
  10,1,1,1,1,1,1,1,1,1,1,1,1
)
write.csv(dat, file = "data/SyntheticDataSetNo1.csv", row.names = FALSE)
n_i = 12
n_p = 10
## Grant mean Bar_X
BarX <- mean(unlist(dat[, 2:13]))
## mean scores for person BarXp
BarXp <- rowMeans(dat[-1])
## mean scores for item BarXi
BarXi <- colMeans(dat[-1])

## Sum of square for persons: SS_p
(SS_p <- n_i * SumSquare(BarXp - BarX))
## Mean square for persons: MS_p
(MS_p <- SS_p / (n_p - 1))
## Sum of square for items: SS_i
(SS_i <- n_p * SumSquare(BarXi - BarX))
## Mean square for item: MS_i
(MS_i <- SS_i / (n_i - 1))
## Sum of square for item X person interaction
X = as.matrix(dat[-1])
(SS_pi <- SumSquare(unlist(sweep(X - BarXp, 2, BarXi) + BarX)))
(MS_pi <- SS_pi / ((n_i-1)*(n_p-1)))

# Variance components
## variance component for persons
(SigmaSqr_p = (MS_p - MS_pi) / n_i)
## variance component for item
(SigmaSqr_i = (MS_i - MS_pi) / n_p)
## variance component for item X person
(SigmaSqr_pi = MS_pi) # 0.1268519
(SigmaSqr_total = var(as.vector(X)))

## Comparison: linear mixed model (same as Brennan's formula) ----------------------------------------
##     grp       vcov
## 1 Item 0.07542093
## 2 Person 0.05740748
## 3 Residual 0.12685183 ---> Brennan: Person X Item interaction
dat_long <- dat |> 
  pivot_longer(starts_with("I"), names_to = "Item", values_to = "Score")
VarCorMatrix <- VarCorr(lmer(Score ~ (1|Person) + (1|Item), dat_long))
VarCorMatrix |> as.data.frame() |> select(grp, vcov)

# Error Variance ----------------------------------------------------------
## d-study sample size
n_i_prime = n_i
(SigmaSqr_I = SigmaSqr_i / n_i_prime)
(SigmaSqr_pI = SigmaSqr_pi / n_i_prime)

## Absolute error variance
(SigmaSqr_Delta = SigmaSqr_I + SigmaSqr_pI)
## Relative error variance
(SigmaSqr_delta = SigmaSqr_pI)

# Reliability Coefficients ------------------------------------------------
### generalizability coefficient: Eq. 2.39 
(rho <- SigmaSqr_p / (SigmaSqr_p + SigmaSqr_delta))
### d-study coefficient: Eq. 2.41 
(Phi <- SigmaSqr_p / (SigmaSqr_p + SigmaSqr_Delta))

# Visualization -----------------------------------------------------------
###### --- 
# Test gtheory function
###### ---
dat_long <- dat |> 
  pivot_longer(starts_with("I"), names_to = "Item", values_to = "Score")
lmeres = lmer(Score ~ (1|Person) + (1|Item), dat_long)
gstudy(lmeres)

## ----------------------------- ##
##              End.             ##
## ----------------------------- ##

gtheoryCoef <- function(dat, n) {
  ## transform wide to long
  dat_long <- dat |> 
    pivot_longer(starts_with("I"), names_to = "Item", values_to = "Score")
  VarCorMatrix <- VarCorr(lmer(Score ~ (1|Person) + (1|Item), dat_long))
  ## Extract component
  VarCorTable <-  VarCorMatrix |> as.data.frame() 
  SigmaSqr_i <- VarCorTable |> filter(grp == "Item") |> pull(vcov)
  SigmaSqr_p <- VarCorTable |> filter(grp == "Person") |> pull(vcov)
  SigmaSqr_pi <- VarCorTable |> filter(grp == "Residual") |> pull(vcov)
  
  SigmaSqr_I = SigmaSqr_i / n
  SigmaSqr_pI = SigmaSqr_pi / n
  
  ## Absolute error variance
  SigmaSqr_Delta = SigmaSqr_I + SigmaSqr_pI
  ## Relative error variance
  SigmaSqr_delta = SigmaSqr_pI
  
  data.frame(
    N = n,
    SigmaSqr_I = SigmaSqr_I,
    SigmaSqr_p = SigmaSqr_p,
    SigmaSqr_pI = SigmaSqr_pI,
    SigmaSqr_Delta = SigmaSqr_Delta, 
    SigmaSqr_delta = SigmaSqr_delta, 
    rho = SigmaSqr_p / (SigmaSqr_p + SigmaSqr_delta),
    Phi = SigmaSqr_p / (SigmaSqr_p + SigmaSqr_Delta)
  )
}

DstudyCoefPlot <- Reduce(rbind, map(1:25, \(n) gtheoryCoef(dat = dat, n = n)) )

## Table 2.4. D study sample size X SEM
ggplot(DstudyCoefPlot) +
  geom_line(aes(x = N, y = sqrt(SigmaSqr_Delta)), alpha = 0.4) +
  geom_text(aes(x = N, y = sqrt(SigmaSqr_Delta)), label = "Delta", parse = TRUE, size = 5) +
  geom_line(aes(x = N, y = sqrt(SigmaSqr_delta)), alpha = 0.4) +
  geom_text(aes(x = N, y = sqrt(SigmaSqr_delta)), label = "delta", parse = TRUE, size = 5) +
  labs(x = "Sample Size", y = "SEM") +
  scale_y_continuous(breaks = seq(0, 1, 0.1)) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) 

## Table 2.4. D study sample size X Coefficient
ticks = seq(0, 1, 0.02)
labels_point = seq(0, 1, 0.1)
ylabels = rep("", length(ticks))
for (i in seq(ticks)) {
  if (as.character(ticks[i]) %in% as.character(labels_point)) {
    ylabels[i] = ticks[i]
  }
}

ggplot(DstudyCoefPlot) +
  geom_line(aes(x = N, y = rho), alpha = 0.4) +
  geom_text(aes(x = N, y = rho), label = "rho", parse = TRUE, size = 5) +
  geom_line(aes(x = N, y = Phi), alpha = 0.4) +
  geom_text(aes(x = N, y = Phi), label = "Phi", parse = TRUE, size = 5) +
  labs(x = "Sample Size", y = "Coefficient") +
  scale_y_continuous(breaks = ticks,
                     limits = c(0, 1),
                     expand = c(0, 0),
                     labels = ylabels) +
  theme_classic() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) 


# 2.4 Nested Design ---------------------------------------------------------
dat2 <- tribble(
  ~Person, ~I1, ~I2, ~I3, ~I4, ~I5, ~I6, ~I7, ~I8,
  1, 2, 6, 7, 5, 2, 5, 5, 5, 
  2, 4, 5, 6, 7, 6, 7, 5, 7, 
  3, 5, 5, 4, 6, 5, 4, 5, 5, 
  4, 5, 9, 8, 6, 5, 7, 7, 6, 
  5, 4, 3, 5, 6, 4, 5, 6, 4, 
  6, 4, 4, 4, 7, 6, 4, 7, 8, 
  7, 2, 6, 6, 5, 2, 7, 7, 5, 
  8, 3, 4, 4, 5, 6, 6, 6, 4, 
  9, 0, 5, 4, 5, 5, 5, 5, 3,
  10, 6, 8, 7, 6, 6, 8, 8, 6
)
write.csv(dat2, file = "Brennan.2.4.nested.csv", row.names = FALSE)
(BarX <- mean(unlist(dat2[-1])))
(BarXi <- colMeans(dat2[-1]))
(BarXp <- rowMeans(dat2[-1]))

## Table 2.9 Variance components
n_p = 10; n_i = 8
(SS_p = n_i*SumSquare(BarXp)-n_p*n_i*BarX^2)
(SS_i.p = SumSquare(dat2[-1]) - n_i*SumSquare(BarXp))
MS_p = SS_p / (n_p - 1)
MS_i.p = SS_i.p / ((n_i - 1)*n_p)
SigmaSqr_p = (MS_p - MS_i.p) / n_i
SigmaSqr_i.p = MS_i.p

### Error variances
## dstudy sample size
n_i_prime = c(4, 8)
(SigmaSqr_I.p = SigmaSqr_i.p / n_i_prime)

## Absolute/Relative error variance
(SigmaSqr_Delta = SigmaSqr_delta = SigmaSqr_I.p)
(SigmaSqr_BarX = SigmaSqr_p / 10 + SigmaSqr_i.p / (10 * n_i_prime))

## Coefficients
(SigmaSqr_p / (SigmaSqr_p + SigmaSqr_Delta))

#------------#
# Comparison: lme4
#------------#
dat2_long <- dat2 |> 
  pivot_longer(starts_with("I"), 
               names_to = "Item",
               values_to = "Score") |> 
  mutate(
    Person = factor(Person, levels = 1:10))

lmeFit <- lmer(
  Score ~ (1 | Person),
  data = dat2_long, REML = TRUE
)
VarCorr(lmeFit) |> as.data.frame()
