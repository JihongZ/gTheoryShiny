# Synthetic Dat a Set No. 4

SynDat4 <- tribble(
~"Person", ~"t1", ~"t1", ~"t1", ~"t1", ~"t2", ~"t2", ~"t2", ~"t2", ~"t3", ~"t3",  ~"t3", ~"t3",
1,     5,6,5,5,  5,3,4,5,  6,7,3,3,
2,     9,3,7,7,  7,5,5,5,  7,7,5,2,
3,     3,4,3,3,  5,3,3,5,  6,5,1,6,
4,     7,5,5,3,  3,1,4,3,  5,3,3,5,
5,     9,2,9,7,  7,7,3,7,  2,7,5,3,
6,     3,4,3,5,  3,3,6,3,  4,5,1,2,
7,     7,3,7,7,  7,5,5,7,  5,5,5,4,
8,     5,8,5,7,  7,5,5,4,  3,2,1,1,
9,     9,9,8,8,  6,6,6,5,  5,8,1,1,
10,     4,4,4,3,  3,5,6,5,  5,7,1,1
)

SynDat4 <- rbind(
  c("Person", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12"),
  SynDat4
)

write.csv(SynDat4, "data/SyntheticDataSetNo.4.csv", row.names = FALSE)


## Transform from wide to long
dat <- rbind(colnames(SynDat4), SynDat4)
colnames(dat) <- c("Person", paste0(dat[1, -1], "_", dat[2, -1]))
dat <- dat[-(1:2),] |> 
  pivot_longer(-Person, names_to = "Item", values_to = "Score") |> 
  separate(Item, into = c("Test", "Rater")) |> 
  mutate(Score = as.numeric(Score))
dat
## Run lme
lme4.res <- lmer(Score ~ (1 | Person) + (1 | Test) + (1 | Rater:Test) + (1 | Person:Test), data = dat)
gstudy.res <- gstudy(lme4.res)

#------------#
# Parameters for dstudy
# n: vector with names 
#------------#
defaultN = c("Test" = n_distinct(dat["Test"]), "Rater" = n_distinct(dat["Rater"]))
# defaultN = sapply(dat[c("Ab", "Ba")], n_distinct)
updatedN = defaultN
updatedN[c("Test", "Rater")] = c(20, 30)
updatedN["Rater"] = 30
dstudy.res <- dstudy(gstudy.res, n = updatedN, unit = "Person")
print.dStudy(dstudy.res)
