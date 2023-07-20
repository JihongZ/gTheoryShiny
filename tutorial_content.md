### 1. Decision making guide of gTheoryShiny

-   Step 1: Confirm whether your data needed to be transformed
-   Step 2: Identify relevant facets, outcome, and ID
-   Step 3: Check missing data pattern and the corresponding addressing method
-   Step 4: Confirm whether to use univariate g-theory or multivariate g-theory
-   Step 5: Confirm the design of data
-   Step 6: Decide on the link function between random effects and outcome
-   Step 7: Confirm whether to include confidence intervals
-   Step 8: Perform g-study estimation
-   Step 9: Perform d-study estimation
-   Step 10: Save results of person scores and random effects

### 2. Features

#### 2.1. Data input and transformation

*gTheoryShiny* currently has build-in example data sets as following:

1.  Rajaratnam.2
2.  Brennan.3.2

Rajaratnam.2 have two relevant facets in the design: *Subtest* and *Item*, with *Person* ID variable and Score as outcome variable.

*Brennan.3.2* have two relevant facets in the design: *Task* and *Rater*. The ID variable and outcome variable are *Person* and *Score* respectively.

Wide-format data can be transformed into long-format data for further analysis. The data will be screened regarding number of levels of facets, ID, and outcome.

#### 2.2. Identify design of facets

gTheoryShiny can automate screen measurement design of the data after specifying names of facets, ID, and outcome variable.

#### 2.3. Multivariate G-theory estimation (Optionally)

#### 2.4. Bootstrapping confidence interval

#### 2.5. Factor scores and random effects

*gTheoryShiny* can generate factor scores and random effects, which can be download.

### Updates:

#### Updates (2023-02-12):

This app has the function to estimate the fixed effects of covariates. To use this function, check", em("covariates"), "in Data Structure tab page. Then, the fixed effects estimates will be printed after users click", em("gtheory estimate"), "button in Data Analysis tag page.

#### Updates (2023-06-03):

Upgrade the UI framework of app to shinydashboard; Univariate Gtheory has been texted. Testing of multivariate gtheory is ongoing. Currently the estimation of MGT uses glmmTMB package
