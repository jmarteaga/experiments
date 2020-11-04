Problem Set 3
================
Experiments and Causality

``` r
# load packages 
library(data.table)
library(foreign)
library(sandwich)
library(lmtest)
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library(stargazer)
```

    ## 
    ## Please cite as:

    ##  Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.

    ##  R package version 5.2.2. https://CRAN.R-project.org/package=stargazer

``` r
library(clubSandwich)
```

    ## Registered S3 method overwritten by 'clubSandwich':
    ##   method    from    
    ##   bread.mlm sandwich

# 0\. Write Functions

You’re going to be doing a few things a *number* of times – calculating
robust standard errors, calculating clustered standard errors, and then
calculating the confidence intervals that are built off these standard
errors.

*After* you’ve worked through a few of these questions, I suspect you
will see places to write a function that will do this work for you.
Include those functions here, if you write them.

``` r
lin_reg <- function(formula_string, data){
  #Execute the linear regression
  m <- lm(as.formula(formula_string), data = data)
  
  return(m)
  
}

robust_se <- function(model){
  model.vcovhc <- vcovHC(model)
  model.se <- sqrt(diag(model.vcovhc))
  
  stargazer(
    model, 
    type = 'text',
    se=list(model.se), 
    header=F
  )
  
  return(model.se)
}

robust_se_omit <- function(model,omitted){
  model.vcovhc <- vcovHC(model)
  model.se <- sqrt(diag(model.vcovhc))
  
  stargazer(
    model, 
    type = 'text',
    omit = omitted,
    se=list(model.se), 
    header=F
  )
  
  return(model.se)
}

classic_confint <- function(model, level){
  #perform a simple confidence interval
  cint <- confint(model,level = level)[2,]
  return(cint)
}

cluster_1_se <- function(model, cluster){
  #calculate the one-way cluster standard error
  model.vcovCL <- vcovCL(model,cluster = cluster)
  model.se <- sqrt(diag(model.vcovCL))
  
  stargazer(
    model, 
    type = 'text',
    se=list(model.se), 
    header=F
  )
  
  return(model.se)
}
```

# 1\. Replicate Results

Skim [Broockman and Green’s](./readings/brookman_green_ps3.pdf) paper on
the effects of Facebook ads and download an anonymized version of the
data for Facebook users only.

``` r
d <- fread("./data/broockman_green_anon_pooled_fb_users_only.csv")

dt <- data.table(d)
head(dt)
```

    ##    studyno treat_ad                   cluster name_recall positive_impression
    ## 1:       2        0 Study 2, Cluster Number 1           0                   0
    ## 2:       2        0 Study 2, Cluster Number 2           1                   0
    ## 3:       2        0 Study 2, Cluster Number 3           0                   0
    ## 4:       2        0 Study 2, Cluster Number 4           1                   0
    ## 5:       2        1 Study 2, Cluster Number 7           1                   1
    ## 6:       2        1 Study 2, Cluster Number 7           0                   0

1.  Using regression without clustered standard errors (that is,
    ignoring the clustered assignment), compute a confidence interval
    for the effect of the ad on candidate name recognition in Study 1
    only (the dependent variable is `name_recall`).

<!-- end list -->

  - **Note**: Ignore the blocking the article mentions throughout this
    problem.
  - **Note**: You will estimate something different than is reported in
    the study.

<!-- end list -->

``` r
#Run a regression without clustered standard errors
formula_str <- "name_recall ~ treat_ad"
m1 <- lin_reg(formula_str, dt[studyno == 1])

#Calculate robust standard error
m1_se <- robust_se(m1)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_ad                      -0.010           
    ##                               (0.021)          
    ##                                                
    ## Constant                     0.182***          
    ##                               (0.016)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,364           
    ## R2                            0.0002           
    ## Adjusted R2                   -0.001           
    ## Residual Std. Error      0.382 (df = 1362)     
    ## F Statistic            0.217 (df = 1; 1362)    
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Compute the confidence interval for the effect of the ad on the candidate name recognition
p1_confint <- classic_confint(m1,0.95)
```

> **Answer:** The 95% confidence interval is \[ -0.0510177 ,
> 0.0314219\].

2.  What are the clusters in Broockman and Green’s study? Why might
    taking clustering into account increase the standard errors?

> **Answer:** Broockman and Green created 1220 clusters in the following
> manner. They used the public list of voters and removed individuals
> under 30 and over 75 years of age, at the campaign’s request. This
> left 32,029 voters that were then clustered by 18 age ranges, 34 towns
> and 2 genders. Clustering generally increases standard errors because
> allowances must be made for correlations between clusters.

3.  Estimate a regression that estimates the effect of the ad on
    candidate name recognition in Study 1, but this time take take
    clustering into account when you compute the standard errors.

<!-- end list -->

  - The estimation of the *model* does not change, only the estimation
    of the standard errors.
  - You can estimate these clustered standard errors using
    `sandwich::vcovCL`, which means: "The `vcovCL` function from the
    `sandwich` package.
  - We talk about this more in code that is availbe in the course repo.

<!-- end list -->

``` r
#Reuse the model from above lin_mod and apply vcovCL.
summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = as.formula(formula_string), data = data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.1825 -0.1825 -0.1727 -0.1727  0.8273 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.182469   0.016142  11.304   <2e-16 ***
    ## treat_ad    -0.009798   0.021012  -0.466    0.641    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3817 on 1362 degrees of freedom
    ## Multiple R-squared:  0.0001596,  Adjusted R-squared:  -0.0005745 
    ## F-statistic: 0.2174 on 1 and 1362 DF,  p-value: 0.6411

``` r
#Calculate the one way cluster standard error and print out via Stargazer
cluster_1_se(m1,dt[studyno == 1, cluster])
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_ad                      -0.010           
    ##                               (0.024)          
    ##                                                
    ## Constant                     0.182***          
    ##                               (0.018)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,364           
    ## R2                            0.0002           
    ## Adjusted R2                   -0.001           
    ## Residual Std. Error      0.382 (df = 1362)     
    ## F Statistic            0.217 (df = 1; 1362)    
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

    ## (Intercept)    treat_ad 
    ##  0.01849151  0.02375363

4.  Change the context: estimate the treatment effect in Study 2, using
    clustered standard errors. If you’ve written your code for part 3
    carefully, you should be able to simply change the row-scoping that
    you’re calling. If you didn’t write it carefully, for legibility for
    your colleagues, you might consider re-writting your solution to the
    last question.

<!-- end list -->

``` r
#Execute the regression for the Study 2 group
m2 <- lin_reg(formula_str, dt[studyno == 2])

#Calculate clustered standard errors
cluster_1_se(m2, dt[studyno == 2, cluster])
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_ad                      -0.003           
    ##                               (0.036)          
    ##                                                
    ## Constant                     0.606***          
    ##                               (0.018)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,337           
    ## R2                            0.00001          
    ## Adjusted R2                   -0.001           
    ## Residual Std. Error      0.489 (df = 1335)     
    ## F Statistic            0.008 (df = 1; 1335)    
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

    ## (Intercept)    treat_ad 
    ##  0.01818893  0.03550334

5.  Run a regression to test for the effect of the ad on candidate name
    recognition, but this time use the entire sample from both studies –
    do not take into account which study the data is from (more on this
    in a moment), but just “pool” the data.

<!-- end list -->

  - Does this estimate tell you anything useful?
  - Why or why not?
  - Can you say that the treatment assignment procedure used is fully
    random when you estimate this model? Or is there some endogeneous
    process that could be confounding your estimate?

<!-- end list -->

``` r
#Run the pooled regression
m3 <- lin_reg(formula_str, dt)

#Run the pooled cluster standard error
cluster_1_se(m3, dt[ ,cluster])
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_ad                     -0.155***         
    ##                               (0.027)          
    ##                                                
    ## Constant                     0.454***          
    ##                               (0.019)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   2,701           
    ## R2                             0.025           
    ## Adjusted R2                    0.024           
    ## Residual Std. Error      0.482 (df = 2699)     
    ## F Statistic          68.313*** (df = 1; 2699)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

    ## (Intercept)    treat_ad 
    ##  0.01857624  0.02673048

``` r
#Compare ATEs
study1_ate <- summary(m1)$coefficients[2,1]

study2_ate <- summary(m2)$coefficients[2,1]
```

> **Answer:** While the F Statistic comes back with a very strong score,
> the pooled data doesn’t tell us anything useful. The two studies have
> very different ATEs and create biased data when grouped. Study 1 had
> an ATE of -0.0097979. While Study 2 had an ATE of -0.0028033.

> Regarding the random assignment model, study 1 and 2 used some
> different techniques to assign cluster, block those clusters and then
> ultimately apply treatment to clusters within the block. Walking
> through this, the cluster creation assumes that it is necessary to
> create some homogenity treatment and control. Then the blocking is
> done to ensure adequate sample sizes. The ultimate treatment and
> control is not randomly assigned because the experiment has
> artifically assigned a specific percentage of representative clusters
> to the treatment group. This means we may have over or
> underrepresentation of specific registered voters with systematic and
> endogenous error.

6.  Estimate a model that uses all the data, but this time include a
    variable that identifies whether an observation was generated during
    Study 1 or Study 2.

<!-- end list -->

  - What is estimated in the “Study 2 Fixed Effect”?
  - What is the treatment effect estimate and associated p-value?
  - Think a little bit more about the treatment effect that you’ve
    estimated: Can this treatment effect, as you’ve entered it in the
    model be *different* between Study 1 and Study 2?
  - Why or why not?

<!-- end list -->

``` r
#Create dummy variable for studyno by converting 1,2 to 0,1
dt[, studyno_bin := studyno - 1]

#Create new formula string and run lin_reg function
formula_str2 <- "name_recall ~ treat_ad + studyno_bin"
m4 <- lin_reg(formula_str2, dt)

#Clustered SEs
cluster_1_se(m4, dt[ ,cluster])
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_ad                      -0.007           
    ##                               (0.020)          
    ##                                                
    ## studyno_bin                  0.426***          
    ##                               (0.021)          
    ##                                                
    ## Constant                     0.181***          
    ##                               (0.017)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   2,701           
    ## R2                             0.193           
    ## Adjusted R2                    0.193           
    ## Residual Std. Error      0.438 (df = 2698)     
    ## F Statistic          322.848*** (df = 2; 2698) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

    ## (Intercept)    treat_ad studyno_bin 
    ##  0.01697018  0.02041542  0.02069695

> **Answer:** - What is estimated in the “Study 2 Fixed Effect”? The
> estimate is 0.4260988.  
> \- What is the treatment effect estimate and associated p-value? The
> treatment effect estimate is -0.0067752 with a p-value of 0.7093688.  
> \- Think a little bit more about the treatment effect that you’ve
> estimated: Can this treatment effect, as you’ve entered it in the
> model be *different* between Study 1 and Study 2? No, it cannot be
> different.  
> \- Why or why not? The model has a single estimate of the treatment
> effect across both pools, while the dummy variable has it’s own value
> of effect which is accounted for when 0 or 1.

7.  Estimate a model that lets the treatment effects be different
    between Study 1 and Study 2. With this model, conduct a formal test
    – it must have a p-value associated with the test – for whether
    the treatment effects are different in Study 1 than Study 2.

<!-- end list -->

``` r
#Create new formula string with interaction variable and run lin_reg function
formula_str3 <- "name_recall ~ treat_ad + studyno_bin + studyno_bin * treat_ad"
m5 <- lin_reg(formula_str3, dt)

#Clustered SEs
cluster_1_se(m5, dt[ ,cluster])
```

    ## 
    ## ================================================
    ##                          Dependent variable:    
    ##                      ---------------------------
    ##                            formula_string       
    ## ------------------------------------------------
    ## treat_ad                       -0.010           
    ##                                (0.024)          
    ##                                                 
    ## studyno_bin                   0.423***          
    ##                                (0.026)          
    ##                                                 
    ## treat_ad:studyno_bin            0.007           
    ##                                (0.043)          
    ##                                                 
    ## Constant                      0.182***          
    ##                                (0.018)          
    ##                                                 
    ## ------------------------------------------------
    ## Observations                    2,701           
    ## R2                              0.193           
    ## Adjusted R2                     0.192           
    ## Residual Std. Error       0.438 (df = 2697)     
    ## F Statistic           215.167*** (df = 3; 2697) 
    ## ================================================
    ## Note:                *p<0.1; **p<0.05; ***p<0.01

    ##          (Intercept)             treat_ad          studyno_bin 
    ##           0.01848800           0.02374912           0.02592963 
    ## treat_ad:studyno_bin 
    ##           0.04270099

> **Answer:** Using the t value on the treat\_ad -0.4061243 \< 1.96 we
> see that the value is not significant at the 0.05 level.

# 2\. Peruvian Recycling

Look at [this article](./readings/recycling_peru.pdf) about encouraging
recycling in Peru. The paper contains two experiments, a “participation
study” and a “participation intensity study.” In this problem, we will
focus on the latter study, whose results are contained in Table 4 in
this problem. You will need to read the relevant section of the paper
(starting on page 20 of the manuscript) in order to understand the
experimental design and variables. (*Note that “indicator variable” is a
synonym for “dummy variable,” in case you haven’t seen this language
before.*)

1.  In Column 3 of Table 4A, what is the estimated ATE of providing a
    recycling bin on the average weight of recyclables turned in per
    household per week, during the six-week treatment period? Provide a
    95% confidence interval.

> **Answer:** The ATE on the average weight of recyclables is 0.281 the
> confidence interval is \[0.25944 , 0.30256\]

2.  In Column 3 of Table 4A, what is the estimated ATE of sending a text
    message reminder on the average weight of recyclables turned in per
    household per week? Provide a 95% confidence interval.

> **Answer:** The ATE of sending a text message reminder on the average
> weight of recyclables turned is was -0.024. The confidence interval is
> \[-0.10044 , 0.05244\].

3.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of providing a recycling bin?

> **Answer:** The Any bin (1) had signficiant effects at the 5% level
> for:  
> \- Percentage of visits turned in bag  
> \- Avg. no. bins turned in per week  
> \- Avg. weight (in kg) of recyclables turned in per week  
> \- Avg. market value of recyclables given per week

4.  Which outcome measures in Table 4A show statistically significant
    effects (at the 5% level) of sending text messages?

> **Answer:** The Any SMS Message (2) had significant effects at the 5%
> level for none of the outcomes.

5.  Suppose that, during the two weeks before treatment, household A
    turns in 2kg per week more recyclables than household B does, and
    suppose that both households are otherwise identical (including
    being in the same treatment group). From the model, how much more
    recycling do we predict household A to have than household B, per
    week, during the six weeks of treatment? Provide only a point
    estimate, as the confidence interval would be a bit complicated.
    This question is designed to test your understanding of slope
    coefficients in regression.

> **Answer:** To answer this question there are two pertinent pieces of
> information from the table.  
> \- Any bins (1) = 0.187 - Avg weight (in kg) of recyclables turned in
> per week, baseline = 0.281 We know the Any bins coefficient to be a
> fixed value meaning if they are both treated it will be 0.187 for both
> A and B thus netting out 0.  
> However, the baseline of 0.281 is not. We then get household A turns
> in 0.281 more kg per week than household B. With a 6 week treatment
> household A should have an additional 1.686 kg.

6.  Suppose that the variable “percentage of visits turned in bag,
    baseline” had been left out of the regression reported in Column 1.
    What would you expect to happen to the results on providing a
    recycling bin? Would you expect an increase or decrease in the
    estimated ATE? Would you expect an increase or decrease in the
    standard error? Explain our reasoning.

> **Answer:** If the *percentage of visits turned in bag, baseline* were
> left out of the regression I would expect that this would increase the
> estimated ATE of providing a recycling bin. This baseline coefficient
> would become a confounding factor and those effects would be visible
> elsewhere. I would also expect an increase in the standard error. The
> lack of baseline would increase the variability between participants
> by ignoring pre-treatment behaviors.

7.  In column 1 of Table 4A, would you say the variable “has cell phone”
    is a bad control? Explain your reasoning.

> **Answer:** Yes, I dont believe *has cell phone* was a strong control.
> Though the table indicates it has statistical significance across many
> variables it is likley highly correlated to *Any SMS message*. It is
> also not a property that should be affected by the application of the
> treatment.

8.  If we were to remove the “has cell phone” variable from the
    regression, what would you expect to happen to the coefficient on
    “Any SMS message”? Would it go up or down? Explain your reasoning.

> **Answer:** If the *has cell phone* variable was removed I would
> expect the coefficent on *Any SMS message* to increase. I also believe
> it will likely make *Any SMS message* have a greater number of
> statistically significant effects. This is likely to happen because
> the ability to receive an SMS is predicated on the existence of having
> a cell phone. So any effect that we see from *has cell phone* would
> transfer some or all of that effect to *Any SMS message*.

# 3\. Multifactor Experiments

Staying with the same experiment, now lets think about multifactor
experiments.

1.  What is the full experimental design for this experiment? Tell us
    the dimensions, such as 2x2x3. (Hint: the full results appear in
    Panel 4B.).

> **Answer:** The dimensions are:  
> Bins:  
> No Bin, Bin with Sticker, Bin without Sticker  
> Cellphone:  
> No cell phone, Has cell phone. SMS:  
> No SMS, Generic SMS, Personalized SMS  
> Resulting in 3x2x3 dimensions.

2.  In the results of Table 4B, describe the baseline category. That is,
    in English, how would you describe the attributes of the group of
    people for whom all dummy variables are equal to zero?

> **Answer:** So to define a participant with all zeros for dummy
> variables they would:  
> Have no bin, Have no cell phone, Receive no SMS.

> These participants have *Percentage of visits turned in bag,
> baseline*, *Avg. no. of bins turned in per week, baseline*, *Avg.
> weight (in kg) of recyclables turned in per week, baseline*, *Avg.
> market value of recyclables given per week, baseline*, *Avg.
> percentage of contamination per week, baseline* which provides an
> average baseline behavior for these participants. This means for
> participants that recevie none of the treatments provide an average
> baseline behavior multiplied by their pre and post treatment behavior
> (these participants should have no pre treatment/post treatment
> difference).

3.  In column (1) of Table 4B, interpret the magnitude of the
    coefficient on “bin without sticker.” What does it mean?

> **Answer:** When a participant receives a *bin without sticker* they
> will have an increase of 0.035 turn in a bag for an additional
> percentage of visits over the baseline only participants.

4.  In column (1) of Table 4B, which seems to have a stronger treatment
    effect, the recycling bin with message sticker, or the recycling bin
    without sticker? How large is the magnitude of the estimated
    difference?

> **Answer:** According to Table 4B the stronger treatement effect is
> from the *recycling bin with sticker* over the *recycling bin without
> sticker*. *Recycling bin with sticker* has an estimated effect of
> 0.055 while *Recycling bin without sticker* has an estimated effect of
> 0.035. The magnitude of estimated difference is 0.02.

5.  Is this difference you just described statistically significant?
    Explain which piece of information in the table allows you to answer
    this question.

> **Answer:** No, they are not statistically significant. The standard
> error is 0.015 for both treatments and the difference of the means is
> less than 2x the standard error.

6.  Notice that Table 4C is described as results from “fully saturated”
    models. What does this mean? Looking at the list of variables in the
    table, explain in what sense the model is “saturated.”

> **Answer:** The fully saturated model means that rather than providing
> a series of dummy variables like: *Any SMS message* this model builds
> out each and every scenario separately such as *Generic SMS message +
> No bin*. This model has less value in the ATE as it requires us to see
> the ATE for all combinations rather than the ATE for the individual
> treatment variable.

# 4\. Now\! Do it with data

Download the data set for the recycling study in the previous problem,
obtained from the authors. We’ll be focusing on the outcome variable
Y=“number of bins turned in per week” (avg\_bins\_treat).

``` r
d <- read.dta("./data/karlan_data_subset_for_class.dta")
d <- data.table(d)
head(d)
```

    ##    street havecell avg_bins_treat base_avg_bins_treat bin sms bin_s bin_g sms_p
    ## 1:      7        1      1.0416666               0.750   1   1     1     0     0
    ## 2:      7        1      0.0000000               0.000   0   1     0     0     1
    ## 3:      7        1      0.7500000               0.500   0   0     0     0     0
    ## 4:      7        1      0.5416667               0.500   0   0     0     0     0
    ## 5:      6        1      0.9583333               0.375   1   0     0     1     0
    ## 6:      8        0      0.2083333               0.000   1   0     0     1     0
    ##    sms_g
    ## 1:     1
    ## 2:     0
    ## 3:     0
    ## 4:     0
    ## 5:     0
    ## 6:     0

``` r
max(d$base_avg_bins_treat)
```

    ## [1] 6.375

``` r
min(d$base_avg_bins_treat)
```

    ## [1] 0

``` r
d[,list(mean_bins_treat=mean(avg_bins_treat), mean_bins_base=mean(base_avg_bins_treat)), by=list(street,havecell)]
```

    ##      street havecell mean_bins_treat mean_bins_base
    ##   1:      7        1       0.4666667      0.3500000
    ##   2:      6        1       0.5625000      0.6875000
    ##   3:      8        0       0.5583333      0.5500000
    ##   4:      8        1       0.5324074      0.5416667
    ##   5:      5        1       0.5297619      0.5892857
    ##  ---                                               
    ## 338:    246        0       0.6250000      0.5937500
    ## 339:    246        1       0.4166667      0.8125000
    ## 340:    257        0       0.4843750      0.5000000
    ## 341:     NA        1       1.2291667      1.7500000
    ## 342:     NA        0       0.0000000      0.0000000

``` r
d[base_avg_bins_treat > 4]
```

    ##    street havecell avg_bins_treat base_avg_bins_treat bin sms bin_s bin_g sms_p
    ## 1:    138        1      4.1666665               6.375   0   0     0     0     0
    ## 2:    117        0      3.6250000               4.750   0   0     0     0     0
    ## 3:    216        1      0.7916667               6.250   0   0     0     0     0
    ## 4:    102        0      2.5833333               5.750   1   0     0     1     0
    ##    sms_g
    ## 1:     0
    ## 2:     0
    ## 3:     0
    ## 4:     0

``` r
d[avg_bins_treat > 3]
```

    ##    street havecell avg_bins_treat base_avg_bins_treat bin sms bin_s bin_g sms_p
    ## 1:    138        1       4.166667               6.375   0   0     0     0     0
    ## 2:    117        0       3.625000               4.750   0   0     0     0     0
    ##    sms_g
    ## 1:     0
    ## 2:     0

``` r
#convert -999 streets to na
d[street==-999] <- NA

#remove na rows from the data set.  These values are found in street, and havecell.
dt <- na.omit(d)


nrow(d)
```

    ## [1] 1785

``` r
nrow(dt)
```

    ## [1] 1661

``` r
## Do some quick exploratory data analysis with this data. There are some values in this data that seem a bit strange. Determine what these are, and figure out what you would like to do with them. Also, notice what happens with your estimates vis-a-vis the estimates that are produced by the authors when you do something sensible with this strange values. 
```

> **Answer:** There are some strange street values of -999, and NA.
> There are also NAs in the *havecell* column.

> To clean this up, I converted the -999 streets to NA and then removed
> all rows which have an NA value.

1.  For simplicity, let’s start by measuring the effect of providing a
    recycling bin, ignoring the SMS message treatment (and ignoring
    whether there was a sticker on the bin or not). Run a regression of
    Y on only the bin treatment dummy, so you estimate a simple
    difference in means. Provide a 95% confidence interval for the
    treatment effect.

<!-- end list -->

``` r
#Create new formula string and linear regression model
formula_str4 <- "avg_bins_treat ~ bin"
m6 <- lin_reg(formula_str4, dt)

#Calculate the robust SE
m6_se <- robust_se(m6)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## bin                          0.135***          
    ##                               (0.021)          
    ##                                                
    ## Constant                     0.635***          
    ##                               (0.012)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,661           
    ## R2                             0.025           
    ## Adjusted R2                    0.024           
    ## Residual Std. Error      0.404 (df = 1659)     
    ## F Statistic          41.683*** (df = 1; 1659)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Calculate the confidence interval
m6_confint <- classic_confint(m6,0.95)
```

> **Answer:** After cleaning the NA values from the data, fitting the
> model, computing robust standard error the confidence interval is,
> \[0.093775, 0.1756157\]

2.  Now add the pre-treatment value of Y as a covariate. Provide a 95%
    confidence interval for the treatment effect. Explain how and why
    this confidence interval differs from the previous one.

<!-- end list -->

``` r
#Create new formula string inlcuding the pre-treatment value of Y as covariate and linear regression model
formula_str5 <- "avg_bins_treat ~ bin + base_avg_bins_treat + bin * base_avg_bins_treat"
m7 <- lin_reg(formula_str5, dt)

#Calculate the robust SE
m7_se <- robust_se(m7)
```

    ## 
    ## ===================================================
    ##                             Dependent variable:    
    ##                         ---------------------------
    ##                               formula_string       
    ## ---------------------------------------------------
    ## bin                              0.116***          
    ##                                   (0.042)          
    ##                                                    
    ## base_avg_bins_treat              0.380***          
    ##                                   (0.045)          
    ##                                                    
    ## bin:base_avg_bins_treat            0.015           
    ##                                   (0.059)          
    ##                                                    
    ## Constant                         0.358***          
    ##                                   (0.031)          
    ##                                                    
    ## ---------------------------------------------------
    ## Observations                       1,661           
    ## R2                                 0.336           
    ## Adjusted R2                        0.335           
    ## Residual Std. Error          0.333 (df = 1657)     
    ## F Statistic              279.926*** (df = 3; 1657) 
    ## ===================================================
    ## Note:                   *p<0.1; **p<0.05; ***p<0.01

``` r
#Calculate the confidence interval
m7_confint <- classic_confint(m7,0.95)

#Create new formula string inlcuding the pre-treatment value of Y as covariate and exclude the interaction term and linear regression model
formula_str6 <- "avg_bins_treat ~ bin + base_avg_bins_treat"
m8 <- lin_reg(formula_str6, dt)

#Calculate the robust SE
m8_se <- robust_se(m8)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## bin                          0.127***          
    ##                               (0.018)          
    ##                                                
    ## base_avg_bins_treat          0.385***          
    ##                               (0.032)          
    ##                                                
    ## Constant                     0.354***          
    ##                               (0.022)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,661           
    ## R2                             0.336           
    ## Adjusted R2                    0.335           
    ## Residual Std. Error      0.333 (df = 1658)     
    ## F Statistic          419.942*** (df = 2; 1658) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Calculate the confidence interval
m8_confint <- classic_confint(m8,0.95)
```

> **Answer:** In this case we add the covariate from the
> *base\_avg\_bins\_treat* as well as the interaction term to arrive at
> the confidence interval \[0.0619488, 0.1700611\]. The interaction term
> is not significant so we can drop it and only use the covariate
> without interaction to get a new confidence interval of \[0.0933373,
> 0.1608755\].

> In this case the confidence interval narrows as we add an additional
> explanatory variable. We’re adding additional information which is
> statistically significant. This also brought down the ATE as this was
> a pre-treatment effect.

3.  Now add the street fixed effects. (You’ll need to use the R command
    factor().) Provide a 95% confidence interval for the treatment
    effect.

<!-- end list -->

``` r
#Create new formula string adding the street fixed effects
formula_str7 <- "avg_bins_treat ~ bin + base_avg_bins_treat + factor(street)"
m9 <- lin_reg(formula_str7, dt)

#Calculate the robust SE
m9_se <- robust_se_omit(m9,omitted='street')
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## bin                          0.116***          
    ##                               (0.020)          
    ##                                                
    ## base_avg_bins_treat          0.367***          
    ##                               (0.031)          
    ##                                                
    ## Constant                     0.276***          
    ##                               (0.054)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,661           
    ## R2                             0.442           
    ## Adjusted R2                    0.374           
    ## Residual Std. Error      0.323 (df = 1480)     
    ## F Statistic          6.516*** (df = 180; 1480) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Calculate the confidence interval
m9_confint <- classic_confint(m9,0.95)
```

> **Answer:** Adding the street as a factor creates a confidence
> interval of \[0.081521, 0.1506072\].

4.  Recall that the authors described their experiment as “stratified at
    the street level,” which is a synonym for blocking by street.
    Explain why the confidence interval with fixed effects does not
    differ much from the previous one.

> **Answer:** The base effect already added most of the explanatory
> power so that the individual street fixed effects were relatively
> small. This held true because the blocking was done at the street
> level and omitting the variable did not create an inherent bias.

5.  Perhaps having a cell phone helps explain the level of recycling
    behavior. Instead of “has cell phone,” we find it easier to
    interpret the coefficient if we define the variable " no cell
    phone." Give the R command to define this new variable, which equals
    one minus the “has cell phone” variable in the authors’ data set.
    Use “no cell phone” instead of “has cell phone” in subsequent
    regressions with this dataset.

<!-- end list -->

``` r
dt[, no_cell_phone := 1 - havecell]
```

6.  Now add “no cell phone” as a covariate to the previous regression.
    Provide a 95% confidence interval for the treatment effect. Explain
    why this confidence interval does not differ much from the previous
    one.

<!-- end list -->

``` r
#Create new formula string adding the no cell phone effect
formula_str8 <- "avg_bins_treat ~ bin + base_avg_bins_treat + factor(street) + no_cell_phone"
m10 <- lin_reg(formula_str8, dt)

#Calculate the robust SE
m10_se <- robust_se_omit(m10,omitted = 'street')
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## bin                          0.117***          
    ##                               (0.020)          
    ##                                                
    ## base_avg_bins_treat          0.367***          
    ##                               (0.031)          
    ##                                                
    ## no_cell_phone                -0.043**          
    ##                               (0.019)          
    ##                                                
    ## Constant                     0.288***          
    ##                               (0.054)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,661           
    ## R2                             0.444           
    ## Adjusted R2                    0.376           
    ## Residual Std. Error      0.323 (df = 1479)     
    ## F Statistic          6.535*** (df = 181; 1479) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Calculate the confidence interval
m10_confint <- classic_confint(m10,0.95)
```

> **Answer:** For this model we see a confidence interval of \[0.082673,
> 0.1516659\]. This doesn’t change the model much as the additional
> covariate does not add explanatory power, but rather shows the
> explanatory power of the covariate. the model already accounted for
> this.

7.  Now let’s add in the SMS treatment. Re-run the previous regression
    with “any SMS” included. You should get the same results as in Table
    4A. Provide a 95% confidence interval for the treatment effect of
    the recycling bin. Explain why this confidence interval does not
    differ much from the previous one.

<!-- end list -->

``` r
#Create new formula string adding the any SMS effect
formula_str9 <- "avg_bins_treat ~ bin + base_avg_bins_treat + factor(street) + no_cell_phone + sms"
m11 <- lin_reg(formula_str9, dt)

#Calculate the robust SE
m11_se <- robust_se_omit(m11,omitted = 'street')
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## bin                          0.117***          
    ##                               (0.020)          
    ##                                                
    ## base_avg_bins_treat          0.367***          
    ##                               (0.031)          
    ##                                                
    ## no_cell_phone                 -0.033           
    ##                               (0.024)          
    ##                                                
    ## sms                            0.017           
    ##                               (0.025)          
    ##                                                
    ## Constant                     0.279***          
    ##                               (0.056)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,661           
    ## R2                             0.445           
    ## Adjusted R2                    0.376           
    ## Residual Std. Error      0.323 (df = 1478)     
    ## F Statistic          6.501*** (df = 182; 1478) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Calculate the confidence interval
m11_confint <- classic_confint(m11,0.95)
```

> **Answer:** The confidence interval when adding SMS is \[0.0824637,
> 0.1514719\]. As above the explanatory power of the model was largely
> accounted for in the core treatment and additional covariates are a
> minor factor in the total model. Adding to the model does more explain
> the value of SMS not change the ATE.

8.  Now reproduce the results of column 2 in Table 4B, estimating
    separate treatment effects for the two types of SMS treatments and
    the two types of recycling-bin treatments. Provide a 95% confidence
    interval for the effect of the unadorned recycling bin. Explain how
    your answer differs from that in part (g), and explain why you think
    it differs.

<!-- end list -->

``` r
#Create new formula string adding the any SMS effect
formula_str10 <- "avg_bins_treat ~ bin_s + bin_g + sms_p + sms_g + no_cell_phone + base_avg_bins_treat + factor(street)"
m12 <- lin_reg(formula_str10, dt)

#Calculate the robust SE
m12_se <- robust_se_omit(m12,omitted = 'street')
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## bin_s                        0.129***          
    ##                               (0.025)          
    ##                                                
    ## bin_g                        0.106***          
    ##                               (0.026)          
    ##                                                
    ## sms_p                          0.007           
    ##                               (0.029)          
    ##                                                
    ## sms_g                          0.030           
    ##                               (0.029)          
    ##                                                
    ## no_cell_phone                 -0.033           
    ##                               (0.024)          
    ##                                                
    ## base_avg_bins_treat          0.367***          
    ##                               (0.031)          
    ##                                                
    ## Constant                     0.279***          
    ##                               (0.055)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                   1,661           
    ## R2                             0.445           
    ## Adjusted R2                    0.376           
    ## Residual Std. Error      0.323 (df = 1476)     
    ## F Statistic          6.434*** (df = 184; 1476) 
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Calculate the confidence interval
m12_confint <- classic_confint(m12,0.95)
```

> **Answer:** The confidence interval when replicating table 4b is
> \[0.0842761, 0.1734477\]. Again the confidence interval narrows, but
> the ATE is relatively stable. This is because the explanatory power of
> the treatement is accounted for adding the covariates is simply
> assigning the explanatory power to the fixed effect or to the
> individual treatments.

# 5\. A Final Practice Problem

Now for a fictional scenario. An emergency two-week randomized
controlled trial of the experimental drug ZMapp is conducted to treat
Ebola. (The control represents the usual standard of care for patients
identified with Ebola, while the treatment is the usual standard of care
plus the drug.)

Here are the (fake) data.

``` r
d <- fread("./data/Ebola_rct2.csv")
head(d)
```

    ##    temperature_day0 vomiting_day0 treat_zmapp temperature_day14 vomiting_day14
    ## 1:         99.53168             1           0          98.62634              1
    ## 2:         97.37372             0           0          98.03251              1
    ## 3:         97.00747             0           1          97.93340              0
    ## 4:         99.74761             1           0          98.40457              1
    ## 5:         99.57559             1           1          99.31678              1
    ## 6:         98.28889             1           1          99.82623              1
    ##    male
    ## 1:    0
    ## 2:    0
    ## 3:    1
    ## 4:    0
    ## 5:    0
    ## 6:    1

You are asked to analyze it. Patients’ temperature and whether they are
vomiting is recorded on day 0 of the experiment, then ZMapp is
administered to patients in the treatment group on day 1. Vomiting and
temperature is again recorded on day 14.

1.  Without using any covariates, answer this question with regression:
    What is the estimated effect of ZMapp (with standard error in
    parentheses) on whether someone was vomiting on day 14? What is the
    p-value associated with this estimate?

<!-- end list -->

``` r
#Create new formula string on zmapp treatment with vomiting on day 14
formula_str11 <- "vomiting_day14 ~ treat_zmapp"
m13 <- lin_reg(formula_str11, d)

#Calculate the robust SE
m13_se <- robust_se(m13)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_zmapp                  -0.238***         
    ##                               (0.091)          
    ##                                                
    ## Constant                     0.847***          
    ##                               (0.048)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    100            
    ## R2                             0.073           
    ## Adjusted R2                    0.063           
    ## Residual Std. Error       0.421 (df = 98)      
    ## F Statistic            7.705*** (df = 1; 98)   
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#store the p-value
m13_p <- coeftest(m13, vcov. = vcovHC(m13))[, 'Pr(>|t|)']['treat_zmapp']
```

> **Answer:** The treatement effect of Zmapp on vomiting on day 14 is:
> -0.2377015 (0.0914595). The p-value associated with this is:
> 0.0107933.

2.  Add covariates for vomiting on day 0 and patient temperature on day
    0 to the regression from part (a) and report the ATE (with standard
    error). Also report the p-value.

<!-- end list -->

``` r
#Create new formula string add covariates vomiting day 0 and patient temp day 0.
formula_str12 <- "vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0"
m14 <- lin_reg(formula_str12, d)

#Calculate the robust SE
m14_se <- robust_se(m14)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_zmapp                  -0.166**          
    ##                               (0.082)          
    ##                                                
    ## vomiting_day0                  0.065           
    ##                               (0.178)          
    ##                                                
    ## temperature_day0             0.206***          
    ##                               (0.078)          
    ##                                                
    ## Constant                     -19.470**         
    ##                               (7.608)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    100            
    ## R2                             0.311           
    ## Adjusted R2                    0.290           
    ## Residual Std. Error       0.367 (df = 96)      
    ## F Statistic           14.447*** (df = 3; 96)   
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Store the robust p-value
m14_p <- coeftest(m14, vcov. = vcovHC(m14))[, 'Pr(>|t|)']['treat_zmapp']
```

> **Answer:** The treatement effect of adding the covariates is:
> -0.1655367 (0.0819765). The p-value associated with this is: 0.046242.

3.  Do you prefer the estimate of the ATE reported in part (a) or part
    (b)? Why? Report the results of the F-test that you used to form
    this opinion.

<!-- end list -->

``` r
anova(m13,m14, test = 'F')
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: vomiting_day14 ~ treat_zmapp
    ## Model 2: vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0
    ##   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
    ## 1     98 17.383                                  
    ## 2     96 12.918  2    4.4653 16.592 6.472e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> **Answer:** The F-test suggests that the part b model is better with
> an F score of 16.5919089. This could still mean that randomness made
> part b a better model, so we then check the p-value 6.471868310^{-7}.
> It is small and significant enough to indicate that randomness does
> not account for the better fit of part b.

4.  The regression from part (b) suggests that temperature is highly
    predictive of vomiting. Also include temperature on day 14 as a
    covariate in the regression from part (b) and report the ATE, the
    standard error, and the p-value.

<!-- end list -->

``` r
#Create new formula string add covariate; temp day 14
formula_str13 <- "vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0 + temperature_day14"
m15 <- lin_reg(formula_str13, d)

#Calculate the robust SE
m15_se <- robust_se(m15)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_zmapp                   -0.120           
    ##                               (0.086)          
    ##                                                
    ## vomiting_day0                  0.046           
    ##                               (0.173)          
    ##                                                
    ## temperature_day0              0.177**          
    ##                               (0.077)          
    ##                                                
    ## temperature_day14             0.060**          
    ##                               (0.026)          
    ##                                                
    ## Constant                    -22.592***         
    ##                               (7.746)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    100            
    ## R2                             0.340           
    ## Adjusted R2                    0.312           
    ## Residual Std. Error       0.361 (df = 95)      
    ## F Statistic           12.244*** (df = 4; 95)   
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

``` r
#Store the robust p-value
m15_p <- coeftest(m15, vcov. = vcovHC(m15))[, 'Pr(>|t|)']['treat_zmapp']

anova(m14,m15, test = 'F')
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0
    ## Model 2: vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0 + 
    ##     temperature_day14
    ##   Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
    ## 1     96 12.918                              
    ## 2     95 12.372  1   0.54609 4.1933 0.04335 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

5.  Do you prefer the estimate of the ATE reported in part (b) or part
    (d)? Why?

> **Answer:** The treatement effect of adding the covariates is:
> -0.1201006 (0.0857982). The p-value associated with this is:
> 0.1648294.

> As before I prefer the answer in part d. Looking at the F-test
> 4.1932503 the score is greater than 1 and the probability is
> sufficiently small to be significant at the 0.05 level 0.0433454.

6.  Now let’s switch from the outcome of vomiting to the outcome of
    temperature, and use the same regression covariates as in part (b).
    Test the hypothesis that ZMapp is especially likely to reduce mens’
    temperatures, as compared to womens’, and describe how you did so.
    What do the results suggest?

<!-- end list -->

``` r
#Create new formula string add covariates vomiting day 0 and patient temp day 0.
formula_str14 <- "temperature_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0 + male + treat_zmapp * male"
m16 <- lin_reg(formula_str14, d)

#Calculate the robust SE
m16_se <- robust_se(m16)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_zmapp                   -0.231*          
    ##                               (0.118)          
    ##                                                
    ## vomiting_day0                  0.041           
    ##                               (0.195)          
    ##                                                
    ## temperature_day0             0.505***          
    ##                               (0.105)          
    ##                                                
    ## male                         3.085***          
    ##                               (0.122)          
    ##                                                
    ## treat_zmapp:male             -2.077***         
    ##                               (0.198)          
    ##                                                
    ## Constant                     48.713***         
    ##                              (10.194)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    100            
    ## R2                             0.906           
    ## Adjusted R2                    0.901           
    ## Residual Std. Error       0.452 (df = 94)      
    ## F Statistic           180.953*** (df = 5; 94)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

> **Answer:** In order to perform this test, I added *male* as a
> covariate and also the interaction effect between *treat\_zmapp* and
> *male*. The interaction effect results in an ATE of -2.0766863. This
> also passes a significance test which indicates Zmapp decreases mens’
> temperatures substantially.

7.  Suspend reality for just a moment – suppose that you had the option
    of being a man or a woman who was a part of this study. Based on
    this data, which sex would you rather be? This time, you need to
    produce evidence (probably from your model estimates) to inform your
    determination. What does your determination depend on?

<!-- end list -->

``` r
male_nettemp <- coeftest(m16, vcov. = vcovHC(m16))[, 'Estimate']['male'] + coeftest(m16, vcov. = vcovHC(m16))[, 'Estimate']['treat_zmapp:male']


#Checking vomiting outcomes for men and women, but results are not significant
#Create new formula string.
formula_str15 <- "vomiting_day14 ~ treat_zmapp + vomiting_day0 + temperature_day0 + male + treat_zmapp * male"
m17 <- lin_reg(formula_str15, d)

#Calculate the robust SE
m17_se <- robust_se(m17)
```

    ## 
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                           formula_string       
    ## -----------------------------------------------
    ## treat_zmapp                   -0.130           
    ##                               (0.095)          
    ##                                                
    ## vomiting_day0                  0.077           
    ##                               (0.181)          
    ##                                                
    ## temperature_day0              0.198**          
    ##                               (0.080)          
    ##                                                
    ## male                           0.016           
    ##                               (0.091)          
    ##                                                
    ## treat_zmapp:male              -0.087           
    ##                               (0.169)          
    ##                                                
    ## Constant                     -18.772**         
    ##                               (7.787)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    100            
    ## R2                             0.314           
    ## Adjusted R2                    0.277           
    ## Residual Std. Error       0.370 (df = 94)      
    ## F Statistic            8.601*** (df = 5; 94)   
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

> **Answer:** Based on the models we see that men have a higher temp.
> When in control and not treated they’re 3.0854861 degrees higher. When
> treated they’re 1.0087998 higher.

8.  Suppose that you had not run the regression in part (f). Instead,
    you speak with a colleague to learn about heterogeneous treatment
    effects. This colleague has access to a non-anonymized version of
    the same dataset and reports that he had looked at heterogeneous
    effects of the ZMapp treatment by each of 10,000 different
    covariates to examine whether each predicted the effectiveness of
    ZMapp on each of 2,000 different indicators of health, for
    20,000,000 different regressions in total. Across these 20,000,000
    regressions your colleague ran, the treatment’s interaction with
    gender on the outcome of temperature is the only heterogeneous
    treatment effect that he found to be statistically significant. He
    reasons that this shows the importance of gender for understanding
    the effectiveness of the drug, because nothing else seemed to
    indicate why it worked. Bolstering his confidence, after looking at
    the data, he also returned to his medical textbooks and built a
    theory about why ZMapp interacts with processes only present in men
    to cure. Another doctor, unfamiliar with the data, hears his theory
    and finds it plausible. How likely do you think it is ZMapp works
    especially well for curing Ebola in men, and why? (This question is
    conceptual can be answered without performing any computation.)

> **Answer:** I find it unlikley that ZMapp works especially well for
> curing Ebola in men. The scale at which they are testing covariates an
> indicators of health make it probable that in some models of the data
> they’ll find a statisically significant outcome, though this is likely
> a non-representative result and should be checked with randomization
> inferrence to understand the outcome better. If this indicates a
> contrary result the experiment should be replicated or reviewed for
> possible design issues and run again.

9.  Now, imagine that what described in part (7) did not happen, but
    that you had tested this heterogeneous treatment effect, and only
    this heterogeneous treatment effect, of your own accord. Would you
    be more or less inclined to believe that the heterogeneous treatment
    effect really exists? Why?

> **Answer:** I think I’d be more inclined to beleieve the heterogenous
> treatment effect really exists since the tremendous number of
> covariates and health indicators had not been shown to not have an
> effect. However, I think it would be a mistake to simply believe the
> outcome of the single test without another replication. In the large
> study we know that there is likley not true independence of the
> covariates. So even without testing, we’re at the same risk level for
> accepting an inaccurate outcome.

10. Another colleague proposes that being of African descent causes one
    to be more likely to get Ebola. He asks you what ideal experiment
    would answer this question. What would you tell him? (*Hint: refer
    to Chapter 1 of Mostly Harmless Econometrics.*)

> **Answer:** There is no method of experiment which can ethically be
> executed. Even with informed consent you cannot knowingly expose a set
> of research subjects to Ebola. It may be possible to study infection
> rates among ethnic populations when an Ebola outbreak takes place, but
> this would necessarily be observational in nature and not a true
> experiment.
