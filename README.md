##### 1. The data orings.csv are the launch temperatures (degrees Fahrenheit) and an indicator of O-ring failures for 24 space shuttle launches prior to the space shuttle Challenger disaster of January 27,1986.

##### (a) Fit the logistic regression with temperature as the predictor. Report the estimated coefficients and their standard errors. Write down the estimated regression equation.

    setwd("E:/maynooth/2nd_sem/ST466[A]AdvancedStatisticalModelling/assignment/3")
    orings <- read.csv("orings.csv")
    head(orings)

    ##   Temperature Failure
    ## 1          53     Yes
    ## 2          56     Yes
    ## 3          57     Yes
    ## 4          63      No
    ## 5          66      No
    ## 6          67      No

    attach(orings)
    fit_lr <- glm(Failure ~ Temperature,data=orings,family = binomial())
    summary(fit_lr)

    ## 
    ## Call:
    ## glm(formula = Failure ~ Temperature, family = binomial(), data = orings)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.2125  -0.8253  -0.4706   0.5907   2.0512  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept) 10.87535    5.70291   1.907   0.0565 .
    ## Temperature -0.17132    0.08344  -2.053   0.0400 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 28.975  on 23  degrees of freedom
    ## Residual deviance: 23.030  on 22  degrees of freedom
    ## AIC: 27.03
    ## 
    ## Number of Fisher Scoring iterations: 4

    #parameter estimates and standard error
    # B0hat = 10:87535 and standard error=5.70291
    # B1hat = -0:17132 and standard error=0.08344

    # Estimated regression equation
    #logit(pi hat)=10.8753-0.1713*temperature

    # Interpretation 
    #log of odds for temperature coefficient is 0:17132 and p value is 0.04 which is less than significance level alpha=0.05.Hence temperature is statistically significant.

##### (b) Using a Wald test, test whether the coefficient of temperature is 0, against the alternative hypothesis that the coefficient is negative.

    #H0 : ß1is zero
    #HA : ß1 is negative , where ß1 is coefficient of temperature
    #Wald statistic = z = ß1hat/SE

    waldv <- -0.17132/(0.08344)
    waldv

    ## [1] -2.053212

    # p-value=2p[z >= 2.053] =0.04

    # Probability for wald statistics for degree of freedom one is 0.04 which is less than significance level 0.05.Hence we reject null hypothesis and conclude that coefficient of temperature is negative. 

    # Second method of doing wald test through R package
    #install.packages("aod")
    library("aod")

    ## Warning: package 'aod' was built under R version 3.3.3

    #Computes the Wald score test for the coefficients of a generalized linear model

    wald.test(Sigma = vcov(fit_lr),b=coef(fit_lr),Terms = 2)

    ## Wald test:
    ## ----------
    ## 
    ## Chi-squared test:
    ## X2 = 4.2, df = 1, P(> X2) = 0.04

    # The p value is less than 0.05 and hence we reject null hypothesis and conclude that the coefficient of temperature is negative.

##### (c) Give and interpret a 95% confidence interval for the coefficient of temperature.

    #exp(confint(fit_lr))

    conf <- exp(-0.1723 + c(+1,-1)*((1.96*0.08344)))
    conf

    ## [1] 0.9912806 0.7147357

    # confidence interval is (0.9922521 0.7154362)
    # Ho: Tempearture has no effect on failure of space shuttle
    # Ha : Temerature affects failure of space shuttle
    # since CI does not contain 1.It means true odd ratio is not equal t0 1.therfore we reject null hypothesis and conclude that temperature has effect.

##### (d) On January 27 1986 the launch temperature was 31 (degrees F). According to the model, what is the estimated logit of probability of failure? What is the estimated probability of failure?

    # First calculating the success probability

    logps <- 10.8753-(0.1713*31)
    logps

    ## [1] 5.565

    success <- exp(logps)/(1+exp(logps))
    success

    ## [1] 0.996185

    #estimated logit of probability of failure

    logitf <- (log(1-success)/(success))
    logitf

    ## [1] -5.590149

    # estimated probability of failure
    Failure <- (1-success)
    Failure

    ## [1] 0.003814971

##### (e) Why must the answer to part (d) be treated cautiously?

    #In case of binary response,The  estimated probability (predicted value) may fall outside the range of available explanatory variables.ideally it should be between 0 and 1.Therefore , the answer to part (d) must be considered with care.

##### (f) Test whether the coefficient of temperature is 0, this time using a likelihood ratio test.

    # Ho: Tempearture has no effect on failure of space shuttle (Coeffcient of temperature is zero)
    # Ha : Temerature affects failure of space shuttle ( Coeffcient of temperature is not equal to zero)

    # log likelihood of full model
    LB <- logLik(fit_lr)
    LB

    ## 'log Lik.' -11.51523 (df=2)

    fit0 <- glm(Failure ~ 1 ,data=orings,family = binomial())
    LA <- logLik(fit_lr)
    LA

    ## 'log Lik.' -11.51523 (df=2)

    gsquare <- 2*(-11.51523 + 14.48729)
    gsquare

    ## [1] 5.94412

    # p value ( gsquare >= 5.94412 for 1 degree of freedom ) = 0.02
    # Since p-value is less than 0.01.Therefore we reject null hypothesis and conclude that coefficient of temperature is not zero.
