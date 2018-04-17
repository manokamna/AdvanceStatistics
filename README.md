#### (4) A study to identify the link between body mass index and physical activity was carried out. Two hundred participants with normal body mass index, 200 overweight participants and 200 obese participants were questioned and classified as to what level of physical activity they participated in during the previous year

#### 4(a) What type of study was this (observational, experimental, retrospective, prospective) and what type of sampling scheme was used?

    # Here We have conducted study where 200 participants choosing each level of physical activity and they have the outcome of their interest.SO this is observational study.Besides this the study is looking into past. so it is retrospective study also called case control study.

    # The type of sampling used is product multinomial . It has fixed column total for every column (that is 200) in advance and observed count in each category. The counts in first row have a multinomial distribution and counts in the second row have another multinomail distribution. Similarly for row 3 and 4.

#### (b) With the aid of R test the hypothesis of independence between physical activity and body mass index. Provide and comment on the break down of the test statistic in your answer.

    Inactive  = c(48, 52, 72)
    Irregular_active = c(56,58,56)
    Regular_Notintense = c(63, 62, 54)
    Regular_intense = c(33, 28, 18)

    bmidata <- rbind(Inactive, Irregular_active, Regular_Notintense, Regular_intense)
    colnames(bmidata) = c("Normal", "Overweight", "Obese")
    bmidata

    ##                    Normal Overweight Obese
    ## Inactive               48         52    72
    ## Irregular_active       56         58    56
    ## Regular_Notintense     63         62    54
    ## Regular_intense        33         28    18

    chisq.test(bmidata)

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  bmidata
    ## X-squared = 11.061, df = 6, p-value = 0.08652

    # Null Hypothesis : Physical activity and BMI are independent
    #Alternative Hypothesis : physical Activity and BMI are dependent

    # pearson chi squared value is 11.06

    #The X-squared value is the test statistics obtained from the observed and expected count
    #The degree of freedom = r-1 * c-1 = (4-1)* (3-1) = 6

    #The p-values under null hypothesis is 0.0865 which is greater than 0.05, so we failed to reject null hypothesis and conclude that physical activity and BMI are independent to each other.
