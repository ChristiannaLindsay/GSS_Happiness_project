    # Project Overview
    
    The goal of this project is to predict the likelihood of someone being very happy,
    given factors such as political orientation, race, age, and marital status.
    Logistic regression is used to measure the importance of independent variables in
    predicting the likelihood of a binary outcome variable. In this case, the outcome
    of interest is being “very happy” as opposed to “pretty happy,” “not too happy,”
    or “do not know/cannot choose,” and the independent variables are certain other
    survey responses. Figure 4 shows the estimated effect of each variable on the odds
    that a person is “very happy,” while holding constant all other variables in the
    model.

    The data for this project is from the General Social Survey, using the years
    2016-2022. The GSS data is available for the even-numbered years between 2000 and 2022,
    excepting 2020 and including 2021. Among the several variables available to weight
    the GSS survey data, WTSSNRPS is chosen for this project, as it contains a nonresponse adjustment
    and is recommended for the years 2004-2022 (Post-stratification Weights for GSS
    1972-2022). The data was downloaded from the GSS Data Explorer website and is contained in this GitHub repository.
