# define discrete, continuous, all variables
variables_disc <- c("respondentage", "respondentgender", "relationship", "highesteducationlevel", "childage", "childgender", "childethnicity", "childnationality", "monthlyhouseholdincome", "housingtype", "paidcaregiver", "bluechas", "orangechas", "publicassistance", "privatedentalinsurance", "financialassistance", "autismspectrumdisorder", "downsyndrome", "cerebralpalsy", "gdd", "schoolwork", "anydentalvisitpastyr", "preventivedentalvisit", "atleastyearlyvisit")
variables_con <- c("respondentagecoded", "childagecoded", "numberlivinginsamehouse")
variables_all <- c(variables_disc, variables_con)
variables_adj <- c("respondentagecoded", "childagecoded", "numberlivinginsamehouse", "respondentgender", "relationship", "highesteducationlevel", "childgender", "childethnicity", "childnationality", "monthlyhouseholdincome", "housingtype", "paidcaregiver", "bluechas", "orangechas", "publicassistance", "privatedentalinsurance", "financialassistance", "autismspectrumdisorder", "downsyndrome", "cerebralpalsy", "gdd", "schoolwork", "anydentalvisitpastyr", "preventivedentalvisit", "atleastyearlyvisit")
 
# select variables
df1 <- df[, variables_adj]

# get predictor matrix
allVars <- names(df1)
# get names of variables with missing values
missVars <- names(df1)[colSums(is.na(df1)) > 0]
predMatrix <- matrix(0, ncol = length(allVars), nrow = length(allVars))
rownames(predMatrix) <- allVars
colnames(predMatrix) <- allVars
predMatrix[c("monthlyhouseholdincome"),1:25] <- 1
predMatrix[c("monthlyhouseholdincome"),c("monthlyhouseholdincome")] <- 0

# random forest imputation with 5 imputations
dfimp <- mice::mice(df1, m = 5, maxit = 5, predictorMatrix = predMatrix, method="rf", seed=12345)

## table 1 ##
for (var in variables_disc){
  var <- ensym(var)
  print(var)
  print(df %>% tabyl(!!var) %>% adorn_totals("row") %>% adorn_pct_formatting(digits = 1))
}

for (var in variables_con){
  var <- ensym(var)
  print(var)
  print(df %>% tabyl(!!var) %>% adorn_totals("row") %>% adorn_pct_formatting(digits = 1))
  print(df %>% summarise_at(c(paste(var)), list(mean = mean, sd = sd, median = median, IQR = IQR, Q1 = ~quantile(., 0.25), Q3 = ~quantile(., 0.75)), na.rm=TRUE)) %>% mutate_all(funs(round(., digits = 3))) %>% as.data.frame()
}

## table 2 ##
# pooled results
# unadjusted and adjusted models
mod1 <- with(dfimp, coeftest(glm(anydentalvisitpastyr ~ monthlyhouseholdincome), family=poisson(link=log)), vcov. = sandwich)
mod1 <- with(dfimp, coeftest(glm(anydentalvisitpastyr ~ monthlyhouseholdincome + childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, family=poisson(link=log)), vcov. = sandwich))
print(pool(mod1))
summary(pool(mod1)) 
df.residual(getfit(mod1, 1))
m1 <- summary(pool(mod1))
m1 <- transform(m1, LL= exp(estimate - 1.96*std.error))
m1 <- transform(m1, UL= exp(estimate + 1.96*std.error))
m1 <- transform(m1, estimateexp = exp(estimate))
print(m1)

mod2 <- with(dfimp, coeftest(glm(preventivedentalvisit ~ monthlyhouseholdincome), family=poisson(link=log)), vcov. = sandwich)
mod2 <- with(dfimp, coeftest(glm(preventivedentalvisit ~ monthlyhouseholdincome + childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, family=poisson(link=log)), vcov. = sandwich))
m2 <- summary(pool(mod2))
m2 <- transform(m2, LL= exp(estimate - 1.96*std.error))
m2 <- transform(m2, UL= exp(estimate + 1.96*std.error))
m2 <- transform(m2, estimateexp = exp(estimate))
print(m2)

mod3 <- with(dfimp, coeftest(glm(atleastyearlyvisit ~ monthlyhouseholdincome), family=poisson(link=log)), vcov. = sandwich)
mod3 <- with(dfimp, coeftest(glm(atleastyearlyvisit ~ monthlyhouseholdincome + childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, family=poisson(link=log)), vcov. = sandwich))
m3 <- summary(pool(mod3))
m3 <- transform(m3, LL= exp(estimate - 1.96*std.error))
m3 <- transform(m3, UL= exp(estimate + 1.96*std.error))
m3 <- transform(m3, estimateexp = exp(estimate))
print(m3)

dfcomp <- mice::complete(dfimp, "long")

## appendix table 1 ##
# replace missing household income values with imputed values
dftab <- as.data.frame(dfimp$imp$monthlyhouseholdincome)
colnames(dftab) <- c("imp1", "imp2", "imp3", "imp4", "imp5")
dfimpmerged <- merge(df1, dftab, by = 'row.names', all = TRUE) 
dfimpmerged$monthlyhouseholdincomecat <- with(dfimpmerged, ifelse(is.na(monthlyhouseholdincome)==TRUE, imp1, monthlyhouseholdincome))
# alternatively, replace with imp2, imp3, imp4, imp5
table(dfimpmerged$a11monthlyhouseholdincomecat)

CrossTable(dfimpmerged$a11monthlyhouseholdincomecat, dfimpmerged$anydentalvisitpastyr)
CrossTable(dfimpmerged$a11monthlyhouseholdincomecat, dfimpmerged$preventivedentalvisit)
CrossTable(dfimpmerged$a11monthlyhouseholdincomecat, dfimpmerged$atleastyearlyvisit)

CrossTable(dfcomp$monthlyhouseholdincome, dfcomp$anydentalvisitpastyr)
CrossTable(dfcomp$monthlyhouseholdincome, dfcomp$preventivedentalvisit)
CrossTable(dfcomp$monthlyhouseholdincome, dfcomp$atleastyearlyvisit)

annextable4 <- CreateTableOne(vars = c("childagecoded", "childgender", "childethnicity", "respondentagecoded", "respondentgender", "highesteducationlevel", "schoolwork", "numberlivinginsamehouse", "bluechasgrp"),
                              data = df1,
                              factorVars = c("childgender", "childethnicity", "respondentgender", "highesteducationlevel", "schoolwork", "bluechasgrp"),
                              strata = "anydentalvisitpastyr",
                              smd = TRUE)
annextable4 <- print(annextable4, smd = TRUE, nonnormal = TRUE, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
annextable5 <- CreateTableOne(vars = c("childagecoded", "childgender", "childethnicity", "respondentagecoded", "respondentgender", "highesteducationlevel", "schoolwork", "numberlivinginsamehouse", "bluechasgrp"),
                              data = df1,
                              factorVars = c("childgender", "childethnicity", "respondentgender", "highesteducationlevelcat", "schoolwork", "bluechasgrp"),
                              strata = "preventivedentalvisit",
                              smd = TRUE)
annextable5 <- print(annextable5, smd = TRUE, nonnormal = TRUE, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
annextable6 <- CreateTableOne(vars = c("childagecoded", "childgender", "childethnicity", "respondentagecoded", "respondentgender", "highesteducationlevel", "schoolwork", "numberlivinginsamehouse", "bluechasgrp"),
                              data = df1,
                              factorVars = c("childgender", "childethnicity", "respondentgender", "highesteducationlevel", "schoolwork", "bluechasgrp"),
                              strata = "atleastyearlyvisit",
                              smd = TRUE)
annextable6 <- print(annextable6, smd = TRUE, nonnormal = TRUE, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
annextable1 <- cbind(annextable4, annextable5, annextable6)
annextable1final <-  as.data.frame(annextable1) 


drop <- c("level.1","level.2")
annextable1final = annextable1final[,!(names(annextable1final) %in% drop)]
write.csv(annextable1final, file = ".csv")
