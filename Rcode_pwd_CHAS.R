# define discrete, continuous, all variables
variables_disc <- c("respondentage", "respondentgender", "relationship", "highesteducationlevel", "childage", "childgender", "childethnicity", "childnationality", "monthlyhouseholdincome", "housingtype", "paidcaregiver", "bluechas", "orangechas", "publicassistance", "privatedentalinsurance", "financialassistance", "autismspectrumdisorder", "downsyndrome", "cerebralpalsy", "gdd", "schoolwork", "anydentalvisitpastyr", "preventivedentalvisit", "atleastyearlyvisit")
variables_con <- c("respondentagecoded", "childagecoded", "numberlivinginsamehouse")
variables_all <- c(variables_disc, variables_con)

## CHAS Analysis ##
df <-  subset(df, chaseligibility==1)
df$bluechasgrp <- ifelse(df$bluechas=="Yes", 1, 0)


## appendix table 2 ##
for (var in variables_con){
  print(wilcox.test(df[[var]] ~ bluechasgrp, data=df, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE))
appendix2 <- CreateTableOne(vars = variables_all,
                            data = df,
                            factorVars = variables_disc,
                            strata = "bluechasgrp",
                            smd = TRUE)
appendix2 <- print(appendix2, smd = TRUE, showAllLevels = TRUE, noSpaces = TRUE, printToggle = FALSE)
write.csv(appendix1, ".csv")
}

## appendix table 3 & table 3 ##
# propensity score matching
# matching (nearest neighbour)
mod_match_chas_replace <- matchit(bluechasgrp ~ childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, method = "nearest", data = df, distance = "glm", replace = TRUE)
summary(mod_match_chas_replace, interactions=TRUE, standardize = TRUE)
bal.tab(mod_match_chas_replace, m.threshold = 0.1, un = TRUE)
bal.tab(mod_match_chas_replace, v.threshold = 2)
dta_m_chas_replace <- match.data(mod_match_chas_replace)

# matching (nearest neighbour with caliper)
mod_match_chas <- matchit(bluechasgrp ~ childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, method = "nearest", data = df, distance = "glm", replace = FALSE, ratio = 1)
ps <- mod_match_chas$distance
ps <- as.data.frame(ps)
ps <- transform(ps, logitps = logit(ps))
0.2*sd(ps$logitps) #recommended caliper

mod_match_chas <- matchit(bluechasgrp ~ childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, method = "nearest", data = df, distance = "logit", replace = FALSE, ratio = 1, caliper=0.15)
summary(mod_match_chas, interactions=TRUE, standardize = TRUE)
bal.tab(mod_match_chas, m.threshold = 0.1, un = TRUE)
bal.tab(mod_match_chas, v.threshold = 2)
dta_m_chas <- match.data(mod_match_chas)
dim(dta_m_chas)

# matching (optimal)
mod_match_chas2 <- matchit(bluechasgrp ~ childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, method = "full", estimand="ATT", data = df)
summary(mod_match_chas2, interactions=TRUE, standardize = TRUE)
bal.tab(mod_match_chas2, m.threshold = 0.1, un = TRUE)
bal.tab(mod_match_chas2, v.threshold = 2)

full.model <- glm(d2anydentalvisitpastyrcoded ~ bluechasgrp + a7childagecoded + a8childgender + factor(a9childethnicity) + schoolwork +  a1respondentagecoded + a2respondentgender + factor(a6highesteducationlevelcat) + a4anumberlivinginsamehouse, weights=weights, data = dta_m_chas, family = poisson(link=log))
full.model <- glm(d2bpreventivedentalvisitcoded ~ bluechasgrp + a7childagecoded + a8childgender + factor(a9childethnicity) + schoolwork +  a1respondentagecoded + a2respondentgender + factor(a6highesteducationlevelcat) + a4anumberlivinginsamehouse, weights=weights, data = dta_m_chas, family = poisson(link=log))
full.model <- glm(atleastyearlyvisit ~ bluechasgrp + a7childagecoded + a8childgender + factor(a9childethnicity) + schoolwork +  a1respondentagecoded + a2respondentgender + factor(a6highesteducationlevelcat) + a4anumberlivinginsamehouse, weights=weights, data = dta_m_chas, family = poisson(link=log))
test <- coeftest(full.model, vcov. = vcovHC(full.model, type = 'HC1'))
test
exp(coef(test))
exp(coefci(full.model, vcov. = vcovHC(full.model, type = 'HC1')))

# ipw
weight <- ipwpoint(exposure = bluechasgrp, family = "binomial", link = "logit", numerator= ~1, denominator = ~ childagecoded + childgender + factor(childethnicity) + schoolwork +  respondentagecoded + respondentgender + factor(highesteducationlevel) + numberlivinginsamehouse, data = as.data.frame(df))
df$weight <- weight$ipw.weights
cluster <- svydesign(id= ~1, weights = ~weight, data= df)
result1 <- svyglm(anydentalvisitpastyr ~ bluechasgrp, design=cluster, family=poisson(link=log))
result2 <- svyglm(preventivedentalvisit ~ bluechasgrp, design=cluster, family=poisson(link=log))
result3 <- svyglm(atleastyearlyvisit ~ bluechasgrp, design=cluster, family=poisson(link=log))

test <- coeftest(result, vcov. = vcovHC(result, type = 'HC1'))
test
exp(cbind(PR = coef(test), coefci(result, vcov. = vcovHC(result, type = 'HC1'))))
# replace result with result1, result2, result3