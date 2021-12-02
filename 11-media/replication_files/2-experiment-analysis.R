sink("experiment-analysis-log.txt",append=F,type="output")

library(apsrtable)
library(plyr)
library(xtable)
library(rms)

load('misinfo.combined.RData')

#Cluster Robust Standard Error Function
fit.w.robust <- function(model, cluster.var, dta){	
	robust.se <- function(model, cluster){
		require(sandwich)
		require(lmtest)
		M <- length(unique(cluster))
		N <- length(cluster)
		K <- model$rank
		dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
		uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
		rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
		rcse.se <- coeftest(model, rcse.cov)
		return(list(rcse.cov, rcse.se))
	}
	
	out <- model$model
	clustervar <- mapply(paste,cluster.var,dta[!(1:dim(dta)[1] %in% model$na.action),cluster.var],sep="")
	vcov <- robust.se(model, clustervar)
	model$se <- vcov[[1]]
	model$coeftest <- vcov[[2]]
	return(model)
}

#############
#############
#Baseline Divides in Information
#############
#############

##
#Table 2: Partisan Information Divides in Unincentivized Conditions
##
baseline.answers <- ddply(misinfo.combined,.(topic),summarise,correct.dem=round(mean(correct.answer[which(pid.binary==1 & incentive==0)],na.rm=TRUE),digits=2),correct.rep=round(mean(correct.answer[which(pid.binary==0 & incentive==0)],na.rm=TRUE),digits=2))
baseline.answers$absolute.difference <- abs(baseline.answers$correct.dem - baseline.answers$correct.rep)
baseline.answers <- baseline.answers[order(baseline.answers$absolute.difference,decreasing=TRUE),]
rownames(baseline.answers) <- NULL
print(x=xtable(baseline.answers),file='table2.txt')

#Baseline-With Weighting
baseline.answers.weights.wave1 <- ddply(misinfo.combined[which(misinfo.combined$wave=='Wave1' & !is.na(misinfo.combined$weight.control.only) & !is.na(misinfo.combined$correct.answer)),],.(topic),summarise,correct.dem=round(weighted.mean(x=correct.answer[which(pid.binary==1 & incentive==0)],w=weight.control.only[which(pid.binary==1 & incentive==0)], na.rm=TRUE),digits=2),correct.rep=round(mean(correct.answer[which(pid.binary==0 & incentive==0)],w=weight.control.only[which(pid.binary==0 & incentive==0)],na.rm=TRUE),digits=2))
baseline.answers.weights.wave1$absolute.difference.weights <- abs(baseline.answers.weights.wave1$correct.dem - baseline.answers.weights.wave1$correct.rep)

baseline.answers.weights.wave2 <- ddply(misinfo.combined[which(misinfo.combined$wave=='Wave2' & !is.na(misinfo.combined$weight) & !is.na(misinfo.combined$correct.answer)),],.(topic),summarise,correct.dem=round(weighted.mean(x=correct.answer[which(pid.binary==1 & incentive==0)],w=weight[which(pid.binary==1 & incentive==0)], na.rm=TRUE),digits=2),correct.rep=round(mean(correct.answer[which(pid.binary==0 & incentive==0)],w=weight[which(pid.binary==0 & incentive==0)],na.rm=TRUE),digits=2))
baseline.answers.weights.wave2$absolute.difference.weights <- abs(baseline.answers.weights.wave2$correct.dem - baseline.answers.weights.wave2$correct.rep)
baseline.answers.weights.combined <- rbind.data.frame(baseline.answers.weights.wave1,baseline.answers.weights.wave2)

##
#Table B2: Partisan Differences in Unincentivized Survey Conditions
##
baseline.weights.frame <- merge(baseline.answers.weights.combined,baseline.answers,by=c('topic'))
baseline.weights.frame$correct.dem.x <- NULL
baseline.weights.frame$correct.rep.x <- NULL
baseline.weights.frame$correct.dem.y <- NULL
baseline.weights.frame$correct.rep.y <- NULL
baseline.weights.frame <- baseline.weights.frame[,c('topic','absolute.difference','absolute.difference.weights')]
baseline.weights.frame <- baseline.weights.frame[order(baseline.weights.frame$absolute.difference,decreasing=TRUE),]
rownames(baseline.weights.frame) <- NULL
colnames(baseline.weights.frame) <- c('Topic','Partisan Difference in Control','Partisan Difference in Control (Weighted)')
print(x=xtable(baseline.weights.frame),file='tableb2.txt')

##
#Table B1: Partisan Differences by Item and Incentive Condition
##
difference.table <- ddply(misinfo.combined,.(topic),summarise,correct.dem=round(mean(correct.answer[which(pid.binary==1 & incentive==0)],na.rm=TRUE),digits=2),correct.rep=round(mean(correct.answer[which(pid.binary==0 & incentive==0)],na.rm=TRUE),digits=2),correct.dem.incentives=round(mean(correct.answer[which(pid.binary==1 & incentive==1)],na.rm=TRUE),digits=2),correct.rep.incentives=round(mean(correct.answer[which(pid.binary==0 & incentive==1)],na.rm=TRUE),digits=2))
difference.table$baseline.difference <- difference.table$correct.dem - difference.table$correct.rep
difference.table$incentive.difference <- difference.table$correct.dem.incentives - difference.table$correct.rep.incentives
difference.table$diff.in.diff <- difference.table$baseline.difference - difference.table$incentive.difference
difference.table <- difference.table[,c('topic','correct.dem','correct.rep','baseline.difference','correct.dem.incentives','correct.rep.incentives','incentive.difference','diff.in.diff')]
names(difference.table) <- c('Topic','Dem Correct (No Incentive)','Rep Correct (No Incentive)','Baseline Difference','Dem Correct (Incentive)','Rep Correct (Incentive)','Incentive Difference','Difference in Difference')
print(x=xtable(difference.table),file='tableb1.txt')

##
#Table B4: Correct Answers Among Political Independents
##
baseline.answers.independent <- ddply(misinfo.combined,.(topic),summarise,correct=round(mean(correct.answer[which(is.na(pid.binary))],na.rm=TRUE),digits=2))
rownames(baseline.answers.independent) <- NULL
print(x=xtable(baseline.answers.independent,caption="Correct Answers Among Political Independents"),file='tableb4.txt')

####
#Table B10: Response Certainty by Item
####
baseline.certainty <- ddply(misinfo.combined,.(topic),summarise,certainty=round(mean(certainty,na.rm=TRUE),digits=2))
baseline.certainty <- baseline.certainty[order(baseline.certainty$certainty,decreasing=TRUE),]
rownames(baseline.certainty) <- NULL
print(x=xtable(baseline.certainty,caption="Response Certainty by Item"),file='tableb10.txt')

#############
#############
#Partisan Information Divide Analysis
#############
#############

#Pooled Model (All Partisan Respondents / All Misinformation Items)
pooled.model.divide <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined)
pooled.model.divide <- fit.w.robust(pooled.model.divide,'caseid',misinfo.combined)

#Pooled Model (Strong Partisan Respondents / All Misinformation Items )
pooled.model.divide.strong <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.strength=='strong'),])
pooled.model.divide.strong <- fit.w.robust(pooled.model.divide.strong,'caseid',misinfo.combined[which(misinfo.combined$pid.strength=='strong'),])

#Pooled Model - (All Partisan Respondents / Left-Valenced Misinformation Items)
pooled.model.divide.leftvalence <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])
pooled.model.divide.leftvalence <- fit.w.robust(pooled.model.divide.leftvalence,'caseid',misinfo.combined[which(misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])

#Pooled Model - (All Partisan Respondents / Right-Valenced Misinformation Items)
pooled.model.divide.rightvalence <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])
pooled.model.divide.rightvalence <- fit.w.robust(pooled.model.divide.rightvalence,'caseid',misinfo.combined[which(misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])

###
#Table 3: Effect of Incentives on Partisan Information Divide
###
cat(x=apsrtable(pooled.model.divide,pooled.model.divide.strong,pooled.model.divide.leftvalence,pooled.model.divide.rightvalence,omitcoef=expression(grep(pattern="topic",coefnames)),coef.names=c('(Intercept)','Democrat','Incentive Treatment','Democrat*Incentive Treatment'),model.names=c('All Partisans','Strong Partisans','Dem-Valenced','Rep-Valenced'),caption='Effect of Incentives on Partisan Information Divide'),file='table3.txt')

###Marginal Effect That Remains after this
remaining.divide <- pooled.model.divide$coefficients[2] + pooled.model.divide$coefficients[13]
remaining.divide.se <- sqrt(pooled.model.divide$se[2,2] + pooled.model.divide$se[13,13] + 2*pooled.model.divide$se[13,2])
remaining.divide; remaining.divide - 2*remaining.divide.se; remaining.divide + 2*remaining.divide.se

#Estimating Effects Among Certain Respondents
pooled.model.certain <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==1),])
pooled.model.certain <- fit.w.robust(pooled.model.certain,'caseid',misinfo.combined[which(misinfo.combined$certainty==1),])

pooled.model.certain.strong <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==1 & misinfo.combined$pid.strength=='strong'),])
pooled.model.certain.strong <- fit.w.robust(pooled.model.certain.strong,'caseid',misinfo.combined[which(misinfo.combined$certainty==1 & misinfo.combined$pid.strength=='strong'),])

pooled.model.certain.leftvalence <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==1 & misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])
pooled.model.certain.leftvalence <- fit.w.robust(pooled.model.certain.leftvalence,'caseid',misinfo.combined[which(misinfo.combined$certainty==1 & misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])

pooled.model.certain.rightvalence <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==1 & misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])
pooled.model.certain.rightvalence <- fit.w.robust(pooled.model.certain.rightvalence,'caseid',misinfo.combined[which(misinfo.combined$certainty==1 & misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])

###
#Table 4: Effect of Incentives on Partisan Information Divide (Certain Respondents)
###
cat(x=apsrtable(pooled.model.certain,pooled.model.certain.strong,pooled.model.certain.leftvalence,pooled.model.certain.rightvalence,omitcoef=expression(grep(pattern="topic",coefnames)),coef.names=c('(Intercept)','Democrat','Incentive Treatment','Democrat*Incentive Treatment'),model.names=c('All Partisans','Strong Partisans','Dem-Valenced','Rep-Valenced'),caption='Effect of Incentives on Partisan Information Divide (Certain Respondents)'),file='table4.txt')

###Marginal Effect That Remains after this
remaining.divide <- pooled.model.certain$coefficients[2] + pooled.model.certain$coefficients[13]
remaining.divide.se <- sqrt(pooled.model.certain$se[2,2] + pooled.model.certain$se[13,13] + 2*pooled.model.certain$se[13,2])
remaining.divide; remaining.divide - 2*remaining.divide.se; remaining.divide + 2*remaining.divide.se

#Estimating Effects Among Uncertain Respondents
pooled.model.uncertain <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==0),])
pooled.model.uncertain <- fit.w.robust(pooled.model.uncertain,'caseid',misinfo.combined[which(misinfo.combined$certainty==0),])

pooled.model.uncertain.strong <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==0 & misinfo.combined$pid.strength=='strong'),])
pooled.model.uncertain.strong <- fit.w.robust(pooled.model.uncertain.strong,'caseid',misinfo.combined[which(misinfo.combined$certainty==0 & misinfo.combined$pid.strength=='strong'),])

pooled.model.uncertain.leftvalence <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==0 & misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])
pooled.model.uncertain.leftvalence <- fit.w.robust(pooled.model.uncertain.leftvalence,'caseid',misinfo.combined[which(misinfo.combined$certainty==0 & misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])

pooled.model.uncertain.rightvalence <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$certainty==0 & misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])
pooled.model.uncertain.rightvalence <- fit.w.robust(pooled.model.uncertain.rightvalence,'caseid',misinfo.combined[which(misinfo.combined$certainty==0 & misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])

###
#Table B12: Effect of Incentives on Partisan Information Divide (Uncertain Respondents)
###
cat(x=apsrtable(pooled.model.uncertain,pooled.model.uncertain.strong,pooled.model.uncertain.leftvalence,pooled.model.uncertain.rightvalence,omitcoef=expression(grep(pattern="topic",coefnames)),coef.names=c('(Intercept)','Democrat','Incentive Treatment','Democrat*Incentive Treatment'),model.names=c('All Partisans','Strong Partisans','Dem-Valenced','Rep-Valenced'),caption='Effect of Incentives on Partisan Information Divide (Uncertain Respondents)'),file='tableb12.txt')

###Marginal Effect That Remains after this
remaining.divide <- pooled.model.uncertain$coefficients[2] + pooled.model.uncertain$coefficients[13]
remaining.divide.se <- sqrt(pooled.model.uncertain$se[2,2] + pooled.model.uncertain$se[13,13] + 2*pooled.model.uncertain$se[13,2])
remaining.divide; remaining.divide - 2*remaining.divide.se; remaining.divide + 2*remaining.divide.se


###
#Table B11: Effect of Incentives on Response Certainty
###
misinfo.combined$certainty[is.na(misinfo.combined$certainty)] <- 0
incentives.certainty <- lm(certainty ~ incentive + pid.binary + factor(topic),data=misinfo.combined[which(!is.na(misinfo.combined$divide.coding)),])
incentives.certainty <- fit.w.robust(incentives.certainty,'caseid',misinfo.combined[which(!is.na(misinfo.combined$divide.coding)),])

cat(x=apsrtable(incentives.certainty,model.names="All Partisans",caption="Effect of Incentives on Response Certainty",omitcoef=expression(grep(pattern="topic|pid",coefnames)),coef.names=c("(Intercept)","Incentive Treatment")),file='tableb11.txt')


###
#Misinformation - Strong Belief in Misinfo Items
###
#Pooled Model (All Partisan Respondents / All Misinformation Items)
pooled.model.divide.certain <- lm(divide.coding.certain ~ pid.binary*incentive + factor(topic),data=misinfo.combined)
pooled.model.divide.certain <- fit.w.robust(pooled.model.divide.certain,'caseid',misinfo.combined)

#Pooled Model (Strong Partisan Respondents / All Misinformation Items )
pooled.model.divide.certain.strong <- lm(divide.coding.certain ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.strength=='strong'),])
pooled.model.divide.certain.strong <- fit.w.robust(pooled.model.divide.certain.strong,'caseid',misinfo.combined[which(misinfo.combined$pid.strength=='strong'),])

#Pooled Model - (All Partisan Respondents / Left-Valenced Misinformation Items)
pooled.model.divide.certain.leftvalence <- lm(divide.coding.certain ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])
pooled.model.divide.certain.leftvalence <- fit.w.robust(pooled.model.divide.certain.leftvalence,'caseid',misinfo.combined[which(misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])

#Pooled Model - (All Partisan Respondents / Right-Valenced Misinformation Items)
pooled.model.divide.certain.rightvalence <- lm(divide.coding.certain ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])
pooled.model.divide.certain.rightvalence <- fit.w.robust(pooled.model.divide.certain.rightvalence,'caseid',misinfo.combined[which(misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])


##
#Table B13: Effect of Incentives on Partisan Information Divide (Isolating Confident and Misinformed Responses)
##
cat(x=apsrtable(pooled.model.divide.certain,pooled.model.divide.certain.strong,pooled.model.divide.certain.leftvalence,pooled.model.divide.certain.rightvalence,omitcoef=expression(grep(pattern="topic",coefnames)),coef.names=c('(Intercept)','Democrat','Incentive Treatment','Democrat*Incentive Treatment'),model.names=c('All Partisans','Strong Partisans','Dem-Valenced','Rep-Valenced'),caption='Effect of Incentives on Partisan Information Divide (Isolating Confident and Misinformed Responses)'),file='tableb13.txt')

#Marginal Effect That Remains after this
remaining.divide.certain <- pooled.model.divide.certain$coefficients[2] + pooled.model.divide.certain$coefficients[13]
remaining.divide.certain.se <- sqrt(pooled.model.divide.certain$se[2,2] + pooled.model.divide.certain$se[13,13] + 2*pooled.model.divide.certain$se[13,2])
remaining.divide.certain; remaining.divide.certain - 2*remaining.divide.certain.se; remaining.divide.certain + 2*remaining.divide.certain.se

###
#Misinformation - Separated by Wave
###

#Pooled Model Wave 1 Only
pooled.model.divide.wave1only <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$wave=='Wave1'),],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave1')])
pooled.model.divide.wave1only <- fit.w.robust(pooled.model.divide.wave1only,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave1'),])

#Pooled Model Wave 2 Only
pooled.model.divide.wave2only <- lm(divide.coding ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$wave=='Wave2'),],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave2')])
pooled.model.divide.wave2only <- fit.w.robust(pooled.model.divide.wave2only,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave2'),])

###
#Table B3: Effect of Incentives on Partisan Divides over Factual Evidence (By Survey)
###
cat(x=apsrtable(pooled.model.divide.wave1only,pooled.model.divide.wave2only,omitcoef=expression(grep(pattern="topic",coefnames)),coef.names=c('(Intercept)','Democrat','Incentive Treatment','Democrat*Incentive Treatment'),model.names=c('Survey 1','Survey 2'),caption='Effect of Incentives on Partisan Divides over Factual Evidence (By Survey)'),file='tableb3.txt')

###
#Misinformation - Effects Among Independents
###
pooled.model.correct.independents <- lm(correct.answer ~ incentive + factor(topic),data=misinfo.combined[which(is.na(misinfo.combined$pid.binary)),])
pooled.model.correct.independents <- fit.w.robust(pooled.model.correct.independents,'caseid',misinfo.combined[which(is.na(misinfo.combined$pid.binary)),])

pooled.model.correct.democrats <- lm(correct.answer ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==1),])
pooled.model.correct.democrats <- fit.w.robust(pooled.model.correct.democrats,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==1),])

pooled.model.correct.republicans <- lm(correct.answer ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==0),])
pooled.model.correct.republicans <- fit.w.robust(pooled.model.correct.republicans,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==0),])

###
#Table B5: Effect of Incentives on Correct Answers
###
cat(x=apsrtable(pooled.model.correct.democrats,pooled.model.correct.independents,pooled.model.correct.republicans,omitcoef=expression(grep(pattern="topic",coefnames)),coef.names=c('(Intercept)','Incentive Treatment'),model.names=c('Democrats','Independents','Republicans'),caption='Effect of Incentives on Correct Answers'),file='tableb5.txt')

###
#Misinformation - Outcome Scale Combining Direction/Strength
###

#Pooled Model (All Partisan Respondents / All Misinformation Items)
pooled.model.divide.wstrength <- lm(divide.coding.wstrength ~ pid.binary*incentive + factor(topic),data=misinfo.combined)
pooled.model.divide.wstrength <- fit.w.robust(pooled.model.divide.wstrength,'caseid',misinfo.combined)

#Pooled Model (Strong Partisan Respondents / All Misinformation Items )
pooled.model.divide.wstrength.strong <- lm(divide.coding.wstrength ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.strength=='strong'),])
pooled.model.divide.wstrength.strong <- fit.w.robust(pooled.model.divide.wstrength.strong,'caseid',misinfo.combined[which(misinfo.combined$pid.strength=='strong'),])

#Pooled Model - (All Partisan Respondents / Left-Valenced Misinformation Items)
pooled.model.divide.wstrength.leftvalence <- lm(divide.coding.wstrength ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])
pooled.model.divide.wstrength.leftvalence <- fit.w.robust(pooled.model.divide.wstrength.leftvalence,'caseid',misinfo.combined[which(misinfo.combined$topic %in% c('unemployment-survey.1','unemployment-survey.2','firearm.sales-survey.2')),])

#Pooled Model - (All Partisan Respondents / Right-Valenced Misinformation Items)
pooled.model.divide.wstrength.rightvalence <- lm(divide.coding.wstrength ~ pid.binary*incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])
pooled.model.divide.wstrength.rightvalence <- fit.w.robust(pooled.model.divide.wstrength.rightvalence,'caseid',misinfo.combined[which(misinfo.combined$topic %in% c('immigrant.crime-survey.1','voter.fraud-survey.1','climate.change-survey.1','obama.wiretap-survey.1','cohen.plea-survey.2','voter.fraud-survey.2','obama.wiretap-survey.2')),])

###
#Table B14: Effect of Incentives on Partisan Information Divide (10-pt DV Incorporating Certainty)
###
cat(x=apsrtable(pooled.model.divide.wstrength,pooled.model.divide.wstrength.strong,pooled.model.divide.wstrength.leftvalence,pooled.model.divide.wstrength.rightvalence,omitcoef=expression(grep(pattern="topic",coefnames)),coef.names=c('(Intercept)','Democrat','Incentive Treatment','Democrat*Incentive Treatment'),model.names=c('All Partisans','Strong Partisans','Dem-Valenced','Rep-Valenced'),caption='Effect of Incentives on Partisan Information Divide (10-pt DV Incorporating Certainty)'),file='tableb14.txt')

remaining.divide.wstrength <- pooled.model.divide.wstrength$coefficients[2] + pooled.model.divide.wstrength$coefficients[13]
remaining.divide.wstrength.se <- sqrt(pooled.model.divide.wstrength$se[2,2] + pooled.model.divide.wstrength$se[13,13] + 2*pooled.model.divide.wstrength$se[13,2])
remaining.divide.wstrength; remaining.divide.wstrength - 2*remaining.divide.wstrength.se; remaining.divide.wstrength + 2*remaining.divide.wstrength.se

#############
#############
#News Choice Analysis
#############
#############

#Baseline Choices in Control Group
mean(misinfo.combined$copartisan.news.choice[which(misinfo.combined$incentive==0)],na.rm=TRUE) #Probabilty of selecting co-partisan news source
mean(misinfo.combined$mainstream.source[which(misinfo.combined$incentive==0)],na.rm=TRUE) #Probabilty of selecting mainstream news source
mean(misinfo.combined$expert.source[which(misinfo.combined$incentive==0)],na.rm=TRUE) #Probabilty of selecting expert news source
mean(misinfo.combined$outpartisan.news.choice[which(misinfo.combined$incentive==0)],na.rm=TRUE) #Probabilty of selecting out-partisan news source

pooled.model.choice.copartisan <- lm( copartisan.news.choice ~ incentive + factor(topic) + pid.binary,data=misinfo.combined)
pooled.model.choice.copartisan <- fit.w.robust(pooled.model.choice.copartisan,'caseid',misinfo.combined)

pooled.model.choice.outpartisan <- lm( outpartisan.news.choice ~ incentive + factor(topic) + pid.binary,data=misinfo.combined)
pooled.model.choice.outpartisan <- fit.w.robust(pooled.model.choice.outpartisan,'caseid',misinfo.combined)

pooled.model.choice.mainstream <- lm( mainstream.source ~ incentive + factor(topic) + pid.binary,data=misinfo.combined)
pooled.model.choice.mainstream <- fit.w.robust(pooled.model.choice.mainstream,'caseid',misinfo.combined)

pooled.model.choice.expert <- lm( expert.source ~ incentive + factor(topic) + pid.binary,data=misinfo.combined)
pooled.model.choice.expert <- fit.w.robust(pooled.model.choice.expert,'caseid',misinfo.combined)

###
#Table 5: Effect of Incentives on Information Source Selection
###
cat(x=apsrtable(pooled.model.choice.copartisan, pooled.model.choice.expert,pooled.model.choice.mainstream, pooled.model.choice.outpartisan,caption='Effect of Incentives on Information Source Selection',coef.names=c("(Intercept)","Incentive Treatment"),omitcoef=expression(grep(pattern="topic|pid",coefnames)), model.names=c('Co-Partisan News','Expert','Mainstream News','Out-Partisan News'),notes=list('Robust standard errors, clustered by Respondent, in parentheses','Models include item fixed effects and condition on respondent partisanship')),file='table5.txt')

copartisan.effect <- pooled.model.choice.copartisan$coefficients[2] 
copartisan.effect.se <- sqrt(pooled.model.choice.copartisan$se[2,2])
copartisan.effect; copartisan.effect - 2*copartisan.effect.se; copartisan.effect + 2*copartisan.effect.se

#############
#############
#Information Source Selection By Sub-Group
#############
#############

#Democrats Only
pooled.model.choice.copartisan.dem <- lm( copartisan.news.choice ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==1), ])
pooled.model.choice.copartisan.dem <- fit.w.robust(pooled.model.choice.copartisan.dem,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==1), ])

pooled.model.choice.mainstream.dem <- lm( mainstream.source ~ incentive + factor(topic), data=misinfo.combined[which(misinfo.combined$pid.binary==1), ])
pooled.model.choice.mainstream.dem <- fit.w.robust(pooled.model.choice.mainstream.dem,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==1), ])

pooled.model.choice.expert.dem <- lm( expert.source ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==1), ])
pooled.model.choice.expert.dem <- fit.w.robust(pooled.model.choice.expert.dem,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==1), ])

pooled.model.choice.outpartisan.dem <- lm( outpartisan.news.choice ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==1), ])
pooled.model.choice.outpartisan.dem <- fit.w.robust(pooled.model.choice.outpartisan.dem,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==1), ])

###
#Table B6: Effect of Incentives on Information Source Selection (Democrats Only)
###
cat(x=apsrtable(pooled.model.choice.copartisan.dem, pooled.model.choice.expert.dem, pooled.model.choice.mainstream.dem, pooled.model.choice.outpartisan.dem,caption='Effect of Incentives on Information Source Selection (Democrats Only)',coef.names=c("(Intercept)","Incentive Treatment"),omitcoef=expression(grep(pattern="topic",coefnames)), model.names=c('Co-Partisan News','Expert','Mainstream News','Out-Partisan News'),notes=list('Robust standard errors, clustered by Respondent, in parentheses')),file='tableb6.txt')

#Republicans Only
pooled.model.choice.copartisan.rep <- lm( copartisan.news.choice ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==0), ])
pooled.model.choice.copartisan.rep <- fit.w.robust(pooled.model.choice.copartisan.rep,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==0), ])

pooled.model.choice.mainstream.rep <- lm( mainstream.source ~ incentive + factor(topic), data=misinfo.combined[which(misinfo.combined$pid.binary==0), ])
pooled.model.choice.mainstream.rep <- fit.w.robust(pooled.model.choice.mainstream.rep,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==0), ])

pooled.model.choice.expert.rep <- lm( expert.source ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==0), ])
pooled.model.choice.expert.rep <- fit.w.robust(pooled.model.choice.expert.rep,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==0), ])

pooled.model.choice.outpartisan.rep <- lm( outpartisan.news.choice ~ incentive + factor(topic),data=misinfo.combined[which(misinfo.combined$pid.binary==0), ])
pooled.model.choice.outpartisan.rep <- fit.w.robust(pooled.model.choice.outpartisan.rep,'caseid',misinfo.combined[which(misinfo.combined$pid.binary==0), ])

###
#Table B7: Effect of Incentives on Information Source Selection (Republican Only)
###
cat(x=apsrtable(pooled.model.choice.copartisan.rep, pooled.model.choice.expert.rep, pooled.model.choice.mainstream.rep, pooled.model.choice.outpartisan.rep,caption='Effect of Incentives on Information Source Selection (Republicans Only)',coef.names=c("(Intercept)","Incentive Treatment"),omitcoef=expression(grep(pattern="topic",coefnames)), model.names=c('Co-Partisan News','Expert','Mainstream News','Out-Partisan News'),notes=list('Robust standard errors, clustered by Respondent, in parentheses')),file='tableb7.txt')

#By Individual Survey (Survey 1)
pooled.model.choice.copartisan.wave1 <- lm( copartisan.news.choice ~ incentive + factor(topic) + pid.binary,data=misinfo.combined[which(misinfo.combined$wave=='Wave1'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave1')])
pooled.model.choice.copartisan.wave1 <- fit.w.robust(pooled.model.choice.copartisan.wave1,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave1'), ])

pooled.model.choice.mainstream.wave1 <- lm( mainstream.source ~ incentive + factor(topic) + pid.binary, data=misinfo.combined[which(misinfo.combined$wave=='Wave1'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave1')])
pooled.model.choice.mainstream.wave1 <- fit.w.robust(pooled.model.choice.mainstream.wave1,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave1'), ])

pooled.model.choice.expert.wave1 <- lm( expert.source ~ incentive + factor(topic) + pid.binary,data=misinfo.combined[which(misinfo.combined$wave=='Wave1'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave1')])
pooled.model.choice.expert.wave1 <- fit.w.robust(pooled.model.choice.expert.wave1,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave1'), ])

pooled.model.choice.outpartisan.wave1 <- lm( outpartisan.news.choice ~ incentive + factor(topic) + pid.binary,data=misinfo.combined[which(misinfo.combined$wave=='Wave1'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave1')])
pooled.model.choice.outpartisan.wave1 <- fit.w.robust(pooled.model.choice.outpartisan.wave1,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave1'), ])

###
#Table B8: Effect of Incentives on Information Source Selection (Survey 1 Only)
###
cat(x=apsrtable(pooled.model.choice.copartisan.wave1, pooled.model.choice.expert.wave1, pooled.model.choice.mainstream.wave1, pooled.model.choice.outpartisan.wave1,caption='Effect of Incentives on Information Source Selection (Survey 1 Only)',coef.names=c("(Intercept)","Incentive Treatment"),omitcoef=expression(grep(pattern="topic|pid",coefnames)), model.names=c('Co-Partisan News','Expert','Mainstream News','Out-Partisan News'),notes=list('Robust standard errors, clustered by Respondent, in parentheses')),file='tableb8.txt')

#By Individual Survey (Survey 2)
pooled.model.choice.copartisan.wave2 <- lm( copartisan.news.choice ~ incentive + factor(topic) + pid.binary,data=misinfo.combined[which(misinfo.combined$wave=='Wave2'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave2')])
pooled.model.choice.copartisan.wave2 <- fit.w.robust(pooled.model.choice.copartisan.wave2,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave2'), ])

pooled.model.choice.mainstream.wave2 <- lm( mainstream.source ~ incentive + factor(topic) + pid.binary, data=misinfo.combined[which(misinfo.combined$wave=='Wave2'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave2')])
pooled.model.choice.mainstream.wave2 <- fit.w.robust(pooled.model.choice.mainstream.wave2,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave2'), ])

pooled.model.choice.expert.wave2 <- lm( expert.source ~ incentive + factor(topic) + pid.binary,data=misinfo.combined[which(misinfo.combined$wave=='Wave2'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave2')])
pooled.model.choice.expert.wave2 <- fit.w.robust(pooled.model.choice.expert.wave2,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave2'), ])

pooled.model.choice.outpartisan.wave2 <- lm( outpartisan.news.choice ~ incentive + factor(topic) + pid.binary,data=misinfo.combined[which(misinfo.combined$wave=='Wave2'), ],weights=misinfo.combined$weight[which(misinfo.combined$wave=='Wave2')])
pooled.model.choice.outpartisan.wave2 <- fit.w.robust(pooled.model.choice.outpartisan.wave2,'caseid',misinfo.combined[which(misinfo.combined$wave=='Wave2'), ])

###
#Table B9: Effect of Incentives on Information Source Selection (Survey 2 Only)
###
cat(x=apsrtable(pooled.model.choice.copartisan.wave2, pooled.model.choice.expert.wave2, pooled.model.choice.mainstream.wave2, pooled.model.choice.outpartisan.wave2,caption='Effect of Incentives on Information Source Selection (Survey 2 Only)',coef.names=c("(Intercept)","Incentive Treatment"),omitcoef=expression(grep(pattern="topic|pid",coefnames)), model.names=c('Co-Partisan News','Expert','Mainstream News','Out-Partisan News'),notes=list('Robust standard errors, clustered by Respondent, in parentheses')),file='tableb9.txt')

sink()