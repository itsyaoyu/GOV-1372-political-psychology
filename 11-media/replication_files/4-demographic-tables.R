sink("demographic-tables-log.txt",append=F,type="output")

###
#Demographic Tables
###
library(xtable)
library(plyr)

##
#Survey Demos (CCES Data come from CCES Dataverse (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GDF6Z0) and are coded in the following manner)
##
# cces <- read.dta('CCES16_Common_OUTPUT_Feb2018_VV.dta')
# cces$republican <- as.numeric(as.character(mapvalues(x=as.character(cces$pid7),from=c('Strong Democrat','Not very strong Democrat','Lean Democrat','Independent','Lean Republican','Not very strong Republican','Strong Republican','Not sure',NA),to=c(0,0,0,0,1,1,1,0,0))))
# cces$democrat <- as.numeric(as.character(mapvalues(x=as.character(cces$pid7),from=c('Strong Democrat','Not very strong Democrat','Lean Democrat','Independent','Lean Republican','Not very strong Republican','Strong Republican','Not sure',NA),to=c(1,1,1,0,0,0,0,0,0))))
# cces$independent <- as.numeric(as.character(mapvalues(x=as.character(cces$pid7),from=c('Strong Democrat','Not very strong Democrat','Lean Democrat','Independent','Lean Republican','Not very strong Republican','Strong Republican','Not sure',NA),to=c(0,0,0,1,0,0,0,1,0))))
# cces$female <- as.numeric(as.character(mapvalues(x=cces$gender,from=c('Male','Female'),to=c(0,1))))
# cces$white <- as.numeric(as.character(mapvalues(x=cces$race,from=c('White','Black','Hispanic','Asian','Native American','Mixed','Other','Middle Eastern'),to=c(1,0,0,0,0,0,0,0))))
# cces$hispanic <- as.numeric(as.character(mapvalues(x=cces$race,from=c('White','Black','Hispanic','Asian','Native American','Mixed','Other','Middle Eastern'),to=c(0,0,1,0,0,0,0,0))))
# cces$black <- as.numeric(as.character(mapvalues(x=cces$race,from=c('White','Black','Hispanic','Asian','Native American','Mixed','Other','Middle Eastern'),to=c(0,1,0,0,0,0,0,0))))
# cces$other.race <- as.numeric(as.character(mapvalues(x=cces$race,from=c('White','Black','Hispanic','Asian','Native American','Mixed','Other','Middle Eastern'),to=c(0,0,0,1,1,1,1,1))))
# cces$college <- as.numeric(as.character(mapvalues(x=cces$educ,from=c('No HS','High school graduate','Some college','2-year','4-year','Post-grad'),to=c(0,0,0,0,1,1))))
# cces$age <- 2016 - as.numeric(as.character(cces$birthyr))
# cces$income <- as.numeric(as.character(mapvalues(x=cces$faminc,from=c("Less than $10,000","$10,000 - $19,999","$20,000 - $29,999","$30,000 - $39,999","$40,000 - $49,999","$50,000 - $59,999","$60,000 - $69,999","$70,000 - $79,999","$80,000 - $99,999","$100,000 - $119,999","$120,000 - $149,999","$150,000 - $199,999","$200,000 - $249,999","$250,000 - $349,999","$350,000 - $499,999","$500,000 or more","$150,000 or more","Prefer not to say"),to=c(10000,15000,25000,35000,45000,55000,65000,75000,90000,110000,135000,175000,225000,300000,425000,500000,150000,NA))))
# cces$weight <- cces$commonweight_vv
# cces$caseid <- cces$V101
# cces$incentives <- NA
# cces.demos <- cces[,c('caseid','incentives','black','hispanic','white','other.race','college','female','age','income','democrat','republican','independent','weight')]
# cces.out <- apply(X=cces.demos[,2:(dim(cces.demos)[2]-1)],MARGIN=2,FUN=weighted.mean,na.rm=TRUE,w=cces.demos$weight)
# cces.out <- c(cces.out,length(cces.demos$weight))
# cces.out <- cces.out[2:length(cces.out)]
# cces.out <- round(cces.out,digits=2)
# cces.out <- c('CCES',cces.out)
# cces.frame <- as.data.frame(t(cces.out))

#Describe Pooled, Survey 1 and Survey 2 Demographics
load('combined.survey.demos.RData')
demo.frame <- ddply(combined.survey.demos,.(study),summarise,black=weighted.mean(x=black,w=weight,na.rm=TRUE),hispanic=weighted.mean(x=hispanic,w=weight,na.rm=TRUE),white=weighted.mean(x=white,w=weight,na.rm=TRUE),other.race=weighted.mean(x=other.race,w=weight,na.rm=TRUE),college=weighted.mean(x=college,w=weight,na.rm=TRUE),female=weighted.mean(x=female,w=weight,na.rm=TRUE),age=weighted.mean(x=age,w=weight,na.rm=TRUE),income=weighted.mean(x=income,w=weight,na.rm=TRUE),democrat=mean(x=democrat,w=weight,na.rm=TRUE),republican=mean(x=republican,w=weight,na.rm=TRUE),independent=mean(x=independent,w=weight,na.rm=TRUE),sample.size=length(unique(caseid)))
demo.frame[,2:dim(demo.frame)[2]] <- round(demo.frame[,2:dim(demo.frame)[2]],digits=2)
demo.frame <- as.data.frame(t(demo.frame))

###
#Table A1: Sample Demographics
###
print(x=xtable(demo.frame,digits=2,caption="Sample Demographics"),file='tablea1.txt')


#Describe Pooled and Web Traffic Demographics
load('traffic.survey.demos.RData')
web.frame <- ddply(traffic.survey.demos,.(study),summarise,black=weighted.mean(x=black,w=weight,na.rm=TRUE),hispanic=weighted.mean(x=hispanic,w=weight,na.rm=TRUE),white=weighted.mean(x=white,w=weight,na.rm=TRUE),other.race=weighted.mean(x=other.race,w=weight,na.rm=TRUE),college=weighted.mean(x=college,w=weight,na.rm=TRUE),female=weighted.mean(x=female,w=weight,na.rm=TRUE),age=weighted.mean(x=age,w=weight,na.rm=TRUE),income=weighted.mean(x=income,w=weight,na.rm=TRUE),democrat=mean(x=democrat,w=weight,na.rm=TRUE),republican=mean(x=republican,w=weight,na.rm=TRUE),independent=mean(x=independent,w=weight,na.rm=TRUE),sample.size=length(unique(caseid)))
web.frame[,2:dim(web.frame)[2]] <- round(web.frame[,2:dim(web.frame)[2]],digits=2)
web.frame <- as.data.frame(t(web.frame))

###
#Table C1: Comparing Pooled Survey Sample and Web Traffic Sample
###
print(x=xtable(web.frame,digits=2,caption="Comparing Pooled Survey Sample and Web Traffic Sample"),file='tablec1.txt')
sink()