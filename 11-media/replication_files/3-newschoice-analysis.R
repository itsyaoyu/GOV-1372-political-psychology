sink("newschoice-analysis-log.txt",append=F,type="output")

###
#News Choice Analysis
###
library(apsrtable)
library(xtable)
load('news.choice.RData')

#Partisan Divides on Survey-Based and Behavioral News Use Measures
out.survey.divide <- lm(visit.alignment ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment)),])

in.survey.divide.incentive.1 <- lm(copartisan.source.scale ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==1),])
in.survey.divide.noincentive.1 <- lm(copartisan.source.scale ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==0),])

in.survey.divide.incentive.2 <- lm(copartisan.source.scale.2 ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==1),])
in.survey.divide.noincentive.2 <- lm(copartisan.source.scale.2 ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==0),])

sd.divide.incentive.1 <- in.survey.divide.incentive.1$coefficients[2]/sd(news.choice$copartisan.source.scale[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==1)],na.rm=TRUE)
sd.divide.noincentive.1 <- in.survey.divide.noincentive.1$coefficients[2]/sd(news.choice$copartisan.source.scale[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==0)],na.rm=TRUE)

sd.divide.incentive.2 <- in.survey.divide.incentive.2$coefficients[2]/sd(news.choice$copartisan.source.scale.2[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==1)],na.rm=TRUE)
sd.divide.noincentive.2 <- in.survey.divide.noincentive.2$coefficients[2]/sd(news.choice$copartisan.source.scale.2[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==0)],na.rm=TRUE)

sd.divide.incentive.pooled <- (sd.divide.incentive.2 + sd.divide.incentive.1)/2
sd.divide.noincentive.pooled <- (sd.divide.noincentive.2 + sd.divide.noincentive.1)/2
sd.divide.behavior <- out.survey.divide$coefficients[2]/sd(news.choice$visit.alignment,na.rm=TRUE)

###
#Table 6: Partisan Divides in Media Choice By Setting
###
setting <- c("Survey + No Incentives","Survey + Incentives","Real-World Media Diet")
partisan.divide <- round(c(sd.divide.noincentive.pooled, sd.divide.incentive.pooled, sd.divide.behavior),digits=2)
print(x=xtable(cbind.data.frame(setting,partisan.divide),caption="Partisan Divides in Media Choice By Setting"),file='table6.txt')

#Appendix Comparison - Alternative 1 - Coding Mainstream News as Partisan Sources
in.survey.divide.incentive.1.alt <- lm(copartisan.source.scale.alt ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==1),])
in.survey.divide.noincentive.1.alt <- lm(copartisan.source.scale.alt ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==0),])

in.survey.divide.incentive.2.alt <- lm(copartisan.source.scale.2.alt ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==1),])
in.survey.divide.noincentive.2.alt <- lm(copartisan.source.scale.2.alt ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==0),])

sd.divide.incentive.1.alt <- in.survey.divide.incentive.1.alt$coefficients[2]/sd(news.choice$copartisan.source.scale.alt[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==1)],na.rm=TRUE)
sd.divide.noincentive.1.alt <- in.survey.divide.noincentive.1.alt$coefficients[2]/sd(news.choice$copartisan.source.scale.alt[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==0)],na.rm=TRUE)

sd.divide.incentive.2.alt <- in.survey.divide.incentive.2.alt$coefficients[2]/sd(news.choice$copartisan.source.scale.2.alt[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==1)],na.rm=TRUE)
sd.divide.noincentive.2.alt <- in.survey.divide.noincentive.2.alt$coefficients[2]/sd(news.choice$copartisan.source.scale.2.alt[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==0)],na.rm=TRUE)

sd.divide.incentive.pooled.alt <- (sd.divide.incentive.2.alt + sd.divide.incentive.1.alt)/2
sd.divide.noincentive.pooled.alt <- (sd.divide.noincentive.2.alt + sd.divide.noincentive.1.alt)/2

#Appendix Comparison - Alternative 2 - Coding News Sources in Survey Based On Their Alignment Score

# Note: Relevant Alignment Scores (From Bakshy et al. 2015) used to label source partisanship in "copartisan.source.scale.alignment" and "copartisan.source.scale.2.alignment"
# MSNBC: -0.8102
# HuffPo: -0.6176
# NY Times: -0.5469
# CNN: -0.2705
# Fox News: 0.7754
# Breitbart: 0.9136
# All Expert Sources receive a 0

in.survey.divide.incentive.1.alignment <- lm(copartisan.source.scale.alignment ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==1),])
in.survey.divide.noincentive.1.alignment <- lm(copartisan.source.scale.alignment ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==0),])

in.survey.divide.incentive.2.alignment <- lm(copartisan.source.scale.2.alignment ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==1),])
in.survey.divide.noincentive.2.alignment <- lm(copartisan.source.scale.2.alignment ~ rep.partisanship,data=news.choice[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==0),])

sd.divide.incentive.1.alignment <- in.survey.divide.incentive.1.alignment$coefficients[2]/sd(news.choice$copartisan.source.scale.alignment[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==1)],na.rm=TRUE)
sd.divide.noincentive.1.alignment <- in.survey.divide.noincentive.1.alignment$coefficients[2]/sd(news.choice$copartisan.source.scale.alignment[which(!is.na(news.choice$visit.alignment) & news.choice$incentive==0)],na.rm=TRUE)

sd.divide.incentive.2.alignment <- in.survey.divide.incentive.2.alignment$coefficients[2]/sd(news.choice$copartisan.source.scale.2.alignment[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==1)],na.rm=TRUE)
sd.divide.noincentive.2.alignment <- in.survey.divide.noincentive.2.alignment$coefficients[2]/sd(news.choice$copartisan.source.scale.2.alignment[which(!is.na(news.choice$visit.alignment) & news.choice$incentive.2==0)],na.rm=TRUE)

sd.divide.incentive.pooled.alignment <- (sd.divide.incentive.2.alignment + sd.divide.incentive.1.alignment)/2
sd.divide.noincentive.pooled.alignment <- (sd.divide.noincentive.2.alignment + sd.divide.noincentive.1.alignment)/2

###
#Table C2: Partisan Divides in Media Choice by Setting (Alternative DV Codings)
###
partisan.divide.alt1 <- round(c(sd.divide.noincentive.pooled.alt, sd.divide.incentive.pooled.alt, sd.divide.behavior),digits=2)
partisan.divide.alt2 <- round(c(sd.divide.noincentive.pooled.alignment, sd.divide.incentive.pooled.alignment, sd.divide.behavior),digits=2)
print(x=xtable(cbind.data.frame(setting,partisan.divide.alt1,partisan.divide.alt2),caption="Partisan Divides in Media Choice By Setting (Alternative DV Codings)"),file='tablec2.txt')

#Appendix Comparison - Regressing Survey News Choice on Real-World Data
useregression.w.pid <- lm(copartisan.source.scale ~ visit.alignment + I(pid.scale==4) + I(pid.scale > 4),data=news.choice)
useregression.w.pid.alt <- lm(copartisan.source.scale.alt ~ visit.alignment + I(pid.scale==4) + I(pid.scale > 4),data=news.choice)
useregression.w.pid.alignment <- lm(copartisan.source.scale.alignment ~ visit.alignment + I(pid.scale==4) + I(pid.scale > 4),data=news.choice)

###
#Table C3: Explaining Partisanship of Survey-Based News Choice
###
cat(apsrtable(useregression.w.pid,useregression.w.pid.alt,useregression.w.pid.alignment,coef.names=c("(Intercept)","Partisanship of Online News Use","Independent","Republican"),model.names=c("In-Text DV","Alt 1","Alt 2"),caption=c("Explaining Partisanship of Survey-Based News Choice")),file='tablec3.txt')

###
#Relating Survey and Real-World Media Choice
###
news.choice.round1 <- news.choice[,c('copartisan.source.scale','copartisan.source.scale.alt','copartisan.source.scale.alignment','incentive','visit.alignment')]
names(news.choice.round1) <- c('copartisan.source.scale','copartisan.source.scale.alt','copartisan.source.scale.alignment','incentive','visit.alignment')

news.choice.round2 <- news.choice[,c('copartisan.source.scale.2','copartisan.source.scale.2.alt','copartisan.source.scale.2.alignment','incentive.2','visit.alignment')]
names(news.choice.round2) <- c('copartisan.source.scale','copartisan.source.scale.alt','copartisan.source.scale.alignment','incentive','visit.alignment')

survey.pooled <- rbind(news.choice.round1,news.choice.round2)

####
#Figure 1: Partisan Slant of Survey and Real-World News Choice
###
combined.choice <- loess.smooth(y=survey.pooled$copartisan.source.scale,x=survey.pooled$visit.alignment)

pdf(file='figure1.pdf',height=4.5,width=6)
par(mar=c(4.1,4.5,1.2,1.2))
plot(x=combined.choice$x,y=combined.choice$y,xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),xlab='Partisanship of Media Diet (Behavior)',ylab='Partisanship of Media Diet (Survey)',type='n',cex.axis=1,cex.lab=1)
points(y=jitter(survey.pooled$copartisan.source.scale,factor=1.2),x=survey.pooled$visit.alignment,pch=16,col='gray70',cex=.8)
lines(x=combined.choice$x,y=combined.choice$y,lwd=6)
rug(x=survey.pooled$visit.alignment,cex=1)
abline(a=0,b=1,lty=2,lwd=2)
dev.off()

combined.choice.alt <- loess.smooth(y=survey.pooled$copartisan.source.scale.alt,x=survey.pooled$visit.alignment)
combined.choice.alignment <- loess.smooth(y=survey.pooled$copartisan.source.scale.alignment,x=survey.pooled$visit.alignment)

####
#Figure C1: Partisan Slant of Survey and Real-World News Choice (Alternative Outcomes)
###
pdf(file='figurec1.pdf',height=5.5,width=11)
par(mfrow=c(1,2),mar=c(4.1,4,1.2,.5))

plot(x=combined.choice.alt$x,y=combined.choice.alt$y,xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),xlab='Partisanship of Media Diet (Behavior)',ylab='Partisanship of Media Diet (Survey)',lwd=6,type='n',cex.axis=1,,cex.lab=1,main="Alternative 1")
#lines(x=no.incentive$x,y=no.incentive$y,col='gray60',lwd=8)
points(y=jitter(survey.pooled$copartisan.source.scale.alt,factor=1.2),x=survey.pooled$visit.alignment,pch=16,col='gray70',cex=.8)
lines(x=combined.choice.alt$x,y=combined.choice.alt$y,lwd=6)
rug(x=survey.pooled$visit.alignment,cex=1)
#text(label='r=0.57',x=0.6,y=-0.25,cex=2)
abline(a=0,b=1,lty=2,lwd=2)

plot(x=combined.choice.alignment$x,y=combined.choice.alignment$y,xlim=c(-1.1,1.1),ylim=c(-1.1,1.1),xlab='Partisanship of Media Diet (Behavior)',ylab='Partisanship of Media Diet (Survey)',lwd=6,type='n',cex.axis=1,,cex.lab=1,main="Alternative 2")
points(y=jitter(survey.pooled$copartisan.source.scale.alignment,factor=1),x=survey.pooled$visit.alignment,pch=16,col='gray70',cex=.8)
lines(x=combined.choice.alignment$x,y=combined.choice.alignment$y,lwd=6)
rug(x=survey.pooled$visit.alignment,cex=1)
abline(a=0,b=1,lty=2,lwd=2)
dev.off()

#Correlation Between Survey-Behavior (Respondents Available in Both Surveys)
cor(x=news.choice$overall.scale,y=news.choice$visit.alignment,use='complete.obs')
cor(x=news.choice$overall.scale.alt,y=news.choice$visit.alignment,use='complete.obs')
cor(x=news.choice$overall.scale.alignment,y=news.choice$visit.alignment,use='complete.obs')

#Correlation Between Survey-Behavior (Survey 1 Only)
cor(x=news.choice$copartisan.source.scale,y=news.choice$visit.alignment,use='complete.obs')
cor(x=news.choice$copartisan.source.scale.alt,y=news.choice$visit.alignment,use='complete.obs')
cor(x=news.choice$copartisan.source.scale.alignment,y=news.choice$visit.alignment,use='complete.obs')

#Correlation Between Survey-Behavior (Survey 2 Only)
cor(x=news.choice$copartisan.source.scale.2,y=news.choice$visit.alignment,use='complete.obs')
cor(x=news.choice$copartisan.source.scale.2.alt,y=news.choice$visit.alignment,use='complete.obs')
cor(x=news.choice$copartisan.source.scale.2.alignment,y=news.choice$visit.alignment,use='complete.obs')
sink()