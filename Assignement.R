# Load the data
data <- read.table("growth txt.txt", header = TRUE, sep = "\t")

#1- Explore the data

#Spaghettiplot
attach(data)
n=length(unique(IDNR))
interaction.plot(AGE,IDNR,MEASURE, 
                 xlab="Age in months", ylab="Growth",
                 legend=FALSE)

#Spaghetti with mean evolution for each sex
plot(AGE, MEASURE, type = "n")

for (i in 1:length(unique(IDNR)))
{ lines(y =data$MEASURE[data$IDNR == unique(data$IDNR)[i]], 
        x =data$AGE[data$IDNR == unique(data$IDNR)[i]], 
        col = 8)
}

mean.prog0=tapply(MEASURE[SEX==1], INDEX =AGE[SEX==1], FUN = mean, na.rm = TRUE)
lines(mean.prog0, x = unique(AGE), lwd = 3)

mean.prog1=tapply(MEASURE[SEX==2], INDEX =AGE[SEX==2], FUN = mean, na.rm = TRUE)
lines(mean.prog1, x = unique(AGE), lwd = 4,col=2)

legend("topright", inset = 0.05, legend = c("SEX 1", "SEX 2"), horiz = TRUE,
       text.col = c("black", "red"), pt.bg = c("black", "red"), pch = c(16, 16), col = c("black", "red"), cex=0.8)


#Descriptive:
## Mean:
early.mean=tapply(MEASURE,list(AGE,SEX),mean)

## Standard deviation:
early.sd=tapply(MEASURE,list(AGE,SEX),sd)

## Variance:
early.var=tapply(MEASURE,list(AGE,SEX),var)

## Frequency:
early.n=table(AGE,SEX)

## Boxplots:
boxplot(MEASURE~AGE,xlab="Age (in months)",ylab="MEASURE")

## Boxplots per program
par(mfrow=c(2,1))
boxplot(MEASURE[SEX==1]~AGE[SEX==1],main="SEX 1",xlab="Age (in months)",ylab="MEASURE")
boxplot(MEASURE[SEX==2]~AGE[SEX==2],main="SEX 2",xlab="Age (in months)",ylab="MEASURE")


## Reshaping the data into a wide form
data2 <- reshape(data, timevar = "AGE", idvar = c("IDNR", "SEX"), direction = "wide")
data2 <- data2[, c(1, 2, 4, 6, 8)]
data2

## Correlation between the Measures at different ages
cor(data2[,3:5])

## Displaying the linear regression per person ??
cf <- sapply(data$IDNR, function(x) coef(lm(MEASURE~AGE, data=subset(data, IDNR==x))))

plot(cf[1,],cf[2,],xlab="Individual regression intercept",
     ylab="Individual regression slope", main="Individual regression intercept versus slope")
identify(cf[1,],cf[2,],n=1)

Sx<-reorder(data$IDNR, cf[1,])
Sx

xyplot(MEASURE ~ AGE|Sx,groups=SEX,data=data2, type=c("p","r"),auto.key=T,aspect="xy",par.settings=list(axis.text=list(cex=0.6),fontsize=list(text=8, points=10)),scales=list(x=list(at=c(8,10,12,14),labels=c("8","10","12","14"))))
##??


## Linear regression per participant of Measure on age

## Coefficients

lin.reg.coef <- by(data, data$IDNR,function(data) coef(lm(MEASURE ~ AGE, data=data)))
lin.reg.coef1 <- unlist(lin.reg.coef)
names(lin.reg.coef1) <- NULL 
lin.reg.coef2=matrix(lin.reg.coef1,length(lin.reg.coef1)/2,2,byrow = TRUE)

## R squared

lin.reg.r.squared <- by(data, data$IDNR, 
                        function(data) summary(lm(MEASURE ~ AGE, data=data))$r.squared )
lin.reg.r.squared1<- as.vector(unlist(lin.reg.r.squared))

## Histograms

par(mfrow=c(3,1))
hist(lin.reg.coef2[,1],xlab="Intercept",col="lightblue",main="Histogram of individual intercepts")
hist(lin.reg.coef2[,2],xlab="Slope",col="lightblue",main="Histogram of individual slopes")
hist(lin.reg.r.squared1,xlab="R squared",col="lightblue",main="Histogram of individual R squared")


## Plotting individual regression lines per group

reg.coef=cbind(lin.reg.coef2, data[data$AGE==8,]$SEX)
mean.int<-tapply(reg.coef[,1],reg.coef[,3],mean)
mean.slope<-tapply(reg.coef[,2],reg.coef[,3],mean)

par(mfrow=c(1,2))
plot(AGE,MEASURE,type="n",xlim=c(1,2),ylim=c(0,30),main="SEX 1",xlab="Age (in months)",ylab="MEASURE",axes=F)
axis(side=1,at=c(8,10,12,14),labels=c(8,10,12,14))
axis(side=2,at=seq(0,30,5))
box()
for (i in 1:27)
{if (reg.coef[i,3]==1) 
{curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")}}
curve(cbind(1,x)%*%c(mean.int[1],mean.slope[1]),add=T,lwd=2)

plot(data$AGE,data$MEASURE,type="n",xlim=c(1,2),ylim=c(0,30),main="SEX 2",xlab="Age (in months)",ylab="MEASURE",axes=F)
axis(side=1,at=c(8,10,12,14),labels=c(8,10,12,14))
axis(side=2,at=seq(0,30,5))
box()
for (i in 1:27)
{if (reg.coef[i,3]==2) 
{curve(cbind(1,x)%*%reg.coef[i,1:2],add=T,col="gray")}}
curve(cbind(1,x)%*%c(mean.int[2],mean.slope[2]),add=T,lwd=2)

## Fitting the model with ML

## Different random intercept and slope
#lmer
data.lmer1<-lmer(MEASURE~AGE*SEX+(1 + AGE|IDNR), REML = FALSE, data=data)
mcp.fnc(data.lmer1)
help(mcp.fnc)

summary(data.lmer1)
display(data.lmer1)
anova(data.lmer1)
#lme + pvalues
data.lme1<-lme(MEASURE~AGE*SEX, random=~1+AGE|IDNR, method = "ML", data=data)
summary(data.lme1)


## Likelihood ratio tests

lmer1.NOSEX<-lmer(MEASURE~1+AGE+(1 + AGE|IDNR), REML = FALSE, data=data)
lmer1.SEX<-lmer(MEASURE~1+AGE+SEX+(1 + AGE|IDNR), REML = FALSE, data=data)
anova(lmer1.NOSEX,lmer1.SEX,data)

#Final model

early.lmer1.slopeprog<-lmer(cog~1+age0+age0:program+(1 + age0|id), REML = FALSE, data=early.int1)

summary(early.lmer1.slopeprog)
