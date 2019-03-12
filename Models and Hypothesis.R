Imputed_Insurance_Datasetv1_2 <- read_excel("E:/Statisitics with R/project/Imputed Insurance Datasetv1.2.xlsx")
View(Imputed_Insurance_Datasetv1_2)
attach(Imputed_Insurance_Datasetv1_2)
############################################
library(MASS)
#remove INDEX
Insurance = na.omit(Imputed_Insurance_Datasetv1_2[-1])
#first regression
reg1 = glm(TARGET_FLAG~.,family = "binomial",data=Insurance) 
summary(reg1) 
#evaluate interaction terms
res = step(reg1,~.^2) 
res$anova 
# add 3 interaction terms to form the second model
reg2 = glm(TARGET_FLAG~.+ KIDSDRIV:Job_dummy+ YOJ_Ceiling:Hom_Val_Ceiling+ OLDCLAIM:REVOKED,
           family = "binomial",data=Insurance)
# stepwise model to optimize the second model
step <- stepAIC(reg2, direction="both")
step$anova 
# removed 4 preditors(age,car_age,homekids,red_car)
reg3 = glm(TARGET_FLAG ~ YOJ_Ceiling + Income_Ceiling + Hom_Val_Ceiling + 
             KIDSDRIV + PARENT1 + MSTATUS + SEX_dummy + Education_dummy + 
             Job_dummy + TRAVTIME + Car_Use_dummy + BLUEBOOK + TIF + Car_Type + 
             OLDCLAIM + CLM_FREQ + REVOKED + MVR_PTS + URBANICITY_dummy + 
             KIDSDRIV:Job_dummy + YOJ_Ceiling:Hom_Val_Ceiling + OLDCLAIM:REVOKED
           ,family = "binomial",data=Insurance)
AIC(reg1,reg2,reg3)
summary(reg3)
#coefficient transformation
sort(exp(coef(reg3)))
install.packages('regclass')
library(regclass)
install.packages("sizeMat")
library(sizeMat)
#R square of the third model
nagelkerkeR2(reg3) 
#confusion matrix of the third model
confusion_matrix(reg3)



############################################
install.packages("rms")
library(rms)
#remove INDEX and TARGET_FLAG
Insurance = na.omit(Imputed_Insurance_Datasetv1_2[-c(1,2)])
attach(Imputed_Insurance_Datasetv1_2)
#first ordinal regression model
oreg = lrm(CLM_FREQ~.,data = Insurance) 
#optimize the first model. 9 predictors remain
fastbw(oreg)
#the second model
oreg1 = lrm(CLM_FREQ~ Hom_Val_Ceiling + KIDSDRIV + TRAVTIME + Car_Use_dummy + BLUEBOOK + OLDCLAIM + REVOKED + MVR_PTS + URBANICITY_dummy, data = Insurance)
AIC(oreg, oreg1)
############################################
install.packages("bbmle")
library(bbmle)
Imputed_Insurance_Datasetv1_2 <- read_excel("E:/Statisitics with R/project/Imputed Insurance Datasetv1.2.xlsx")
Insurance = na.omit(Imputed_Insurance_Datasetv1_2[-1])

f1 = function(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19,b20,b21,b22,b23)
{
  x = b0+b1*Insurance$YOJ_Ceiling+b2*Insurance$Income_Ceiling+b3*Insurance$Hom_Val_Ceiling+b4*Insurance$Car_Age_Ceiling+b5*Insurance$KIDSDRIV+b6*Insurance$AGE+b7*Insurance$HOMEKIDS+b8*Insurance$PARENT1+b9*Insurance$MSTATUS+b10*Insurance$SEX_dummy+b11*Insurance$Job_dummy+b12*Insurance$TRAVTIME+b13*Insurance$Car_Use_dummy+b14*Insurance$BLUEBOOK+b15*Insurance$TIF+b16*Insurance$Car_Type+b17*Insurance$RED_CAR+b18*Insurance$OLDCLAIM+b19*Insurance$CLM_FREQ+b20*Insurance$REVOKED+b21*Insurance$MVR_PTS+b22*Insurance$URBANICITY_dummy+b23*Insurance$Education_dummy
  L = ifelse(Insurance$TARGET_FLAG==0,1/(1+exp(x)),exp(x)/(1+exp(x)))
  LLsum = sum(log(L))
  return(-1*LLsum)
}

res = mle2(minuslogl = f2,start=list(b0=0,b1=0,b2=0,b3=0,b4=0,b5=0,b6=0,b7=0,b8=0,b9=0,b10=0,b11=0,b12=0,b13=0,b14=0,b15=0,b16=0,b17=0,b18=0,b19=0,b20=0,b21=0,b22=0,b23=0), method = "Nelder-Mead", hessian.opts = FALSE)
summary(res)

#########################################################
#1.H0:Red car has no impact on getting into a crash

p_tflag = prop.table(table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG))
p_red = prop.table(table(Imputed_Insurance_Datasetv1_2$RED_CAR))

p_tflag
p_red
p = p_tflag %*% t(p_red)
p
n = nrow(Imputed_Insurance_Datasetv1_2)
E = p*n
E
plot(table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG, Imputed_Insurance_Datasetv1_2$RED_CAR), xlab = 'Target Flag', ylab = "Red Car", 
     main = "Red Car by Car Type", col = "blue")

f1 = function()
{
  s1 = sample(x = c(0, 1), size = n, replace = T, prob = p_tflag)
  s2 = sample(x = c("no", "yes"), size = n, replace = T, prob = p_red)
  O = table(s1, s2)
  return(sum((O-E)^2/E))
}
f1()
sdist = replicate(10000, f1())

plot(density(sdist))
polygon(density(sdist), col="green")
tstat=cor(Imputed_Insurance_Datasetv1_2$CLM_FREQ, Imputed_Insurance_Datasetv1_2$RED_CAR)
abline(v=tstat, lwd=2)
rset=sdist[sdist>=tstat]
p_value=length(rset)/length(sdist)
p_value

###########################################################################
#2. H0:Car type has an impact on crash potential.


p_tflag = prop.table(table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG))
p_type = prop.table(table(Imputed_Insurance_Datasetv1_2$Car_Type))

p_tflag
p_type
p = p_tflag %*% t(p_type)
p
n = nrow(Imputed_Insurance_Datasetv1_2)
E = p*n
E

f1 = function()
{
  s1 = sample(x = c(0, 1), size = n, replace = T, prob = p_tflag)
  s2 = sample(x = c(0, 1, 2, 3, 4, 5), size = n, replace = T, prob = p_type)
  O = table(s1, s2)
  return(sum((O-E)^2/E))
}
f1()
sdist = replicate(10000, f1())
plot(density(sdist))
polygon(density(sdist), col="green")
abline(v=tstat, lwd=2)
rset=sdist[sdist>=tstat]
p_value=length(rset)/length(sdist)
p_value
######################################################################################
#3. HO: Car value have an impact on claim frequency

# Draw a syntetic sample from the population
f1 = function()
{
  s1 = rnorm(n, mean = mean(Imputed_Insurance_Datasetv1_2$CLM_FREQ), sd = sd(Imputed_Insurance_Datasetv1_2$CLM_FREQ))
  s2 = rnorm(n, mean = mean(Imputed_Insurance_Datasetv1_2$BLUEBOOK), sd = sd(Imputed_Insurance_Datasetv1_2$BLUEBOOK))
  return(mean(cor(s1,s2)))
}
f1()
# Replicate to create sampling distribution
sdist = replicate(10000, f1())
plot(density(sdist))
polygon(density(sdist), col="yellow")

# Evaluate the hypothesis
tstat = mean(cor(Imputed_Insurance_Datasetv1_2$CLM_FREQ, Imputed_Insurance_Datasetv1_2$BLUEBOOK))
tstat
abline(v=tstat)
abline(v=mean(sdist), col="green", lwd=3)
gap = abs(tstat-mean(sdist))
abline(v=mean(sdist)-gap)
lside=sdist[sdist<mean(sdist)-gap]
rside=sdist[sdist>mean(sdist)+gap]
pvalue=(length(lside)+length(rside))/length(sdist)
pvalue

################################################################################

#4.H0: The distance to work plays no role in cars getting into a crash.
# To test this hypothesis,we did Two sample test with numeric (TRAVTIME) and categorical data  (TARGET_FLAG)

attach(Imputed_Insurance_Datasetv1_2)
df1<-read_excel("E:/Statisitics with R/project/Imputed Insurance Datasetv1.2.xlsx")
crash_yes = df1[TARGET_FLAG==1,15]
Crash_no = df1[TARGET_FLAG==0,15]
crash_yes
Crash_no
nrow(crash_yes)
nrow(Crash_no)
mean(crash_yes$TRAVTIME)
mean(Crash_no$TRAVTIME)
sd(crash_yes$TRAVTIME)
sd(Crash_no$TRAVTIME)
tmean=mean(df1$TRAVTIME)
tmean
f1 = function()
{
  meanT = mean(rnorm(n = 2153,mean = tmean,sd = sd(crash_yes$TRAVTIME)))
  meanC = mean(rnorm(n = 6008,mean = tmean,sd=sd(Crash_no$TRAVTIME)))
  return(abs(meanT-meanC))
}
sdist = replicate(100000,f1())
plot(density(sdist))

polygon(density(sdist),col="green")
tstat = abs(mean(crash_yes$TRAVTIME)-mean(Crash_no$TRAVTIME))
tstat
abline(v=tstat,lwd=2)
rset = sdist[sdist>=tstat]
p_value = length(rset)/length(sdist)
p_value

#Since P value is 0<0.05, which means we will reject the null hypothesis.
#The distance to work does have an impact on cars getting into accident.People who drive longer tend to get into crashes more often than others.

###############################################################################################
#5 H0: License Revoked in the Past 7 Yearsand Involvement in crash are independent

table(TARGET_FLAG)
table(REVOKED)
p_crash=prop.table(table(TARGET_FLAG))
p_crash
p_revoked=prop.table(table(REVOKED))
p_revoked
p=p_crash%*%t(p_revoked)
p
n=nrow(Imputed_Insurance_Datasetv1_2)
n
E=p*n
E
O=table(TARGET_FLAG,REVOKED)
O
tstat=sum((O-E)^2/E)
tstat
f1 = function()
{
  s1 = sample(x = c(0,1),size = n,replace = T,prob =
                p_crash)
  s2 = sample(x = c(0,1),size = n,replace =
                T,prob = p_revoked)
  O = table(s1,s2)
  return(sum((O-E)^2/E))
}
f1()
sdist = replicate(10000,f1())
plot(density(sdist),xlim=c(0,200))
abline(v=tstat,lwd=2)

polygon(density(sdist),col="green")

rset = sdist[sdist>=tstat]
p_value = length(rset)/length(sdist)
p_value

#Since P value is 0<0.05, which means we will reject the null hypothesis.Both are dependent. A car involved in crash is likely to have license revoked in the past.

############################################################################################



#6 H0:Claim frequency in past and involvement in crash are not dependent.

table(CLM_FREQ)
table(TARGET_FLAG)
p_claimfreq=prop.table(table(CLM_FREQ))
p_crash=prop.table(table(TARGET_FLAG))
p_claimfreq
p_crash
p=p_claimfreq%*%t(p_crash)
p
n=nrow(Imputed_Insurance_Datasetv1_2)
n
E=p*n
E
O=table(CLM_FREQ,TARGET_FLAG)
O
tstat= sum((O-E)^2/E)
tstat
f1 = function()
{
  s1 = sample(x = c(0,1,2,3,4,5),size = n,replace = T,prob =
                p_claimfreq)
  s2 = sample(x = c(0,1),size = n,replace =
                T,prob = p_crash)
  O = table(s1,s2)
  return(sum((O-E)^2/E))
}
sdist = replicate(10000,f1())
plot(density(sdist),xlim=c(0,500))
polygon(density(sdist),col="green")
abline(v=tstat,lwd=2)
rset = sdist[sdist>=tstat]
p_value = length(rset)/length(sdist)

p_value
#since p value is 0, we reject null hypo. We can conclude that both are dependent

#############################################################################
#Visualisation
#############################################################################
Education = Imputed_Insurance_Datasetv1_2$Education_dummy
Marital_status = Imputed_Insurance_Datasetv1_2$MSTATUS
parent1 = Imputed_Insurance_Datasetv1_2$PARENT1
Kids_drive = Imputed_Insurance_Datasetv1_2$KIDSDRIV
job = Imputed_Insurance_Datasetv1_2$Job_dummy
Income = Imputed_Insurance_Datasetv1_2$Income_Ceiling
Home_kids = Imputed_Insurance_Datasetv1_2$HOMEKIDS
claim_freq = Imputed_Insurance_Datasetv1_2$CLM_FREQ
crash = Imputed_Insurance_Datasetv1_2$TARGET_FLAG

install.packages("corrgram")
library(corrgram)
Personal = data.frame(Education,Marital_status,parent1,Kids_drive,job,Income,Home_kids,claim_freq,crash)

corrgram(x = Personal,order = T,upper.panel = panel.pie, main = "Correlation among set of variables (Theme: Personal Life) with 2 target variables")


#**********Plotting Education v/s Target flag***********#
type3 = aggregate(Imputed_Insurance_Datasetv1_2$TARGET_FLAG, by=list(Category = Education), FUN = sum)
type3
plot(type3)

install.packages("plotrix")
library(plotrix)
lbs = c("<high school","high school","Bachelors","Masters","Phd")
pct = round(type3[,2]/sum(type3[,2])*100)
lbs1 = paste(lbs,pct)
lbs2 = paste(lbs1,"%")
pie3D(type3[,2],labels = lbs2,pct = round(type3[,2]/sum(type3[,2])*100), explode = 0.1, main = "Education v/s Crash count",theta = 1.2)
geom_freqpoly(type3)

#*******************************************************#


#***Education v/s urban/rural**********#
type4 = aggregate(Imputed_Insurance_Datasetv1_2$URBANICITY_dummy, by=list(Category = Education), FUN = sum)
type4
table(Education,Imputed_Insurance_Datasetv1_2$URBANICITY_dummy)
x = prop.table(table(Education,Imputed_Insurance_Datasetv1_2$URBANICITY_dummy),1)
lbs = "<high school"
lbs1 = "high school"
lbs2 = "Bachelors"
lbs3 = "Masters"
lbs4 = "Phd"
lbs5 = "Urban"
lbs6 = "Rural"

mosaicplot(x,col = "blue")
axis(side = 3, at = 0.05, labels = lbs)
axis(side = 3, at = 0.29, labels = lbs1)
axis(side = 3, at = 0.5, labels = lbs2)
axis(side = 3, at = 0.7, labels = lbs3)
axis(side = 3, at = 0.9, labels = lbs4)
axis(side = 2, at = 0.35, labels = lbs5)
axis(side = 2, at = 0.9, labels = lbs6)

#***********************************#


#***urban/rural v/s target_flag**********#
type5 = aggregate(Imputed_Insurance_Datasetv1_2$TARGET_FLAG, by=list(Category = Imputed_Insurance_Datasetv1_2$URBANICITY_dummy), FUN = sum)
type5
table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG,Imputed_Insurance_Datasetv1_2$URBANICITY_dummy)
x = prop.table(table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG,Imputed_Insurance_Datasetv1_2$URBANICITY_dummy))
x
lbs = c("Urban", "Rural")
pct = round(type5[,2]/sum(type5[,2])*100)
lbs1 = paste(lbs,pct)
lbs2 = paste(lbs1,"%")

pie3D(type5[,2],labels = lbs2,pct = round(type5[,2]/sum(type5[,2])*100), explode = 0.1, main = "Urban/Rural V/S Crash Rate",theta = 1.2)
#****************************************************************************************#


#***Number of home kids v/s crash rate**********#
type6 = aggregate(Imputed_Insurance_Datasetv1_2$TARGET_FLAG, by=list(Category = Imputed_Insurance_Datasetv1_2$HOMEKIDS), FUN = sum)
type6
table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG,Imputed_Insurance_Datasetv1_2$HOMEKIDS)
x = prop.table(table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG,Imputed_Insurance_Datasetv1_2$HOMEKIDS),2)
x
lbs = c("0 kids = ", "1 kid = ","2 kids = ","3 kids = ","4 kids = ","5 kids = ")
y = x[2,]
y
y1 = trunc(y*10^3)/10
y1

pct = y1
lbs1 = paste(lbs,pct)
lbs2 = paste(lbs1,"%")

pie3D(y1,labels = lbs2,pct = round(y1/sum(y1)*100), explode = 0.1, main = "Number of Kids at home V/S Crash Rate",theta = 1.2)
#****************************************************************************************#


#***Number of Driving kids v/s crash rate**********#

type6 = aggregate(Imputed_Insurance_Datasetv1_2$TARGET_FLAG, by=list(Category = Imputed_Insurance_Datasetv1_2$KIDSDRIV), FUN = sum)
type6
table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG,Imputed_Insurance_Datasetv1_2$KIDSDRIV)
x = prop.table(table(Imputed_Insurance_Datasetv1_2$TARGET_FLAG,Imputed_Insurance_Datasetv1_2$KIDSDRIV),2)
x
lbs = c("0 kids = ", "1 kid = ","2 kids = ","3 kids = ","4 kids = ")
y = x[2,]
y
y1 = trunc(y*10^3)/10
y1

pct = y1
lbs1 = paste(lbs,pct)
lbs2 = paste(lbs1,"%")

pie3D(y1,labels = lbs2,pct = round(y1/sum(y1)*100), explode = 0.1, main = "Number of driving Kids V/S Crash Rate",theta = 1.2)
#****************************************************************************************#
#subset the gender of drivers who had a crash before
t = Imputed_Insurance_Datasetv1_2[Imputed_Insurance_Datasetv1_2$TARGET_FLAG == 1, 12]
#3D pie of proportion of gender among drivers who had a crash
slices <- prop.table(table(t$SEX_dummy))
lbls <- c("men 45%","women 55%")
pie3D(slices,labels=lbls,main="Gender in crash",col = c('red','blue'))

#subset the age of drivers who had a crash before
t1 = Imputed_Insurance_Datasetv1_2[Imputed_Insurance_Datasetv1_2$TARGET_FLAG == 1, 8]
#bar chart of the frequency of age in crash 
barplot(table(t1), main = 'Age In Crash',col = c('red','blue'),width = 3)

###################################

plot(MVR_PTS~RED_CAR)
DF <- Imputed_Insurance_Datasetv1_2
DF.red <- DF[which(DF$RED_CAR == 1,)]
DF.red
View(DF.red)
boxplot(RED_CAR,MVR_PTS)
p <- hist(DF.red$MVR_PTS, main = "Red Car Frequency vs MVR_PTS", xlab = "MVR_PTS", border = "black", col = "blue")
p <- hist(DF.red$CLM_FREQ, main = "Red Car Frequency vs CLAIMS FREQUENCY", xlab = "CLMS_FRQ", border = "black", col = "blue")
# divide dataset into only red car values and compute histograms for data analysis


# create histograms for each car type on divided data
DF.type1 <- DF[which(DF$Car_Type == 1), ]
p <- hist(DF.type1$MVR_PTS, main = "Car (Type1) vs MVR_PTS", xlab = "MVR_PTS", border = "black", col = "blue")

DF.type2 <- DF[which(DF$Car_Type == 2), ]
p <- hist(DF.type2$MVR_PTS, main = "Car (Type2) vs MVR_PTS", xlab = "MVR_PTS", border = "black", col = "blue")

DF.type3 <- DF[which(DF$Car_Type == 3), ]
p <- hist(DF.type3$MVR_PTS, main = "Car (Type3) vs MVR_PTS", xlab = "MVR_PTS", border = "black", col = "blue")

DF.type4 <- DF[which(DF$Car_Type == 4), ]
p <- hist(DF.type4$MVR_PTS, main = "Car (Type4) vs MVR_PTS", xlab = "MVR_PTS", border = "black", col = "blue")

DF.type5 <- DF[which(DF$Car_Type == 5), ]
p <- hist(DF.type5$MVR_PTS, main = "Car (Type5) vs MVR_PTS", xlab = "MVR_PTS", border = "black", col = "blue")


# Analyze the relations and correlations between certain variables
table(Car_Type,MVR_PTS)
table(Car_Type,CLM_FREQ)
cor(MVR_PTS,CLM_FREQ)

# data aggregation for 3d pie charts
type1 = aggregate(Imputed_Insurance_Datasetv1_2$MVR_PTS, by=list(Category =Imputed_Insurance_Datasetv1_2$Car_Type), FUN = sum)
type1

type11 = aggregate(Imputed_Insurance_Datasetv1_2$MVR_PTS, by=list(Category = Imputed_Insurance_Datasetv1_2$RED_CAR), FUN = sum)
type11
# 3d pie charts on different car types
pie3D(type11$x, labels = c("Nor a red car", "Red Car"), col = c("Blue", "Red"), main = "MVR Points by Red Car", explode = 0.1, theta = 1)
lbls = c("Pickup", "Panel Track", "Van", "Sports Car", "Minivan", "Suv")
type1 = aggregate(Imputed_Insurance_Datasetv1_2$CLM_FREQ, by=list(Category =Imputed_Insurance_Datasetv1_2$Car_Type), FUN = sum)
type1

# 3d pie charts for claim frequency and different car type distributions
type1 = aggregate(Imputed_Insurance_Datasetv1_2$CLM_FREQ, by=list(Category = Imputed_Insurance_Datasetv1_2$Car_Type), FUN = sum)
type1

pie3D(type1$x, labels = c("Pickup", "Panel Track", "Van", "Sports Car", "Minivan", "Suv"), 
      main = "Claim Frequency by Car type Distribution", explode = 0.1, theta = 1)
# plot car types vs mvr points
plot(type2$x~type1$Category, xlab = 'Car Type', ylab = "MVR_PTS", main = "Car type vs MVR_PTS", col = "blue")

barplot(type1)

# 3d pie charts on aggregaretd data
install.packages("plotrix")
library(plotrix)

DF <- Imputed_Insurance_Datasetv1_2
DF.red <- Imputed_Insurance_Datasetv1_2[which(Imputed_Insurance_Datasetv1_2$RED_CAR == 1), ]
DF.red
x = length(DF.red$TARGET_FLAG)
x
x2 = sum(DF.red$TARGET_FLAG)
x2
x1 = x-x2
slices = c(x, x2)
x1
x2
pie3D(slices, labels = c("Total Red Cars","Red Cars in Accident"), main = "Red Cars in Accidents", explode = 0.1, theta = 1)

y = length(DF.red$MVR_PTS)
y2 = sum(DF.red$MVR_PTS)
y1=x-x2
slices = c(y1, y2)
pie3D(slices, labels = c("Total Red Cars","Red Cars in with TIckets"), main = "Red Cars in Accidents", explode = 0.1, theta = 1)

barchart(DF.red$MVR_PTS)

type3 = aggregate(DF.red$CLM_FREQ, by=list(Category = DF.red$CLM_FREQ), FUN = sum)
pie3D(DF.red$MVR_PTS)

##########################################
corrgram(Imputed_Insurance_Datasetv1_2,order = T,upper.panel = panel.pie)

