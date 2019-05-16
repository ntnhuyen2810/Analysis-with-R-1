setwd("~/Documents/Academic/BA with R/Problem set")
install.packages("DBI")
install.packages("RSQLite")
library(DBI)
library(RSQLite)
library(data.table)
con <- dbConnect(RSQLite::SQLite(),'wooldridge2.db')
dbListTables(con)

wpull <- function(tablename){
  con <- DBI::dbConnect(RSQLite::SQLite(),'wooldridge2.db')
  dt <- DBI::dbReadTable(con,tablename)
  dt <- data.table(dt)
  print(DBI::dbReadTable(con,paste(tablename,'labels',sep='_')))
  DBI::dbDisconnect(con)
  rm(con)
  return(dt)
}

## Question 1
wage1 <- wpull('wage1')
summary(wage1$edu)
summary(wage1$wage)
wage1$women <- as.numeric(wage1$female==1)
sum(wage1$women) 
wage1$men <- as.numeric(wage1$female==0)
sum(wage1$men) 

## Question 2
meap01 <- wpull('meap01')
summary(meap01$math4)
meap01$math4 
meap01$passrate100 <- as.numeric(meap01$math4 ==100)
sum(meap01$passrate100)
meap01$passrate50 <- as.numeric(meap01$math4 ==50)
sum(meap01$passrate50)
summary(meap01$read4)

cor(meap01$math4,meap01$read4)
sd(meap01$exppp)
summary(meap01$exppp)

AexceedB <- ((6000-5500)/5500)
AexceedB

C <- (100*(log(6000) - log(5500)))
C

(AexceedB-C)/C

## Question 3
k401 <- wpull('401k')
summary(k401$prate)
summary(k401$mrate)
summary(lm(prate~mrate,data=k401))
83.0755 + 5.8611*3.5

##Question 4
ceosal2 <- wpull('ceosal2')
summary(ceosal2)  # Avg sal = 865.9, Avg ten = 7.955
ceosal2$firstyr <- as.numeric(ceosal2$ceoten==0)
sum(ceosal2$firstyr) # 5 ceos in 1st year
summary(lm(log(salary)~ceoten,data=ceosal2))

##Question 5
wage2 <- wpull('wage2')
summary(wage2$wage)
summary(wage2$IQ)
sd(wage2$IQ)
summary(lm(wage~IQ,data=wage2))
116.9916 + 8.3031*15
summary(lm(log(wage)~IQ,data=wage2))
0.0088072 *100*15 

##Question 6
meap93 <- wpull('meap93')
summary(lm(math10~log(expend),data=meap93))
summary(meap93$expend)
-69.341 + 11.164 * log(7419)

## Question 7
hprice1 <- wpull('hprice1')
summary(lm(price~sqrft+bdrms,data=hprice1))
0.12844*140 + 15.19819*1
-19.31500 + 0.12844*2438 + 15.19819*4

## Question 8
summary(lm(log(salary)~log(sales)+log(mktval),data=ceosal2))
summary(lm(log(salary)~log(sales)+log(mktval)+profits,data=ceosal2))
summary(lm(log(salary)~log(sales)+log(mktval)+profits+ceoten,data=ceosal2))

cor(log(ceosal2$mktval),ceosal2$profits)

## Question 9
attend <- wpull('attend')
summary(attend$atndrte)
summary(attend$priGPA)
summary(attend$ACT)
summary(lm(atndrte~priGPA+ACT,data=attend))
75.700 + 17.261*3.65 -1.717*20

student <- as.numeric(attend$priGPA==3.65, attend$ACT==20)
sum(student)

attend[attend$priGPA==3.65,attend$ACT==20]

75.700 + 17.261*3.1 -1.717*21
75.700 + 17.261*2.1 -1.717*26
93.1521 - 67.3061 

## Question 10
htv <- wpull('htv')
summary(htv$educ)
grade12 <- as.numeric(htv$educ < 13)
sum(grade12)
htv$educ(which(htv$educ ==13))
698/1230 
summary(htv$motheduc)
summary(htv$fatheduc)

summary(lm(educ~motheduc+fatheduc,data=htv))
summary(lm(educ~motheduc+fatheduc+abil,data=htv))

summary(lm(educ~motheduc+fatheduc+abil+I(abil^2),data=htv))

sd(htv$abil)
summary(htv$abil)

predictededu <- (8.240226+0.190126*12.18+0.108939 *12.45+0.401462*htv$abil+0.050599*I(htv$abil^2))
plot(predictededu, htv$abil, main="Predicted Edu By Abil Scatterplot", 
     xlab="Predicted Edu ", ylab="Abil", pch=19)
