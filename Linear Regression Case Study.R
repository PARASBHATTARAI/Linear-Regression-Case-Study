####LINEAR REGRESSION ASSIGNMENT



#Load the Data set in R

require(readxl)
 Data_Base <- readxl::read_excel("Linear Regression Case.xlsx",sheet = 1)


### Check The Data Summary
 
summary(Data_Base)



 ### Check The Data Type of column

 str(Data_Base)
 sapply(Data_Base, class)

### Data type of Column( all Column  are numeric except customerid and birth month)

### Creating column "Total_Spend" which is a sum of primary and secondary spend
 
 Data_Base$Total_Spend <- Data_Base$cardspent + Data_Base$card2spent
 Data_Base$Card_Item <- Data_Base$carditems + Data_Base$card2items
 
 ### Droping Useless Column (cardspent Primary card, card2spend Secondary card, custid, carditems and card2items  ) 

Data_Base$custid <-NULL
Data_Base$cardspent <- NULL
Data_Base$card2spent <- NULL
Data_Base$carditems <- NULL
Data_Base$card2items <- NULL

#### Dropping the redudant variable (Birth Month)

Data_Base$birthmonth <- NULL

### Above Data Set is All Numeric DataSet
isnum <- sapply(Data_Base, is.numeric)
df_num <- Data_Base[,isnum]
 
Data_Base <- df_num


mystats <- function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, var=var, range=range, pctl=pctl))
}

num.stats.analysis <- data.frame(t(apply(Data_Base,2,FUN=mystats)))


### write.csv(num.stats.analysis, file = "analysis.csv")

#### It Can Be Seen In analysis file that some column have too many Missing Value


require(dplyr)
Percen_Tage= apply(Data_Base, 2,function(col)sum(is.na(col))/length(col)*100)

Percent_Age_Desc = sort(Percen_Tage, decreasing = TRUE )
Percent_Age_Desc

### Following Column Has too Many Missing Value
### droping These Column

### lnwiremon lnwireten lnequipmon lnequipten lntollmon lntollten lncardten lncardmon 

Data_Base$lnwiremon  <-NULL
Data_Base$lnwireten  <-NULL
Data_Base$lnequipmon <-NULL
Data_Base$lnequipten <-NULL
Data_Base$lntollmon <-NULL
Data_Base$lntollten <-NULL
Data_Base$lncardten <-NULL
Data_Base$lncardmon <-NULL


dim(Data_Base)

outlier_treatment <- function(x){
  UC = quantile(x, p=0.95, na.rm=T)
  LC = quantile(x, p=0.05, na.rm=T)
  x = ifelse(x>UC, UC, x)
  x = ifelse(x<LC, LC, x)
  return(x)
}

missing_value_treatment = function(x){
  x[is.na(x)] = mean(x, na.rm=T)
  return(x)
}


df_outlier_trated <- data.frame(apply(Data_Base, 2, FUN = outlier_treatment))

df_outlier_and_missing_trated <- data.frame(apply(df_outlier_trated, 2, FUN = missing_value_treatment))

num.stats.analysis2 <- data.frame(t(apply(df_outlier_and_missing_trated,2,FUN=mystats)))
require(writexl)

### writexl::write_xlsx(num.stats.analysis2, "analysis2.xlsx")



### There is No Categorical Variable. Thus no Need of Encoding or Dummy 


Data_Base_Lin <- df_outlier_and_missing_trated

# FEATURE REDUCTION  


features <- dplyr::select(Data_Base_Lin, -Total_Spend)
Y_var <- dplyr::select(Data_Base_Lin, Total_Spend)


features <- data.matrix(features)
Y_var <- data.matrix(Y_var)
dim(Y_var)
dim(features)




# RFE 


library(caret)

### Set Seed value
set.seed(0)

### WARNING : Time Consuming Step (approx 1-2 mins)

### Use the full model and run the rfe()
results_rfe <- rfe(features,Y_var,size=c(1:120), rfeControl=rfeControl(functions = lmFuncs))
results_rfe

### Finding the top 10 vars

### We use the update() and pass the rfe object, features, Y var and the no. of top
### impr variables that we want

RFE_top10 <- update(results_rfe,features,Y_var,size =10)
RFE_top10[["bestVar"]]


 RFE <- RFE_top10[["bestVar"]]
 RFETOP10 <- data.frame(RFE)

### writexl::write_xlsx(RFETOP10,"RFETOP10.xlsx")

### Top 10 variable 

### card
### marital
### lninc
###Card_Item
### card2
### equip
### tollfree
### spousedcat
### gender
### voice


# STEPWISE REGRESSION 

### Creating a full and an empty model
modelF <- lm(Total_Spend~.,data=Data_Base_Lin)
modelF

modelnull <- lm(Total_Spend~1,data=Data_Base_Lin)
modelnull

### Running stepwise Regression

stepwise <- step(modelnull, scope = list(upper=modelF), data=Data_Base_Lin,  direction= "both")


### Important Variable according to Stepwise Regression 


### Card_Item + lninc + card + card2 + income + gender + 
###  age + churn + multline + commutewalk + region + townsize + 
###  card2fee + ownpc + cardtenure + callcard + creddebt + inccat + 
### pets_cats + callid + voice + ownpda + pets_dogs + cardtenurecat + 
###  equipten + card2type + agecat


### LASSO 

lasso = train(Total_Spend~.,
    data=Data_Base_Lin,method='glmnet',
    trControl = trainControl(method="none"),
 tuneGrid=expand.grid(alpha=1,lambda=5))

pred_lasso = predict(lasso,newdata = subset(Data_Base_Lin,select = c(1:120)))

 
coef(lasso$finalModel, s = lasso$bestTune$lambda)


### final value



Final_Vars <- c("card",
      "marital",
      "lninc",
      "Card_Item",
      "card2",
     "equip",
     "tollfree",
      "spousedcat",
     "gender",
      "voice",
      "income",
      "age",
      "churn",
     "multline",
      "commutewalk",
      "region",
      "townsize",
      "card2fee",
      "ownpc",
      "cardtenure",
      "callcard",
      "creddebt",
      "inccat",
      "pets_cats",
      "callid",
      "ownpda",
      "pets_dogs",
      "cardtenurecat",
      "equipten",
      "card2type",
      "agecat",
      "retire",
      "othdebt",
      "Total_Spend")

Reduce_DataBase <- Data_Base_Lin[,Final_Vars]

length(Reduce_DataBase)


###Now I have 34 features 



###############
# ASSUMPTIONS #
###############


### Assumption 1 : Y is normally distributed

require(ggplot2)
YVarHist <- ggplot(data = Reduce_DataBase) + aes(Total_Spend) +
  geom_histogram(bins = 10,fill = "skyblue1",color = "black") + 
  theme_bw() ;plot(YVarHist)



Reduce_DataBase$ln_Y <-  log(Reduce_DataBase$Total_Spend)
require(moments)
skewness(Reduce_DataBase$Total_Spend)
skewness(Reduce_DataBase$ln_Y)

kurtosis(Reduce_DataBase$Total_Spend)
kurtosis(Reduce_DataBase$ln_Y)

YVarHist <- ggplot(data = Reduce_DataBase) + aes(ln_Y) +
  geom_histogram(bins = 10,fill = "skyblue1",color = "black") + 
  theme_bw() ;plot(YVarHist)



### Assumption 2 and 3: Core between X and Y and X and X variables
cor_mat <- data.frame(cor(Reduce_DataBase))


require(writexl)
### writexl::write_xlsx(cor_mat,'corr_matrix.xlsx')

### There are a number of X vars having decent corr with the Y 

### Can remove ininc  as it is highly corr with incat and income 
### EQUIP AND equipten HAVE ALSO HIGH CORRELATIOn
### Spousedcat and Marital Are Highly Correlated
### INCOMECAT AND INCOME ARE ALSO HIGHLY CORRELATE
### agecat and age are Highly Correlated Thus removing age
### cardtenurecat and cardtenure are also Highly Correlated
### equip and equipten are Highly Correlated Thus Removing equipten


df_final <- Reduce_DataBase


df_final$spousedcat <- NULL
df_final$lninc <- NULL
df_final$inccat <- NULL
df_final$age <- NULL
df_final$cardtenurecat <- NULL
df_final$equipten <- NULL
require(dplyr)
 

## RUNNING VIF

require(car)

modelF <- lm(df_final$Total_Spend~., data=df_final)

car::vif(modelF)



##  DATA SPLITTING 

samp<-sample(1:nrow(df_final), floor(nrow(df_final)*0.7))

dev <- df_final[samp,]
val<-df_final[-samp,]

nrow(dev)
nrow(val)
nrow(df_final)


colnames(df_final)



## FITTING THE MODEL 


## Iteration 1
fit1<- lm(Total_Spend~card+marital+Card_Item+card2+gender+voice+  income+churn 
    
+multline+region+townsize+ownpc+creddebt+retire,data = dev)



summary(fit1)


# Iteration 2

fit2<- lm(Total_Spend~card+Card_Item+card2+gender+income+churn 
          +multline+creddebt, data=dev)

summary(fit2)


### Validating the Model

dev1 <- data.frame(cbind(dev,pred = predict(fit2, newdata = dev)))
val1 <- data.frame(cbind(val,pred = predict(fit2, newdata = val)))


######  MAPE, RMSE for development and validation dataset

require(Metrics)

Metrics::mape(dev1$Total_Spend,dev1$pred)
Metrics::mape(val1$Total_Spend,val1$pred)

Metrics::rmse(dev1$Total_Spend,dev1$pred)
Metrics::rmse(val1$Total_Spend,val1$pred)





# Using Cooks Distance to reduce error 

cooksd <- cooks.distance(fit2)
sample_size <- nrow(dev)


plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") 
abline(h = 4/sample_size, col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red") 

# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])

influential

dev3 <- dev[-influential, ]

nrow(dev)
nrow(dev3)

# ICreate new model using dev3

fit3 <- lm(Total_Spend~card+Card_Item+card2+gender+income+churn 
           +multline+creddebt,data=dev3)

summary(fit3)



### Validating the Model

dev1 <- (data.frame(cbind(dev,pred = predict(fit3, newdata = dev))))
val1 <- (data.frame(cbind(val,pred = predict(fit3, newdata = val))))


###### Calculate R2, Adj R2, MAPE, RMSE for development and validation dataset

Metrics::mape(dev1$Total_Spend,dev1$pred)
Metrics::mape(val1$Total_Spend,val1$pred)

Metrics::rmse(dev1$Total_Spend,dev1$pred)
Metrics::rmse(val1$Total_Spend,val1$pred)





### The main variable that are driving the credit card spend are 
### card, Card_Item, card2, gender, income, churn, multline, creddebt



####################
## DECILE ANALYSIS #
####################

###################################
## **** DEVELOPMENT DATASET **** ##
###################################
dev11 <- dev1

# Coming up with the deciles
decLocations <- quantile(dev11$pred, probs = seq(0.1,0.9,by=0.1))
decLocations

# Coming up with the decile intervals
dev11$decile <- findInterval(dev11$pred, c(-Inf,decLocations,Inf))



# Confirm the decile by checking the summary
summary(dev11$decile)

# Cross Freq Tab to confirm if the deciles are proper
xtabs(~decile, dev11)


# In terms of R, we now have to group the data by decile, calcualte
# the average Y and the predicted Y
# As we have discussed all of this using dplyr
# Let us explore performing the same task using SQL

require(sqldf)


dev111 <- dev11[,c("decile","Total_Spend","pred")]
colnames(dev111) <- c("decile","Total_Spend","pred")


decDEV <- sqldf::sqldf("select decile, 
    count(decile) as count, 
    avg(pred) as avg_pred_Y, 
    avg(Total_Spend) as avg_Y 
    from dev111 
    group by decile 
    order by decile")



### writexl::write_xlsx(decDEV,'Decile_Analysis_Dev.xlsx')


library(caret)
Impo=as.data.frame(varImp(fit3))
### write.csv(Impo,"ImportantVariable.csv")





#-------------------------------------------------------------------------
# Diagnostics and other Miscellaneous functions
#-------------------------------------------------------------------------



# Other useful functions 
coefficients(fit3) # model coefficients
confint(fit3, level=0.95) # CIs for model parameters 
fitted(fit3) # predicted values
residuals(fit3) # residuals
anova(fit3) # anova table 
vcov(fit3) # covariance matrix for model parameters 
influence(fit3) # regression diagnostics

# diagnostic plots 
par(mar=c(1,1,1,1))
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit3)


#-------------------------------------------------------------------------
#****************                         End           ******************
#--------------------------------------------------------------------------




