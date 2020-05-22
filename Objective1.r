df<-read.csv("EdmontonRealEstateDataU3.csv")
names(df)
notused<- c(taxroll_number,building_name,effective_build_year,house_number,street_name,
            postal_code,city,full_address,geometry,lat,lon)

df<-subset(df , select=- c(landuse_description,market_building_class,taxroll_number,building_name,effective_build_year,house_number,street_name,
                     postal_code,city,full_address,geometry,lat,lon))
df<-df[df$assessed_value!=0,]     

df<-df[df$property_type=='RESIDENTIAL',]//URBAN,INDUSTRIAL,MULTI-RES
df<-subset(df,select=-property_type)
# numeric variables in data
summary(df$net_area)
sum(is.na(df$net_area))

hist(df$net_area, prob=TRUE, col="grey")
lines(density(df$net_area), col="blue", lwd=2) 
lines(density(df$net_area, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
outlier_values <- boxplot.stats(df$net_area)$out  # outlier values.
boxplot(df$net_area, main="Net Area", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6,col = 'red')

summary(df$lot_size.1)
sum(is.na(df$lot_size.1))

hist(df$lot_size.1, prob=TRUE, col="grey")
lines(density(df$lot_size.1), col="blue", lwd=2) 
lines(density(df$lot_size.1, adjust=2), lty="dotted", col="darkgreen", lwd=2) 
outlier_values <- boxplot.stats(df$lot_size.1)$out  # outlier values.
boxplot(df$lot_size.1, main="Lot size", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6,col = 'red')


summary(df$tot_gross_area_description.1)
sum(is.na(df$tot_gross_area_description.1))
hist(df$tot_gross_area_description.1)

outlier_values <- boxplot.stats(df$tot_gross_area_description.1)$out  # outlier values.
boxplot(df$tot_gross_area_description.1, main="Total Gross area", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6,col = 'red')


# Fill NA with mean

df[sapply(df, is.numeric)] <- 
  lapply(df[sapply(df, is.numeric)], 
         function(x) 
           ifelse(is.na(x), mean(x, na.rm = TRUE), x))
summary(df)


#Normalize Data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#denormalize <- function(x) {
#    return ((x - min(x)) / (max(x) - min(x)))
#}

#log Transformation
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

inverseLog=function(x){
  
  10**x
}
#df[c('net_area','lot_size.1', 'tot_gross_area_description.1')] <-lapply(df[c('net_area','lot_size.1', 'tot_gross_area_description.1')], signedlog10)
df[c('net_area','lot_size.1', 'tot_gross_area_description.1')] <-lapply(df[c('net_area','lot_size.1', 'tot_gross_area_description.1')], normalize)
#df[c('net_area', 'tot_gross_area_description.1')] <-lapply(df[c('net_area', 'tot_gross_area_description.1')], normalize)
df[c('site_coverage')] <-lapply(df[c('site_coverage')], normalize)
#df[c('assessed_value')]<-sapply(df[c('assessed_value')],signedlog10)

#1-n encode Categorical data
mod <- dummyVars(" ~ .", data = df,fullRank = TRUE)
dfEncoded <-data.frame(predict(mod, newdata = df))
dim(dfEncoded) # it as 1128 columns after encoding


# Divide into test train Randomly  assesed value column is Y and use all other as X
set.seed(1)
trainindex<-createDataPartition(dfEncoded$assessed_value,p=0.9,list=FALSE)
traindf<-dfEncoded[trainindex,]
testdf<-dfEncoded[-trainindex,]
dim(traindf)
dim(testdf)

#Mean Absolute Error
mae=function(actual, predicted)
{
  mean(abs(actual-predicted))
  
}

#Mean absolute percentage Error 
mape=function(actual, predicted)
{
  mean(abs(((actual-predicted)/actual)*100))
}
set.seed(1)
#Linear function
fit.linear=lm(signedlog10(assessed_value)~.,data=traindf)
#fitControl <- trainControl(method = "cv", number = 5)
#fit <- train(signedlog10(assessed_value) ~ ., data = traindf,method = "lm",trControl = fitControl)
#summary(fit.linear)
ypredL.linear<- predict(fit.linear,testdf)
ypred.linear=inverseLog(ypredL.linear)
summary(ypred.linear)
mae.linear=mae(testdf$assessed_value,ypred.linear)
mape.linear=mape(testdf$assessed_value,ypred.linear)
mean((ypred.linear-testdf$assessed_value)^2)
Rmse<-sqrt(mean((ypred.linear-testdf$assessed_value)^2))
Rmse
summary(ypredL.linear)
#Bagging 
#fit.bag=randomForest(signedlog10(assessed_value)~.,data=traindf,mtry=ncol(traindf)-1, importance=TRUE)
#ypredL.bag = predict(fit.bag,newdata=testdf)
#ypred.bag=inverseLog(ypredL.bag)
#importance(fit.bag)
#varImpPlot(fit.bag)
#mae.bag=mae(testdf$assessed_value,ypred.bag)
#mape.bag=mape(testdf$assessed_value,ypred.bag)

#Boosting
set.seed(1)
fit.boost=gbm(signedlog10(assessed_value)~.,data=traindf,distribution="gaussian",n.trees = 500,interaction.depth = 4,shrinkage = 0.2)
#fitControl_1<- trainControl(method = "cv", number = 5)
#fit_1<- train(signedlog10(assessed_value) ~ ., data = traindf,method = "gbm",trControl = fitControl_1)

ypredL.boost = predict(fit.boost,newdata=testdf,n.trees=500)
ypred.boost=inverseLog(ypredL.boost)
mae.boost=mae(testdf$assessed_value,ypred.boost)
mape.boost=mape(testdf$assessed_value,ypred.boost)
mean((ypred.boost-testdf$assessed_value)^2)
Rmse<-sqrt(mean((ypred.boost-testdf$assessed_value)^2))
summary(ypredL.boost)
#svm
#library(e1071)
svmfit=svm(signedlog10(assessed_value)~.,data=traindf, kernel="radial",gamma=1,cost=0.1)

ypredL.svm = predict(svmfit,newdata=testdf)
ypred.svm=inverseLog(ypredL.svm)
mae.svm=mae(testdf$assessed_value,ypred.svm)
mape.svm=mape(testdf$assessed_value,ypred.svm)
mean((ypred.svm-testdf$assessed_value)^2)
Rmse<-sqrt(mean((ypred.svm-testdf$assessed_value)^2))

plot(testdf$assessed_value,type='l',col="red",ylab ="assessed value")
par(new=TRUE)
plot(ypred.boost,type='l',col="green",axes=F,ylab="assessed value")
legend("topleft", legend=c("Actual", "Predicted"),
       col=c("red", "Green"), lty=1:2, cex=0.8)
par(new=F)
diffLessThan1000<-abs(testdf$assessed_value-ypred.boost)<=1000
length(which(diffLessThan1000))
diffLessThan5000<-abs(testdf$assessed_value-ypred.boost)<=5000
length(which(diffLessThan5000))
diffLessThan10000<-abs(testdf$assessed_value-ypred.boost)<=10000
length(which(diffLessThan10000))
diffLessThan50000<-abs(testdf$assessed_value-ypred.boost)<=50000
length(which(diffLessThan50000))
above50000<-abs(testdf$assessed_value-ypred.boost)>50000
length(which(above50000))

plot(testdf$assessed_value[diffLessThan10000],type='l',col="red",xlab="Number of Houses",ylab = "Price")
par(new=TRUE)
plot(ypred.boost[diffLessThan10000],type='l',col="green",axes=F,xlab="Number of Houses",ylab = "Price")
legend("topleft", legend=c("Actual", "Predicted"),
       col=c("red", "Green"), lty=1:2, cex=0.8)
par(new=F)

plot(testdf$assessed_value[diffLessThan50000],type='l',col="red",xlab="Number of Houses",ylab = "Price")
par(new=TRUE)
plot(ypred.boost[diffLessThan50000],type='l',col="green",axes=F,xlab="Number of Houses",ylab = "Price")
legend("topleft", legend=c("Actual", "Predicted"),
       col=c("red", "Green"), lty=1:2, cex=0.8)
par(new=F)


plot(testdf$assessed_value[above50000],type='l',col="red",xlab="Number of Houses",ylab = "Price")
par(new=TRUE)
plot(ypred.boost[above50000],type='l',col="green",axes=F,xlab="Number of Houses",ylab = "Price")
legend("topleft", legend=c("Actual", "Predicted"),
       col=c("red", "Green"), lty=1:2, cex=0.8)
par(new=F)
svmfit=svm(signedlog10(assessed_value)~.,data=traindf, kernel="radial",gamma=2,cost=1)
ypredL.svm = predict(svmfit,newdata=testdf)
ypred.svm=inverseLog(ypredL.svm)
mae.svm=mae(testdf$assessed_value,ypred.svm)
mape.svm=mape(testdf$assessed_value,ypred.svm)

plot(testdf$assessed_value,type='l',col="red")
par(new=TRUE)
plot(ypred.boost,type='l',col="green",axes=F)
par(new=F)
diffLessThan1000<-abs(testdf$assessed_value-ypred.boost)<=1000
length(which(diffLessThan1000))
diffLessThan5000<-abs(testdf$assessed_value-ypred.boost)<=5000
length(which(diffLessThan5000))
diffLessThan10000<-abs(testdf$assessed_value-ypred.boost)<=10000
length(which(diffLessThan10000))
diffLessThan50000<-abs(testdf$assessed_value-ypred.boost)<=50000
length(which(diffLessThan50000))
above50000<-abs(testdf$assessed_value-ypred.boost)>50000
length(which(above50000))

plot(testdf$assessed_value[diffLessThan10000],type='l',col="red")
par(new=TRUE)
plot(ypred.boost[diffLessThan10000],type='l',col="green",axes=F)
par(new=F)

plot(testdf$assessed_value[diffLessThan50000],type='l',col="red")
par(new=TRUE)
plot(ypred.boost[diffLessThan50000],type='l',col="green",axes=F)
par(new=F)


plot(testdf$assessed_value[above50000],type='l',col="red")
par(new=TRUE)
plot(ypred.boost[above50000],type='l',col="green",axes=F)
par(new=F)
Summary(mape.boost)
Summary(fit.boost)
