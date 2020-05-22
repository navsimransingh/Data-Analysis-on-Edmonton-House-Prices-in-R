dfC<-read.csv("Property_Assessment_Data__2012_-_2016.csv")
dfC['Assessed.Value']<- as.numeric(gsub("\\$", "", dfC$Assessed.Value))
dfC2015<-dfC[dfC$Assessment.Year==2015,c("Assessment.Year","Account.Number","Assessed.Value")]
dfC2016<-dfC[dfC$Assessment.Year==2016,c("Assessment.Year","Account.Number","Assessed.Value")]
dfCFinal<-merge(x = dfC2015, y = dfC2016, by = "Account.Number", all.x=TRUE)
dfCFinal<-dfCFinal
dfCFinal[,"diff"]<-dfCFinal$Assessed.Value.y-dfCFinal$Assessed.Value.x 

#dfCFinal[,"change"]<-ifelse(dfCFinal$diff>0, "increase",
 #                           ifelse(dfCFinal$diff<0, "decrease","noChange")
#)
dfCFinal[,"change"]<-ifelse(dfCFinal$diff>0, "increase","Noincrease")

df<-read.csv("EdmontonRealEstateDataU3.csv")
data<-merge(x = df, y = dfCFinal[,c("Account.Number","change","diff")], by.x = "taxroll_number",by.y = "Account.Number", all.x=TRUE)
df<-subset(data, select=- c(landuse_description,market_building_class,taxroll_number,building_name,effective_build_year,house_number,street_name,
                           postal_code,city,full_address,geometry,lat,lon,assessed_value,diff,neighbourhood))

df<-df[df$property_type=='RESIDENTIAL',]

boxplot(abs(df$diff)~df$change)

ggplot(df, aes(x=, y=assessed_value)) + 
    geom_bar(stat="identity", width=.5, fill="tomato3") + 
    labs(title="Ordered Bar Chart", 
         subtitle="Make Vs Avg. Mileage", 
         caption="source: mpg")
    df<-subset(df,select=-c(diff))
df<-subset(df,select=-property_type)
#data preprocessing

# Fill NA with mean

df[sapply(df, is.numeric)] <- 
    lapply(df[sapply(df, is.numeric)], 
           function(x) 
               ifelse(is.na(x), mean(x, na.rm = TRUE), x))
dim(df)
df<-df[complete.cases(df), ]
dim(df)
#Normalize Data
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

df[c('net_area','lot_size.1', 'tot_gross_area_description.1','site_coverage','effective_build_year_1')] <-
    lapply(df[c('net_area','lot_size.1', 'tot_gross_area_description.1','site_coverage','effective_build_year_1')], normalize)
df$change <- as.factor(df$change)

#dummy vars
library(Matrix)
mod <- dummyVars("change ~ .", data = df,fullRank = TRUE)
dfEncoded <-data.frame(predict(mod, newdata = df))
dfEncoded$change<-df$change
dim(dfEncoded) # it as 1128 columns after encoding
class(dfEncoded$change)


#0- increse, 1-decrease, 3-nochange
#dfEncoded$change<-ifelse(dfEncoded$change=='increase', 0,

 #                           ifelse(dfEncoded$change=='decrease', 1,2)
#)

#1-increase , 0-decrease
dfEncoded$change<-ifelse(dfEncoded$change=='increase', 1,0)

dfEncoded$change<-as.factor(dfEncoded$change)

#test-train split 0.1

set.seed(1)
trainindex<-createDataPartition(dfEncoded$change,p=0.9,list=FALSE)
traindf<-dfEncoded[trainindex,]
train_label=traindf[,"change"]
traindf<-subset(traindf,select=-c(change))
train_matrix <- xgb.DMatrix(data =as.matrix(traindf), label = train_label)
dim(train_matrix)

testdf<-dfEncoded[-trainindex,]
test_label<-testdf[,"change"]
testdf<-subset(testdf,select=-c(change))
test_matrix <- xgb.DMatrix(data = as.matrix(testdf), label =test_label )

dim(testdf)
#logistic regression
set.seed(1)
set.seed(1)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
fit <- glm(change~., data=traindf, family = "binomial")
cv.fit<-cv.glm(data=traindf, glmfit=fit, cost=cost, K=5)
cv.fit$delta[1]
1-cv.fit$delta[1]
glm.probs=predict(fit,testdf,type="response")
fitted.results <- ifelse(glm.probs > 0.5,1,0)

#Boosting
set.seed(1)
library(gbm) 
#gbm.model = gbm(change~., data=traindf, shrinkage=0.01,distribution = "multinomial" ,cv.folds=5, n.trees=500, verbose=F)
#best.iter = gbm.perf(gbm.model, method="cv")
#best.iter
set.seed(1)
traindf$change<-ifelse(traindf$change=='1', 'increase','decrease')
traindf$change<-as.factor(traindf$change)
testdf$change<-ifelse(testdf$change=='1', 'increase','decrease')
testdf$change<-as.factor(testdf$change)
gbmControl <- trainControl(method='cv', number=5, returnResamp='all', summaryFunction = twoClassSummary, classProbs = TRUE)
grid <- expand.grid(n.trees=c(200,300,500,600), shrinkage=c(0.01,0.1,0.2),interaction.depth=c(1,2,4),n.minobsinnode=c(1,10))
gbm.model = train(change~., data=traindf, method="gbm", trControl=gbmControl, verbose=F, 
              tuneGrid=grid )
model1000<-gbm.model
summary(gbm.model)
confusionMatrix(gbm.model)



#Neural Network
library(h2o) 
h2o.init() 
train <- as.h2o(traindf)
test <- as.h2o(testdf)
response <- "change"
predictors <- setdiff(names(traindf), response)
predictors

#HyperParameterSearch
hyper_params <- list(
    hidden=list(10,c(20,20),c(50,50),c(30,30,30),c(25,25,25,25)),
    input_dropout_ratio=c(0,0.05),
    l1=seq(0,1e-4,1e-6),
    l2=seq(0,1e-4,1e-6)
 
)
hyper_params
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100, seed=1234567, stopping_rounds=5, stopping_tolerance=1e-2)
splits <- h2o.splitFrame(train, 0.8, seed=1234)
train_ <- h2o.assign(splits[[1]], "train.hex") # 80%
valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
dl_random_grid <- h2o.grid(
    algorithm="deeplearning",
    grid_id = "dl_grid_random",
    training_frame=train_,
    validation_frame=valid, 
    x=predictors, 
    y=response,
    epochs=50,
    stopping_metric="misclassification",
    stopping_tolerance=1e-2,        ## stop when misc does not improve by >=1% for 2 scoring events
    stopping_rounds=2,
     ## downsample validation set for faster scoring
    score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
    max_w2=10,                      ## can help improve stability for Rectifier
    hyper_params = hyper_params,
    search_criteria = search_criteria
)                                

grid <- h2o.getGrid("dl_grid_random",sort_by="accuracy",decreasing = TRUE)
grid
grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
best_model

n.model <- h2o.deeplearning(x = predictors, y = response, training_frame = train,variable_importances=T,
                             hidden=c(30,30,30),  l1=0.000076,                 
                            l2=0.000024,max_w2=10 ) 
summary(n.model)

 
#neural networkon Test Set
predictions <- h2o.predict(n.model,test)
perf <- h2o.performance(n.model, test)
plot(perf,main="ROC for Neural Network")
h2o.auc(perf)
h2o.confusionMatrix(n.model, test)
head(h2o.metric(perf))
h2o.varimp(n.model)
# Gradient Boosted Model on Test Results
ypred<-predict(gbm.model,newdata=testdf,n.trees=600)
misClasificError <- mean(ypred != testdf$change)
print(paste('Accuracy',1-misClasificError))

ypredNumeric<-ifelse(ypred=='increase', 1,0)
labels<-ifelse(testdf$change=='increase', 1,0)
table(testdf$change,ypred)

library(ROCR)

glm.pr <- prediction(ypredNumeric,labels)
prf <- performance(glm.pr, measure = "tpr", x.measure = "fpr")
plot(prf,main="ROC curve for Gradient Boosted Model")
auc <- performance(glm.pr, measure = "auc")
auc <- auc@y.values[[1]]
auc



