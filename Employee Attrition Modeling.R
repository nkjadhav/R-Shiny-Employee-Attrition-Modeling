
# Importing data
d <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")

# Dropping the the columns with constant values or no variability.
library(caret)
head(d[nearZeroVar(d)])

d$Over18 <- NULL
d$EmployeeCount <- NULL
d$StandardHours <- NULL
d$EmployeeNumber <- NULL

# Next, change all the categorical variables to number as fllows,
sapply(d, function(x) is.factor(x))

# https://dzone.com/articles/r-filtering-data-frames-column

d$Attrition <- as.integer(as.factor(d$Attrition)) - 1 # Subtracting 1 from converted value as Yes = 2 and No = 1
d$BusinessTravel <- as.integer(as.factor(d$BusinessTravel))
d$Department <- as.integer(as.factor(d$Department))
d$EducationField <- as.integer(as.factor(d$EducationField))
d$Gender <- as.integer(as.factor(d$Gender))
d$JobRole <- as.integer(as.factor(d$JobRole))
d$MaritalStatus <- as.integer(as.factor(d$MaritalStatus)) 
d$OverTime <- as.integer(as.factor(d$OverTime))

# draw the correlation matrix plot
library(corrplot)
corrplot(cor(d), method = "circle", tl.col = "#3982B7", mar = c(2, 0, 0, 0), tl.cex = 0.8)

library(ggplot2)

# construct correlation plot using ggplot2 stat_bin2d
ggplot(d, aes(YearsInCurrentRole, YearsWithCurrManager))+ 
stat_bin2d(bins = c(15, 10))+           #set bin numbers
guides(colour = guide_legend(override.aes = list(alpha = 1)),
fill = guide_legend(override.aes = list(alpha = 1)))+
theme_bw()+theme(axis.text=element_text(size=10),
axis.title=element_text(size=10,face="bold"),
legend.text=element_text(size=10),legend.title=element_text(size=14),
legend.position = "bottom")+
xlab("YearsInCurrentRole")+ylab("YearsWithCurrManager")

# construct violin plot using ggplot2 geom_violin
ggplot(d, aes(factor(Attrition), WorkLifeBalance))+   
geom_violin(alpha = 0.2, aes(fill = factor(Attrition)))+ 
#set violin plot
theme_bw()+                         #set theme and legend
guides(fill=FALSE)+theme(axis.text=element_text(size=10),
axis.title=element_text(size=10,face="bold"),
legend.text=element_text(size=10),
legend.title=element_text(size=14),legend.position = "bottom")+
xlab("Attrition")   

# load packages
library("xgboost")
library("e1071")
library("MASS")
library("xtable")

# Create data for training and test
set.seed(0)
tr.number<-sample(nrow(d),nrow(d)*2/3)  
# we split whole dataset into 2/3 training data and 1/3 testing data
train<-d[tr.number,]
test<-d[-tr.number,]

column_names = names(test)

# split dataset
train_Y = as.numeric(train$Attrition)
train$Attrition<-NULL
test_Y = test$Attrition
test$Attrition<-NULL

# numericize training and testing data
train[] <- lapply(train, as.numeric)
test[] <- lapply(test, as.numeric)

# Construct xgb.DMatrix object from training matrix
dtrain <- xgb.DMatrix(as.matrix(train), label = train_Y)

# Create a list of parameters for XGBoost model
param <- list(max_depth=3, 
              silent=1, 
              eta = 0.3,
              objective='binary:logistic',
              eval_metric = 'auc')

# Training a XGBoost model using training dataset and chosen parameters
bst <- xgb.train(param, nrounds = 82, dtrain)

# Predicting the results using testing dataset
pred.xgb <- predict(bst, as.matrix(test))

# Create a table indicating the most important features of XGBoost model
importance <- xgb.importance(feature_names = column_names, model = bst)

#We can show the first 5 predictions here as,

head(pred.xgb)
cat("The result shows us the first five predicted probabilities for the test dataset. For example, the first observation’s prediction is", {pred.xgb[1]}, "based on our model, that employee will have a chance to attrite by around",{ceiling(pred.xgb[1]*100)},"%")

xgb.plot.importance(importance_matrix = importance, top_n = 10)

train$Attrition<-train_Y

# Training a SVM 
svm_model<-svm(Attrition~.,                #set model formula
                type="C-classification",   #set classification machine
                gamma=0.001668101,         #set gamma parameter
                cost=35.93814,             #set cost parameter
                data=train,
                cross=3,                   #3-fold cross validation
                probability = TRUE        #allow for probability prediction
)

# Predicting the results using testing dataset
# Obtain the predicted class 0/1
svm_model.predict<-predict(svm_model, test, probability=TRUE) 

# Obtain the predicted probability for class 0/1
svm_model.prob <-attr(svm_model.predict,"probabilities") 
svm_model

# Training a logistic regression model
LR_model <- glm(Attrition ~.,family=binomial(link='logit'),data=train)

# Predicting the results using testing dataset
LR_model.predict <- predict(LR_model, test, type = "response")
coef(LR_model)

# load packages
library("ggplot2"); 
library("corrplot"); 
library("ROCR"); 
library("caret")

# Create a prediction object using previously saved results
ROCRpred_xgb <- prediction(pred.xgb, test_Y)
ROCRpred_svm <- prediction(svm_model.prob[,2], test_Y)
ROCRpred_lr <- prediction(LR_model.predict, test_Y)

#XGBoost roc data
perf_xgb <- performance(ROCRpred_xgb, 'tpr','fpr')                  
roc_xgb.data <- data.frame(fpr=unlist(perf_xgb@x.values),
                tpr=unlist(perf_xgb@y.values), model="XGBoost")
                    
#SVM roc data
perf_svm <- performance(ROCRpred_svm, 'tpr','fpr')                  
roc_svm.data <- data.frame(fpr=unlist(perf_svm@x.values),
                tpr=unlist(perf_svm@y.values), model="SVM")
                    
#Logistic Regression roc data
perf_lr <- performance(ROCRpred_lr, 'tpr','fpr')                    
roc_lr.data <- data.frame(fpr=unlist(perf_lr@x.values),

                          tpr=unlist(perf_lr@y.values), model="LR")

# Plotting ROC Curve

# Define colors for roc plot
cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")

# Create roc plot
ggplot() + 
geom_line(data = roc_xgb.data, aes(x=fpr, y=tpr, colour = "XGBoost")) + #set XGBoost roc curve
geom_line(data = roc_svm.data, aes(x = fpr, y=tpr, colour = "SVM")) + #set SVM roc curve
geom_line(data = roc_lr.data, aes(x = fpr, y=tpr, colour = "Logistic Regression")) + 

#set LR roc curve
geom_vline(xintercept = 0.5, color = "red", linetype=2) + theme_bw() + #set themes
scale_colour_manual(name = "Models", values = cols) + 
xlab("False Positive Rate") +
ylab("True Positive Rate") +
theme(legend.position = c(0.8, 0.2), 
legend.text = element_text(size = 15), 
legend.title = element_text(size = 15))

# Define a function to obtain the cutoff probability
# @perf is a S4 object gotten from @performance function
# @threshold is the targeted fpr
# In the ShinyApp, users can adjust the threshold by themselves and
# obtain different confusion matrix accordingly. Here, we always set
# threshold = 0.5 just for illustration.
get_cutoff_point <- function(perf, threshold)
  {
    cutoffs <- data.frame(cut=perf@alpha.values[[1]], fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
    cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
    cutoffs <- subset(cutoffs, fpr <= threshold)
    if(nrow(cutoffs) == 0){ return(1.0)}
    else return(cutoffs[1, 1])
}

# Define a function to draw a confusion matrix plot
# @cm is a confusion matrix obtained from @confusionMatrix function
# @auc is the auc value obtained from @performance function
# @color is the kind of color you want for true positive and true negative areas
# In this function, we also add in accuracy information which calculates the
# overall performance of model
draw_confusion_matrix <- function(cm, auc, color) {

  layout(matrix(c(1,1,2)))
  par(mar=c(0,0.1,1,0.1))
  plot(c(125, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')

  # create the matrix 
  rect(150, 430, 240, 370, col=color)
  text(195, 435, '0', cex=1.2)
  rect(250, 430, 340, 370, col='white')
  text(295, 435, '1', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='white')
  rect(250, 305, 340, 365, col=color)
  text(140, 400, '0', cex=1.2, srt=90)
  text(140, 335, '1', cex=1.2, srt=90)

  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='black')
  text(295, 400, res[3], cex=1.6, font=2, col='black')
  text(295, 335, res[4], cex=1.6, font=2, col='white')

  # add in the specifics 
  plot(c(0, 100), c(0, 50), type = "n", xlab="", ylab="", main = "", xaxt='n', yaxt='n')

  # add in the accuracy information 

  text(25, 30, "AUC", cex=1.8, font=2)
  text(25, 20, round(as.numeric(auc), 3), cex=1.8)
  text(75, 30, names(cm$overall[1]), cex=1.8, font=2)
  text(75, 20, round(as.numeric(cm$overall[1]), 3), cex=1.8)
}

# draw XGBoosting confusion matrix
auc_xgb <- performance(ROCRpred_xgb, measure = "auc")  #obtain auc from @performance
perf_xgb <- performance(ROCRpred_xgb, 'tpr','fpr')  #obtain tpr and fpr from @performance                   
cut <- get_cutoff_point(perf_xgb, 1) #obtain the cutoff probability
pred_values_xgb <- ifelse(pred.xgb > cut,1,0) #classify using cutoff probability
cm_xgb <- confusionMatrix(data = factor(pred_values_xgb), reference = factor(test_Y)) #obtain confusion matrix
draw_confusion_matrix(cm_xgb, auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot

# draw SVM confusion matrix
auc_svm <- performance(ROCRpred_svm, measure = "auc")
perf_svm <- performance(ROCRpred_svm, 'tpr','fpr')                  
cut <- get_cutoff_point(perf_svm, 0.5)
pred_values_svm <- ifelse(svm_model.prob[,2] > cut,1,0)
cm_svm <- confusionMatrix(data = factor(pred_values_svm), reference = factor(test_Y))
draw_confusion_matrix(cm_svm, auc_svm@y.values, "#FF8849")

# draw Logistic regression confusion matrix
auc_lr <- performance(ROCRpred_lr, measure = "auc")
perf_lr <- performance(ROCRpred_lr, 'tpr','fpr')                    
cut <- get_cutoff_point(perf_lr, 0.5)
pred_values_lr <- ifelse(LR_model.predict > cut,1,0)
cm_lr <- confusionMatrix(data = factor(pred_values_lr), reference = factor(test_Y))
draw_confusion_matrix(cm_lr, auc_lr@y.values, "#69BE28")

#Create precision plot

#XGBoost
perf_xgb <- performance(ROCRpred_xgb,'prec', 'cutoff') #use 'prec' and 'cutoff' as measurements                 
xgb.data <- data.frame(x=unlist(perf_xgb@x.values), y=unlist(perf_xgb@y.values),
            model="XGBoost")
                    
#SVM
perf_svm <- performance(ROCRpred_svm,'prec', 'cutoff')                  
svm.data <- data.frame(x=unlist(perf_svm@x.values), y=unlist(perf_svm@y.values), 
            model="SVM")
                    
#Logistic Regression
perf_lr <- performance(ROCRpred_lr,'prec', 'cutoff')                    
lr.data <- data.frame(x=unlist(perf_lr@x.values), y=unlist(perf_lr@y.values),
           model="LR")
                    

cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
                    
ggplot() +
geom_line(data = xgb.data, aes(x=x, y=y, colour = "XGBoost")) + 
geom_line(data = svm.data, aes(x =x, y=y, colour = "SVM")) + 
geom_line(data = lr.data, aes(x =x, y=y, colour = "Logistic Regression")) + 
scale_colour_manual(name = "Models", values = cols) + 
xlab("Cutoff") +
ylab("Precision") +
geom_vline(xintercept = 0.5, color = "red", linetype=2) + theme_bw() +
theme(legend.position = c(0.8, 0.2), 
legend.text = element_text(size = 15), 
legend.title = element_text(size = 15))

#Create recall plot

#XGBoost
perf_xgb <- performance(ROCRpred_xgb,'rec', 'cutoff')                   
xgb.data <- data.frame(x=unlist(perf_xgb@x.values), y=unlist(perf_xgb@y.values), model="XGBoost")
                    
#SVM
perf_svm <- performance(ROCRpred_svm,'rec', 'cutoff')                   
svm.data <- data.frame(x=unlist(perf_svm@x.values), y=unlist(perf_svm@y.values), model="SVM")
                    
#Logistic Regression
perf_lr <- performance(ROCRpred_lr,'rec', 'cutoff')                 
lr.data <- data.frame(x=unlist(perf_lr@x.values), y=unlist(perf_lr@y.values), model="LR")

cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")

ggplot() +
geom_line(data = xgb.data, aes(x=x, y=y, colour = "XGBoost")) + 
geom_line(data = svm.data, aes(x=x, y=y, colour = "SVM")) + 
geom_line(data = lr.data, aes(x=x, y=y, colour = "Logistic Regression")) + 
scale_colour_manual(name = "Models", values = cols) + 
xlab("Cutoff") +
ylab("Recall") +
geom_vline(xintercept = 0.5, color = "red", linetype=2) + theme_bw() +
theme(legend.position = c(0.8, 0.8), 
legend.text = element_text(size = 15), 
legend.title = element_text(size = 15))
