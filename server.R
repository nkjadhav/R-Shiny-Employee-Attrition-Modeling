library(shiny)
library(ggplot2)
library(shinythemes)

function(input, output){
  
  # load packages
  library("xgboost")
  library("e1071")
  library("MASS")
  library("xtable")
  library("ggplot2")
  library("ROCR")
  library("caret")
  
  # Importing data
  d <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
  
  # Dropping the the columns with constant values or no variability.
  d$Over18 <- NULL
  d$EmployeeCount <- NULL
  d$StandardHours <- NULL
  d$EmployeeNumber <- NULL
  
  # Next, change all the categorical variables to number as fllows,
  d$Attrition <- as.integer(as.factor(d$Attrition)) - 1 # Subtracting 1 from converted value as Yes = 2 and No = 1
  d$BusinessTravel <- as.integer(as.factor(d$BusinessTravel))
  d$Department <- as.integer(as.factor(d$Department))
  d$EducationField <- as.integer(as.factor(d$EducationField))
  d$Gender <- as.integer(as.factor(d$Gender))
  d$JobRole <- as.integer(as.factor(d$JobRole))
  d$MaritalStatus <- as.integer(as.factor(d$MaritalStatus)) 
  d$OverTime <- as.integer(as.factor(d$OverTime))
  
  # Create data for training and test
  set.seed(0)
  tr.number<-sample(nrow(d),nrow(d)*2/3)  
  # we split whole dataset into 2/3 training data and 1/3 testing data
  train<-d[tr.number,]
  test<-d[-tr.number,]
  
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
  
  # Training a logistic regression model
  LR_model <- glm(Attrition ~.,family=binomial(link='logit'),data=train)
  
  # Predicting the results using testing dataset
  LR_model.predict <- predict(LR_model, test, type = "response")
  
  # Create a prediction object using previously saved results
  ROCRpred_xgb <- prediction(pred.xgb, test_Y)
  ROCRpred_svm <- prediction(svm_model.prob[,2], test_Y)
  ROCRpred_lr <- prediction(LR_model.predict, test_Y)
  
  # Plotting ROC Curve
  
  # Define colors for roc plot
  cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
  
  xintercept_roc <- reactive({
    input$thresh_roc
  })
  
  xintercept_precision <- reactive({
    input$thresh_precision
  })
  
  xintercept_recall <- reactive({
    input$thresh_recall
  })
  # Define a function to obtain the cutoff probability
  # @perf is a S4 object gotten from @performance function
  # @threshold is the targeted fpr
  # In the ShinyApp, users can adjust the threshold by themselves and
  # obtain different confusion matrix accordingly. Here, we always set
  # threshold = 0.5 just for illustration.
  get_cutoff_point <- function(perf, threshold)
  {
    cutoffs <- data.frame(fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
    cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
    cutoffs <- subset(cutoffs, fpr <= threshold)
    if(nrow(cutoffs) == 0){ return(1.0)}
    else return(cutoffs[1, 1])
  }
  
  # ROCRpred_xgb@cutoffs[[1]]
  
  #-------------------------------------------------------
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
  #-------------------------------------------------------
  
  
  
  
  # Create roc plot
  #-------------------------------------------------------
  #XGBoost roc data
  roc_perf_xgb <- performance(ROCRpred_xgb, 'tpr','fpr')                  
  roc_xgb.data <- data.frame(fpr=unlist(roc_perf_xgb@x.values),
                             tpr=unlist(roc_perf_xgb@y.values), model="XGBoost")
  
  #SVM roc data
  roc_perf_svm <- performance(ROCRpred_svm, 'tpr','fpr')                  
  roc_svm.data <- data.frame(fpr=unlist(roc_perf_svm@x.values),
                             tpr=unlist(roc_perf_svm@y.values), model="SVM")
  
  #Logistic Regression roc data
  roc_perf_lr <- performance(ROCRpred_lr, 'tpr','fpr')                    
  roc_lr.data <- data.frame(fpr=unlist(roc_perf_lr@x.values),
                            tpr=unlist(roc_perf_lr@y.values), model="LR")
  
  output$plot_roc<-renderPlot({ggplot() + 
      geom_line(data = roc_xgb.data, aes(x=fpr, y=tpr, colour = "XGBoost")) + #set XGBoost roc curve
      geom_line(data = roc_svm.data, aes(x = fpr, y=tpr, colour = "SVM")) + #set SVM roc curve
      geom_line(data = roc_lr.data, aes(x = fpr, y=tpr, colour = "Logistic Regression")) + 
      
      #set LR roc curve
      geom_vline(xintercept = xintercept_roc(), color = "red", linetype=2) + theme_bw() + #set themes
      scale_colour_manual(name = "Models", values = cols) + 
      xlab("False Positive Rate") +
      ylab("True Positive Rate") +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
  # draw XGBoosting confusion matrix
  output$confusionMatrix_roc_xgb<-renderPlot({
    roc_auc_xgb <- performance(ROCRpred_xgb, measure = "auc")  #obtain auc from @performance
    roc_perf_xgb <- performance(ROCRpred_xgb, 'tpr','fpr')  #obtain tpr and fpr from @performance                   
    roc_cut <- get_cutoff_point(roc_perf_xgb, xintercept_roc()) #obtain the cutoff probability
    roc_pred_values_xgb <- ifelse(pred.xgb > roc_cut,1,0) #classify using cutoff probability
    roc_cm_xgb <- confusionMatrix(data = factor(roc_pred_values_xgb), reference = factor(test_Y)) #obtain confusion matrix
    draw_confusion_matrix(roc_cm_xgb, roc_auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot
  })
  
  # draw SVM confusion matrix
  output$confusionMatrix_roc_svm<-renderPlot({
    roc_auc_svm <- performance(ROCRpred_svm, measure = "auc")
    roc_perf_svm <- performance(ROCRpred_svm, 'tpr','fpr')                  
    roc_cut <- get_cutoff_point(roc_perf_svm, xintercept_roc())
    roc_pred_values_svm <- ifelse(svm_model.prob[,2] > roc_cut,1,0)
    roc_cm_svm <- confusionMatrix(data = factor(roc_pred_values_svm), reference = factor(test_Y))
    draw_confusion_matrix(roc_cm_svm, roc_auc_svm@y.values, "#FF8849")
  })
  
  # draw Logistic regression confusion matrix
  output$confusionMatrix_roc_lr<-renderPlot({
    roc_auc_lr <- performance(ROCRpred_lr, measure = "auc")
    roc_perf_lr <- performance(ROCRpred_lr, 'tpr','fpr')                    
    roc_cut <- get_cutoff_point(roc_perf_lr, xintercept_roc())
    roc_pred_values_lr <- ifelse(LR_model.predict > roc_cut,1,0)
    roc_cm_lr <- confusionMatrix(data = factor(roc_pred_values_lr), reference = factor(test_Y))
    draw_confusion_matrix(roc_cm_lr, roc_auc_lr@y.values, "#69BE28")
  })
  
  #-------------------------------------------------------
  
  #Create precision plot
  #-------------------------------------------------------
  #XGBoost
  precision_perf_xgb <- performance(ROCRpred_xgb, "prec", "cutoff") #use 'prec' and 'cutoff' as measurements                 
  precision_xgb.data <- data.frame(x=unlist(precision_perf_xgb@x.values), y=unlist(precision_perf_xgb@y.values),
                                   model="XGBoost")
  
  #SVM
  precision_perf_svm <- performance(ROCRpred_svm, "prec", "cutoff")                  
  precision_svm.data <- data.frame(x=unlist(precision_perf_svm@x.values), y=unlist(precision_perf_svm@y.values),
                                   model="SVM")
  
  #Logistic Regression
  precision_perf_lr <- performance(ROCRpred_lr, "prec", "cutoff")                    
  precision_lr.data <- data.frame(x=unlist(precision_perf_lr@x.values), y=unlist(precision_perf_lr@y.values),
                                  model="LR")
  
  
  cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
  
  output$plot_precision<-renderPlot({ggplot() +
      geom_line(data = precision_xgb.data, aes(x=x, y=y, colour = "XGBoost")) + 
      geom_line(data = precision_svm.data, aes(x =x, y=y, colour = "SVM")) + 
      geom_line(data = precision_lr.data, aes(x =x, y=y, colour = "Logistic Regression")) + 
      scale_colour_manual(name = "Models", values = cols) + 
      xlab("Cutoff") +
      ylab("Precision") +
      geom_vline(xintercept = xintercept_precision(), color = "red", linetype=2) + theme_bw() +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
  
  # draw XGBoosting confusion matrix
  output$confusionMatrix_precision_xgb<-renderPlot({
    precision_auc_xgb <- performance(ROCRpred_xgb, measure = "auc")  #obtain auc from @performance
    precision_perf_xgb <- performance(ROCRpred_xgb, "prec", "cutoff") #use 'prec' and 'cutoff' as measurements                 
    precision_cut <- get_cutoff_point(precision_perf_xgb, xintercept_precision()) #obtain the cutoff probability
    precision_pred_values_xgb <- ifelse(pred.xgb >  precision_cut,1,0) #classify using cutoff probability
    precision_cm_xgb <- confusionMatrix(data = factor(precision_pred_values_xgb), reference = factor(test_Y)) #obtain confusion matrix
    draw_confusion_matrix(precision_cm_xgb, precision_auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot
  })
  
  # draw SVM confusion matrix
  output$confusionMatrix_precision_svm<-renderPlot({
    precision_auc_svm <- performance(ROCRpred_svm, measure = "auc")
    precision_perf_svm <- performance(ROCRpred_svm, "prec", "cutoff")                  
    precision_cut <- get_cutoff_point(precision_perf_svm, xintercept_precision())
    precision_pred_values_svm <- ifelse(svm_model.prob[,2] >  precision_cut,1,0)
    precision_cm_svm <- confusionMatrix(data = factor(precision_pred_values_svm), reference = factor(test_Y))
    draw_confusion_matrix(precision_cm_svm, precision_auc_svm@y.values, "#FF8849")
  })
  
  # draw Logistic regression confusion matrix
  output$confusionMatrix_precision_lr<-renderPlot({
    precision_auc_lr <- performance(ROCRpred_lr, measure = "auc")
    precision_perf_lr <- performance(ROCRpred_lr, "prec", "cutoff")                    
    precision_cut <- get_cutoff_point(precision_perf_lr, xintercept_precision())
    precision_pred_values_lr <- ifelse(LR_model.predict >  precision_cut,1,0)
    precision_cm_lr <- confusionMatrix(data = factor(precision_pred_values_lr), reference = factor(test_Y))
    draw_confusion_matrix(precision_cm_lr, precision_auc_lr@y.values, "#69BE28")
  })
  
  
  
  
  
  
  
  #-------------------------------------------------------
  
  #Create recall plot
  #-------------------------------------------------------
  #XGBoost
  recall_perf_xgb <- performance(ROCRpred_xgb, "rec", "cutoff") #use 'rec' and 'cutoff' as measurements                 
  recall_xgb.data <- data.frame(x=unlist(recall_perf_xgb@x.values), y=unlist(recall_perf_xgb@y.values),
                                model="XGBoost")
  
  #SVM
  recall_perf_svm <- performance(ROCRpred_svm, "rec", "cutoff")                  
  recall_svm.data <- data.frame(x=unlist(recall_perf_svm@x.values), y=unlist(recall_perf_svm@y.values),
                                model="SVM")
  
  #Logistic Regression
  recall_perf_lr <- performance(ROCRpred_lr, "rec", "cutoff")                    
  recall_lr.data <- data.frame(x=unlist(recall_perf_lr@x.values), y=unlist(recall_perf_lr@y.values),
                               model="LR")
  
  
  cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
  
  
  output$plot_recall<-renderPlot({ggplot() +
      geom_line(data = recall_xgb.data, aes(x=x, y=y, colour = "XGBoost")) + 
      geom_line(data = recall_svm.data, aes(x =x, y=y, colour = "SVM")) + 
      geom_line(data = recall_lr.data, aes(x =x, y=y, colour = "Logistic Regression")) + 
      scale_colour_manual(name = "Models", values = cols) + 
      xlab("Cutoff") +
      ylab("recall") +
      geom_vline(xintercept = xintercept_recall(), color = "red", linetype=2) + theme_bw() +
      theme(legend.position = c(0.8, 0.8), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
  
  # draw XGBoosting confusion matrix
  output$confusionMatrix_recall_xgb<-renderPlot({
    recall_auc_xgb <- performance(ROCRpred_xgb, measure = "auc")  #obtain auc from @performance
    recall_perf_xgb <- performance(ROCRpred_xgb, "prec", "cutoff") #use 'prec' and 'cutoff' as measurements                 
    recall_cut <- get_cutoff_point(recall_perf_xgb, xintercept_recall()) #obtain the cutoff probability
    recall_pred_values_xgb <- ifelse(pred.xgb > recall_cut,1,0) #classify using cutoff probability
    recall_cm_xgb <- confusionMatrix(data = factor(recall_pred_values_xgb), reference = factor(test_Y)) #obtain confusion matrix
    draw_confusion_matrix(recall_cm_xgb, recall_auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot
  })
  
  # draw SVM confusion matrix
  output$confusionMatrix_recall_svm<-renderPlot({
    recall_auc_svm <- performance(ROCRpred_svm, measure = "auc")
    recall_perf_svm <- performance(ROCRpred_svm, "prec", "cutoff")                  
    recall_cut <- get_cutoff_point(recall_perf_svm, xintercept_recall())
    recall_pred_values_svm <- ifelse(svm_model.prob[,2] > recall_cut,1,0)
    recall_cm_svm <- confusionMatrix(data = factor(recall_pred_values_svm), reference = factor(test_Y))
    draw_confusion_matrix(recall_cm_svm, recall_auc_svm@y.values, "#FF8849")
  })
  
  # draw Logistic regression confusion matrix
  output$confusionMatrix_recall_lr<-renderPlot({
    recall_auc_lr <- performance(ROCRpred_lr, measure = "auc")
    recall_perf_lr <- performance(ROCRpred_lr, "prec", "cutoff")                    
    recall_cut <- get_cutoff_point(recall_perf_lr, xintercept_recall())
    recall_pred_values_lr <- ifelse(LR_model.predict > recall_cut,1,0)
    recall_cm_lr <- confusionMatrix(data = factor(recall_pred_values_lr), reference = factor(test_Y))
    draw_confusion_matrix(recall_cm_lr, recall_auc_lr@y.values, "#69BE28")
  })
}
