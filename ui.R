library(shiny)
library(ggplot2)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  column(
    width = 12,
    titlePanel("Employee Attrition Prediction - ML Models Analysis")
  ),
  # sidebarLayout(
  #   sidebarPanel(
  #     helpText("It is recommend to select 3 or fewer options at a time."),
  #     width = 2,
  #     selectInput(
  #       "Age",
  #       label = "Select an age range",
  #       choices = c("< 35", "35-45", "> 45", "all"),
  #       selected = "all"
  #     ),
  #     selectInput(
  #       "Gender",
  #       label = "Select a gender",
  #       choices = c("Female", "Male", "all"),
  #       selected = "all"
  #     ),
  #     selectInput(
  #       "Education",
  #       label = "Select an education level",
  #       choices = c("1", "2", "3", "4", "all"),
  #       selected = "all"
  #     ),
  #     selectInput(
  #       "MonthlyIncome",
  #       label = "Select a monthly income range",
  #       choices = c(
  #         "< 2500",
  #         "2500-5000",
  #         "5001-7500",
  #         "7501-10000",
  #         "> 10001",
  #         "all"
  #       ),
  #       selected = "all"
  #     ),
  #     selectInput(
  #       "MaritalStatus",
  #       label = "Select a marital status",
  #       choices = c("Single", "Married", "Divorced", "all"),
  #       selected = "all"
  #     )
  #   ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "About",
          h5("Employee attrition is the rate at which employees leave a company. The goal of this analysis is to model employee attrition with different Machine Learning Algorithms. Through this kind of analysis, we can understand not only how many employees are likely to leave, but also which employees are at the highest risk of leaving and why. Companies face significant costs for having to search for, interview and hire new employees. Although this depends on the industry, in general, a company aims to retain their employees for a significant period of time. This analysis is particularly useful if a company wants to lower attrition levels but is unsure of the source of the problem.",
             tags$br(),tags$br(),"The dataset used in this analysis is provided from IBM HR to study about employee attrition, which can be downloaded from URL:",
             tags$br(),tags$br(),"Dataset - https://community.watsonanalytics.com/wp-content/uploads/2015/03/WA_Fn-UseC_-HR-Employee-Attrition.xlsx",
             tags$br(),tags$br(),"In this Shiny Application, analysis of different algorithms used to model employee attrition is done: Extreme Gradient Boosting (XGBoost), Support Vector Machines (SVM), and Logistic Regression.",
             tags$br(),"1. XGBoost uses boosted decision trees which classify the members of a family into different leaves, then assign them a score on that leaf. Multiple trees are ensembled to improve the predictive power of the model.",
             tags$br(),"2. SVM is a discriminative classifier that takes labeled training data and constructs a hyperplane to categorize new examples.",   
             tags$br(),"3. Finally, logistic regression is a simple classifier used to estimate the probability of an binary outcome based on several predictors.",
             tags$br(),tags$br(),"Please Note - Here, on shinyapps.io only stable features of this application are published, although this application is under improvement for further feature addition",
             tags$br(),tags$br(),"Nilkanth Jadhav"
             )),
        tabPanel(
          "Training (ROC)",
          column(
            width = 7,
            class = "well",
            h4("ROC Curve"),
            plotOutput("plot_roc"),
            style = "background-color:white;",
            sliderInput(
              "thresh_roc",
              label = "",
              min = 0,
              max = 1,
              value = c(0.5)
            )
          ),
          column(
            width = 5,
            class = "well",
            tabsetPanel(
              tabPanel(
                "XGBoost",
                h4("Confusion Matrix (XGBoost)"),
                plotOutput("confusionMatrix_roc_xgb"),
                style = "background-color:white;"
              ),
              tabPanel(
                "SVM",
                h4("Confusion Matrix (SVM)"),
                plotOutput("confusionMatrix_roc_svm"),
                style = "background-color:white;"
              ),
              tabPanel(
                "Logistic Regression",
                h4("Confusion Matrix (Logistic Regression)"),
                plotOutput("confusionMatrix_roc_lr"),
                style = "background-color:white;"
              )
            ),
            style = "background-color:white;"
          )
        ),
        tabPanel(
          "Training (Precision)",
          column(
            width = 7,
            class = "well",
            h4("Precision vs Cutoff Curve"),
            plotOutput("plot_precision"),
            style = "background-color:white;",
            sliderInput(
              "thresh_precision",
              label = "",
              min = 0,
              max = 1,
              value = c(0.5)
            )
          ),
          column(
            width = 5,
            class = "well",
            tabsetPanel(
              tabPanel(
                "XGBoost",
                h4("Confusion Matrix (XGBoost)"),
                plotOutput("confusionMatrix_precision_xgb"),
                style = "background-color:white;"
              ),
              tabPanel(
                "SVM",
                h4("Confusion Matrix (SVM)"),
                plotOutput("confusionMatrix_precision_svm"),
                style = "background-color:white;"
              ),
              tabPanel(
                "Logistic Regression",
                h4("Confusion Matrix (Logistic Regression)"),
                plotOutput("confusionMatrix_precision_lr"),
                style = "background-color:white;"
              )
            ),
            style = "background-color:white;"
          )
        ),
        tabPanel(
          "Training (Recall)",
          column(
            width = 7,
            class = "well",
            h4("Recall vs Cutoff Curve"),
            plotOutput("plot_recall"),
            style = "background-color:white;",
            sliderInput(
              "thresh_recall",
              label = "",
              min = 0,
              max = 1,
              value = c(0.5)
            )
          ),
          column(
            width = 5,
            class = "well",
            tabsetPanel(
              tabPanel(
                "XGBoost",
                h4("Confusion Matrix (XGBoost)"),
                plotOutput("confusionMatrix_recall_xgb"),
                style = "background-color:white;"
              ),
              tabPanel(
                "SVM",
                h4("Confusion Matrix (SVM)"),
                plotOutput("confusionMatrix_recall_svm"),
                style = "background-color:white;"
              ),
              tabPanel(
                "Logistic Regression",
                h4("Confusion Matrix (Logistic Regression)"),
                plotOutput("confusionMatrix_recall_lr"),
                style = "background-color:white;"
              )
            ),
            style = "background-color:white;"
          )
          
        )
        
      )
    ))
)
