library(shiny)
library(dplyr)
library(glmnet)
library(randomForest)
library(xgboost)

# Load saved models
ss_rf_model <- readRDS("ss_rf_model.rds")
ss_xgb_model <- readRDS("ss_xgb_model.rds")
sm_rf_reg_model <- readRDS("sm_rf_reg_model.rds")
rs_cv_model <- readRDS("rs_cv_model.rds")
rs_xgb_model <- readRDS("rs_xgb_model.rds")
rm_rf_reg_model <- readRDS("rm_rf_reg_model.rds")

ui <- fluidPage(
  
  # Add custom CSS styles
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(to right, #f8f9fa, #e9ecef);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      .title-panel {
        text-align: center;
        font-size: 36px;
        font-weight: bold;
        color: #007BFF;
        text-shadow: 2px 2px 4px #6c757d;
        margin-bottom: 30px;
      }
      .sidebar-panel {
        background: #ffffff;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
      }
      .main-panel {
        background: #ffffff;
        padding: 20px;
        border-radius: 12px;
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.1);
      }
      .btn-primary {
        background-color: #28a745 !important;
        border-color: #28a745 !important;
        font-weight: bold;
        font-size: 16px;
        width: 100%;
        margin-top: 15px;
        border-radius: 8px;
      }
      .btn-primary:hover {
        background-color: #218838 !important;
      }
      h3 {
        color: #343a40;
        font-weight: bold;
      }
      /* Prediction button */
      .btn-primary {
        background-color: #3498db;
        border-color: #2980b9;
        font-size: 18px;
        padding: 10px 20px;
        transition: background-color 0.3s ease;
      }
      .btn-primary:hover {
        background-color: #2980b9;
        border-color: #1c5980;
      }
      /* Main panel output text */
      #prediction_results {
        background-color: #f8f9fa;
        border: 2px dashed #74b9ff;
        padding: 20px;
        font-size: 16px;
        color: #2d3436;
        border-radius: 10px;
        margin-top: 20px;
      }
      .sidebar-panel {
        background-color: #f1f2f6;
        border: 2px solid #ced6e0;
        border-radius: 10px;
        box-shadow: 2px 2px 10px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  # Title panel
  div(class = "title-panel",
      "Clinical Decision Support System for Breast Cancer Outcomes"
  ),
  
  # Layout
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-panel",
          h3("Enter Patient Details:"),
          numericInput("age", "Age at Diagnosis:", 50, min = 1, max = 100),
          numericInput("tumor_size", "Tumor Size (cm):", 3.5, min = 0.1, max = 10),
          selectInput("cancer_type", "Cancer Type Detailed:",
                      choices = c("Breast", "Breast Invasive Ductal Carcinoma",
                                  "Breast Invasive Lobular Carcinoma", "Breast Invasive Mixed Mucinous Carcinoma",
                                  "Breast Mixed Ductal and Lobular Carcinoma", "Invasive Breast Carcinoma", "Others")),
          numericInput("lymph_nodes", "Lymph Nodes Examined Positive:", 2, min = 0, max = 50),
          numericInput("npi", "Nottingham Prognostic Index:", 4.5, min = 1, max = 10),
          selectInput("tumor_stage", "Tumor Stage:", choices = c("Stage I", "Stage II", "Stage III", "Stage IV")),
          selectInput("grade", "Histologic Grade:", choices = c(1, 2, 3)),
          selectInput("er", "ER Status:", choices = c("Negative" = 0, "Positive" = 1)),
          selectInput("pr", "PR Status:", choices = c("Negative" = 0, "Positive" = 1)),
          selectInput("her2", "HER2 Status:", choices = c("Negative" = 0, "Positive" = 1)),
          selectInput("chemo", "Received Chemotherapy?", choices = c("No" = 0, "Yes" = 1)),
          selectInput("hormone", "Received Hormone Therapy?", choices = c("No" = 0, "Yes" = 1)),
          selectInput("radio", "Received Radiotherapy?", choices = c("No" = 0, "Yes" = 1)),
          selectInput("menopause", "Menopausal Status:", choices = c("Pre" = 0, "Post" = 1)),
          
          actionButton("predict_btn", "Predict Outcome")
      )
    ),
    
    mainPanel(
      div(class = "main-panel",
          h3("Prediction Results"),
          verbatimTextOutput("prediction_results")
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    # Get the age input value
    age_value <- input$age
    
    # Check if the value is within the valid range
    if (age_value < 1 || age_value > 100) {
      # Prompt the user to enter a valid age
      showModal(modalDialog(
        title = "Invalid Age",
        "Please enter a valid age between 10 and 100.",
        easyClose = TRUE,
        footer = NULL
      ))
      
      # Reset to default value if outside the range
      updateNumericInput(session, "age", value = 50)
    }
  })
  observeEvent(input$predict_btn, {
    
    new_data <- data.frame(
      Age_at_Diagnosis = input$age,
      Tumor_Size = input$tumor_size,
      Cancer_Type_Detailed = factor(input$cancer_type,
                                    levels = c("Breast", "Breast Invasive Ductal Carcinoma",
                                               "Breast Invasive Lobular Carcinoma", "Breast Invasive Mixed Mucinous Carcinoma",
                                               "Breast Mixed Ductal and Lobular Carcinoma", "Invasive Breast Carcinoma", "Others")),
      Lymph_nodes_examined_positive = input$lymph_nodes,
      Nottingham_prognostic_index = input$npi,
      Tumor_Stage = factor(input$tumor_stage, levels = c("Stage I", "Stage II", "Stage III", "Stage IV")),
      Neoplasm_Histologic_Grade = factor(input$grade, levels = c(1, 2, 3)),
      ER = factor(as.numeric(input$er), levels = c(0,1)),
      PR = factor(as.numeric(input$pr), levels = c(0,1)),
      HER2 = factor(as.numeric(input$her2), levels = c(0,1)),
      Chemo = factor(as.numeric(input$chemo), levels = c(0,1)),
      Hormone = factor(as.numeric(input$hormone), levels = c(0,1)),
      Radio = factor(as.numeric(input$radio), levels = c(0,1)),
      Menopause = factor(as.numeric(input$menopause), levels = c(0,1))
    )
    
    new_data <- new_data %>%
      select(Age_at_Diagnosis, Tumor_Size, Cancer_Type_Detailed,
             Lymph_nodes_examined_positive, Nottingham_prognostic_index, Tumor_Stage, 
             Neoplasm_Histologic_Grade, ER, PR, HER2, Chemo, Hormone, Radio, Menopause)
    
    new_x <- model.matrix(~ . - 1, data = new_data)
    rs_prediction <- predict(rs_cv_model, newx = new_x, s = "lambda.min", type = "response")
    rs_class <- ifelse(rs_prediction > 0.5, "Relapse Predicted (Recurrent Disease)", "No Relapse Predicted (Disease-Free)")
    
    rm_prediction <- predict(rm_rf_reg_model, newdata = new_data)
    sm_prediction <- predict(sm_rf_reg_model, newdata = new_data)
    ss_prediction <- predict(ss_xgb_model, newdata = new_x)
    ss_class <- ifelse(ss_prediction == 1, "Alive", "Deceased")
    
    output$prediction_results <- renderPrint({
      
      total_days <- sm_prediction * 30.44
      months <- floor(total_days / 30.44)
      days <- round(total_days %% 30.44)
      
      hormone_code <- ifelse(input$hormone == 1, "Z79.890 (Hormone Therapy Ongoing)", "Not Applicable")
      
      cancer_type_code <- switch(
        input$cancer_type,
        "Breast" = "C50.9 (Malignant neoplasm of breast, unspecified site)",
        "Breast Invasive Ductal Carcinoma" = "C50.911 (Invasive ductal carcinoma of breast, unspecified site)",
        "Breast Invasive Lobular Carcinoma" = "C50.912 (Invasive lobular carcinoma of breast, unspecified site)",
        "Breast Invasive Mixed Mucinous Carcinoma" = "C50.9 (Rare mucinous and mixed carcinoma of breast)",
        "Breast Mixed Ductal and Lobular Carcinoma" = "C50.9 (Mixed ductal-lobular breast carcinoma)",
        "Invasive Breast Carcinoma" = "C50.9 (General invasive breast carcinoma)",
        "Others" = "C50.9 (Other malignant neoplasms of breast)"
      )
      
      chemo_status <- ifelse(input$chemo == 1, "Chemotherapy Administered", "No Chemotherapy")
      radio_status <- ifelse(input$radio == 1, "Radiotherapy Administered", "No Radiotherapy")
      
      cat("=== Oncological Prognostic Summary ===\n")
      
      cat("Primary Diagnosis: ", cancer_type_code, "\n \n")
      cat("Tumor Stage: ", input$tumor_stage, "\n \n")
      cat("Histologic Grade: Grade ", input$grade, "\n \n")
      
      cat("\n--- Predicted Clinical Outcomes ---\n")
      cat("Relapse Prediction: ", rs_class, "\n")
      
      # Report the relapse-free interval (in months) and the overall survival duration
      cat("Predicted Relapse-Free Interval: ", round(rm_prediction, 1), " months\n \n")
      
      # cat("Predicted Survival Duration: ", months, " months and ", days, " days remaining\n \n")
      
      # Specific relapse-free time in months and days
      relapse_free_months <- floor(rm_prediction)
      relapse_free_days <- round((rm_prediction - relapse_free_months) * 30.44)
      
      cat("Specific Relapse-Free Time: ", relapse_free_months, " months and ", relapse_free_days, " days\n \n")
      
      # Only report overall survival time without mentioning vital status
      cat("Overall Survival Duration: ", months, " months and ", days, " days.\n")
      
      cat("\n--- Treatment Summary ---\n")
      if (chemo_status == "Chemotherapy Administered") cat("- Chemotherapy: Completed or ongoing\n")
      if (radio_status == "Radiotherapy Administered") cat("- Radiotherapy: Completed or ongoing\n")
      if (hormone_code != "Not Applicable") cat("- Hormonal Therapy: ", hormone_code, "\n")
      if (input$menopause == 1) {
        cat("- Menopausal Status: Postmenopausal\n")
      } else {
        cat("- Menopausal Status: Premenopausal\n")
      }
      
      cat("\n--- Clinical Recommendations ---\n")
      
      cat("- Schedule regular surveillance imaging and oncological follow-up visits.\n")
      cat("- Monitor endocrine therapy adherence if applicable.\n")
      cat("- Evaluate for late effects of adjuvant therapies (cardiotoxicity, bone health).\n")
      
      cat("\n--- Clinical Codes ---\n")
      cat("* Primary Cancer ICD-10 Code: ", cancer_type_code, "\n")
      if (hormone_code != "Not Applicable") {
        cat("* Hormonal Therapy ICD-10 Code: ", hormone_code, "\n")
      }
    })
  })
  
}

shinyApp(ui = ui, server = server)
