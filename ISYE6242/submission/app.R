library(shiny)
library(shinydashboard)
library(readxl)
library(plotly)
library(tibble)
library(tidyr)
library(dplyr)
library(tidymodels)
library(xgboost)

source("scripts/helpers/helpers.R")

assessment_options <- readxl::read_xlsx("app_data/variable_list_app_models_adjusted.xlsx")

assessment_questions <- assessment_options %>%
  select(QuestionType, CleanQuestion, `SAS Variable Name`) %>% 
  distinct() %>%
  arrange(QuestionType)

# UI for disclaimer
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("About Us & Disclaimer"),
    div(
      p("This project was developed as part of a class project with the goal to raise awareness about the risk factors associated with diabetes. It is intended for educational purposes only."),
      p("The information provided by our Risk Assessment Tool is based on general patterns and statistical data, and should not be taken as medical advice. Our team consists of students from various disciplines and does not include medical professionals."),
      p("The tool is not a substitute for professional medical advice, diagnosis, or treatment. Always seek the advice of your physician or other qualified health provider with any questions you may have regarding a medical condition."),
      p("By using this tool, you understand and agree that neither the creators of the tool nor any entities they represent have any liability for any decision made or action taken in reliance on the information provided through this tool."),
      p("If you have any questions about how to use the tool or interpret your results, please contact a health professional."),
      p("No personal data will be used or tracked in any way by us or any one at Georgia Tech.")
    ),
    div(ns("acknowledgment"),
        h4("Acknowledgments"),
        p("We would like to thank our instructors and peers for their support and feedback throughout the development of this project.")
    )
  )
}


# UI for Instructions
instructionsUI <- function(id) {
  tagList(
    h2("Instructions for the Risk Assessment Tool"),
    div(
      p("Welcome to the Risk Assessment Tool. This tool is designed to help you understand potential health risks based on your lifestyle, medical history, and other factors."),
      tags$ol(
        tags$li("Navigate to the 'Input: General Information' tab and provide details such as your sex at birth, state of residence, income range, and education level."),
        tags$li("Proceed to the 'Input: Lifestyle and Health' tab to answer questions about your health, physical activity, and habits."),
        tags$li("In the 'Input: Medical History' tab, provide information about any known medical conditions or past health events."),
        tags$li("Review your entries in each section carefully. If you need to make any changes, you can navigate back to the respective tab."),
        tags$li("Once you have completed all sections, navigate to the 'Your Risk-Score' tab."),
        tags$li("Click the 'Submit' button to calculate your risk score. A gauge will display your results, indicating your overall risk level."),
        tags$li("Please note that this tool provides an estimate based on the information provided and is not a substitute for professional medical advice.")
      ),
      p("If you have any questions about how to use the tool or interpret your results, please contact a health professional.")
    )
  )
}


# UI for General Information
generalInfoUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("sexvar")
                , label = filter(assessment_questions, `SAS Variable Name` == "SEXVAR")$CleanQuestion
                , choices = filter(assessment_options, `SAS Variable Name` == "SEXVAR")$CleanValue
                )
    , selectInput(ns("state")
                  , label = filter(assessment_questions, `SAS Variable Name` == "_STATE")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "_STATE")$CleanValue
    )
    , selectInput(ns("income")
                  , label = filter(assessment_questions, `SAS Variable Name` == "_INCOMG1")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "_INCOMG1")$CleanValue
    )
    , selectInput(ns("education")
                  , label = filter(assessment_questions, `SAS Variable Name` == "_EDUCAG")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "_EDUCAG")$CleanValue
    )
    , selectInput(ns("children")
                  , label = filter(assessment_questions, `SAS Variable Name` == "_CHLDCNT")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "_CHLDCNT")$CleanValue
    )
    , selectInput(ns("age")
                  , label = filter(assessment_questions, `SAS Variable Name` == "_AGE80")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "_AGE80")$CleanValue
    )
    , selectInput(ns("employment")
                  , label = filter(assessment_questions, `SAS Variable Name` == "EMPLOY1")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "EMPLOY1")$CleanValue
    )
  )
}


# Server for General Information
generalInfoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      data.frame(
        Question = c(
          filter(assessment_questions, `SAS Variable Name` == "SEXVAR")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "_STATE")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "_INCOMG1")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "_EDUCAG")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "_CHLDCNT")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "_AGE80")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "EMPLOY1")$CleanQuestion
        ),
        Response = c(input$sexvar, input$state, input$income, input$education, input$children, input$age, input$employment),
        stringsAsFactors = FALSE
      )
    })
  })
}


# UI for Lifestyle and Health
lifestyleHealthUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("health")
                , label = filter(assessment_questions, `SAS Variable Name` == "_RFHLTH")$CleanQuestion
                , choices = filter(assessment_options, `SAS Variable Name` == "_RFHLTH")$CleanValue
    )
    , selectInput(ns("bmi")
                  , label = filter(assessment_questions, `SAS Variable Name` == "_BMI5CAT")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "_BMI5CAT")$CleanValue
    )
    , selectInput(ns("alcohol")
                  , label = filter(assessment_questions, `SAS Variable Name` == "DRNKANY6")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "DRNKANY6")$CleanValue
    )
    , selectInput(ns("persdoc")
                  , label = filter(assessment_questions, `SAS Variable Name` == "PERSDOC3")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "PERSDOC3")$CleanValue
    )
    , selectInput(ns("checkup")
                  , label = filter(assessment_questions, `SAS Variable Name` == "CHECKUP1")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "CHECKUP1")$CleanValue
    )
    , selectInput(ns("exercise")
                  , label = filter(assessment_questions, `SAS Variable Name` == "EXERANY2")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "EXERANY2")$CleanValue
    )
  )
}

# Server for Lifestyle and Health
lifestyleHealthServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      data.frame(
        Question = c(
          filter(assessment_questions, `SAS Variable Name` == "_RFHLTH")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "_BMI5CAT")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "DRNKANY6")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "PERSDOC3")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "CHECKUP1")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "EXERANY2")$CleanQuestion
        ),
        Response = c(input$health, input$bmi, input$alcohol, input$persdoc, input$checkup, input$exercise),
        stringsAsFactors = FALSE
      )
    })
  })
}


# UI for Medical History
medicalHistoryUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("CVDINFR4")
                , label = filter(assessment_questions, `SAS Variable Name` == "CVDINFR4")$CleanQuestion
                , choices = filter(assessment_options, `SAS Variable Name` == "CVDINFR4")$CleanValue
    )
    , selectInput(ns("CVDCRHD4")
                  , label = filter(assessment_questions, `SAS Variable Name` == "CVDCRHD4")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "CVDCRHD4")$CleanValue
    )
    , selectInput(ns("CVDSTRK3")
                  , label = filter(assessment_questions, `SAS Variable Name` == "CVDSTRK3")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "CVDSTRK3")$CleanValue
    )
    , selectInput(ns("CHCKDNY2")
                  , label = filter(assessment_questions, `SAS Variable Name` == "CHCKDNY2")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "CHCKDNY2")$CleanValue
    )
    , selectInput(ns("HAVARTH4")
                  , label = filter(assessment_questions, `SAS Variable Name` == "HAVARTH4")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "HAVARTH4")$CleanValue
    )
    , selectInput(ns("COVIDPOS")
                  , label = filter(assessment_questions, `SAS Variable Name` == "COVIDPOS")$CleanQuestion
                  , choices = filter(assessment_options, `SAS Variable Name` == "COVIDPOS")$CleanValue
    )
  )
}

# Server for Medical History
medicalHistoryServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      data.frame(
        Question = c(
          filter(assessment_questions, `SAS Variable Name` == "CVDINFR4")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "CVDCRHD4")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "CVDSTRK3")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "CHCKDNY2")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "HAVARTH4")$CleanQuestion
          , filter(assessment_questions, `SAS Variable Name` == "COVIDPOS")$CleanQuestion
        ),
        Response = c(input$CVDINFR4, input$CVDCRHD4, input$CVDSTRK3, input$CHCKDNY2, input$HAVARTH4, input$COVIDPOS),
        stringsAsFactors = FALSE
      )
    })
  })
}


ui <- dashboardPage(
  title = "Risk Assessment Tool"
  
  , dashboardHeader(title = tagList(
    tags$img(src = "GeorgiaTech_Navy.png", height = "50px", style = "padding-right: 10px")
    , "Risk Assessment Tool"
    )
    , titleWidth = 400
    )
  
  , dashboardSidebar(
    width = 400,
    sidebarMenu(id = "tabs"
                , menuItem("About Us & Disclaimer", tabName = "about", icon = icon("exclamation-circle"))
                , menuItem("Instructions", tabName = "instructions", icon = icon("info-circle"))
                , menuItem("Assessment", icon = icon("edit")
                           , menuSubItem("Input: General Information", tabName = "general_info", icon = icon("angle-right"))
                           , menuSubItem("Input: Lifestyle and Health", tabName = "lifestyle_health", icon = icon("angle-right"))
                           , menuSubItem("Input: Medical History", tabName = "medical_history", icon = icon("angle-right"))
                           , menuSubItem("Your Risk-Score", tabName = "risk_score", icon = icon("angle-right")
                                         )
                           )
                )
    ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "georgia_tech_theme.css")
    ),
    
    tabItems(
      tabItem(tabName = "about", aboutUI("about_module")
              , actionButton("next_about", "Next")
              ),
      tabItem(tabName = "instructions", instructionsUI("instructions_module")
              , actionButton("prev_instructions", "Previous")
              , actionButton("next_instructions", "Next")
              ),
      tabItem(tabName = "general_info", generalInfoUI("general_info_module")
              , actionButton("prev_general_info", "Previous")
              , actionButton("next_general_info", "Next")
              ),
      tabItem(tabName = "lifestyle_health", lifestyleHealthUI("lifestyle_health_module")
              , actionButton("prev_lifestyle_health", "Previous")
              , actionButton("next_lifestyle_health", "Next")
              ),
      tabItem(tabName = "medical_history", medicalHistoryUI("medical_history_module")
              , actionButton("prev_medical_history", "Previous")
              , actionButton("next_medical_history", "Next")
              ),
      tabItem(tabName = "risk_score" 
              , h2("Your Risk-Score")
              , actionButton("prev_risk_score", "Previous")
              , actionButton("submit", "Submit")
              , plotlyOutput("riskScorePlot")
              , div(tableOutput("combinedDataFrame"), class = "custom-table-width")
      )
    )
  )
)




server <- function(input, output, session) {
  generalInfoValues <- generalInfoServer("general_info_module")
  lifestyleHealthValues <- lifestyleHealthServer("lifestyle_health_module")
  medicalHistoryValues <- medicalHistoryServer("medical_history_module")
  
  observeEvent(input$next_about, {
    updateTabsetPanel(session, "tabs", selected = "instructions")
  })
  
  observeEvent(input$prev_instructions, {
    updateTabsetPanel(session, "tabs", selected = "about")
  })
  
  observeEvent(input$next_instructions, {
    updateTabsetPanel(session, "tabs", selected = "general_info")
  })
  
  observeEvent(input$prev_general_info, {
    updateTabsetPanel(session, "tabs", selected = "instructions")
  })
  
  observeEvent(input$next_general_info, {
    updateTabsetPanel(session, "tabs", selected = "lifestyle_health")
  })
  
  observeEvent(input$prev_lifestyle_health, {
    updateTabsetPanel(session, "tabs", selected = "general_info")
  })
  
  observeEvent(input$next_lifestyle_health, {
    updateTabsetPanel(session, "tabs", selected = "medical_history")
  })
  
  observeEvent(input$next_medical_history, {
    updateTabsetPanel(session, "tabs", selected = "risk_score")
  })
  
  observeEvent(input$prev_medical_history, {
    updateTabsetPanel(session, "tabs", selected = "lifestyle_health")
  })
  
  observeEvent(input$prev_risk_score, {
    updateTabsetPanel(session, "tabs", selected = "medical_history")
  })
  
  observeEvent(input$submit, {
    showModal(modalDialog(
      title = "Please wait",
      "Calculating risk score...",
      easyClose = FALSE,
      footer = NULL
    ))
    gc()
    combined <- rbind(generalInfoValues(), lifestyleHealthValues(), medicalHistoryValues())
    # output$combinedDataFrame <- renderTable(combined, rownames = FALSE)
    
    model_combined <- combined %>%
      left_join(assessment_options %>%
                  select(Value, CleanQuestion, CleanValue, `SAS Variable Name`), by = c("Question" = "CleanQuestion", "Response" = "CleanValue")) %>%
      select(Value, `SAS Variable Name`) %>%
      pivot_wider(names_from = `SAS Variable Name`, values_from = Value)
    
    # Apply the function to each factor variable
    
    # factor_vars <- names(readr::read_rds("models/model_data/enhanced_model_training_data.RDS")[,3:ncol(readr::read_rds("models/model_data/enhanced_model_training_data.RDS"))])  # replace with your actual variable names
    
    # new_data <- model_combined
    # for (var in factor_vars) {
    #   new_data[[var]] <- match_factor_levels(model_combined, var)
    # }
    
    model_combined[,1:ncol(model_combined)] <- lapply(model_combined[,1:ncol(model_combined)], as.factor)
    gc()
    log_pred <- predict_lazily("log",model_combined)
    xgboost_pred <- predict_lazily("xgboost",model_combined)
    
    ensemble_method <- ensembled_predictions(log_pred,xgboost_pred)
    # ensemble_method <- ensembled_predictions(xgboost_pred)
    
    output$riskScorePlot <- renderPlotly({
      plot_ly() %>%
        add_trace(
          type = "indicator",
          mode = "gauge+number",
          value = 100*(ensemble_method %>% pull(avg_pred)),
          gauge = list(
            axis = list(range = list(0, 100)),
            bar = list(color = "black"),
            steps = list(
              list(range = c(0, 20), color = '#E0A85E'),   # Lighter GT Gold
              list(range = c(20, 40), color = '#B3A369'),  # GT Gold
              list(range = c(40, 60), color = '#007BB3'),  # Lighter GT Blue
              list(range = c(60, 80), color = '#0055A2'),  # GT Blue
              list(range = c(80, 100), color = '#003057')  # Darker GT Blue
            ),
            threshold = list(
              value = 100*(ensemble_method %>% pull(avg_pred))
            )
          )
        ) %>%
        layout(
          title = "RISK Score",
          #autosize = TRUE,
          width = 900,
          margin = list(t = 100),
          annotations = list(
            list(xref = 'paper', x = 0.1, y = 0, xanchor = 'center', yanchor = 'bottom',
                 text = 'Low Risk', showarrow = FALSE, font = list(size = 12)),
            list(xref = 'paper', x = 0.5, y = 1, xanchor = 'center', yanchor = 'bottom',
                 text = 'Moderate Risk', showarrow = FALSE, font = list(size = 12)),
            list(xref = 'paper', x = 0.9, y = 0, xanchor = 'center', yanchor = 'bottom',
                 text = 'High Risk', showarrow = FALSE, font = list(size = 12))
          )
        )
    })
    
    
    gc()
    output$combinedDataFrame <- renderTable(combined, rownames = FALSE, table.width = NULL)
    gc()
    removeModal()
    
  })
}

shinyApp(ui, server)

