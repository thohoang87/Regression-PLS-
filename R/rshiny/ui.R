library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(devtools)
install_github("Samibgh/Regression-PLS-", force = T)
library("PlsRegression")
library("dplyr")
library(reactable)
library(shinythemes)

# Define UI for data upload app ----
ui <- 
  navbarPage("PLS-DA Application", collapsible = TRUE, inverse = TRUE,
             theme = shinytheme("flatly"),
             setBackgroundColor(
               color = "#F8F8F8",
               gradient = "radial",
               direction = "bottom"
             ),
##################### Pour l'onglet "Data view":##########################             
      tabPanel("Data view",
          fluidPage(
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for first inputs ----
               sidebarPanel(
                 strong("Training Data Upload"),
                 br(),
                 br(),
                 textInput("sheet1","For a XLSX file, please choose Sheet Name (for Training)", value="Sheet1"),
                
                 # Input: Select first file ----
                 fileInput("file1", "Choose CSV or XLSX File for Training",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv",".xlsx"),
                           buttonLabel = "Choose file"),
 
                 # Horizontal line ----
                 tags$hr(),
               
                 strong("Prediction Data Upload"),
                 br(),
                 br(),
                 textInput("sheet2","For a XLSX file, please choose Sheet Name (for Prediction)", value="Sheet2"),

                 # Input: Select second file ----
                 fileInput("file2", "Choose CSV or XLSX File for Prediction",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv",".xlsx"),
                           buttonLabel = "Choose file"),
                 
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 checkboxInput("header", "Header", TRUE),

                 strong("For a CSV file, please choose :"),
                 
                 # Input: Select separator for csv file----
                 radioGroupButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","
                              ),
                 
                 # Input: Select quotes for csv file----
                 radioGroupButtons("quote", "Quote",
                              choices = c(None = "",
                                          "Double Quote" = '"',
                                          "Single Quote" = "'"),
                              selected = '"'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display for fist file----
                 radioGroupButtons("disp1", "Display for Training Data",
                              choices = c(Head = "head_training",
                                          Summary = "summary_training"),
                              selected = "head_training"),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display for second file----
                 radioGroupButtons("disp2", "Display for Prediction Data",
                              choices = c(Head = "head_prediction",
                                          Summary = "summary_prediction"),
                              selected = "head_prediction")
               ),
            mainPanel(
              tabsetPanel(
                br(),tabPanel("Training Data",reactableOutput("contents1")), # Output: Data first file
                br(),tabPanel("Prediction Data",reactableOutput("contents2"))# Output: Data second file
))))), 
    
#################### Pour l'onglet "Define status": ##########################
    tabPanel("Define Status",
        fluidPage(
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        sidebarPanel(
          # choose explained variables x:
          selectInput(
            inputId = "x",
            label = "Select x variable",
            multiple = TRUE,
            choices = c()),
          # choose target y:
          selectInput(
            inputId = "y",
            label = "Select y variable",
            multiple = TRUE,
            choices = c()),
          actionButton("viewx", "Submit X Selection"),
          br(),
          br(),
          actionButton("viewy", "Submit Y Selection"),
          
          # Horizontal line ----
          tags$hr(),
          br(),
          # Input: select parameters of fit:
          textInput("algorithm","Choose an algorithm",value="simpls"),
          numericInput("threshold.algorithm","Choose threshold of algorithm NIPALS",value="0.001"),
          numericInput("iter.max","Choose iter.max of algorithm NIPALS",value="100"),
          numericInput("threshold.vip","Choose threshold of VIP",value="1"),
          numericInput("ncomponents","Choose number of components",value="0")

        ),
        mainPanel(tabsetPanel(br(),tabPanel("X Dataframe",reactableOutput("definex")), # output of x selection
                  br(),tabPanel("Y Dataframe",reactableOutput("definey")))) # output of y selection
      ))),
    
#################### Pour l'onglet "Fit values": ##########################
tabPanel("Fit",
         fluidPage(
         sidebarLayout(
           sidebarPanel(
             selectInput("Select_Train_data", "Select train or full data",
                         choices = c(Train = "Train",
                                     Full_Data = "Full_Data",
                                     selected="Train")),
             selectInput(inputId = "fit", "Results of fit",
                         choices = c(Classification = "Classification",
                                     Xweights = "Xweights",
                                     Yweights="Yweights",
                                     Xscores ="Xscores",
                                     Yscores ="Yscores",
                                     Xloadings = "Xloadings",
                                     Yloadings ="Yloadings"),
                         selected="Classification"),
             
             actionButton("viewfit", "apply"),
             br(),
           ),
           mainPanel(br(),reactableOutput("fitvalue"))
         ))),
#################### Pour l'onglet "Prediction": ##########################
    tabPanel("Prediction Results",
             fluidPage(
             tabsetPanel(
               tabPanel("Training Data Prediction",
                        sidebarLayout(
                          sidebarPanel(
                            # Input: select prediction values for the first file----
                            selectInput("pred1", "Results of Training Prediction",
                                               choices = c(Class = "Class",
                                                           Probability = "Probability",
                                                           Confusion_Matrix="Confusion_Matrix",
                                                           Accuracy ="Accuracy",
                                                           Recall ="Recall",
                                                           Precision = "Precision",
                                                           F1_Score ="F1_Score"),
                                               selected="Class"),
                            actionButton("viewpred", "apply")),
                          mainPanel(br(),reactableOutput("predictvalues1"))
               )),
               tabPanel("Prediction Result",
                        sidebarLayout(
                          sidebarPanel(
                            # Input: prediction values for the second file----
                            actionButton("class", "Show Result"),
                            # Horizontal line ----
                            tags$hr(),
                            # download the result of the prediciton for second file:
                            strong("Download your result"),
                            br(),
                            #download on csv
                            downloadButton("downloadcsv", "Download .csv"),
                            br(),
                            br(),
                            # download on excel
                            downloadButton("downloadexcel", "Download .xlsx")),
                          mainPanel(br(),reactableOutput("predictvalues2"))
))))),
#################### Pour l'onglet "Graphics": ############################
tabPanel("Graphics",
         fluidPage(
         sidebarLayout(
           sidebarPanel(
             selectInput(
               inputId = "Graphic_1",
               label = "Select graphic",
               choices = c(CircleCorrelationplot="Circle_Correlation_plot",
                           Individuplot="Individu_plot",
                           Correlationplot="Correlation_plot")),
             selectInput(
               inputId = "comp1",
               label = "Select the first component",
               choice = c()),
             selectInput(
               inputId = "comp2",
               label = "Select the second component",
               choice = c()),
             actionButton("viewgraph_1", "View graphic"),
             br(),
             br(),
             br(),
             selectInput(
               inputId = "Graphic_2",
               label = "Select graphic",
               choices = c(Cos2plot="Cos2plot",
                           Contribution_variable = "Contribution_variable",
                           Screeplot ="Screeplot",
                           Plot_Vip = "Plot_Vip")),
             selectInput(
               inputId = "ncomp",
               label = "Select number of component",
               choice = c()),
             actionButton("viewgraph_2", "View graphic")
           ),
           
           mainPanel(tabsetPanel(tabPanel("First selection graph",br(),plotlyOutput("plotly1")),
                     tabPanel("Second selection graph",br(),plotlyOutput("plotly2"))))
         )))
)



