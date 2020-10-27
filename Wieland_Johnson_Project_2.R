rm(list = ls())

#Install ShinySky if not loaded
devtools::install_github("AnalytixWare/ShinySky")

#Load Libraries
library(shiny)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(tidyverse)
library(forcats)
library(readr)
library(benchmarkme)
library(numbers)
library(devtools)
library(plyr)
library(shinysky)


# #Read in Data
# admissions <- read_tsv("AdmissionsCorePopulatedTable.txt", col_names = TRUE)
# diagnoses <- read_tsv("AdmissionsDiagnosesCorePopulatedTable.txt", col_names = TRUE)
# labs <- read_tsv("LabsCorePopulatedTable.txt", col_names = TRUE)
# corePopulated <- read_tsv("PatientCorePopulatedTable.txt", col_names = TRUE)
#patientData <- list(corePopulated, admissions, diagnoses, labs)
patientData<-NULL
#
# ######################################################################################################
# ############################# Statistical Analysis ###################################################
# ######################################################################################################
# 
# #T Test
# analyses_t_test <- function(compareVariable, group1, group2) {
#   group1PatientID <- as.data.frame(subset(patientData[[4]], patientData[[4]]$PatientGender == group1))
#   group1PatientIDtest <- unique(group1PatientID$PatientID)
#   variableCompare1 <- subset(patientData[[3]], patientData[[3]]$PatientID %in% group1PatientIDtest == TRUE
#                              & patientData[[3]]$LabName == variableOfInterest)
#   
#   group2PatientID <- as.data.frame(subset(patientData[[4]], patientData[[4]]$PatientGender == group2))
#   group2PatientIDtest <- unique(group2PatientID$PatientID)
#   variableCompare2 <- subset(patientData[[3]], patientData[[3]]$PatientID %in% group2PatientIDtest == TRUE
#                              & patientData[[3]]$LabName == compareVariable)
#   
#   return(t.test(variableCompare1$LabValue,variableCompare2$LabValue ))
# }
# 
# possible_options <- function(patientData) {
#   emptyList <- vector(mode = "list", length = length(patientData))
#   emptyDataFrame <- data.frame(matrix(nrow = 0, ncol = 0))
#   for (i in 1:length(patientData)) {
#     for (j in 2:dim(patientData[[i]])[2]) {
#       print(c(i,j))
#       emptyDataFrame <- as.data.frame(t(rbind.fill(as.data.frame(emptyDataFrame), as.data.frame(t((unique(patientData[[i]])[j]))))))
#     }
#     emptyList[i] <- emptyDataFrame
#   }
#   
#   return(emptyList)
# }
# 
# test_function <- function(patientData) {
#   
#   print("test")
# }
# 
# #variableOfInterest <- readline(prompt = "Enter variable to analyze:")
# #analyses_t_test(variableOfInterest, "Male", "Female")
# test <- possible_options(patientData)

#----------------------------------------------------------------------------------------------------
ui <- fluidPage(navbarPage("VERITAS",
                 tabPanel("Home",
                          h3("Welcome to VERITAS"),
                          p("VERITAS is a web-based tool to allow clinicians easily input, access, store, and analyze patient data"),
                          ),
                 
                 tabPanel("Patient Data",
                          p("Pull EHR for a specific patient here:"),
                          selectizeInput(inputId = 'patientID',
                                         label = 'Search Patient ID',
                                         choices = patientData[[1]]$PatientID,
                                         selected = NULL,
                                         multiple = TRUE, #though only 1 input, necessary to prevent autofill
                                         options = list(maxItems = 1)),#caps to only 1 input, but does not autofill the box
                          tableOutput('contents'),
                          
                          ),
                 
                 
                 tabPanel("Input Data",
                          h3("Upload Patient Data by Filetype"),
                          fileInput("admissions", "Admission Data", accept = ".txt"),
                          fileInput("diagnoses", "Diagnostic Data", accept = ".txt"),
                          fileInput("labs", "Lab Data", accept = ".txt"),
                          fileInput("patients", "Patient Core Populated Data", accept = ".txt"),
                          
                 ),
                 
                 tabPanel("Analysis Tool")
                 )
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
    # req(admissions)
    # validate(need(ext == ".txt", "Please upload a tsv file"))
    # admissions <- read_tsv(input$admissions, col_names = TRUE)
    # 
    # req(diagnoses)
    # validate(need(ext == ".txt", "Please upload a tsv file"))
    # diagnoses <- read_tsv(input$diagnoses, col_names = TRUE)
    # 
    # req(labs)
    # validate(need(ext == ".txt", "Please upload a tsv file"))
    # labs <- read_tsv(input$labs, col_names = TRUE)
    # 
    # req(patients)
    # validate(need(ext == ".txt", "Please upload a tsv file"))
    # patients <- read_tsv(input$patients, col_names = TRUE)
    # 
    # patientData <- list(patients, admissions, diagnoses, labs)

    output$contents <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects and uploads a 
      # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
      # columns. The 'datapath' column will contain the local filenames where the 
      # data can be found.
      patients <- input$patients

      if (is.null(patients))
        return(NULL)
      
      patients <- read_tsv(patients$datapath, col_names = TRUE)
      updateSelectizeInput(session, 'patientID', choices = patients$PatientID, server = TRUE)
      
    })
}

shinyApp(ui = ui, server = server)








