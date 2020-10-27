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


#Read in Data
admissions <- read_tsv("AdmissionsCorePopulatedTable.txt", col_names = TRUE)
diagnoses <- read_tsv("AdmissionsDiagnosesCorePopulatedTable.txt", col_names = TRUE)
labs <- read_tsv("LabsCorePopulatedTable.txt", col_names = TRUE)
corePopulated <- read_tsv("PatientCorePopulatedTable.txt", col_names = TRUE)
patientData = list(corePopulated, admissions, diagnoses, labs)

######################################################################################################
############################# Statistical Analysis ###################################################
######################################################################################################

#T Test
analyses_t_test <- function(compareVariable, group1, group2) {
  group1PatientID <- as.data.frame(subset(patientData[[4]], patientData[[4]]$PatientGender == group1))
  group1PatientIDtest <- unique(group1PatientID$PatientID)
  variableCompare1 <- subset(patientData[[3]], patientData[[3]]$PatientID %in% group1PatientIDtest == TRUE
                             & patientData[[3]]$LabName == variableOfInterest)
  
  group2PatientID <- as.data.frame(subset(patientData[[4]], patientData[[4]]$PatientGender == group2))
  group2PatientIDtest <- unique(group2PatientID$PatientID)
  variableCompare2 <- subset(patientData[[3]], patientData[[3]]$PatientID %in% group2PatientIDtest == TRUE
                             & patientData[[3]]$LabName == compareVariable)
  
  return(t.test(variableCompare1$LabValue,variableCompare2$LabValue ))
}

possible_options <- function(patientData) {
  emptyList <- vector(mode = "list", length = length(patientData))
  emptyDataFrame <- data.frame(matrix(nrow = 0, ncol = 0))
  for (i in 1:length(patientData)) {
    for (j in 2:dim(patientData[[i]])[2]) {
      print(c(i,j))
      emptyDataFrame <- as.data.frame(t(rbind.fill(as.data.frame(emptyDataFrame), as.data.frame(t((unique(patientData[[i]])[j]))))))
    }
    emptyList[i] <- emptyDataFrame
  }
  
  return(emptyList)
}

test_function <- function(patientData) {
  
  print("test")
}

variableOfInterest <- readline(prompt = "Enter variable to analyze:")
analyses_t_test(variableOfInterest, "Male", "Female")
test <- possible_options(patientData)

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
                          ),
                 
                 tabPanel("Input Data"),
                 
                 tabPanel("Analysis Tool")
                 )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)








