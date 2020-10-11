rm(list = ls())

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

admissions <- read_tsv("Data_AdmissionsCorePopulatedTable_10000.txt", col_names = TRUE)
diagnoses <- read_tsv("Data_AdmissionsDiagnosesCorePopulatedTable_10000.txt", col_names = TRUE)
labs <- read_tsv("Data_LabsCorePopulatedTable_10000.txt", col_names = TRUE)
corePopulated <- read_tsv("Data_PatientCorePopulatedTable_10000.txt", col_names = TRUE)

patientData = list(admissions, diagnoses, labs, corePopulated)

variableOfInterest <- readline(prompt = "Enter variable to analyze:")
group1 <- readline(prompt = "Group 1:")
group2 <- readline(prompt = "Group 2:")
# group1PatientID <- as.data.frame(subset(patientData[[4]], patientData[[4]]$PatientGender == group1))
# group1PatientIDtest <- unique(group1PatientID$PatientID)
# variableCompare1 <- subset(patientData[[3]], patientData[[3]]$PatientID %in% group1PatientIDtest == TRUE
#                            & patientData[[3]]$LabName == variableOfInterest)
# variableCompare1Test <- subset(patientData[[3]], patientData[[4]]$PatientGender == "Male"
#                                & patientData[[3]]$LabName == variableOfInterest)
# 
# group2PatientID <- as.data.frame(subset(patientData[[4]], patientData[[4]]$PatientGender == group2))
# group2PatientIDtest <- unique(group2PatientID$PatientID)
# variableCompare2 <- subset(patientData[[3]], patientData[[3]]$PatientID %in% group2PatientIDtest == TRUE
#                            & patientData[[3]]$LabName == variableOfInterest)

# t.test(variableCompare1$LabValue,variableCompare2$LabValue )

analyses_t_test(variableOfInterest, "Male", "Female")

test <- possible_options(patientData)

