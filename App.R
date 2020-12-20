rm(list = ls())

#Install ShinySky if not loaded
devtools::install_github("AnalytixWare/ShinySky")
devtools::install_github("daattali/shinyjs")

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
library(textreadr)
library(stringr)
library(utils)
library(htmltools)
library(rmarkdown)
library(tinytex)
library(pagedown)
library(rmarkdown)
library(shinyjs)
library(pastecs)
library(DT)
library(ggplot2)
library(mixlm)


# library(shinysky)


#Read in Data
# admissions <- read_tsv("AdmissionsCorePopulatedTable.txt", col_names = TRUE)
# diagnoses <- read_tsv("AdmissionsDiagnosesCorePopulatedTable.txt", col_names = TRUE)
# labs <- read_tsv("LabsCorePopulatedTable.txt", col_names = TRUE)
# corePopulated <- read_tsv("PatientCorePopulatedTable.txt", col_names = TRUE)
# patientData <- list(corePopulated, admissions, diagnoses, labs)

# #Read in Data
# admissions <- read_tsv("Data_AdmissionsCorePopulatedTable_10000.txt", col_names = TRUE)
# diagnoses <- read_tsv("Data_AdmissionsDiagnosesCorePopulatedTable_10000.txt", col_names = TRUE)
# labs <- read_tsv("Data_LabsCorePopulatedTable_10000.txt", col_names = TRUE)
# corePopulated <- read_tsv("Data_PatientCorePopulatedTable_10000.txt", col_names = TRUE)
# patientData <- list(corePopulated, admissions, diagnoses, labs)

admissions <- paste(getwd(),"/Data/", "Data_AdmissionsCorePopulatedTable_10000.txt", sep = "") %>% 
  read_tsv(col_names = TRUE)
diagnoses <- paste(getwd(),"/Data/", "Data_AdmissionsDiagnosesCorePopulatedTable_10000.txt", sep = "") %>% 
  read_tsv(col_names = TRUE)
labs <- paste(getwd(), "/Data/", "Data_LabsCorePopulatedTable_10000.txt", sep = "") %>% 
  read_tsv(col_names = TRUE)
corePopulated <- paste(getwd(),"/Data/", "Data_PatientCorePopulatedTable_10000.txt", sep = "") %>% 
  read_tsv(col_names = TRUE)

#Modification of naming convention of the dataframe
names(labs)[2] <- "LabAdmissionID"
names(diagnoses)[2] <- "ClinicianAdmissionID"
names(admissions)[2] <- "ClinicianAdmissionID"
# names(corePopulated)[1] <- "CorePatientID"

patientData <- list(corePopulated, admissions, diagnoses, labs)


# patientReportTemplate <- read_html("Patient_Report_Template.html", skip = 0, remove.empty = TRUE, trim = TRUE)
#Reading in patient info report html templates
patientReportTemplate <- paste(getwd(), "/Report_Templates/", "Patient_Report_Template.html", sep = "") %>%
  readLines() %>% paste(collapse="", sep = "")
# patientLabReportTemplate <- paste(getwd(), "/Report_Templates/", "Patient_Report_Template_Lab_History_Only.html", sep = "") %>%
#   readLines() %>% paste(collapse = "", sep = "")
patientLabReportHeaderTemplate <- paste(getwd(), "/Report_Templates/", "Lab_History_Template_Col_Names_and_Header.html", sep = "") %>%
  readLines() %>% paste(collapse = "", sep = "")
patientLabReportRowTemplate <- paste(getwd(), "/Report_Templates/", "Lab_History_Template_Rows.html", sep = "") %>%
  readLines() %>% paste(collapse = "", sep = "")

#Read in reference lab results
labReference <- paste(getwd(), "/Analysis_and_Report_Tools/", "Normal_Lab_Results.txt", sep = "") %>%
  read.csv(header = TRUE, sep = "\t")
responseVariableList <- paste(getwd(), "/Analysis_and_Report_Tools/", "Response_Variable_List.csv", sep = "") %>%
  read.csv(header = TRUE, sep = ",")
condNamingIndexTempl <- paste(getwd(), "/Analysis_and_Report_Tools/", "Conditions_Naming_Index_Template.csv", sep = "") %>%
  read.csv(header = TRUE, sep = ",")

#Global Variable
current_patient<- list();

#Global Functions
#----------------------------------------------------------------------------------------------------

generateHistoryReport <- function(patient_Report_Template, patient_Lab_Report_Header_Template, 
                                  patient_Lab_Report_Row_Template, lab_Reference, patient_ID, patient_Data) {
  outputHTML <- patient_Report_Template
  patientCorePopulated <- subset(patient_Data[[1]], patient_Data[[1]]$PatientID == patient_ID)
  patientAdmissions <- subset(patient_Data[[2]], patient_Data[[2]]$PatientID == patient_ID)
  patientDiagnoses <- subset(patient_Data[[3]], patient_Data[[3]]$PatientID == patient_ID)
  patientLab <- subset(patient_Data[[4]], patient_Data[[4]]$PatientID == patient_ID)
  
  outputHTML <- str_replace(outputHTML, "replace_Name", as.character(patient_ID))
  outputHTML <- str_replace(outputHTML, "replace_DOB", as.character(patientCorePopulated$PatientDateOfBirth))
  outputHTML <- str_replace(outputHTML, "replace_Sex", as.character(patientCorePopulated$PatientGender))
  outputHTML <- str_replace(outputHTML, "replace_Race", as.character(patientCorePopulated$PatientRace))
  outputHTML <- str_replace(outputHTML, "replace_Language", as.character(patientCorePopulated$PatientLanguage))
  outputHTML <- str_replace(outputHTML, "replace_Marital_Status", as.character(patientCorePopulated$PatientMaritalStatus))
  outputHTML <- str_replace(outputHTML, "replace_Percentage_Below_Poverty", as.character(patientCorePopulated$PatientPopulationPercentageBelowPoverty))
  
  for (i in 1:nrow(patientDiagnoses)) {
    diagnosesReplacementText <- paste("<em>Admission ID: ", patientDiagnoses$ClinicianAdmissionID[i], "</em>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                      "Start Date: ", patientAdmissions$AdmissionStartDate[i], "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                      "End Date: ", patientAdmissions$AdmissionEndDate[i], "<br>",
                                      "Primary Diagnosis Code: ", patientDiagnoses$PrimaryDiagnosisCode[i], "<br>",
                                      "Description: ", patientDiagnoses$PrimaryDiagnosisDescription[i], "<br><br>",
                                      "replace_Diagnoses_History",
                                      sep = "")
    outputHTML <- str_replace(outputHTML, "replace_Diagnoses_History", diagnosesReplacementText)
  }
  outputHTML <- str_remove(outputHTML, "replace_Diagnoses_History")
    labReportHTMLReplacement <- ""
  for (i in unique(patientLab$LabAdmissionID)) {
    labAdmission <- subset(patientLab, patientLab$LabAdmissionID == i)
    labAdmission <- labAdmission[order(labAdmission$LabName),]
    earliestTestDate <- as.character(min(labAdmission$LabDateTime))
    latestTestDate <- as.character(max(labAdmission$LabDateTime))
    
    labAdmissionReplacementText <- paste("<em>Admission ID: ", as.character(i), "</em>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                        "Test Period: ", earliestTestDate, " To ", latestTestDate, "<br><br>",
                                        sep = ""
                                        )
    
    labHistoryRows <- ""
    for (j in 1:nrow(labAdmission)) {
      
      tempLabHistoryRows <- patient_Lab_Report_Row_Template
      tempLabHistoryRows <- str_replace(tempLabHistoryRows, "replace_Lab_Name", as.character(labAdmission$LabName[j]))
      
      if (labAdmission$LabUnits[j] == "no unit" | labAdmission$LabUnits[j] == "NA") {
        tempLabHistoryRows <- str_replace(tempLabHistoryRows, "replace_Lab_Value", as.character(labAdmission$LabValue[j]))
      } else {
        tempLabHistoryRows <- str_replace(tempLabHistoryRows, "replace_Lab_Value", 
                                          paste(as.character(labAdmission$LabValue[j]), as.character(labAdmission$LabUnits[j]), sep = "&nbsp;"))
      }

      tempLabHistoryRows <- str_replace(tempLabHistoryRows, "replace_Lab_Date", as.character(labAdmission$LabDateTime[j]))
      
      k <- which(lab_Reference$LabName == labAdmission$LabName[j])
      if (is.na(lab_Reference$Units[k]) == TRUE) {
        labReferenceReplacementText <- paste(lab_Reference$ReferenceLowEnd[k], " - ",
                                             lab_Reference$ReferenceHighEnd[k], "&nbsp;",
                                             "<br>",
                                             sep = "")
      } else {
        labReferenceReplacementText <- paste(lab_Reference$ReferenceLowEnd[k], " - ",
                                             lab_Reference$ReferenceHighEnd[k], "&nbsp;",
                                             lab_Reference$Units[k], "<br>",
                                             sep = "")
      }

      tempLabHistoryRows <- str_replace(tempLabHistoryRows, "replace_Reference_Range", labReferenceReplacementText)
      if (labAdmission$LabValue[j] >= lab_Reference$ReferenceLowEnd[k] & 
          labAdmission$LabValue[j] <= lab_Reference$ReferenceHighEnd[k]) {
        flag = ""
      } else {
        flag = "Abnormal"
      }
      tempLabHistoryRows <- str_replace(tempLabHistoryRows, "replace_Abnormal_Flag", flag)
      labHistoryRows <- paste(labHistoryRows, tempLabHistoryRows, sep = "")
    }
    
    
    tempLabReportHTMLReplacement <- str_replace(patient_Lab_Report_Header_Template, "replace_Lab_History_Row", labHistoryRows)
    labReportHTMLReplacement <- paste(labReportHTMLReplacement, tempLabReportHTMLReplacement, "<br><br>", sep = "")
    labReportHTMLReplacement <- str_replace(labReportHTMLReplacement, "replace_Admission_History", labAdmissionReplacementText)
  }
  
  outputHTML <- str_replace(outputHTML, "<p class=MsoNoSpacing>replace_Lab_History</p>", labReportHTMLReplacement)
  outputHTML <- str_remove(outputHTML, "replace_Lab_History")
  
  return(outputHTML)
}

subsetPatientData<- function(patient_ID, patient_Data) {
  patientCorePopulated <- subset(patient_Data[[1]], patient_Data[[1]]$PatientID == patient_ID)
  patientAdmissions <- subset(patient_Data[[2]], patient_Data[[2]]$PatientID == patient_ID)
  patientDiagnoses <- subset(patient_Data[[3]], patient_Data[[3]]$PatientID == patient_ID)
  patientLab <- subset(patient_Data[[4]], patient_Data[[4]]$PatientID == patient_ID)
  
  patientInfoList<- list(patientCorePopulated, patientAdmissions, patientDiagnoses, patientLab)
  
  return(patientInfoList)
  
}

##Define Variable Numbers for Readabiliy when indexing patient Data
core_populated_id<- 1;
admissions_id<-2;
diagnoses_id<-3;
lab_id<-4;


singleSelectizeInput <- function(inputID, label, choices) {
  selectizeInput(inputId = inputID,
                 label = label,
                 choices = choices,
                 selected = NULL,
                 multiple = TRUE, #though only 1 input, necessary to prevent autofill
                 options = list(maxItems = 1)
  )#caps to only 1 input, but does not autofill the box   
}

multipleSelectizeInput <- function(inputID, label, choices) {
  selectizeInput(inputId = inputID,
                 label = label,
                 choices = choices,
                 selected = NULL,
                 multiple = TRUE,
                 options = list(maxOptions = 100000) #allows the widget to display up to 100000 options, can change as necessary
  ) 
}

sliderInputModified <- function(inputID, label, min, max) {
  min <- as.numeric(min)
  max <- as.numeric(max)
  sliderInput(inputId = inputID,
              label = label,
              min = min,
              max = max,
              step = 0.1,
              value = c(min, max)
              )
}

sliderInputDiscreteModified <- function(inputID, label, min, max) {
  min <- as.numeric(min)
  max <- as.numeric(max)
  sliderInput(inputId = inputID,
              label = label,
              min = min,
              max = max,
              step = 1,
              value = c(min, max)
  )
}

sliderInputDate <- function(inputID, label, min, max) {
  # min <- as.POSIXct(min,tz="UTC")
  # max <- as.POSIXct(max,tz="UTC")
  min <- as.Date(min)
  max <- as.Date(max)
  
  sliderInput(inputId = inputID,
              label = label,
              min = min,
              max = max,
              timeFormat = "%F", #display dates
              step = 1, #step of one day
              value = c(min, max)
  )
}




displayMultGroupCondSelec <- function(num_Groups) {
  num_Groups <- as.numeric(num_Groups)
  for (i in 1:num_Groups) {
    name <- paste("groupCond", as.character(i))
    p(name)
  }
}

generateConditionsNamingIndex <- function(num_Groups, cond_Naming_Index_Templ) {
  if (length(num_Groups) == 0) {
    num_Groups <- 10
  }
  num_Groups <- as.numeric(num_Groups)
  if (num_Groups < 10) {
    num_Groups <- 10
  }
  print("at generateConditionsNamingIndex function")
  outputDataFrame <- cond_Naming_Index_Templ
  outputDataFrame[,2] <- paste0(cond_Naming_Index_Templ[,3], "_Uni")
  names(outputDataFrame)[2] <- "Pointer_ID_Uni"
  for (i in 1:num_Groups) {
    outputDataFrame[,i+2] <- paste0(cond_Naming_Index_Templ[,3], "_", as.character(i))
    names(outputDataFrame)[i+2] <- paste0("Pointer_ID_", as.character(i))
  }
  return(outputDataFrame)
}



generateConditionsOptions <- function(cond_Naming_Index_Templ, patient_Data) {
  print("at generateConditionsOptions)")
  corePopulatedUpdated <- patient_Data[[1]]
  admissionsUpdated <- patient_Data[[2]]
  diagnosesUpdated <- patient_Data[[3]]
  # admissionDiagnosesUpdated <- unique(merge(patient_Data[[2]], patient_Data[[3]]))
  labsUpdated <- patient_Data[[4]]
  labsNamesList <- unique(patient_Data[[4]]$LabName)
  maxNumRow <- 2
  
  # for (i in 1:length(patient_Data)) {
  #   if (nrow(patient_Data[[i]]) > maxNumRow) {
  #     maxNumRow <- nrow(patient_Data[[i]])
  #   } 
  # }
  
  for (i in 1:nrow(cond_Naming_Index_Templ)) {
    # print(c("testing this loop", i))
    if (cond_Naming_Index_Templ[i,1] %in% names(admissionsUpdated) == TRUE &&
        cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == FALSE) {
      j <- which(colnames(admissionsUpdated) == condNamingIndexTempl[i,1])
      tempMax <- nrow(unique(admissionsUpdated[,j]))
    } else if (cond_Naming_Index_Templ[i,1] %in% names(diagnosesUpdated) == TRUE &&
              cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == FALSE) {
      j <- which(colnames(diagnosesUpdated) == condNamingIndexTempl[i,1])
      tempMax <- nrow(unique(diagnosesUpdated[,j]))
    } else if (cond_Naming_Index_Templ[i,1] %in% names(corePopulatedUpdated) == TRUE &&
               cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == FALSE) {
      j <- which(colnames(corePopulatedUpdated) == condNamingIndexTempl[i,1])
      tempMax <- nrow(unique(corePopulatedUpdated[,j]))
    } else if(cond_Naming_Index_Templ[i,1] %in% names(labsUpdated) == TRUE &&
              cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == FALSE) {
      j <- which(colnames(labsUpdated) == condNamingIndexTempl[i,1])
      tempMax <- nrow(unique(labsUpdated[,j]))
    }

    if (tempMax > maxNumRow) {
      # print(c("are we here", tempMax))
      maxNumRow <- tempMax
    }
  }
  
  # print(c("is this slow", maxNumRow))
  # print(maxNumRow)
  
  outputDataFrame <- data.frame(matrix(nrow = maxNumRow, ncol = nrow(cond_Naming_Index_Templ)))

  for (i in 1:nrow(cond_Naming_Index_Templ)) {
    names(outputDataFrame)[i] <- paste0(cond_Naming_Index_Templ[i,3])

    if (cond_Naming_Index_Templ[i,1] %in% names(admissionsUpdated) == TRUE &&
        cond_Naming_Index_Templ[i,1] != "PatientID" &&
        cond_Naming_Index_Templ[i,1] != "ClinicianAdmissionID") {
      j <- which(colnames(admissionsUpdated) == condNamingIndexTempl[i,1])
      print(c("admissionDiagnosisUpdated", i, j))
      if (cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == TRUE) {
        outputDataFrame[1,i] <- as.character(min(admissionsUpdated[,j][[1]])) ###
        outputDataFrame[2,i] <- as.character(max(admissionsUpdated[,j][[1]])) ###
      } else {
        tempDataFrame1 <- as.data.frame(unique(admissionsUpdated[,j]))
        outputDataFrame[1:nrow(tempDataFrame1),i] <- (sort(tempDataFrame1[,1]))
      }
    } else if (cond_Naming_Index_Templ[i,1] %in% names(diagnosesUpdated) == TRUE &&
        cond_Naming_Index_Templ[i,1] != "PatientID"
    ) {
      print(c("diagnosesUpdated", i))
      j <- which(colnames(diagnosesUpdated) == condNamingIndexTempl[i,1])
      if (cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == TRUE) {
        outputDataFrame[1,i] <- as.character(min(diagnosesUpdated[,j][[1]])) ###
        outputDataFrame[2,i] <- as.character(max(diagnosesUpdated[,j][[1]])) ###
      } else {
        tempDataFrame1 <- as.data.frame(unique(diagnosesUpdated[,j]))
        outputDataFrame[1:nrow(tempDataFrame1), i] <- (sort(tempDataFrame1[,1]))
      }
    } else if (cond_Naming_Index_Templ[i,1] %in% names(corePopulatedUpdated) == TRUE) {
      j <- which(colnames(corePopulatedUpdated) == condNamingIndexTempl[i,1])
      print(c("corePopulatedUpdated", i, j))
      if (cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == TRUE) {
        outputDataFrame[1,i] <- as.character(min(corePopulatedUpdated[,j][[1]])) ###
        outputDataFrame[2,i] <- as.character(max(corePopulatedUpdated[,j][[1]])) ###
      } else {
        tempDataFrame1 <- as.data.frame(unique(corePopulatedUpdated[,j]))
        outputDataFrame[1:nrow(tempDataFrame1),i] <- (sort(tempDataFrame1[,1]))
      }
    } else if (cond_Naming_Index_Templ[i,1] %in% names(labsUpdated) == TRUE &&
        cond_Naming_Index_Templ[i,1] != "PatientID") {
      j <- which(colnames(labsUpdated) == condNamingIndexTempl[i,1])
      print(c("labsUpdated", i, j))
      if (cond_Naming_Index_Templ[i,1] %in% cond_Naming_Index_Templ[,4] == TRUE) {
        outputDataFrame[1,i] <- as.character(min(labsUpdated[,j][[1]])) ###
        outputDataFrame[2,i] <- as.character(max(labsUpdated[,j][[1]])) ###
      } else {
        tempDataFrame1 <- as.data.frame(unique(labsUpdated[,j]))
        outputDataFrame[1:nrow(tempDataFrame1),i] <- (sort(tempDataFrame1[,1]))
      }
    } else if (cond_Naming_Index_Templ[i,1] %in% labsNamesList == TRUE) {
      tempDataFrame1 <- subset(labsUpdated, labsUpdated$LabName == cond_Naming_Index_Templ[i,1])
      outputDataFrame[1,i] <- (min(tempDataFrame1$LabValue))
      outputDataFrame[2,i] <- (max(tempDataFrame1$LabValue))
      print(c("labTests", i))
      # outputDataFrame[1:nrow(tempDataFrame1),i] <- as.numeric(sort(tempDataFrame1$LabValue))
      # outputDataFrame[1,i] <- "test"
    }
  }
  
  # outputDataFrame <- outputDataFrame[rowSums(is.na(outputDataFrame)) != ncol(outputDataFrame), ] #Remove all rows that are completely made of NAs
  return(outputDataFrame)
}

#Testing the two functions here
# index <- generateConditionsNamingIndex(3, condNamingIndexTempl)
# indexOptions <- generateConditionsOptions(condNamingIndexTempl, patientData)




generateCondSelectedListIndex <- function(num_Groups, selected_Uni_List, selected_Group_List, cond_Naming_Index_Templ) {
  num_Groups <- as.numeric(num_Groups)
  
  if (class(selected_Uni_List) == "list") {
    selected_Uni_List <- as.character(selected_Uni_List)
  }
  
  print("at generateCondSelectedListIndex Function")
  outputDataFrame <- data.frame(matrix(nrow = nrow(cond_Naming_Index_Templ), ncol = (2 + num_Groups), NA))
  names(outputDataFrame)[1] <- "Selected_Pointer_ID_Uni_Index"
  names(outputDataFrame)[2] <- "Group_Cond_Options_Index"
  
  for (i in 1:num_Groups) {
    tempName <- paste0("Selected_Pointer_ID_Group_", as.character(i), "_Index")
    names(outputDataFrame)[i+2] <- tempName
  }
  
  i <- 1
  
  if (length(selected_Uni_List > 0)) {
    for (j in 1:length(selected_Uni_List)) {
      print(c("at generatedCondSelectedListIndex Uni", j))
      if ((selected_Uni_List[j]) %in% cond_Naming_Index_Templ[,2] == TRUE) {
        k <- which(cond_Naming_Index_Templ[,2] == (selected_Uni_List[j]))
        outputDataFrame[i,1] <- k
        i = i + 1
      }
    }
  }
  

  
  tempIndexList1 <- (1:nrow(cond_Naming_Index_Templ))
  tempIndexList2 <- setdiff(tempIndexList1, outputDataFrame[,1])
  
  for (i in 1:length(tempIndexList2)) {
    outputDataFrame[i,2] <- tempIndexList2[i]
  }
  
  # print("are we right here?")
  if (length(selected_Group_List) > 0) {
    for (i in 1:length(selected_Group_List)) {
      if (length(selected_Group_List[[i]]) > 0) {
        j <- 1
        for (k in 1:length(selected_Group_List[[i]])) {
          print(c("at generatedCondSelectedListIndex Group", i,j,k))
          if (selected_Group_List[[i]][[k]] %in% cond_Naming_Index_Templ[,2] == TRUE) {
            l <- which(cond_Naming_Index_Templ[,2] == selected_Group_List[[i]][[k]])
            outputDataFrame[j,i+2] <- l
            print(c(selected_Group_List[[i]][[k]],l,j,k,i+2))
            j <- j+1
          }
        }
      }
    }
  }
  
  # print("did we make it to here?")
  
  
  
  outputDataFrame <- outputDataFrame[rowSums(is.na(outputDataFrame)) != ncol(outputDataFrame), ] #Remove all rows that are completely made of NAs
  return(as.data.frame(outputDataFrame))
}

# testList <- generateCondSelectedListIndex(c("METABOLIC: ANION GAP", "Patient Date of Birth"), condNamingIndexTempl)


mapplyWidgetListInputGenerator <- function(group_ID, cond_Selected_List_Index, conditions_Naming_Index, condition_Options, cond_Naming_Index_Templ) {
  if (group_ID == "Uni") {
    tempList1 <- lapply(cond_Selected_List_Index, function(x) x[!is.na(x)])[[1]]
    widgetList <- list()
    print("At mapplyWidgetListInputGenerator")
    tempList2 <- list()
    tempList3 <- list()
    tempList4 <- list()
    tempList4 <<- list()
    tempList1 <- lapply(cond_Selected_List_Index, function(x) x[!is.na(x)])[[1]]

    for (i in 1:length(tempList1)) {
      tempList2[[i]] <- conditions_Naming_Index[tempList1[[i]], 2]
      tempList3[[i]] <- cond_Naming_Index_Templ[tempList1[[i]], 2]
      tempList4[[i]] <- na.omit(condition_Options[ ,tempList1[[i]]])
      tempList4[[i]] <<- na.omit(condition_Options[ ,tempList1[[i]]])
    }
    
    widgetList <- mapply(SIMPLIFY = FALSE, multipleSelectizeInput,
           tempList2,
           tempList3,
           tempList4
           )
  }
  return(widgetList)
}

generateTotalWidgetList <- function(num_Groups, conditions_Naming_Index, condition_Options, cond_Naming_Index_Templ) {
  num_Groups <- as.numeric(num_Groups)
  print("at generateTotalWidgetList function")
  num_Groups_Updated <- 10
  if (num_Groups > num_Groups_Updated) {
    num_Groups_Updated <- num_Groups
  }
  
  outputList <- list()
  rowIDList <- (1:nrow(cond_Naming_Index_Templ))
  
  for (colID in 1:(num_Groups_Updated + 1)) {
    print(c("at generateTotalWidgetList", colID))
    outputList[[colID]] <- lapply(rowIDList, generateTotalWidgetListChildren,
      colID,
      conditions_Naming_Index,
      condition_Options,
      cond_Naming_Index_Templ
      )
  }
  
  # for (i in 1:(num_Groups_Updated + 1)) {
  #   tempList1 <- list()
  #   for (j in 1:nrow(cond_Naming_Index_Templ)) {
  #     print(c("at generateTotalWidgetList", i, j))
  #     if (cond_Naming_Index_Templ[j,1] %in% cond_Naming_Index_Templ[j,4] == FALSE) {
  #       tempList1[[j]] <- multipleSelectizeInput(conditions_Naming_Index[j,i + 1],
  #                                           cond_Naming_Index_Templ[j,2],
  #                                           na.omit(condition_Options[,j])
  #                                           )
  #     } else if (cond_Naming_Index_Templ[j,1] %in% cond_Naming_Index_Templ[j,4] == TRUE) {
  #       tempList1[[j]] <- sliderInputModified(conditions_Naming_Index[j,i + 1],
  #                                                cond_Naming_Index_Templ[j,2],
  #                                                condition_Options[1,j], #min value
  #                                                condition_Options[2,j]) #max value
  #     }
  #       
  #   }
  #   
  #   outputList[[i]] <- tempList1
  #   
  # }
  
  return(outputList)
}

generateTotalWidgetListChildren <- function(row_ID, col_ID, conditions_Naming_Index, condition_Options, cond_Naming_Index_Templ) {
  # print(c("at generateTotalWidgetListChildren", row_ID))
  if (cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,4] == FALSE &&
      cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,6] == FALSE) {
    outputFunction <- multipleSelectizeInput(conditions_Naming_Index[row_ID,col_ID + 1],
                                             paste(cond_Naming_Index_Templ[row_ID,2], conditions_Naming_Index[row_ID,col_ID + 1]),
                                             na.omit(condition_Options[,row_ID]))
  } else if (cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,4] == TRUE &&
             cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,6] == TRUE) {
    outputFunction <- sliderInputDiscreteModified(conditions_Naming_Index[row_ID,col_ID + 1],
                                          paste(cond_Naming_Index_Templ[row_ID,2], conditions_Naming_Index[row_ID,col_ID + 1]),
                                          condition_Options[1,row_ID], #min value
                                          condition_Options[2,row_ID]) #max value
    } else if (cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,4] == TRUE &&
             cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,5] == FALSE) {
    # print(c(conditions_Naming_Index[row_ID,col_ID + 1],
    #         cond_Naming_Index_Templ[row_ID,2],
    #         (condition_Options[1,row_ID]),
    #         (condition_Options[2,row_ID])
    #         ))
    outputFunction <- sliderInputModified(conditions_Naming_Index[row_ID,col_ID + 1],
                                          paste(cond_Naming_Index_Templ[row_ID,2], conditions_Naming_Index[row_ID,col_ID + 1]),
                                          (condition_Options[1,row_ID]), #min value
                                          (condition_Options[2,row_ID])) #max value
  } else if (cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,4] == TRUE &&
             cond_Naming_Index_Templ[row_ID,1] %in% cond_Naming_Index_Templ[,5] == TRUE) {
    outputFunction <- sliderInputDate(conditions_Naming_Index[row_ID,col_ID + 1],
                                      paste(cond_Naming_Index_Templ[row_ID,2], conditions_Naming_Index[row_ID,col_ID + 1]),
                                      (condition_Options[1,row_ID]), #min value
                                      (condition_Options[2,row_ID])) #max value
  }
  
  return(outputFunction)
}

getListedInputs <- function(input, input_ID_List) { #lists all the inputs for a given set of input IDs in the R shiny function

  print(c("at getListedInputs function beginning", input_ID_List))
  
  if (length(input_ID_List) > 0) {
    print(c("at getListedInputs function", length(input_ID_List)))
    listedInputsIndex <- (1:length(input_ID_List))
    outputList <- lapply(listedInputsIndex, getListedInputsChildren, input, input_ID_List)
  } else {
    outputList <- NULL
  }

  return(outputList)
}

getListedInputsChildren <- function(listed_Inputs_Index, input, input_ID_List) { #called in getListedInputs through lappy
  print(c("at getListedInputsChildren ", listed_Inputs_Index))
  outputValue <- input[[input_ID_List[[listed_Inputs_Index]]]]
  outputValue <- as.list(outputValue) #should we add as.list here?
  return(outputValue)
}

getAllCondWidgetsListedInputs <- function(input, num_Groups, conditions_Naming_Index) { #return the condition widgets for all the groups and the Uni group
  outputList <- list()
  print("at getAllCondWidgetsListedInputs")
  if (is.null(num_Groups) == FALSE) {
    num_Groups <- as.numeric(num_Groups)
    for (i in 1:( num_Groups + 1)) { #"+1" is to account for the Uni group
      tempInputIDList <- conditions_Naming_Index[,i+1]
      outputList[[i]] <- getListedInputs(input, tempInputIDList)
    }
  }
  
  return(outputList)
}

listedVerbatimTextOutput <- function(input, input_ID_List) {
  print("at listedVerbatimTextOutput function")
  outputList <- getListedInputs(input, input_ID_List)
  outputString <- ""
  for (i in 1:length(outputList)) {
    for (j in 1:length(outputList[[i]])) {
      outputString <- paste(outputString, outputList[[i]][[j]])
    }
  }
  return(outputString)
}

outputWidgetList <- function(group_ID, cond_Selected_List_Index, total_Widget_List) {
  print("at outputWidgetList function")
  
  outputList <- list()
  if (group_ID == "Uni") {
    tempIndex <- na.omit(cond_Selected_List_Index[,1])
    if (length(tempIndex) > 0) {
      for (i in 1:length(tempIndex)) {
        print(c("at outputWidgetList function uni",i))
        j <- tempIndex[i]
        outputList[[i]] <- total_Widget_List[[1]][[j]]
      }
    }
  } else if (group_ID >= "0" && group_ID != "Uni") {
    group_ID <- as.numeric(group_ID)
    tempIndex <- na.omit(cond_Selected_List_Index[,(group_ID+2)])
    if (length(tempIndex) > 0) {
      for (i in 1:length(tempIndex)) {
        j <- tempIndex[i]
        print(c("at outputWidgetList function group", "group_ID", group_ID, "i in tempIndex", i, "type of widget", j))
        tempWidgetOutput <<- total_Widget_List[[group_ID+1]][[j]]
        outputList[[i]] <- total_Widget_List[[group_ID+1]][[j]]
      }
    }
  }
  
  return(outputList)
}

outputGroupConditionPointers <- function(num_Groups, selected_Uni_List, cond_Naming_Index_Templ) {
  num_Groups <- as.numeric(num_Groups)
  outputList <- list()
  condSelectedListIndex <- generateCondSelectedListIndex(num_Groups,selected_Uni_List, list(), cond_Naming_Index_Templ)
  conditionsNamingIndex <- generateConditionsNamingIndex(num_Groups, cond_Naming_Index_Templ)
  tempPointerOptions <- cond_Naming_Index_Templ[na.omit(condSelectedListIndex[,2]),2] #pulls all the available conditions optiosn for the groups excluding those selected from the universal
  
  for (i in 1:num_Groups) {
    print(c("at outputGroupConditionPointers", i))
    tempLabel <- paste("Select Group", as.character(i), "Conditions")
    tempUIOutputID <- paste0("UI_Output_Group_", as.character(i))
    
    
    outputList[[i]] <- 
      fluidRow(
        column(10, offset = 0,
               multipleSelectizeInput(
                 names(conditionsNamingIndex)[i+2],
                 paste(tempLabel, names(conditionsNamingIndex)[i+2], tempUIOutputID),
                 tempPointerOptions
               )
        ),
        column(10, offset = 1,
               uiOutput(tempUIOutputID)
        )
      )
  }
  
  return(outputList)
}


outputGroupConditionWidgets <- function(num_Groups, output, selected_Uni_List, selected_Group_List, 
                                        total_Widget_List, cond_Naming_Index_Templ) {
  print("at outputGroupConditionsWidgets")
  num_Groups <- as.numeric(num_Groups)
  outputList <- list()
  outputRenderTestList <<- list()
  outputTestList <<- list()
  
  condSelectedListIndex <- generateCondSelectedListIndex(num_Groups,selected_Uni_List, selected_Group_List, cond_Naming_Index_Templ)
  # condNamInd <- generateConditionsNamingIndex(num_Groups, cond_Naming_Index_Templ)
  conditionsNamingIndex <- generateConditionsNamingIndex(num_Groups, cond_Naming_Index_Templ)
  
  testList <- list(
    (multipleSelectizeInput("blah", "blahLabel 1", c("choice1", "Choice 2"))),
    (multipleSelectizeInput("blah", "blahLabel 2", c("choice1", "Choice 2")))
  )
  
  
  for (i in 1:num_Groups) {
    print(c("at outputGroupConditionsWidgets", i))
    tempOutputWidgetList <- outputWidgetList(i, condSelectedListIndex, total_Widget_List)
    tempUIOutputID <- paste0("UI_Output_Group_", as.character(i))
    print(c("at outputGroupConditionsWidgets", tempUIOutputID))

    
    outputList[[i]] <-
      (tempOutputWidgetList)
    outputRenderTestList[[i]] <<- renderUI(tempOutputWidgetList)
    outputTestList[[i]] <<- tempOutputWidgetList

  }
  
  return(outputList)
  # return(output)
}


listSubsettedDataFrames <- function(patient_Data, response_Variable, num_Groups, cond_Selected_List_Index, 
                                    conditions_Selected_List, response_Variable_List, cond_Naming_Index_Templ) {
  print("at listSubsettedDataFrames function")
  outputDataFrameList <- list()
  
  print(c("response_Variable", response_Variable))
  rowID <- which(response_Variable_List$All_Response_Var == response_Variable)
  responseVariableDataSetID <- response_Variable_List$Dataset_ID[rowID]
  
  print(c("responseVariableDataSetID", responseVariableDataSetID))
  
  dataSet <- patient_Data[[responseVariableDataSetID]]
  
  #First subset entirely with regard to the universal conditions
  
  uniDataSet <- patient_Data[[responseVariableDataSetID]]
  uni_Cond_Selected_List <- na.omit(cond_Selected_List_Index[,1])
  
  if (length(uni_Cond_Selected_List) > 0) { #are there any non_NA elements in the Uni pointer?, if so, then we need to subset them
    uniPatientData <- patient_Data
    
    for (i in 1:length(uni_Cond_Selected_List)) {
      rowID <- uni_Cond_Selected_List[i]
      dataSetID <- cond_Naming_Index_Templ$Dataset_ID[rowID]
      
      print(c("Uni  i", i, "rowID", rowID))
      
      if (is.null(conditions_Selected_List[[1]][[rowID]]) == FALSE) {
        colID <- cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID]
        print(c("Uni colID", colID, cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID]))
        if (cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID] %in% cond_Naming_Index_Templ$Exact_Names_Sliders == TRUE) {
          if (cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID] %in% cond_Naming_Index_Templ$Exact_Name_Lab_Test == TRUE) {
            min <- conditions_Selected_List[[1]][[rowID]][[1]]
            max <- conditions_Selected_List[[1]][[rowID]][[2]]
            print(c("Uni Slider type min", min, "max", max))
            tempLabDataFrame <- uniPatientData[[4]]
            tempLabDataFrame <- subset(tempLabDataFrame, tempLabDataFrame$LabName == cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID]) #Excludes all patients with a single case that violates the min and max
            tempLabDataFrame <- subset(tempLabDataFrame, (tempLabDataFrame$LabValue < min | tempLabDataFrame$LabValue > max))
            uniPatientData[[dataSetID]] <- subset(uniPatientData[[dataSetID]], (uniPatientData[[dataSetID]]$PatientID %in% tempLabDataFrame$PatientID == FALSE))
          } else {
            min <- conditions_Selected_List[[1]][[rowID]][[1]]
            max <- conditions_Selected_List[[1]][[rowID]][[2]]
            print(c("Uni Slider type min", min, "max", max))
            uniPatientData[[dataSetID]] <- subset(uniPatientData[[dataSetID]], uniPatientData[[dataSetID]][[colID]] > min)
            uniPatientData[[dataSetID]] <- subset(uniPatientData[[dataSetID]], uniPatientData[[dataSetID]][[colID]] < max)
          }
        } else {
          print(c("Uni non_Slider type"))
          tempInclusiveList <- conditions_Selected_List[[1]][[rowID]]
          uniPatientData[[dataSetID]] <- subset(uniPatientData[[dataSetID]], uniPatientData[[dataSetID]][[colID]] %in% tempInclusiveList)
        }
      }
    }
    uniDataSet <- subset(uniDataSet, uniDataSet$PatientID %in% uniPatientData[[1]]$PatientID)
    uniDataSet <- subset(uniDataSet, uniDataSet$PatientID %in% uniPatientData[[2]]$PatientID)
    uniDataSet <- subset(uniDataSet, uniDataSet$PatientID %in% uniPatientData[[3]]$PatientID)
    uniDataSet <- subset(uniDataSet, uniDataSet$PatientID %in% uniPatientData[[4]]$PatientID)
    
    # uniDataSetGlobalEnviro <<- uniDataSet
  }
  
  for (i in 1:num_Groups) {
    rowID <- which(response_Variable_List$All_Response_Var == response_Variable)
    groupDataSetID <- response_Variable_List$Dataset_ID[rowID]
    groupDataSet <- patient_Data[[groupDataSetID]]
    # groupDataSetGlobal <<- patient_Data[groupDataSetID] ##Removal
    group_Cond_Selected_List <- na.omit(cond_Selected_List_Index[,(i+2)])
    
    if (is.null(group_Cond_Selected_List) == FALSE) {
      groupPatientData <- patient_Data
      
      for (j in 1:length(group_Cond_Selected_List)) {
        rowID <- group_Cond_Selected_List[j]
        dataSetID <- cond_Naming_Index_Templ$Dataset_ID[rowID]
        print(c("Group  i", i, "j", j, "rowID", rowID, "dataSetID", dataSetID, cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID]))
        
        if (is.null(conditions_Selected_List[[i+1]][[rowID]]) == FALSE) {
          colID <- cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID]
          if (cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID] %in% cond_Naming_Index_Templ$Exact_Names_Sliders == TRUE) {
            if (cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID] %in% cond_Naming_Index_Templ$Exact_Name_Lab_Test == TRUE) {
              min <- conditions_Selected_List[[i+1]][[rowID]][[1]]
              max <- conditions_Selected_List[[i+1]][[rowID]][[2]]
              print(c("Group Slider for labs type min", min, "max", max))
              tempLabDataFrame <- groupPatientData[[4]]
              tempLabDataFrame <- subset(tempLabDataFrame, tempLabDataFrame$LabName == cond_Naming_Index_Templ$Exact_Conditions_Labels[rowID]) #Excludes all patients with a single case that violates the min and max
              tempLabDataFrame <- subset(tempLabDataFrame, (tempLabDataFrame$LabValue < min | tempLabDataFrame$LabValue > max))
              groupPatientData[[dataSetID]] <- subset(groupPatientData[[dataSetID]], (groupPatientData[[dataSetID]]$PatientID %in% tempLabDataFrame$PatientID == FALSE))
            } else {
              min <- conditions_Selected_List[[i+1]][[rowID]][[1]]
              max <- conditions_Selected_List[[i+1]][[rowID]][[2]]
              print(c("Group Slider non-lab type min", min, "max", max))
              groupPatientData[[dataSetID]] <- subset(groupPatientData[[dataSetID]], groupPatientData[[dataSetID]][[colID]] > min)
              groupPatientData[[dataSetID]] <- subset(groupPatientData[[dataSetID]], groupPatientData[[dataSetID]][[colID]] < max)
            }
          } else {
            print(c("Group Non-Slider type"))
            tempInclusiveList <- conditions_Selected_List[[i+1]][[rowID]]
            groupPatientData[[dataSetID]] <- subset(groupPatientData[[dataSetID]], groupPatientData[[dataSetID]][[colID]] %in% tempInclusiveList)
          }
        }
      }
      # groupDataSetGlobal1 <<- groupDataSet
      groupDataSet <- subset(groupDataSet, groupDataSet$PatientID %in% groupPatientData[[1]]$PatientID)
      # groupDataSetGlobal2 <<- groupDataSet
      groupDataSet <- subset(groupDataSet, groupDataSet$PatientID %in% groupPatientData[[2]]$PatientID)
      # groupDataSetGlobal3 <<- groupDataSet
      groupDataSet <- subset(groupDataSet, groupDataSet$PatientID %in% groupPatientData[[3]]$PatientID)
      # groupDataSetGlobal4 <<- groupDataSet
      groupDataSet <- subset(groupDataSet, groupDataSet$PatientID %in% groupPatientData[[4]]$PatientID)
      # groupDataSetBeforeUniGlobal <<- groupDataSet
      
      groupDataSet <- subset(groupDataSet, groupDataSet$PatientID %in% uniDataSet$PatientID)
      # groupDataSetAfterUniGlobal <<- groupDataSet
      
      outputDataFrameList[[i]] <- groupDataSet
    }
  }
  return(outputDataFrameList)
}


consolidateGroupDataSets <- function(num_Groups, subsetted_Data_Frames_List, response_Variable, ind_Variable, response_Variable_List, patient_Data) {
  print("at consolidatedGroupDataSets function")
  num_Groups <- as.numeric(num_Groups)
  if (is.null(response_Variable) == FALSE) {
    if (response_Variable %in% response_Variable_List$Discrete_Response_Var == TRUE && is.null(ind_Variable) == TRUE) { #if responseVar is discrete variable and no indVar selected
      if (response_Variable == "Number of Patients") {
        print("responseVar is Number of Patients")
        outputDataFrame <- data.frame(matrix(nrow = num_Groups, ncol = 2))
        names(outputDataFrame)[1] <- "Group ID"
        names(outputDataFrame)[2] <- "Number of Patients"
        for (i in 1:num_Groups) {
          outputDataFrame[i,1] <- paste("Group", as.character(i))
          outputDataFrame[i,2] <- nrow(subsetted_Data_Frames_List[[i]])
        }
      } else if (response_Variable == "Number of Clinican Admissions") {
        print("responseVar is Number of Clinican Admissions")
        outputDataFrame <- data.frame(matrix(nrow = num_Groups, ncol = 4))
        names(outputDataFrame)[1] <- "Group ID"
        names(outputDataFrame)[2] <- "Number of Patients"
        names(outputDataFrame)[3] <- "Average Number of Clinicain Admissions Per Patient"
        names(outputDataFrame)[4] <- "Sum of All Clinician Admissions"
        
        for (i in 1:num_Groups) {
          tempIDList <- unique(subsetted_Data_Frames_List[[i]]$PatientID)
          countHits <- vector()
          for (j in 1:length(tempIDList)) {
            tempDataFrame1 <- subset(subsetted_Data_Frames_List[[i]], subsetted_Data_Frames_List[[i]]$PatientID == tempIDList[j])
            countHits[[j]] <- max(tempDataFrame1$ClinicianAdmissionID)
          }
          outputDataFrame[i,1] <- paste("Group", as.character(i))
          outputDataFrame[i,2] <- length(tempIDList)
          outputDataFrame[i,3] <- mean(countHits)
          outputDataFrame[i,4] <- sum(countHits)
        }
      } else if (response_Variable == "Number of Lab Admissions") {
        print("responseVar is Number of Lab Admissions")
        outputDataFrame <- data.frame(matrix(nrow = num_Groups, ncol = 4))
        names(outputDataFrame)[1] <- "Group ID"
        names(outputDataFrame)[2] <- "Number of Patients"
        names(outputDataFrame)[3] <- "Average Number of Lab Admissions Per Patient"
        names(outputDataFrame)[4] <- "Sum of All Lab Admissions"
        
        for (i in 1:num_Groups) {
          tempIDList <- unique(subsetted_Data_Frames_List[[i]]$PatientID)
          countHits <- vector()
          for (j in 1:length(tempIDList)) {
            tempDataFrame1 <- subset(subsetted_Data_Frames_List[[i]], subsetted_Data_Frames_List[[i]]$PatientID == tempIDList[j])
            countHits[[j]] <- max(tempDataFrame1$LabAdmissionID)
          }
          outputDataFrame[i,1] <- paste("Group", as.character(i))
          outputDataFrame[i,2] <- length(tempIDList)
          outputDataFrame[i,3] <- mean(countHits)
          outputDataFrame[i,4] <- sum(countHits)
        }
      } else if (response_Variable == "Number of Lab Tests") {
        print("responseVar is Number of Lab Tests")
        outputDataFrame <- data.frame(matrix(nrow = num_Groups, ncol = 4))
        names(outputDataFrame)[1] <- "Group ID"
        names(outputDataFrame)[2] <- "Number of Patients"
        names(outputDataFrame)[3] <- "Average Number of Lab Tests Per Patient"
        names(outputDataFrame)[4] <- "Sum of Total Number of Lab Tests Performed"
        
        for (i in 1:num_Groups) {
          tempIDList <- unique(subsetted_Data_Frames_List[[i]]$PatientID)
          countHits <- vector()
          for (j in 1:length(tempIDList)) {
            tempDataFrame1 <- subset(subsetted_Data_Frames_List[[i]], subsetted_Data_Frames_List[[i]]$PatientID == tempIDList[j])
            countHits[[j]] <- nrow(tempDataFrame1)
          }
          outputDataFrame[i,1] <- paste("Group", as.character(i))
          outputDataFrame[i,2] <- length(tempIDList)
          outputDataFrame[i,3] <- mean(countHits)
          outputDataFrame[i,4] <- sum(countHits)
        }
      } else if (response_Variable == "Number of Diagnoses") {
        print("responseVar is Number of Diagnoses")
        outputDataFrame <- data.frame(matrix(nrow = num_Groups, ncol = 4))
        names(outputDataFrame)[1] <- "Group ID"
        names(outputDataFrame)[2] <- "Number of Patients"
        names(outputDataFrame)[3] <- "Average Number of Diagnoses Per Patient"
        names(outputDataFrame)[4] <- "Sum of Total Number of Diagnoses for All Patients"
        
        for (i in 1:num_Groups) {
          tempIDList <- unique(subsetted_Data_Frames_List[[i]]$PatientID)
          countHits <- vector()
          for (j in 1:length(tempIDList)) {
            tempDataFrame1 <- subset(subsetted_Data_Frames_List[[i]], subsetted_Data_Frames_List[[i]]$PatientID == tempIDList[j])
            countHits[[j]] <- nrow(tempDataFrame1)
          }
          outputDataFrame[i,1] <- paste("Group", as.character(i))
          outputDataFrame[i,2] <- length(tempIDList)
          outputDataFrame[i,3] <- mean(countHits)
          outputDataFrame[i,4] <- sum(countHits)
        }
      }
    } else if (response_Variable %in% response_Variable_List$Cont_Response_Var == TRUE && is.null(ind_Variable) == TRUE) { #if responseVar is a cont variable and no indVar selected
      outputDataFrame <- data.frame(matrix(nrow = num_Groups, ncol = 7))
      names(outputDataFrame)[1] <- "Group ID"
      names(outputDataFrame)[2] <- paste(response_Variable, "Min")
      names(outputDataFrame)[3] <- paste(response_Variable, "1st Qu.")
      names(outputDataFrame)[4] <- paste(response_Variable, "Median")
      names(outputDataFrame)[5] <- paste(response_Variable, "Mean")
      names(outputDataFrame)[6] <- paste(response_Variable, "3rd Qu.")
      names(outputDataFrame)[7] <- paste(response_Variable, "Max")
        if (response_Variable == "Patient Percentage Below Poverty") {
          for (i in 1:num_Groups) {
            testStat <- summary(subsetted_Data_Frames_List[[i]])
            for (j in 1:length(testStat)) {
              outputDataFrame[i,1] <- paste("Group", as.character(i)) 
              outputDataFrame[i,(j+1)] <- testStat[[j]] 
            }
          }
        } else if (response_Variable %in% response_Variable_List$Lab_Test_Name == TRUE) {
          for (i in 1:num_Groups) {
            tempDataSet <- subset(subsetted_Data_Frames_List[[i]], subsetted_Data_Frames_List[[i]]$LabName == response_Variable)
            testStat <- summary(tempDataSet$LabValue)
            for (j in 1:length(testStat)) {
              outputDataFrame[i,1] <- paste("Group", as.character(i)) 
              outputDataFrame[i,(j+1)] <- testStat[[j]] 
            }
          }
        }
      } else if (response_Variable %in% response_Variable_List$Cont_Response_Var == TRUE 
                 && ind_Variable %in% response_Variable_List$Cont_Response_Var == TRUE) {
        
        outputDataFrame = data.frame(matrix(nrow = num_Groups, ncol = 11))
        names(outputDataFrame)[1] <- "Group ID"
        names(outputDataFrame)[2] <- "n"
        names(outputDataFrame)[3] <- "Slope"
        names(outputDataFrame)[4] <- "Intercept"
        names(outputDataFrame)[5] <- "R-Value (Cor)"
        names(outputDataFrame)[6] <- paste(ind_Variable, "Median")
        names(outputDataFrame)[7] <- paste(ind_Variable, "Mean")
        names(outputDataFrame)[8] <- paste(ind_Variable, "StdDev")
        names(outputDataFrame)[9] <- paste(response_Variable, "Median")
        names(outputDataFrame)[10] <- paste(response_Variable, "Mean")
        names(outputDataFrame)[11] <- paste(response_Variable, "StdDev")
        
        if (response_Variable == "Patient Percentage Below Poverty" || ind_Variable == "Patient Percentage Below Poverty") {
          print("about to lapply extractLinearNonLabComparisonChildren function")
          linearCompDataFrameList <- lapply(subsetted_Data_Frames_List, extractLinearNonLabComparisonChildren, 
                                                  ind_Variable = ind_Variable, response_Variable = response_Variable, patient_Data = patient_Data)
        } else {
          print("about to lapply extractLinearLabComparisonChildren function")
          linearCompDataFrameList <- lapply(subsetted_Data_Frames_List, extractLinearLabComparisonChildren, 
                                            ind_Variable = ind_Variable, response_Variable = response_Variable)
        }
          
        for (i in 1:num_Groups) {
          outputDataFrame[i,1] <- paste("Group", as.character(i))
          outputDataFrame[i,2] <- nrow(linearCompDataFrameList[[i]])
          
          print(c("linear Analysis", i))
          if (nrow(linearCompDataFrameList[[i]]) < 2) { #to account for situtations where there are not enough elements for a linear analysis
            print(c("linear Analysis Rejected", i))
            outputDataFrame[i,3] <- NA
            outputDataFrame[i,4] <- NA
            outputDataFrame[i,5] <- NA
          } else {
            print(c("linear Analysis Accepted", i))
            testDataFrame <<- linearCompDataFrameList[[i]]
            linearAnalysis <- lm(linearCompDataFrameList[[i]][[response_Variable]] ~ linearCompDataFrameList[[i]][[ind_Variable]])
            outputDataFrame[i,3] <- linearAnalysis[[1]][[2]] #slope
            outputDataFrame[i,4] <- linearAnalysis[[1]][[1]] #intercept
            outputDataFrame[i,5] <- cor(x = linearCompDataFrameList[[i]][[ind_Variable]], y = linearCompDataFrameList[[i]][[response_Variable]], use="complete.obs") #Corr
          }
          
          print(c("indVariable Analysis", i))
          indVariableAnalysis <- summary(linearCompDataFrameList[[i]][[ind_Variable]])
          print(c("responseVariable Analysis", i))
          responseVariableAnalysis <- summary(linearCompDataFrameList[[i]][[response_Variable]])
          
          outputDataFrame[i,6] <- indVariableAnalysis[[3]]
          outputDataFrame[i,7] <- indVariableAnalysis[[4]]
          outputDataFrame[i,8] <- sd(linearCompDataFrameList[[i]][[ind_Variable]], na.rm = TRUE)
          
          outputDataFrame[i,9] <- responseVariableAnalysis[[3]]
          outputDataFrame[i,10] <- responseVariableAnalysis[[4]]
          outputDataFrame[i,11] <- sd(linearCompDataFrameList[[i]][[response_Variable]], na.rm = TRUE)
          

          # for (j in 1:length(indVariableAnalysis)) {
          #   outputDataFrame[i,4+j] <- indVariableAnalysis[[j]]
          # }
          # for (j in 1:length(responseVariableAnalysis)) {
          #   outputDataFrame[i,10+j] <- responseVariableAnalysis[[j]]
          # }
        }
          
        }
      
  }
  
  return(outputDataFrame)
}


extractLinearLabComparisonChildren <- function(data_set, ind_Variable, response_Variable) {
  print("at extractLinearLabComparisonChildren")
  tempTestDataFrame <- data.frame(matrix(nrow = 1, ncol = 4))
  names(tempTestDataFrame)[1] <- "PatientID"
  names(tempTestDataFrame)[2] <- "LabAdmissionID"
  names(tempTestDataFrame)[3] <- ind_Variable
  names(tempTestDataFrame)[4] <- response_Variable
  
  tempIDList <- unique(data_set$PatientID)
  tempSubset0 <- subset(data_set, (data_set$LabName == response_Variable | data_set$LabName == ind_Variable))
  rowShift = 1
  
  for (i in 1:length(tempIDList)) {
    # print(c(i, length(tempIDList)))
    tempSubSet1 <- subset(tempSubset0, tempSubset0$PatientID == tempIDList[i])
    maxLabAdmissionID <- max(tempSubSet1$LabAdmissionID)
    # print(maxLabAdmissionID)
    for (j in 1:maxLabAdmissionID) {
      tempSubSet2 <- subset(tempSubSet1, tempSubSet1$LabAdmissionID == j)
      tempSubSet2 <<- subset(tempSubSet1, tempSubSet1$LabAdmissionID == j)
      tempTestDataFrame[rowShift,1] <- tempIDList[i]
      tempTestDataFrame[rowShift,2] <- j
      
      rowID1 <- which(tempSubSet2$LabName == ind_Variable)
      tempTestDataFrame[rowShift,3] <- mean(tempSubSet2$LabValue[rowID1])
      
      
      rowID2 <- which(tempSubSet2$LabName == response_Variable)
      tempTestDataFrame[rowShift,4] <- mean(tempSubSet2$LabValue[rowID2])
      
      rowShift = rowShift +1
    }
  }
  return(tempTestDataFrame)
}

extractLinearNonLabComparisonChildren <- function(data_set, ind_Variable, response_Variable, patient_Data) { #data_set must be inclusive of the response_Variable
  print("at extractLinearNonLabComparisonChildren")
  tempTestDataFrame <- data.frame(matrix(nrow = 1, ncol = 3))
  names(tempTestDataFrame)[1] <- "PatientID"
  names(tempTestDataFrame)[2] <- ind_Variable
  names(tempTestDataFrame)[3] <- response_Variable
  
  if ((response_Variable == "Patient Percentage Below Poverty" || response_Variable == "Percentage Below Poverty") &&
      ind_Variable != "Patient Percentage Below Poverty") {
    tempLabs <- patient_Data[[4]]
    tempDataFrame1 <- subset(tempLabs, tempLabs$LabName == ind_Variable)
    patientIDList <- unique(data_set$PatientID)
    
    for(i in 1:length(patientIDList)) {
      # print(c("response_Variable is percentage poverty", i, length(patientIDList)))
      tempTestDataFrame[i,1] <- patientIDList[i]
      
      tempDataFrame2 <- subset(tempDataFrame1, tempDataFrame1$PatientID == patientIDList[i])
      tempTestDataFrame[i,2] <- mean(tempDataFrame2$LabValue)
      
      rowID2 <- which(data_set$PatientID == patientIDList[i])
      tempTestDataFrame[i,3] <- data_set$PatientPopulationPercentageBelowPoverty[rowID2]
    }
  } else if ((ind_Variable == "Patient Percentage Below Poverty" || ind_Variable == "Percentage Below Poverty") 
             && response_Variable != "Patient Percentage Below Poverty") {
    tempCorePopulated <- patient_Data[[1]]
    tempDataFrame1 <- subset(data_set, data_set$LabName == response_Variable)
    patientIDList <- unique(tempDataFrame1$PatientID)
    
    
    for(i in 1:length(patientIDList)) {
      # print(c("ind_Variable is percentage poverty", i, length(patientIDList)))
      tempTestDataFrame[i,1] <- patientIDList[i]
      
      rowID1 <- which(tempCorePopulated$PatientID == patientIDList[i])
      tempTestDataFrame[i,2] <- tempCorePopulated$PatientPopulationPercentageBelowPoverty[rowID1]
      
      rowID2 <- which(tempDataFrame1$PatientID == patientIDList[i])
      tempTestDataFrame[i,3] <- mean(tempDataFrame1$LabValue[rowID2])
    }
  } else if (ind_Variable == "Patient Percentage Below Poverty" && response_Variable == "Patient Percentage Below Poverty") {
    patientIDList <- unique(data_set$PatientID)
    
    for (i in 1:length(patientIDList)) {
      # print(c("both Var is percentage poverty", i, length(patientIDList)))
      tempTestDataFrame[i,1] <- patientIDList[i]
      rowID <- which(data_set$PatientID == patientIDList[i])
      tempTestDataFrame[i,2] <- data_set$PatientPopulationPercentageBelowPoverty[rowID]
      tempTestDataFrame[i,3] <- data_set$PatientPopulationPercentageBelowPoverty[rowID]
    }
  }
  
  return(tempTestDataFrame)
}




#TROUBLESHOOTING FUNCTIONS HERE
# testListUni <- list("Patient Gender", "CBC: HEMOGLOBIN")
# testListUni <- c("Patient Gender", "CBC: HEMOGLOBIN")
# 
# testList1 <- list("Primary Diagnosis Code", "Lab Date and Time")
# testList2 <- list("Clinician Admission ID", "METABOLIC: SODIUM", "Percentage Below Poverty")
# # testList2 <- list("Clinician Admission ID", "METABOLIC: SODIUM")
# testList3 <- list(testList1, testList2)
# 
# testList4 <- vector(mode = "list", length = 48)
# testList5 <- vector(mode = "list", length = 48)
# testList6 <- vector(mode = "list", length = 48)
# testList4[[2]] <- list("Female", "Male") #patient gender
# testList4[[17]] <- list(10,19) #cbc hemoglobin
# testList5[[8]] <- list("O98.612", "C40.31", "O9A.12") #primary diagnosis code
# testList5[[13]] <- list("1938-05-21 02:09:34", "2017-08-05 15:24:38") #lab date and time
# testList6[[9]] <- list(5,12) #clinician admission ID
# testList6[[18]] <- list(125,155) #metabolic sodium
# testList6[[7]] <- list(1,20) #percentage below poverty
# testList4 <- list(list("Male"), list(11,13)) #Uni selections
# testList5 <- list(list("A18.7", "A22.1"), list("1938-05-21 02:09:34", "2017-08-05 15:24:38")) #Group 1 seleciton
# testList6 <- list(list(5,12), list(130,150)) #Group 2 selection
# testList7 <- list(testList4, testList5, testList6)

# condSel <- generateCondSelectedListIndex(2,testListUni, testList3,condNamingIndexTempl)
# condNamInd <- generateConditionsNamingIndex(2, condNamingIndexTempl)
# condOptions <- generateConditionsOptions(condNamingIndexTempl, patientData)

# totWidgetList <- generateTotalWidgetList(2, condNamInd, condOptions, condNamingIndexTempl)
# outputWidList <- outputWidgetList("Uni", condSel, totWidgetList)
# c("Primary Diagnosis Code", "CBC: HEMOGLOBIN")
# groupCondWidgets <- outputGroupConditionWidgets(10, c("Primary Diagnosis Code", "CBC: HEMOGLOBIN"), testList3, condSel, totWidgetList, condNamingIndexTempl)
# groupCondPointers <- outputGroupConditionPointers(10, c("Primary Diagnosis Code", "CBC: HEMOGLOBIN"), condNamingIndexTempl)
# subsetDataFrames <- listSubsettedDataFrames(patientData, "CBC: HEMOGLOBIN", 2, condSel, testList7, responseVariableList, condNamingIndexTempl)


#consolGroupDatSet <- consolidateGroupDataSets(2, subsetDataFrames, "CBC: HEMOGLOBIN", "CBC: HEMOGLOBIN", responseVariableList, patientData)


#view(testDataFrame)
#<p class=MsoNoSpacing>replace_Lab_History</p>

#TROUBLESHOOTING FUNCTION HERE
# exampleID <- "DBB78149-D86C-435E-82C4-341999FD0719"
# test <- generateHistoryReport(patientReportTemplate, patientLabReportTemplate, labReference,exampleID, patientData)
# patientDiagnoses <- subset(patientData[[3]], patientData[[3]]$PatientID == exampleID)
# patientAdmissions <- subset(patientData[[2]], patientData[[2]]$PatientID == exampleID)
# patientLab <- subset(patientData[[4]], patientData[[4]]$PatientID == exampleID)
# labAdmission <- subset(patientLab, patientLab$AdmissionID == 1)
# labAdmission <- labAdmission[order(labAdmission$LabName),]

# for (i in unique(patientLab$AdmissionID)) {
#   print(i)
# }

totalDataPresentationUI <- function(num_Groups) {
  num_Groups <- as.numeric(num_Groups)
  outputList <- list()
  
  for (i in 1:num_Groups) {
    tempUIOutputID <- Total_Data_Presentation
    
  }
  
}


#----------------------------------------------------------------------------------------------------
#####################################################################################################
############################ User Design Progress ###################################################
#####################################################################################################


ui <- fluidPage(navbarPage("VERITAS", id="mainTabset",
                 tabPanel("Home",
                          h3("Welcome to VERITAS"),
                          p("VERITAS is a web-based tool to allow clinicians easily input, access, store, and analyze patient data"),
                          ),
                 
                 tabPanel("Patient Data",value = "patient_data",
                          sidebarLayout(
                            sidebarPanel(
                              p("Pull EHR for a specific patient here:"),
                              selectizeInput(inputId = 'patientHistoryID',
                                             label = 'Search Patient ID',
                                             choices = patientData[[1]]$PatientID,
                                             selected = NULL,
                                             multiple = TRUE, #though only 1 input, necessary to prevent autofill
                                             options = list(maxItems = 1)
                                             ),#caps to only 1 input, but does not autofill the box
                              downloadButton("downloadPatientHistoryReport", "Download Patient History Report"),
                              br(""),
                              actionButton("switchToEdit", "Edit Patient Data"),
                              actionButton("addPatientHistory", "Add Patient Data")
                              # downloadLink("downloadPlot", "Download Plot")
                            ),
                            
                            mainPanel(
                              htmlOutput("patientHistoryReport")
                            )
                          )
                 ),
                 
                 
                 
                 tabPanel("Manage Data", value = "manage_data",
                          navlistPanel(fluid=TRUE,widths = c(2, 10), id="manageDataSubpanel",
                             tabPanel("Add Data", value="add_data",
                                navlistPanel(fluid=TRUE, widths=c(2,10), id="addDataSubpanel",
                                  tabPanel("Add Admission Record", value="add_admission",
                                    h1("Add Visit Record"),
                                      dateInput("visit_start_date", "Admission Date: "),
                                      dateInput("visit_end_date", "Release Date: "),
                                      textInput("visit_diagnosis_code", "Primary Diagnosis Code: "),
                                      textInput("visit_description", "Description: "),
                                      actionButton("addVisitLog", "Add Visit Record")
                                    ),
                                  tabPanel("Add Lab Record", value="add_lab_record",
                                    h1("Add Lab Record"),
                                    selectInput("lab_name", "Lab Name: ", choices=c("CBC: HEMATOCRIT", "METABOLIC: ANION GAP","CBC: LYMPHOCYTES","CBC: HEMOGLOBIN", "METABOLIC: SODIUM","METABOLIC: ALBUMIN","METABOLIC: BUN","CBC: NEUTROPHILS","METABOLIC: CALCIUM","METABOLIC: GLUCOSE","URINALYSIS: PH", "METABOLIC: BILI TOTAL"
                                                                                                                          , "METABOLIC: POTASSIUM","URINALYSIS: RED BLOOD CELLS","METABOLIC: CARBON DIOXIDE","METABOLIC: CREATININE","URINALYSIS: SPECIFIC GRAVITY","CBC: MEAN CORPUSCULAR VOLUME","METABOLIC: CHLORIDE","METABOLIC: ALT/SGPT","METABOLIC: AST/SGOT","METABOLIC: ALK PHOS","CBC: EOSINOPHILS","CBC: ABSOLUTE NEUTROPHILS"," CBC: MCH", "URINALYSIS: WHITE BLOOD CELLS",
                                                                                                                          "CBC: ABSOLUTE LYMPHOCYTES","CBC: PLATELET COUNT","CBC: RED BLOOD CELL COUNT","CBC: WHITE BLOOD CELL COUNT","CBC: RDW","CBC: MCHC","CBC: MONOCYTES","METABOLIC: TOTAL PROTEIN","CBC: BASOPHILS")),
                                    textInput("lab_value", "Lab Value: "),
                                    textInput("lab_units", "Lab Units: "),
                                    dateInput("lab_date", "Lab Date: "),
                                    actionButton("addLabVisit", "Add Lab Record")
                                  )
                                )
                             ),
                            tabPanel("Edit Data", value="edit_data",
                              h1("Edit Patient Data"),
                              h3(textOutput("patientHistoryID")),
                              br(""),
                              h3("Patient Demographic"),
                              textInput("patient_race", "Race: "),
                              textInput("patient_marital_status", "Marital Status: "),
                              textInput("patient_sex", "Sex: "),
                              textInput("patient_language", "Language: "),
                              textInput("patient_percent_below_poverty", "Percentage Below Poverty: "),
                              actionButton("updatePatientDemographic", "Update Patient Demographic")
                            )
                          )
                    ),

                 
                 tabPanel("Analysis Tool",
                          useShinyjs(),
                          sidebarLayout(
                            sidebarPanel(
                              p("Variable Selection and Conditions",
                                fluidRow(
                                  column(10, offset = 0,
                                         selectizeInput("observationAnalysis", 
                                                        "Select Observation or Analysis Mode",
                                                        c("Observation: Summarize Data",
                                                          "Analysis: Student T-test",
                                                          "Analysis: One-way ANOVA",
                                                          "Analysis: Linear Regression Model")),
                                         uiOutput("responseVariableUIOutput"),
                                         # singleSelectizeInput("responseVariable", "Response Variable (select first)", responseVariableList$All_Response_Var),
                                         uiOutput("indVariableUIOutput"),
                                         checkboxInput("universalCondButton", "Apply Universal Inclusive Conditions", value = TRUE),
                                         uiOutput("Pointer_ID_Uni_UI_Output"),
                                         uiOutput("Select_Input_ID_Uni"),
                                         verbatimTextOutput("textOutputValues"),
                                         uiOutput("numGroupsUIOutput"),
                                         )
                                  
                                ),
                                uiOutput("groupConditionsWidgets"),
                                actionButton("executeButton", "Execute"),
                                actionButton("helpButton", "Help")

                                )

                            ),
                            mainPanel(
                              h1("Display Results"),
                              uiOutput("Select_Display_Choices_UI_Output"),
                              DT::dataTableOutput("Summarized_Data_Presentation"),
                              DT::dataTableOutput("Total_Data_Presentation")
                              # checkboxGroupInput()
                            )
                          )
                )

))

server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  current_patient<-NULL
  #This observe function will update a list with currently selected patient information for easy access in other functions#
  
  observeEvent(input$patientHistoryID, {
    if (is.null(input$patientHistoryID)){
      current_patient<<- NULL
    }else{
      current_patient<<- subsetPatientData(patient_ID = toString(input$patientHistoryID), patient_Data = patientData)
    }
  })
    
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
    
    output$patientHistoryReport <- renderText({
      if (is.null(input$patientHistoryID) == TRUE) {
        return(NULL)
      } else {
        patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportHeaderTemplate, patientLabReportRowTemplate,
                                                        labReference, input$patientHistoryID, patientData)
        return(patient_History_Report)
      }
      
    })
    
    
    
output$downloadPatientHistoryReport <- downloadHandler(

      # filename <- paste(as.character(input$patientHistoryID), "_", Sys.Date(), ".pdf", sep=""),
      filename <- "report.pdf",
      content <- function(file) {

        tempReport <- file.path(tempdir(), "Report.Rmd")
        file.copy("Report.Rmd", tempReport, overwrite = TRUE)
        patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportHeaderTemplate, patientLabReportRowTemplate,
                                                        labReference, input$patientHistoryID, patientData)
        patient_History_Report <<- generateHistoryReport(patientReportTemplate, patientLabReportHeaderTemplate, patientLabReportRowTemplate,
                                                        labReference, input$patientHistoryID, patientData)
        
        # text_output <- str_remove(patient_History_Report, "</p>\n\n</div>\n\n<span style='font-size:12.0pt;font-family:\"Times New Roman\",serif'><br\nclear=all style='page-break-before:auto'>\n</span>\n\n<div class=WordSection2>\n\n<div style='border:none;border-bottom:solid windowtext 1.0pt;padding:0in 0in 1.0pt 0in'>\n\n")
        # text_output <- str_remove(text_output, "\n\n</div>\n\n<p class=MsoNoSpacing>")
        # Set up parameters to pass to Rmd document
        params <- list(HTML_File = patient_History_Report)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        html<- rmarkdown::render(tempReport, params = params,
                          envir = new.env(parent = globalenv()))
        out <- pagedown::chrome_print(html, "test.pdf")
        file.rename(out, file)
      }
    )
    
    output$patientHistoryID<-renderText({
      if (is.null(input$patientHistoryID)) {
        return("No Patient Selected")
      } else {
        return(paste("Patient ID: ", toString(input$patientHistoryID)))
    }
    })
    
    
    observeEvent(input$switchToEdit, {
      updateTabsetPanel(session, "mainTabset", selected = "manage_data")
      updateNavlistPanel(session, 'manageDataSubpanel', selected='edit_data')
      
      ##Update all patient boxes to display current value to be edited
      print(current_patient)
      ##Update Current Patient in case it was changed
      if (is.null(input$patientHistoryID)){
        current_patient<<- NULL
      }else{
        current_patient<<- subsetPatientData(patient_ID = toString(input$patientHistoryID), patient_Data = patientData)
      }
      if(is.null(current_patient)){
      }else{
        updateTextInput(session, "patient_dob", value = current_patient[[core_populated_id]]$PatientDateOfBirth)
        updateTextInput(session, "patient_race", value = current_patient[[core_populated_id]]$PatientRace)
        updateTextInput(session, "patient_marital_status", value = current_patient[[core_populated_id]]$PatientMaritalStatus)
        updateTextInput(session, "patient_sex", value = current_patient[[core_populated_id]]$PatientGender)
        updateTextInput(session, "patient_language", value = current_patient[[core_populated_id]]$PatientLanguage)
        updateTextInput(session, "patient_percent_below_poverty", value = current_patient[[core_populated_id]]$PatientPopulationPercentageBelowPoverty)
      }
  
     })
    
    observeEvent(input$addPatientHistory, {
      updateTabsetPanel(session, "mainTabset", selected="manage_data")
      updateNavlistPanel(session, 'manageDataSubpanel', selected='add_data')
      updateTextInput(session, "visit_diagnosis_code", value = "")
      updateTextInput(session, "visit_description", value = "")

    })
    
    observeEvent(input$updatePatientDemographic, {
      updateTabsetPanel(session, "mainTabset", selected = "patient_data")
      patientData[[core_populated_id]]$PatientRace[[match(current_patient[[core_populated_id]]$PatientID, patientData[[core_populated_id]]$PatientID)]]<<-input$patient_race
      patientData[[core_populated_id]]$PatientMaritalStatus[[match(current_patient[[core_populated_id]]$PatientID, patientData[[core_populated_id]]$PatientID)]]<<-input$patient_marital_status
      patientData[[core_populated_id]]$PatientGender[[match(current_patient[[core_populated_id]]$PatientID, patientData[[core_populated_id]]$PatientID)]]<<-input$patient_sex
      patientData[[core_populated_id]]$PatientLanguage[[match(current_patient[[core_populated_id]]$PatientID, patientData[[core_populated_id]]$PatientID)]]<<-input$patient_language
      patientData[[core_populated_id]]$PatientPopulationPercentageBelowPoverty[[match(current_patient[[core_populated_id]]$PatientID, patientData[[core_populated_id]]$PatientID)]]<<-input$patient_percent_below_poverty
      
      output$patientHistoryReport <<- renderText({
        if (is.null(input$patientHistoryID) == TRUE) {
          return(NULL)
        } else {
          patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportHeaderTemplate, patientLabReportRowTemplate,
                                                          labReference, input$patientHistoryID, patientData)
          return(patient_History_Report)
        }
        
      })
    })

    observeEvent(
      {
          c(input$responseVariable,
          input$observationAnalysis)
      }, {
        if (is.null(input$responseVariable) == FALSE && is.null(input$observationAnalysis) == FALSE) {
          if (
            (input$responseVariable %in% responseVariableList$Cont_Response_Var == TRUE) &&
              (input$observationAnalysis %in% c("Observation: Summarize Data", "Analysis: Linear Regression Model") == TRUE)) {
            # print(input$observationAnalysis)
            output$indVariableUIOutput <- 
              renderUI(singleSelectizeInput("indVariable", "Independent Variable (if applicable)", responseVariableList$Cont_Response_Var))
          } else {
            output$indVariableUIOutput <- renderUI(NULL)
          }
        } else {
         output$indVariableUIOutput <- renderUI(NULL)
        }

      }
    )
    
    observeEvent(
      {
        c(input$observationAnalysis)
      }, {
        if (is.null(input$observationAnalysis) == FALSE) {
          if (input$observationAnalysis == "Observation: Summarize Data") {
            print("at display group checkbox for viewing data")
            output$Select_Display_Choices_UI_Output <- 
              renderUI(checkboxGroupInput("Select_Display_Choices", "Display Options", 
                                          choices = list("Summarized Data Presentation", "Total Data Presentation", "Visualization"),
                                          selected = list("Summarized Data Presentation", "Total Data Presentation", "Visualization")))
          } else {
            output$Select_Display_Choices_UI_Output <- 
              renderUI(checkboxGroupInput("Select_Display_Choices", "Display Options", 
                                          choices = list("Analysis Results", "Summarized Data Presentation", "Total Data Presentation", "Visualization"),
                                          selected = list("Analysis Results", "Summarized Data Presentation", "Total Data Presentation", "Visualization")))
          }
        } else {
          output$Select_Display_Choices_UI_Output <- renderUI(NULL)
        }
        
      }
    )
    
    
    
    # observeEvent(
    #   {input$mainTabset
    #     input$responseVariable}
    #     , {
    #   if (is.null(input$responseVariable) == TRUE) {
    #     hide("indVariable")
    #   } else if (input$responseVariable %in% responseVariableList$Cont_Response_Var == TRUE) {
    #     show("indVariable")
    #   } else {
    #     hide("indVariable")
    #   }
    # })

    observeEvent(input$helpButton, {
      updateTabsetPanel(session, "mainTabset", selected = "home")
      
    })
    
    observeEvent(input$addLabVisit, {
      current_patient<<- subsetPatientData(patient_ID = toString(input$patientHistoryID), patient_Data = patientData)
      temp_lab_number<-1
      temp_lab<- data.frame(input$patientHistoryID, temp_lab_number, input$lab_name, input$lab_value, input$lab_units, input$lab_date)
      colnames(temp_lab)<-c("PatientID", "LabAdmissionID", "LabName", "LabValue", "LabUnits", "LabDateTime")
      patientData[[4]]<<-rbind(patientData[[4]], temp_lab)
      print(tail(patientData[[4]]))
      
      updateTabsetPanel(session, "mainTabset", selected = "patient_data")
      
      output$patientHistoryReport <- renderText({
        if (is.null(input$patientHistoryID) == TRUE) {
          return(NULL)
        } else {
          patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportHeaderTemplate, patientLabReportRowTemplate,
                                                          labReference, input$patientHistoryID, patientData)
          return(patient_History_Report)
        }
        
      })

    })
    observeEvent(input$addVisitLog, {
      current_patient<<- subsetPatientData(patient_ID = toString(input$patientHistoryID), patient_Data = patientData)
      
      temp_clinician_number<-max(current_patient[[3]]$ClinicianAdmissionID)+1
      temp_admission_date<- data.frame(input$patientHistoryID, temp_clinician_number, input$visit_start_date, input$visit_end_date)
      colnames(temp_admission_date)<-c("PatientID", "ClinicianAdmissionID", "AdmissionStartDate", "AdmissionEndDate")
      patientData[[2]]<<-rbind(patientData[[2]],temp_admission_date)
      
      temp_diagnostic_date <- data.frame(input$patientHistoryID, temp_clinician_number, input$visit_diagnosis_code, input$visit_description)
      colnames(temp_diagnostic_date) <-c("PatientID", "ClinicianAdmissionID", "PrimaryDiagnosisCode", "PrimaryDiagnosisDescription")
      patientData[[3]]<<-rbind(patientData[[3]], temp_diagnostic_date)

      updateTabsetPanel(session, "mainTabset", selected = "patient_data")
      
      output$patientHistoryReport <- renderText({
        if (is.null(input$patientHistoryID) == TRUE) {
          return(NULL)
        } else {
          patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportHeaderTemplate, patientLabReportRowTemplate,
                                                          labReference, input$patientHistoryID, patientData)
          return(patient_History_Report)
        }
        
      })
      
    })
    
    observeEvent(input$universalCondButton, {
      if (input$universalCondButton == TRUE) {
        output$Pointer_ID_Uni_UI_Output <- 
          renderUI(multipleSelectizeInput("Pointer_ID_Uni", "Select Universal Conditions", condNamingIndexTempl[,2]))
      } else {
        output$Pointer_ID_Uni_UI_Output <- renderUI(NULL)
      }
    })
    
    
    observeEvent(
        input$observationAnalysis, {
        if (is.null(input$observationAnalysis) == FALSE) {
          observationAnalysis <- input$observationAnalysis
          if (observationAnalysis == "Observation: Summarize Data") {
            output$numGroupsUIOutput <- renderUI(
              selectizeInput("numGroups", "Number of Groups", c(1:10), selected = "1", multiple = FALSE)
            )
          } else if (observationAnalysis == "Analysis: Student T-test") {
            output$numGroupsUIOutput <- renderUI(
              selectizeInput("numGroups", "Number of Groups", c(2), selected = "2", multiple = FALSE)
            )
          } else if (observationAnalysis == "Analysis: One-way ANOVA") {
            output$numGroupsUIOutput <- renderUI(
              selectizeInput("numGroups", "Number of Groups", c(2:10), selected = "2", multiple = FALSE)
            )
          } else if (observationAnalysis == "Analysis: Linear Regression Model") {
            output$numGroupsUIOutput <- renderUI(
              selectizeInput("numGroups", "Number of Groups", c(1), selected = "1", multiple = FALSE)
            )
          }
        } else {
          output$numGroupsUIOutput <- renderUI(NULL)
        }}
    )
    
    observeEvent(
      input$observationAnalysis, {
        if (is.null(input$observationAnalysis) == FALSE) {
          observationAnalysis <- input$observationAnalysis
          if (observationAnalysis == "Observation: Summarize Data") {
            output$responseVariableUIOutput <- renderUI(
              singleSelectizeInput("responseVariable", "Response Variable (select first)", responseVariableList$All_Response_Var)
            )
          } else {
            output$responseVariableUIOutput <- renderUI(
              singleSelectizeInput("responseVariable", "Response Variable (select first)", responseVariableList$Cont_Response_Var)
            )
          }
        } else {
          output$numGroupsUIOutput <- renderUI(NULL)
        }}
    )
    
    
###CHECK_HERE
    observeEvent(input$mainTabset, {
      if (input$mainTabset == "Analysis Tool") {
        generateConditionsOptionsReactive <- reactive({
          tempDataFrame2 <- generateConditionsOptions(condNamingIndexTempl, patientData)
          return(tempDataFrame2)
        })
      }
    })

    
    generateConditionsNamingIndexReactive <- eventReactive(
      input$numGroups, {
        generateConditionsNamingIndex(input$numGroups, condNamingIndexTempl)
      }
    )
    
    generateConditionsOptionsReactive <- eventReactive(
      input$mainTabset, {
        if (input$mainTabset == "Analysis Tool") {
            generateConditionsOptions(condNamingIndexTempl, patientData)
        }
      }
    )
    
    generateTotalWidgetListReactive <- eventReactive(
      input$mainTabset, {
        if (input$mainTabset == "Analysis Tool") {
          generateTotalWidgetList(
            input$numGroups, 
            generateConditionsNamingIndexReactive(),
            generateConditionsOptionsReactive(),
            condNamingIndexTempl)
        }
      }
    )
    
    #Make this another generateCondSelectedListReactive function
    generateCondSelectedListIndexReactiveTotal <- eventReactive(
      input$mainTabset, {
        if (input$mainTabset == "Analysis Tool") {
          tempSelectedGroupList <- getListedInputs(input, names(generateConditionsNamingIndexReactive())[3:(2+as.numeric(input$numGroups))])
          tempSelectedGroupList <<- getListedInputs(input, names(generateConditionsNamingIndexReactive())[3:(2+as.numeric(input$numGroups))])
            generateCondSelectedListIndex(
              input$numGroups, 
              input$Pointer_ID_Uni, 
              tempSelectedGroupList, 
              condNamingIndexTempl)
        }
      }
    )
    
    
    getPointerListedInputsReactive <- reactive({
      getListedInputs(input, names(generateConditionsNamingIndexReactive())[3:(2+as.numeric(input$numGroups))])
      # print(c("at getPointerListedInputsReactive 3:", (2+as.numeric(input$numGroups))))
      print(c("at getPointerListedInputsReactive",
              names(generateConditionsNamingIndexReactive())[3:(2+as.numeric(input$numGroups))]))
      
    })
    
    
    
    observeEvent(input$universalCondButton, {
      if (input$universalCondButton == TRUE) {
        output$Select_Input_ID_Uni <- renderUI({
          if (is.null(input$Pointer_ID_Uni) == TRUE) {
            print("no Pointer_ID_Uni")
            NULL
          } else {
            print("yes Pointer_ID_Uni")
            generateCondSelectedListIndexReactive <- reactive({
              generateCondSelectedListIndex(input$numGroups, input$Pointer_ID_Uni, list(), condNamingIndexTempl)
            })
            
            outputWidgetListReactive <- reactive({
              outputWidgetList("Uni",
                               generateCondSelectedListIndexReactive(),
                               generateTotalWidgetListReactive()
              )
            })
            outputWidgetListReactive()
          }
        })
      } else {
        output$Select_Input_ID_Uni <- renderUI(NULL)
      }
    })
    

    

    
    
    output$groupConditionsWidgets <- renderUI({
      if (is.null(input$numGroups) == FALSE) {
        
        outputGroupConditionPointersReactive <- reactive({
          outputGroupConditionPointers(
            input$numGroups,
            input$Pointer_ID_Uni,
            condNamingIndexTempl
          )
          
        })
     
        outputGroupConditionPointersReactive()
      }
    })
    
    
    
    outputGroupConditionWidgetsReactive <- reactive({
      print("at outputGroupConditionWidgetsReactive")
      tempSelectedGroupList <- getListedInputs(input, names(generateConditionsNamingIndexReactive())[3:(2+as.numeric(input$numGroups))])
      outputGroupConditionWidgets(
        input$numGroups,
        output,
        input$Pointer_ID_Uni,
        tempSelectedGroupList,
        generateTotalWidgetListReactive(),
        condNamingIndexTempl
      )
    })

    
    
    # observeEvent(
    #   {
    #       c(input$Pointer_ID_Uni,
    #       input$Pointer_ID_1,
    #       input$Pointer_ID_2,
    #       input$Pointer_ID_3,
    #       input$Pointer_ID_4,
    #       input$Pointer_ID_5,
    #       input$Pointer_ID_6,
    #       input$Pointer_ID_7,
    #       input$Pointer_ID_8,
    #       input$Pointer_ID_9,
    #       input$Pointer_ID_10)
    #     }, {
    #       print("at observeEvent for outputGroupConditionWidgetsReactive")
    #       renderWidgetList <- outputGroupConditionWidgetsReactive()
    # 
    #       #THERE IS A BUG WITH R, if a For loop or some other vectorized statement (e.g. lapply, mapply) is applied, these expressions do not work.
    #       #The only solution was to hard code these groups in
    #       output[["UI_Output_Group_1"]] <- renderUI(renderWidgetList[[1]])
    #       output[["UI_Output_Group_2"]] <- renderUI(renderWidgetList[[2]])
    #       output[["UI_Output_Group_3"]] <- renderUI(renderWidgetList[[3]])
    #       output[["UI_Output_Group_4"]] <- renderUI(renderWidgetList[[4]])
    #       output[["UI_Output_Group_5"]] <- renderUI(renderWidgetList[[5]])
    #       output[["UI_Output_Group_6"]] <- renderUI(renderWidgetList[[6]])
    #       output[["UI_Output_Group_7"]] <- renderUI(renderWidgetList[[7]])
    #       output[["UI_Output_Group_8"]] <- renderUI(renderWidgetList[[8]])
    #       output[["UI_Output_Group_9"]] <- renderUI(renderWidgetList[[9]])
    #       output[["UI_Output_Group_10"]] <- renderUI(renderWidgetList[[10]])
    # })
    
    
    #THERE IS A BUG WITH R, if a For loop or some other vectorized statement (e.g. lapply, mapply) is applied, these expressions do not work.
    #The only solution was to hard code these groups in
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_1)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_1"]] <- renderUI(renderWidgetList[[1]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_2)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_2"]] <- renderUI(renderWidgetList[[2]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_3)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_3"]] <- renderUI(renderWidgetList[[3]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_4)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_4"]] <- renderUI(renderWidgetList[[4]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_5)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_5"]] <- renderUI(renderWidgetList[[5]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_6)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_6"]] <- renderUI(renderWidgetList[[6]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_7)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_7"]] <- renderUI(renderWidgetList[[7]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_8)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_8"]] <- renderUI(renderWidgetList[[8]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_9)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_9"]] <- renderUI(renderWidgetList[[9]])
      })
    observeEvent(
      {
        c(input$Pointer_ID_Uni,
          input$Pointer_ID_10)
      }, {
        print("at observeEvent for outputGroupConditionWidgetsReactive")
        renderWidgetList <- outputGroupConditionWidgetsReactive()
        output[["UI_Output_Group_10"]] <- renderUI(renderWidgetList[[10]])
      })
    
    
    observeEvent(
        input$executeButton, {
        # if (input$executeButton == TRUE)
        #   {
          print("at observeEvent for rendering summarized_Table")
          conditionsSelectedListIndex <- generateCondSelectedListIndexReactiveTotal()
          conditionsSelectedListIndex <<- generateCondSelectedListIndexReactiveTotal()
          print("is the problem here 1")
          condSelectedList <- getAllCondWidgetsListedInputs(input, input$numGroups, generateConditionsNamingIndexReactive())
          condSelectedList <<- getAllCondWidgetsListedInputs(input, input$numGroups, generateConditionsNamingIndexReactive())
          
          subsetDataFrames <- listSubsettedDataFrames(patientData, input$responseVariable, input$numGroups, 
                                                      conditionsSelectedListIndex, condSelectedList, responseVariableList, condNamingIndexTempl)
          subsetDataFrames <<- listSubsettedDataFrames(patientData, input$responseVariable, input$numGroups, 
                                                      conditionsSelectedListIndex, condSelectedList, responseVariableList, condNamingIndexTempl)
          print("is the problem here 2")
          consolGroupDatSet <- consolidateGroupDataSets(input$numGroups, subsetDataFrames, input$responseVariable, input$indVariable, responseVariableList, patientData)
          consolGroupDatSet <<- consolidateGroupDataSets(input$numGroups, subsetDataFrames, input$responseVariable, input$indVariable, responseVariableList, patientData)
          print("is the problem here 3")
          output$Summarized_Data_Presentation <- DT::renderDataTable({consolGroupDatSet})
          print("is the problem here 4")
          # output$Total_Data_Presentation <- DT::renderDataTable({subsetDataFrames})
        # }

        
      })


    # return(output$textOutputValues <- renderText({listedVerbatimTextOutput(input, condNamInd[,2])}))
    
    
    # output$moreControls <- renderUI({f
    #   lapply(as.character(1:input$numGroups), singleSelectizeInput, label = "whatever", choices = c("no choices"))
    # })
    
}

shinyApp(ui = ui, server = server)


