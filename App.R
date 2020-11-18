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
library(textreadr)
library(stringr)
library(utils)
library(htmltools)
library(rmarkdown)
library(tinytex)
library(pagedown)
library(rmarkdown)


# library(shinysky)


#Read in Data
admissions <- read_tsv("AdmissionsCorePopulatedTable.txt", col_names = TRUE)
diagnoses <- read_tsv("AdmissionsDiagnosesCorePopulatedTable.txt", col_names = TRUE)
labs <- read_tsv("LabsCorePopulatedTable.txt", col_names = TRUE)
corePopulated <- read_tsv("PatientCorePopulatedTable.txt", col_names = TRUE)
patientData <- list(corePopulated, admissions, diagnoses, labs)

# #Read in Data
# admissions <- read_tsv("Data_AdmissionsCorePopulatedTable_10000.txt", col_names = TRUE)
# diagnoses <- read_tsv("Data_AdmissionsDiagnosesCorePopulatedTable_10000.txt", col_names = TRUE)
# labs <- read_tsv("Data_LabsCorePopulatedTable_10000.txt", col_names = TRUE)
# corePopulated <- read_tsv("Data_PatientCorePopulatedTable_10000.txt", col_names = TRUE)
# patientData <- list(corePopulated, admissions, diagnoses, labs)

# patientReportTemplate <- read_html("Patient_Report_Template.html", skip = 0, remove.empty = TRUE, trim = TRUE)
patientReportTemplate <- paste(readLines("Patient_Report_Template.html"), collapse="\n")
patientLabReportTemplate <- paste(readLines("Patient_Report_Template_Lab_History_Only.html"), collapse="\n")

#Read in reference lab results
labReference <- read_tsv("Normal_Lab_Results.txt", col_names = TRUE)

#Global Functions
#----------------------------------------------------------------------------------------------------

generateHistoryReport <- function(patient_Report_Template, patient_Lab_Report_Template, lab_Reference, patient_ID, patient_Data) {
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
    diagnosesReplacementText <- paste("<em>Admission ID: ", patientDiagnoses$AdmissionID[i], "</em>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                      "Start Date: ", patientAdmissions$AdmissionStartDate[i], "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                      "End Date: ", patientAdmissions$AdmissionEndDate[i], "<br>",
                                      "Primary Diagnosis Code: ", patientDiagnoses$PrimaryDiagnosisCode[i], "<br>",
                                      "Description: ", patientDiagnoses$PrimaryDiagnosisDescription[i], "<br><br>",
                                      "replace_Diagnoses_History",
                                      sep = "")
    outputHTML <- str_replace(outputHTML, "replace_Diagnoses_History", diagnosesReplacementText)
  }
  outputHTML <- str_remove(outputHTML, "replace_Diagnoses_History")
  
  for (i in unique(patientLab$AdmissionID)) {
    # print(c("are we here", i))
    labAdmission <- subset(patientLab, patientLab$AdmissionID == i)
    labAdmission <- labAdmission[order(labAdmission$LabName),]
    earliestTestDate <- as.character(min(labAdmission$LabDateTime))
    latestTestDate <- as.character(max(labAdmission$LabDateTime))
    
    labAdmissionReplacementText <- paste("<em>Admission ID: ", as.character(i), "</em>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;",
                                        "Test Period: ", earliestTestDate, " To ", latestTestDate, "<br><br>",
                                        sep = ""
                                        )
    labNameReplacementText <- ""
    labValueReplacementText <- ""
    labDateReplacementText <- ""
    labReferenceReplacementText <- ""
    labAbnormalFlagReplacementText <- ""
    
    for (j in 1:nrow(labAdmission)) {
      labNameReplacementText <- paste(labNameReplacementText, labAdmission$LabName[j], "<br>", sep = "")
      labValueReplacementText <- paste(labValueReplacementText, labAdmission$LabValue[j], "<br>", sep = "")
      labDateReplacementText <- paste(labDateReplacementText, labAdmission$LabDateTime[j], "<br>", sep = "")
      k <- which(lab_Reference$LabName == labAdmission$LabName[j])
      labReferenceReplacementText <- paste(labReferenceReplacementText, 
                                           lab_Reference$ReferenceLowEnd[k], " - ",
                                           lab_Reference$ReferenceHighEnd[k], "&nbsp;",
                                           lab_Reference$Units[k], "<br>",
                                           sep = "")
      if (labAdmission$LabValue[j] >= lab_Reference$ReferenceLowEnd[k] & 
          labAdmission$LabValue[j] <= lab_Reference$ReferenceHighEnd[k]) {
        flag = ""
      } else {
        flag = "Abnormal"
      }
      labAbnormalFlagReplacementText <- paste(labAbnormalFlagReplacementText, flag, "<br>", sep = "")
    }
    
    labReportHTMLReplacement <- patient_Lab_Report_Template
    labReportHTMLReplacement <- str_replace(labReportHTMLReplacement, "replace_Admission_History", labAdmissionReplacementText)
    labReportHTMLReplacement <- str_replace(labReportHTMLReplacement, "replace_Lab_Name", labNameReplacementText)
    labReportHTMLReplacement <- str_replace(labReportHTMLReplacement, "replace_Lab_Value", labValueReplacementText)
    labReportHTMLReplacement <- str_replace(labReportHTMLReplacement, "replace_Lab_Date", labDateReplacementText)
    labReportHTMLReplacement <- str_replace(labReportHTMLReplacement, "replace_Reference_Range", labReferenceReplacementText)
    labReportHTMLReplacement <- str_replace(labReportHTMLReplacement, "replace_Abnormal_Flag", labAbnormalFlagReplacementText)
    
    outputHTML <- str_replace(outputHTML, "<p class=MsoNoSpacing>replace_Lab_History</p>", labReportHTMLReplacement)
    
  }
    
  outputHTML <- str_remove(outputHTML, "replace_Lab_History")
  
  return(outputHTML)
}

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




#----------------------------------------------------------------------------------------------------
#####################################################################################################
############################ User Design Progress ###################################################
#####################################################################################################

ui <- fluidPage(navbarPage("VERITAS", id="mainTabset",
                 tabPanel("Home",
                          h3("Welcome to VERITAS"),
                          p("VERITAS is a web-based tool to allow clinicians easily input, access, store, and analyze patient data"),
                          ),
                 
                 tabPanel("Patient Data",
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
                              actionButton("switchToEdit", "Edit Patient History"),
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
                            tabPanel("Upload Data", value="upload_data",
                              h3("Upload Patient Data by File Type"),
                              fileInput("admissions", "Admission Data", accept = ".txt"),
                              fileInput("diagnoses", "Diagnostic Data", accept = ".txt"),
                              fileInput("labs", "Lab Data", accept = ".txt"),
                              fileInput("patients", "Patient Core Populated Data", accept = ".txt"),
                            ),
                            tabPanel("Edit Data", value="edit_data",
                              h3("Edit Patient Data"),
                              h3(textOutput("patientHistoryID"))
                            )
                          )
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
    
    output$patientHistoryReport <- renderText({
      if (is.null(input$patientHistoryID) == TRUE) {
        return(NULL)
      } else {
        patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportTemplate, 
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
        
        patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportTemplate, 
                                                        labReference, input$patientHistoryID, patientData)
        # Set up parameters to pass to Rmd document
        params <- list(HTML_File = patient_History_Report)

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        html<- rmarkdown::render(tempReport, params = params,
                          envir = new.env(parent = globalenv()))
        out <- pagedown::chrome_print(html,wait = 2, "test.pdf")
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
     })
    
    # options(shiny.usecairo=T)
    # 
    # output$downloadPlot <- downloadHandler(
    #   filename = function(){paste(input$patientHistoryID, '.pdf', sep = '')},
    #   
    #   content = function(file){
    #     cairo_pdf(filename = file,
    #               width = 18, height = 10, pointsize = 12, family = "sans", bg = "transparent",
    #               antialias = "subpixel",fallback_resolution = 300)
    #     patient_History_Report <- generateHistoryReport(patientReportTemplate, patientLabReportTemplate, 
    #                                                      labReference, input$patientHistoryID, patientData)
    #     patient_History_Report
    #     dev.off()
    #   },
    #   
    #   contentType = "application/pdf"
    # )

    
    
    # content = function(patient_History_Report) {
    #   rmarkdown::render(output_file = patient_History_Report)
    # }
}

shinyApp(ui = ui, server = server)


