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
conditionsNamingIndex <- paste(getwd(), "/Analysis_and_Report_Tools/", "Conditions_Naming_Index.csv", sep = "") %>%
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
    labReportHTMLReplacement <- ""
  for (i in unique(patientLab$AdmissionID)) {
    labAdmission <- subset(patientLab, patientLab$AdmissionID == i)
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
                 multiple = TRUE
  ) 
}

displayMultGroupCondSelec <- function(numGroups) {
  group1CondSelec <- selectizeInput()
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
                              h1("Edit Patient Data"),
                              h3(textOutput("patientHistoryID")),
                              br(""),
                              h3("Patient Demographic"),
                              textInput("patient_dob", "Date of Birth: "),
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
                                  column(9, offset = 0,
                                         singleSelectizeInput("responseVariable", "Response Variable (select first)", responseVariableList$All_Response_Var)
                                         )
                                  
                                ),
                                fluidRow(
                                  column(9, offset = 0,
                                         singleSelectizeInput("indVariable", "Independent Variable (if applicable)", responseVariableList$Cont_Response_Var)
                                  )
                                ),
                                fluidRow(
                                  column(9, offset = 0,
                                         selectizeInput("numGroups", "Number of Groups", c(1,2,3,4,5), selected = 1, multiple = FALSE)
                                  )
                                ),
                                fluidRow(
                                  column(9, offset = 0,
                                         selectizeInput("numGroups", "Number of Groups", c(1,2,3,4,5), selected = 1, multiple = FALSE)
                                  )
                                )
                                )
                            ),
                            mainPanel(
                              p("Mainpanel 1")
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
      current_patient<<- subsetPatientData(patient_ID =toString(input$patientHistoryID), patient_Data = patientData)
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
    
    observeEvent(input$updatePatientDemographic, {
      updateTabsetPanel(session, "mainTabset", selected = "patient_data")
      patientData[[core_populated_id]]$PatientRace[[match(current_patient[[core_populated_id]]$PatientID, patientData[[core_populated_id]]$PatientID)]]<-input$patient_race
      print(input$patient_race)
      print(patientData[[core_populated_id]]$PatientRace[[match(current_patient[[core_populated_id]]$PatientID, patientData[[core_populated_id]]$PatientID)]])
    })

    observeEvent(input$responseVariable, {
      if (input$responseVariable %in% responseVariableList$Cont_Response_Var == TRUE) {
        show("indVariable")
      } else {
        hide("indVariable")
      }
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


