# app.R
# -----------------------------------------------------------------------------
# A single-file Shiny app suitable for exporting to Shinylive.
# It replicates your Measles Case Report Form but stores data in memory instead
# of writing to Excel (since openxlsx is not supported in Shinylive).
# -----------------------------------------------------------------------------

library(shiny)
library(dplyr)
library(lubridate)

ui <- fluidPage(
  titlePanel("Measles Case Report Form (with Download Button)"),
  mainPanel(
    # 1) The form to collect basic info
    textInput("patient_name", "Patient Name"),
    numericInput("age", "Age", value = 0, min = 0),
    dateInput("onset_date_text", "Date of Rash Onset (MM/DD/YYYY)",
              value = format(Sys.Date(), "%m/%d/%Y")),
    tags$small("Please use format MM/DD/YYYY (e.g. 03/22/2025)"),
    selectInput("vacc_status", "Vaccination Status",
                choices = c("Unknown", "0 doses", "1 dose", "2 doses")),
    
    # ============= FULL CASE REPORT FIELDS =============
    wellPanel(
      h3("1. Patient Information"),
      textInput("state_case_id", "State Case ID"),
      textInput("lab_id", "Lab ID"),
      textInput("nndss_id", "NNDSS ID"),
      textInput("patient_name_form", "Patient Name (last, first)"),
      dateInput("report_date_hd", "Date reported to Health Department"),
      dateInput("invest_start_date", "Date investigation began"),
      dateInput("db_entry_date", "Database entry date"),
      textInput("interviewer_name", "Interviewer Name"),
      textInput("interviewer_contact", "Interviewer Phone/Email")
    ),
    wellPanel(
      h3("2. Interview/Call Log (Repeated Entries)"),
      textAreaInput("call_log", "Call Log Details", height = "100px")
    ),
    wellPanel(
      h3("3. Working Status (Repeated Entries)"),
      textAreaInput("working_status", "Working Status Details", height = "100px")
    ),
    wellPanel(
      h3("4. Final Investigative Findings"),
      selectInput("final_status", "Final patient status",
                  choices = c("Ruled out", "Confirmed (Epi)", "Confirmed (Lab)", "Unknown")),
      selectInput("outbreak_related", "Case outbreak related?",
                  choices = c("Yes", "No", "Unknown")),
      textInput("outbreak_name", "Outbreak name (if related)"),
      selectInput("import_status", "Import status",
                  choices = c("International importation", "U.S. acquired")),
      selectInput("us_acquired_detail", "U.S. acquired detail",
                  choices = c("Import-linked case", "Imported-virus case", 
                              "Endemic case", "Unknown source case"))
    ),
    wellPanel(
      h3("5. Demographic Information"),
      textInput("respondent_name", "Respondent Name"),
      textInput("respondent_relation", "Relationship to patient"),
      textInput("address", "Address"),
      textInput("city", "City"),
      textInput("county", "County"),
      textInput("state_form", "State"),
      textInput("zip", "Zip"),
      textInput("telephone", "Telephone"),
      textInput("residence_country", "Country of usual residence"),
      selectInput("sex_at_birth", "Sex at Birth",
                  choices = c("Male", "Female", "No answer")),
      numericInput("age_form", "Age", value = NA, min = 0),
      dateInput("dob", "Date of Birth"),
      selectInput("hispanic_latino", "Hispanic/Latino",
                  choices = c("Yes", "No", "Unknown")),
      selectInput("race", "Race",
                  choices = c("White", "Black/African American", "Asian/Pacific Islander",
                              "American Indian/Alaska Native", "Other", "Unknown"))
    ),
    wellPanel(
      h3("6. Clinical Information"),
      radioButtons("symptom_rash", "Rash?", c("Yes", "No", "Unknown")),
      dateInput("rash_onset_date", "Rash onset date"),
      radioButtons("rash_generalized", "Rash generalized?", c("Yes", "No", "Unknown")),
      textAreaInput("rash_desc", "Rash description", height = "40px"),
      radioButtons("rash_current", "Rash still present?", c("Yes", "No", "Unknown")),
      numericInput("rash_duration_days", "Rash duration (days)", value = NA),
      radioButtons("symptom_fever", "Fever?", c("Yes", "No", "Unknown")),
      dateInput("fever_onset_date", "Fever onset date"),
      numericInput("max_temp", "Highest temperature", value = NA),
      radioButtons("temp_scale", "Temperature scale", c("°F", "°C")),
      radioButtons("symptom_cough", "Cough?", c("Yes", "No", "Unknown")),
      radioButtons("symptom_coryza", "Stuffy/runny nose (coryza)?", c("Yes", "No", "Unknown")),
      radioButtons("symptom_conjunctivitis", "Swollen/red/itchy eyes (conjunctivitis)?",
                   c("Yes", "No", "Unknown")),
      radioButtons("symptom_otitis", "Ear infection (otitis)?", c("Yes", "No", "Unknown")),
      radioButtons("symptom_pneumonia", "Pneumonia?", c("Yes", "No", "Unknown")),
      radioButtons("symptom_diarrhea", "Diarrhea?", c("Yes", "No", "Unknown")),
      radioButtons("symptom_vomiting", "Vomiting?", c("Yes", "No", "Unknown")),
      radioButtons("symptom_dehydration", "Dehydration?", c("Yes", "No", "Unknown")),
      radioButtons("symptom_low_platelets", "Low platelets (Thrombocytopenia)?",
                   c("Yes", "No", "Unknown")),
      radioButtons("symptom_encephalitis", "Brain inflammation (Encephalitis)?",
                   c("Yes", "No", "Unknown")),
      textAreaInput("symptom_other", "Other symptoms", height = "40px")
    ),
    wellPanel(
      h3("7. Healthcare Interaction"),
      radioButtons("healthcare_visited", "Visited healthcare provider?",
                   c("Yes", "No", "Unknown")),
      textInput("healthcare_location", "Healthcare location type (Clinic, ER, etc.)"),
      dateInput("healthcare_visit_date", "Healthcare visit date"),
      textInput("healthcare_facility", "Healthcare facility name"),
      radioButtons("hospitalized", "Hospitalized?", c("Yes", "No", "Unknown")),
      textInput("hospital_name", "Hospital name"),
      dateInput("hospital_admit_date", "Admission date"),
      dateInput("hospital_discharge_date", "Discharge date"),
      radioButtons("patient_death", "Did the patient die?", c("Yes", "No", "Unknown")),
      dateInput("death_date", "Date of death")
    ),
    wellPanel(
      h3("8. Past Medical History"),
      radioButtons("pregnancy_status", "(If female) Are you pregnant?",
                   c("Yes", "No", "Unknown")),
      radioButtons("immune_weakened", "Weakened immune system?",
                   c("Yes", "No", "Unknown")),
      textAreaInput("immune_conditions",
                    "Conditions or medications leading to weakened immunity",
                    height = "40px")
    ),
    wellPanel(
      h3("9. Epidemiologic Investigation"),
      radioButtons("recent_travel", "Travel outside U.S. past 21 days?",
                   c("Yes", "No", "Unknown")),
      dateInput("travel_depart_date", "Departure date from U.S."),
      textInput("countries_visited", "Countries visited"),
      dateInput("travel_return_date", "Return date to U.S."),
      radioButtons("known_contact", "Contact with known measles case/symptoms before rash?",
                   c("Yes", "No", "Unknown")),
      textInput("contact_location_type", "Location type of contact"),
      textInput("contact_location_name", "Contact location name"),
      textInput("contact_dates", "Contact dates"),
      textAreaInput("contact_details", "Additional contact details", height = "40px"),
      textInput("source_case_id", "Source case ID (if known)")
    ),
    wellPanel(
      h3("10. Exposure Period (days -21 to -5)"),
      textAreaInput("exposure_period_table", "Exposure Period Table", height = "100px")
    ),
    wellPanel(
      h3("11. Infectious Period (days -4 to +4)"),
      textAreaInput("infectious_period_table", "Infectious Period Table", height = "100px")
    ),
    wellPanel(
      h3("12. Household Contacts (multiple records)"),
      textAreaInput("household_contacts_table", "Household Contacts Table", height = "100px")
    ),
    wellPanel(
      h3("13. Vaccination Status"),
      radioButtons("vaccine_received", "Received measles-containing vaccine?",
                   c("Yes", "No", "Unknown")),
      numericInput("vaccine_doses_num", "Number of doses", value = 0, min = 0),
      dateInput("vaccine_dose1_date", "Date Dose 1"),
      dateInput("vaccine_dose2_date", "Date Dose 2"),
      dateInput("vaccine_dose3_date", "Date Dose 3"),
      radioButtons("pep_received", "Received PEP?", c("Yes", "No", "Unknown")),
      selectInput("pep_type", "Type of PEP", c("Vaccine", "Immunoglobulin (IG)", "Unknown")),
      dateInput("pep_date", "Date PEP received"),
      radioButtons("pep_within_timeframe", "PEP given within 3/6 days of exposure?",
                   c("Yes", "No", "Unknown")),
      selectInput("ig_admin_method", "If IG given, route",
                  c("Intramuscular", "Intravenous", "Unknown"))
    ),
    wellPanel(
      h3("14. Laboratory Information"),
      radioButtons("testing_performed", "Measles testing performed?",
                   c("Yes", "No", "Unknown")),
      radioButtons("specimen_permission", "Permission granted for specimen?", c("Yes", "No")),
      textInput("test_pcr_result", "PCR result (Pos, Neg, Indet, Pend)"),
      dateInput("test_pcr_date", "PCR date"),
      textInput("test_pcr_lab", "PCR lab"),
      textInput("test_igm_result", "IgM result (Pos, Neg, Indet, Pend)"),
      dateInput("test_igm_date", "IgM date"),
      textInput("test_igm_lab", "IgM lab"),
      textInput("test_igg_acute_result", "IgG acute result (Pos, Neg, Indet, Pend)"),
      dateInput("test_igg_acute_date", "IgG acute date"),
      textInput("test_igg_acute_lab", "IgG acute lab"),
      textInput("test_igg_conval_result", "IgG convalescent result (Pos, Neg, Indet, Pend)"),
      dateInput("test_igg_conval_date", "IgG conval date"),
      textInput("test_igg_conval_lab", "IgG conval lab"),
      textInput("test_genotype_result", "Genotype result (Pos, Neg, Indet, Pend)"),
      dateInput("test_genotype_date", "Genotype date"),
      textInput("test_genotype_lab", "Genotype lab"),
      textInput("test_meva_result", "MeVA result (Pos, Neg, Indet, Pend)"),
      dateInput("test_meva_date", "MeVA date"),
      textInput("test_meva_lab", "MeVA lab")
    ),
    
    actionButton("save", "Save Case"),
    br(),
    verbatimTextOutput("save_status"),
    hr(),
    # >>> Download Button (CSV) <<<
    downloadButton("download_data", "Download Current Data (CSV)")
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store all cases in memory (disappears if browser is closed/refreshed)
  rv <- reactiveVal(
    data.frame(
      case_id          = character(),
      patient_name     = character(),
      age              = numeric(),
      onset_date       = as.Date(character()),
      vacc_status      = character(),
      state_case_id    = character(),
      lab_id           = character(),
      nndss_id         = character(),
      patient_name_form= character(),
      report_date_hd   = as.Date(character()),
      invest_start_date= as.Date(character()),
      db_entry_date    = as.Date(character()),
      interviewer_name = character(),
      interviewer_contact = character(),
      
      call_log         = character(),
      working_status   = character(),
      final_status     = character(),
      outbreak_related = character(),
      outbreak_name    = character(),
      import_status    = character(),
      us_acquired_detail = character(),
      
      respondent_name     = character(),
      respondent_relation = character(),
      address             = character(),
      city                = character(),
      county              = character(),
      state_form          = character(),
      zip                 = character(),
      telephone           = character(),
      residence_country   = character(),
      sex_at_birth        = character(),
      age_form            = numeric(),
      dob                 = as.Date(character()),
      hispanic_latino     = character(),
      race                = character(),
      
      symptom_rash          = character(),
      rash_onset_date       = as.Date(character()),
      rash_generalized      = character(),
      rash_desc             = character(),
      rash_current          = character(),
      rash_duration_days    = numeric(),
      symptom_fever         = character(),
      fever_onset_date      = as.Date(character()),
      max_temp              = numeric(),
      temp_scale            = character(),
      symptom_cough         = character(),
      symptom_coryza        = character(),
      symptom_conjunctivitis= character(),
      symptom_otitis        = character(),
      symptom_pneumonia     = character(),
      symptom_diarrhea      = character(),
      symptom_vomiting      = character(),
      symptom_dehydration   = character(),
      symptom_low_platelets = character(),
      symptom_encephalitis  = character(),
      symptom_other         = character(),
      
      healthcare_visited      = character(),
      healthcare_location     = character(),
      healthcare_visit_date   = as.Date(character()),
      healthcare_facility     = character(),
      hospitalized            = character(),
      hospital_name           = character(),
      hospital_admit_date     = as.Date(character()),
      hospital_discharge_date = as.Date(character()),
      patient_death           = character(),
      death_date              = as.Date(character()),
      
      pregnancy_status  = character(),
      immune_weakened   = character(),
      immune_conditions = character(),
      
      recent_travel       = character(),
      travel_depart_date  = as.Date(character()),
      countries_visited   = character(),
      travel_return_date  = as.Date(character()),
      known_contact       = character(),
      contact_location_type = character(),
      contact_location_name = character(),
      contact_dates         = character(),
      contact_details       = character(),
      source_case_id        = character(),
      
      exposure_period_table    = character(),
      infectious_period_table  = character(),
      household_contacts_table = character(),
      
      vaccine_received     = character(),
      vaccine_doses_num    = numeric(),
      vaccine_dose1_date   = as.Date(character()),
      vaccine_dose2_date   = as.Date(character()),
      vaccine_dose3_date   = as.Date(character()),
      pep_received         = character(),
      pep_type             = character(),
      pep_date             = as.Date(character()),
      pep_within_timeframe = character(),
      ig_admin_method      = character(),
      
      testing_performed   = character(),
      specimen_permission = character(),
      test_pcr_result     = character(),
      test_pcr_date       = as.Date(character()),
      test_pcr_lab        = character(),
      test_igm_result     = character(),
      test_igm_date       = as.Date(character()),
      test_igm_lab        = character(),
      test_igg_acute_result  = character(),
      test_igg_acute_date    = as.Date(character()),
      test_igg_acute_lab     = character(),
      test_igg_conval_result = character(),
      test_igg_conval_date   = as.Date(character()),
      test_igg_conval_lab    = character(),
      test_genotype_result   = character(),
      test_genotype_date     = as.Date(character()),
      test_genotype_lab      = character(),
      test_meva_result       = character(),
      test_meva_date         = as.Date(character()),
      test_meva_lab          = character(),
      
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$save, {
    # Validate onset date:
    onset_date_clean <- tryCatch(
      as.Date(input$onset_date_text, format="%m/%d/%Y"),
      error = function(e) NA
    )
    if (is.na(onset_date_clean) || onset_date_clean > Sys.Date()) {
      showNotification("Invalid or future date for rash onset. Use MM/DD/YYYY.", type="error")
      return(NULL)
    }
    
    # Build a new row from user inputs
    new_row <- data.frame(
      case_id      = paste0("Case_", as.integer(Sys.time())),
      patient_name = input$patient_name,
      age          = input$age,
      onset_date   = onset_date_clean,
      vacc_status  = input$vacc_status,
      
      state_case_id        = input$state_case_id,
      lab_id               = input$lab_id,
      nndss_id             = input$nndss_id,
      patient_name_form    = input$patient_name_form,
      report_date_hd       = as.Date(input$report_date_hd),
      invest_start_date    = as.Date(input$invest_start_date),
      db_entry_date        = as.Date(input$db_entry_date),
      interviewer_name     = input$interviewer_name,
      interviewer_contact  = input$interviewer_contact,
      
      call_log         = input$call_log,
      working_status   = input$working_status,
      final_status     = input$final_status,
      outbreak_related = input$outbreak_related,
      outbreak_name    = input$outbreak_name,
      import_status    = input$import_status,
      us_acquired_detail = input$us_acquired_detail,
      
      respondent_name     = input$respondent_name,
      respondent_relation = input$respondent_relation,
      address             = input$address,
      city                = input$city,
      county              = input$county,
      state_form          = input$state_form,
      zip                 = input$zip,
      telephone           = input$telephone,
      residence_country   = input$residence_country,
      sex_at_birth        = input$sex_at_birth,
      age_form            = input$age_form,
      dob                 = as.Date(input$dob),
      hispanic_latino     = input$hispanic_latino,
      race                = input$race,
      
      symptom_rash          = input$symptom_rash,
      rash_onset_date       = as.Date(input$rash_onset_date),
      rash_generalized      = input$rash_generalized,
      rash_desc             = input$rash_desc,
      rash_current          = input$rash_current,
      rash_duration_days    = input$rash_duration_days,
      symptom_fever         = input$symptom_fever,
      fever_onset_date      = as.Date(input$fever_onset_date),
      max_temp              = input$max_temp,
      temp_scale            = input$temp_scale,
      symptom_cough         = input$symptom_cough,
      symptom_coryza        = input$symptom_coryza,
      symptom_conjunctivitis= input$symptom_conjunctivitis,
      symptom_otitis        = input$symptom_otitis,
      symptom_pneumonia     = input$symptom_pneumonia,
      symptom_diarrhea      = input$symptom_diarrhea,
      symptom_vomiting      = input$symptom_vomiting,
      symptom_dehydration   = input$symptom_dehydration,
      symptom_low_platelets = input$symptom_low_platelets,
      symptom_encephalitis  = input$symptom_encephalitis,
      symptom_other         = input$symptom_other,
      
      healthcare_visited      = input$healthcare_visited,
      healthcare_location     = input$healthcare_location,
      healthcare_visit_date   = as.Date(input$healthcare_visit_date),
      healthcare_facility     = input$healthcare_facility,
      hospitalized            = input$hospitalized,
      hospital_name           = input$hospital_name,
      hospital_admit_date     = as.Date(input$hospital_admit_date),
      hospital_discharge_date = as.Date(input$hospital_discharge_date),
      patient_death           = input$patient_death,
      death_date              = as.Date(input$death_date),
      
      pregnancy_status  = input$pregnancy_status,
      immune_weakened   = input$immune_weakened,
      immune_conditions = input$immune_conditions,
      
      recent_travel      = input$recent_travel,
      travel_depart_date = as.Date(input$travel_depart_date),
      countries_visited  = input$countries_visited,
      travel_return_date = as.Date(input$travel_return_date),
      known_contact      = input$known_contact,
      contact_location_type = input$contact_location_type,
      contact_location_name = input$contact_location_name,
      contact_dates         = input$contact_dates,
      contact_details       = input$contact_details,
      source_case_id        = input$source_case_id,
      
      exposure_period_table    = input$exposure_period_table,
      infectious_period_table  = input$infectious_period_table,
      household_contacts_table = input$household_contacts_table,
      
      vaccine_received     = input$vaccine_received,
      vaccine_doses_num    = input$vaccine_doses_num,
      vaccine_dose1_date   = as.Date(input$vaccine_dose1_date),
      vaccine_dose2_date   = as.Date(input$vaccine_dose2_date),
      vaccine_dose3_date   = as.Date(input$vaccine_dose3_date),
      pep_received         = input$pep_received,
      pep_type             = input$pep_type,
      pep_date             = as.Date(input$pep_date),
      pep_within_timeframe = input$pep_within_timeframe,
      ig_admin_method      = input$ig_admin_method,
      
      testing_performed   = input$testing_performed,
      specimen_permission = input$specimen_permission,
      test_pcr_result     = input$test_pcr_result,
      test_pcr_date       = as.Date(input$test_pcr_date),
      test_pcr_lab        = input$test_pcr_lab,
      test_igm_result     = input$test_igm_result,
      test_igm_date       = as.Date(input$test_igm_date),
      test_igm_lab        = input$test_igm_lab,
      test_igg_acute_result  = input$test_igg_acute_result,
      test_igg_acute_date    = as.Date(input$test_igg_acute_date),
      test_igg_acute_lab     = input$test_igg_acute_lab,
      test_igg_conval_result = input$test_igg_conval_result,
      test_igg_conval_date   = as.Date(input$test_igg_conval_date),
      test_igg_conval_lab    = input$test_igg_conval_lab,
      test_genotype_result   = input$test_genotype_result,
      test_genotype_date     = as.Date(input$test_genotype_date),
      test_genotype_lab      = input$test_genotype_lab,
      test_meva_result       = input$test_meva_result,
      test_meva_date         = as.Date(input$test_meva_date),
      test_meva_lab          = input$test_meva_lab,
      
      stringsAsFactors = FALSE
    )
    
    # Append to existing in-memory dataset
    updated_df <- bind_rows(rv(), new_row)
    rv(updated_df)
    
    # Status message
    output$save_status <- renderText({
      paste("Case saved in memory:", new_row$case_id)
    })
  })
  
  # Download Handler: let user download all entered data as CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("Measles_Cases_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)




