source("functions/utilities.R")
time_check <- function(df, time_min, time_max){
  df <- df %>% mutate(interview_duration = difftime(as.POSIXct(ymd_hms(end)), as.POSIXct(ymd_hms(start)), units = "mins"),
                      CHECK_interview_duration = case_when(
                        interview_duration < time_min ~ "Too short",
                        interview_duration > time_max ~ "Too long",
                        TRUE ~ "Okay"
                      )
  )
  return(df)
}

# Function 2 : logsheet
log_sheet <- function(data, question.name, issue, action){
  cleaning_logbook <- data.frame("uuid" = as.character(),
                                 "question.name" = as.character(),
                                 "issue" = as.character(),
                                 "feedback" = as.character(),
                                 "action" = as.character(),
                                 "old.value" = as.character(),
                                 "new.value" = as.character(),
                                 stringsAsFactors = F)
  if(nrow(data) > 0){
    for(a in 1:nrow(data)) {
      cleaning_logbook <- cleaning_logbook %>% 
        add_row(
          tibble_row(
            uuid = as.character(data[a, "uuid"]),
            question.name = as.character(question.name),
            issue = as.character(issue),
            feedback = as.character(""),
            action = as.character(action),
            old.value = as.character(data[a, question.name]),
            new.value = as.character("")
            
          )
        )
    }
  }
  return(cleaning_logbook)
}

# Function 3 : Empty log book
logbook <- function() {
  data.frame(
    uuid = numeric(),
    question.name = numeric(),
    issue = character(),
    feedback = character(),
    action = as.character(),
    old.value = as.character(),
    new.value = as.character()
  )
  
}

# Function 4 : Check other response
check_other_responses <- function(data){
  df <- data %>% dplyr::select(ends_with("_other"), "uuid") %>% 
    pivot_longer(ends_with("_other"),
                 names_to = "question.name",
                 values_to = "old.value") %>% 
    filter(!is.na(old.value)) %>% filter(!old.value %in% c("", TRUE, FALSE, 1, 0, "VRAI", "FAUX", "TRUE", "FALSE", "<NA>", "NA")) %>% 
    mutate(
      issue = "other response, may need recording",
      feedback = "",
      action = "change",
      new.value = ""
    ) %>% select(
      uuid, question.name, issue, feedback, action, old.value, new.value
    )
}

# Function 5 - survey tracker
survey_tracker <- function(data, region, district){
  survey_tracking <- summarise(group_by(data, region, district), count = n())
  check_survey <- unique(na.omit(data$today))
  for(j in check_survey){
    survey_tracking[, ncol(survey_tracking)+1] <- (data %>% 
                                                     group_by(region, district) %>% 
                                                     summarise(val=sum(na.omit(today==j))) %>% 
                                                     arrange(region, district)) $val
    names(survey_tracking)[ncol(survey_tracking)] <- paste0(j)
  }
  return(survey_tracking)
}

# Function 6 - Enumerator productivity
enum_productivity <- function(data, enum_name){
  enum_summary <- summarise(group_by(data, enum_name, ))
  
  enum_report <- summarise(group_by(data, enum_name), count = n())
  check_summary <- unique(na.omit(data$today))
  
  for(j in check_summary){
    enum_report[, ncol(enum_report)+1] <- (data %>% 
                                             group_by(enum_name) %>% 
                                             summarise(val=sum(na.omit(today==j))) %>% 
                                             arrange(enum_name)) $val
    names(enum_report)[ncol(enum_report)] <- paste0(j)
  }
  
  return(enum_report)
}

# Function 7 - Assessment Productivity
assessment_productivity <- function(data){
  df <- data %>% group_by(today) %>% 
    summarise(NbSurvey = n())
}


# Function 8 - Batch issues
batch_issue_checks <- function(df, conditions, tests, meta_to_keep = c()){
  
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.character(conditions))
  assertthat::assert_that(is.character(tests))
  if(length(meta_to_keep>0)){assertthat::assert_that(is.vector(meta_to_keep))}
  
  
  data_with_issues <- composr::recode_batch(df, tos = rep(1,length(conditions)),
                                            wheres = conditions,
                                            targets = tests) %>% (composr::end_recoding)
  
  
  unique_targets <- unique(tests)
  
  data_with_issues[, unique_targets] <- lapply(data_with_issues[, unique_targets], function(x){
    x[is.na(x)] <-0
    x }) %>% (tibble::as_tibble)
  
  data_with_issues %>% dplyr::select(c(meta_to_keep, unique_targets))
  
}

# Function 9 - Run checks from checklist
run_checks_from_dataframe<-function(df, conditions_df, condition.column, test.name.column, meta_to_keep = c()){
  
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(is.data.frame(conditions_df))
  assertthat::assert_that(is.vector(condition.column))
  assertthat::assert_that(is.vector(test.name.column))
  if(length(meta_to_keep>0)){assertthat::assert_that(is.vector(meta_to_keep))}
  
  if(!(condition.column%in% names(conditions_df))){stop(paste(condition.column, " not a column name in conditions_df"))}
  if(!(test.name.column %in% names(conditions_df))){stop(paste(test.name.column, " not a column name in conditions_df"))}
  
  conditions <- conditions_df[[condition.column]]
  tests <- conditions_df[[test.name.column]]
  
  batch_issue_checks(df, conditions = conditions, tests = tests, meta_to_keep = meta_to_keep)
  
}

# Function 10 - Hash Phone-numbers
hash_ki_phones <- function(){
  
}




