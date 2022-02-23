source("functions/utilities.R")
# Function 1 : Check survey time taken
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







