

if(nrow(logbook) > 0){
  for(a in 1:nrow(logbook)) {
    check_single <- logbook %>% 
      slice(a)
    
    if (!is.na(check_single$new.value)){
      old_value <- data[which(data$uuid == check_single$uuid),check_single$question.name]
      
      if (check_single$issue == 'change' || check_single$issue == 'other response') {
        if (as.character(check_single$corrected_value) == 'delete')
      }
    }
  }
} #


# action (change or delete)