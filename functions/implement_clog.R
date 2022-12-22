# Creating a dummy data set
city_name <- c("kabul", "new dlehi", "peshawar","new york")
population <- c(4430000, 21750000, 1970000, 8419000)
uuid <- c("eae001", "eae002", "eae003","eae004")

my_df <- data.frame(city_name, population, uuid)

# Creating a dummy cleaning log
old.value <- c("kabul", 4430000, "europe","kabul")
question.name <- c("city_name", "population", "continent","city_name")
new.value <- c("moscow",11920000,"asia","moscow")
uuid <- c("eae001","eae001","eae001","eae001")
changed <- c("yes","yes","yes","yes")

cleaning_log <- data.frame(uuid,question.name, old.value, new.value, changed)

### declaring the function
incorporate_logs = function(raw_df, cleaning_log, df_group_seprator = "/", uuid_col = "_uuid"){
  error <- "Error!
Execution was haulted due to one of the following issues:
  - Cleaning log is empty
  - There is no changes in data (in cleaning log changed property for all logs is set to 'NO')
  - One/morethan one of the (uuid, question.name, old.value, new.value, and changed) columns are missing or column names are misspelled
"
  if (sum(grepl("uuid|question.name|old.value|new.value|changed", names(cleaning_log)))==5) {
    `%nin%` = Negate(`%in%`)
    # changing the group seprator (operator) from "/" to "."
    names(raw_df) <- gsub(df_group_seprator,".",names(raw_df))
    cleaning_log$question.name <- gsub(df_group_seprator,".", cleaning_log$question.name)
    
    # subsetting logs that their question is not (available) in dataset
    logs_not_in_rawdf <- cleaning_log[cleaning_log$question.name %nin% names(raw_df) | cleaning_log$uuid %nin% raw_df[[uuid_col]], ]
    logs_not_in_rawdf <- logs_not_in_rawdf[logs_not_in_rawdf$changed %in% c("yes","Yes"),]
    
    # subsetting logs that their question exist in raw data frame and its new value is changed
    cleaning_log.changed <- cleaning_log[cleaning_log$question.name %in% names(raw_df) & cleaning_log$uuid %in% raw_df[[uuid_col]], ]
    cleaning_log.changed <- cleaning_log.changed[cleaning_log.changed$changed %in% c("yes","Yes"),]
    
    # capturing duplicate logs
    cleaning_log$unique_key <- paste(cleaning_log$uuid, cleaning_log$question.name, sep = "_")
    duplicate_logs <- cleaning_log[(duplicated(cleaning_log$unique_key) | duplicated(cleaning_log$unique_key, fromLast = T)),]
    
    # cleaning master cleaning log
    cleaning_log <- cleaning_log[cleaning_log$uuid %nin% logs_not_in_rawdf$uuid | cleaning_log$question.name %nin% logs_not_in_rawdf$question.name,]
    cleaning_log <- cleaning_log[!is.na(cleaning_log$question.name), ]
    cleaning_log <- cleaning_log[!is.na(cleaning_log$uuid), ]
    
    raw_df_valid <- raw_df
    if (nrow(cleaning_log.changed)>0) {
      # Apply cleaning log on raw data
      for (rowi in 1:nrow(cleaning_log.changed)){
        uuid_i <- cleaning_log.changed$uuid[rowi]
        var_i <- cleaning_log.changed$question.name[rowi]
        old_i <- cleaning_log.changed$old.value[rowi]
        new_i <- cleaning_log.changed$new.value[rowi]
        if(class(raw_df_valid[[var_i]]) == "character"){
          new_i <- as.character(new_i)
        }else if(class(raw_df_valid[[var_i]]) == "numeric"){
          new_i <- as.numeric(new_i)
        }else if(class(raw_df_valid[[var_i]]) == "logical"){
          new_i <- as.integer(new_i)
        }else if(class(raw_df_valid[[var_i]]) == "integer"){
          new_i <- as.integer(new_i)
        }
        # Find the variable according to the row of the cleaning log
        raw_df_valid[raw_df_valid[[uuid_col]] == uuid_i, var_i] <- new_i
        print(paste(rowi,"uuid:", uuid_i, "Old value:", old_i, "changed to", new_i, "for", var_i))
      }
      return(list(cleaned_df = raw_df_valid, cleaning_log.applied = cleaning_log.changed, logs_not_in_rawDF = logs_not_in_rawdf, duplicate_logs = duplicate_logs, master_cleaning_log = cleaning_log))
    }else{
      cat(error)
      return(list(cleaned_df = raw_df_valid, cleaning_log.applied = cleaning_log.changed,logs_not_in_rawdf = logs_not_in_rawdf))
    }
  }else{
    cat(error)
  }
}

### Applying the function to data frame
incorprated_logs <- incorporate_logs(my_df, cleaning_log, uuid_col = "uuid")


cleaned_data <- incorprated_logs$cleaned_df
master_cleaning_log <- incorprated_logs$master_cleaning_log
logs_not_in_rawDf <- incorprated_logs$logs_not_in_rawDF
cleaning_log.applied <- incorprated_logs$cleaning_log.applied
duplicate_log <- incorprated_logs$duplicate_logs



