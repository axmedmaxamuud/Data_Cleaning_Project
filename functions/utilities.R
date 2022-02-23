empty_issues_table <- function() {
  data.frame(
    uuid = numeric(),
    question.name = numeric(),
    issue = character(),
    feedback = character(),
    action = as.character(),
    action = as.character(),
    old.value = as.character(),
    new.value = as.character()
  )
  
}


select_other_columns <- function(data) {
  othernames <- grep("other$|Other$|autre$|Autre$", names(data),
                     value = T
  )
  data[othernames]
}

setcolnames <- function(x, colnames) {
  colnames(x) <- colnames
  return(x)
}
