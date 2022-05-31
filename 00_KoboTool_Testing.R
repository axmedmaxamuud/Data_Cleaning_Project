## Checking Kobo Tool before uploading it to the server
rm(list = rm())
today <- Sys.Date()

# load packages


# load xlsx tool
questions <- read_excel("input/SOM_REACH_MSNA_2022_Tool_v5.xlsx", sheet = 1)
choices <- read_excel("input/SOM_REACH_MSNA_2022_Tool_v5.xlsx", sheet = 2)


wrong_constraints <- check_constraints(questions, choices)

