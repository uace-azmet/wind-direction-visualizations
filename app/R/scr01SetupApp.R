# Load auxiliary files
stationNames <- vroom::vroom(
  file = "aux-files/azmet-stations-api-db.csv", 
  delim = ",", 
  col_names = TRUE, 
  show_col_types = FALSE
)

# Set auxiliary variables
databaseStart <- lubridate::as_date("2021-01-01")
databaseEnd <- lubridate::today() - 1
timeStep <- "Daily"
