# This script defines functions to query the GCAM database using the rgcam package
# These functions will be called when the user needs to build datasets from scratch by querying GCAM databases

library(rgcam)
library(dplyr)

# A function to set up and return an rgcam connection
rgcam_setup = function(path_to_gcam) {
  # db will always be in the output folder
  db_path = paste0(path_to_gcam, "/output/")
  # the database should have the same name
  db_name = "database_basexdb"
  conn = localDBConn(db_path, db_name, maxMemory = "2g")
  return(conn)
}

# A function to perform two queries on the database
# Takes as input a database connection returned by the function rgcam_setup() and an xml file with queries
# Returns two dataframes with input data usable by the IO tool
rgcam_query = function(conn, query_file) {
  # perform the queries
  prj = addScenario(conn, './processed_data/my-project-name.dat', NULL, query_file, clobber = TRUE)
  # extract the relevant data
  inputs_tech = data.frame(prj$Reference$`inputs by tech`)
  inputs_resources = data.frame(prj$Reference$`energy inputs to resource production`)
  
  # minor formatting tweaks
  inputs_tech = inputs_tech %>% rename(Year = year)
  inputs_resources = inputs_resources %>% rename(Year = year)
  
  # build a list two return both dataframes
  inputs_list = list(
    'inputs_tech' = inputs_tech,
    'inputs_resources' = inputs_resources
  )
  return(inputs_list)
}

conn = rgcam_setup("C:/Users/sweisberg/Documents/s70")
