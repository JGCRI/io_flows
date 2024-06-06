# This script defines several functions that relate to the config file used by the IO tool
# These functions deal with the creation and use of the config file as well as providing some error-checking functionality


# A function to prompt the user for inputs and use the information to craft a config file
write_config = function() {
  # blank config file to fill
  config_contents = list(
    "series" = NULL,
    "nodes_and_edges_directory" = NULL,
    "inputs_tech_path" = NULL,
    "inputs_resources_path" =  NULL,
    "gcam_path" =  NULL,
    "gcam_queries"= NULL
  )
  
  # Pop up a series of windows with welcome/instruction text and opportunities to point to relevant data/locations
  rstudioapi::showDialog(title = "Instructions", 
                         message = "Welcome to the IO Flows data visualization tool for the GCAM model. </p> 
                         <p>The first step is to generate your config file to customize the tool to your specifications. These instructional dialogs will walk you through the process. </p> 
                         <p>For more detailed instructions, check out the documentation at the link below: </p>",
                         url = "https://github.com/JGCRI/io_flows"
                         )
  Sys.sleep(0.5)
  rstudioapi::showDialog(title = "Instructions",
                         message = "<p> You'll first be prompted to label your series--choose a recognizeable name. </p>
                         <p> For more detailed instructions, check out the documentation at the link below:</p>",
                         url = "https://github.com/JGCRI/io_flows")
  Sys.sleep(0.5)
  config_contents$series = rstudioapi::showPrompt(title = "Series Label", 
                                                  message = "What would you like your series to be labeled?",
                                                  default = "Series_X"
                                                  ) 
  rstudioapi::showDialog(title = "Instructions",
                         message = "<p> Now you'll be prompted to select a directory where your nodes and edges files (used to display the network) are stored. If you haven't generated these files yet, click 'cancel' on the next screen to leave this blank. </p>
                         <p> For more detailed instructions, check out the documentation at the link below:</p>",
                         url = "https://github.com/JGCRI/io_flows")
  Sys.sleep(0.5)
  config_contents$nodes_and_edges_directory = rstudioapi::selectDirectory(caption = "Select a folder where nodes and edges data is located or should be written or cancel generate these files",
                                                                          label = "Select",
                                                                          path = getwd()
  )
  rstudioapi::showDialog(title = "Instructions",
                         message = "<p>Next you'll be prompted to select two files: inputs_tech.csv and inputs_resources.csv. These files should be the outputs of queries applied to a GCAM database. Select cancel on the following dialogs to leave these fields blank and generate these files from scratch. </p>
                         <p> For more detailed instructions, check out the documentation at the link below:</p>",
                         url = "https://github.com/JGCRI/io_flows")
  Sys.sleep(0.5)
  config_contents$inputs_tech_path = rstudioapi::selectFile(caption = "Select your inputs_tech.csv file if it exists or cancel to generate this file",
                                                            label = "Select",
                                                            path = getwd(),
                                                            filter = "CSV Files (*.csv)"
                                                            )
  config_contents$inputs_resources_path = rstudioapi::selectFile(caption = "Select your inputs_resources.csv file if it exists or cancel to generate this file",
                                                                 label = "Select",
                                                                 path = getwd(),
                                                                 filter = "CSV Files (*.csv)"
                                                                 )
  rstudioapi::showDialog(title = "Instructions",
                         message = "<p>If the csvs from the previous step need to be generated, you'll need a local GCAM database. Select the GCAM directory here. This directory should have a folder named 'output' containing the database itself. Select cancel on the following screen to leave this field blank. </p>
                         <p> For more detailed instructions, check out the documentation at the link below:</p>",
                         url = "https://github.com/JGCRI/io_flows")
  Sys.sleep(0.5)
  config_contents$gcam_path = rstudioapi::selectDirectory(caption = "Select a folder where your gcam output file is located or cancel if a local gcam instance does not exist",
                                                          label = "Select",
                                                          path = getwd()
                                                          )
  rstudioapi::showDialog(title = "Instructions",
                         message = "<p>Finally input the name of the xml file containing the relevant GCAM queries to generate the csv files described above. For most users, selecting the default name should suffice. </p>
                         <p> For more detailed instructions, check out the documentation at the link below:</p>",
                         url = "https://github.com/JGCRI/io_flows")
  Sys.sleep(0.5)
  config_contents$gcam_queries = rstudioapi::showPrompt(title = "Query Name", 
                                                        message = "Input the name of the gcam queries to be executed (include the extension .xml)",
                                                        default = "Flow_viz_queries.xml"
                                                        )
  if (is.null(config_contents$series)) {
    config_contents$series = "Series"
  }
  if (is.null(config_contents$nodes_and_edges_directory)) {
    config_contents$nodes_and_edges_directory = "NA"
  }
  if (is.null(config_contents$inputs_tech_path)) {
    config_contents$inputs_tech_path = "NA"
  }
  if (is.null(config_contents$inputs_resources_path)) {
    config_contents$inputs_resources_path = "NA"
  }
  if (is.null(config_contents$gcam_path)) {
    config_contents$gcam_path = "NA"
  }
  if (is.null(config_contents$gcam_queries)) {
    config_contents$gcam_queries = "Flow_viz_queries.xml"
  }
  
  # write to a config file so the user only has to do this once
  configr::write.config(
    config_contents,
    file.path = "io_flows.cfg",
    write.type = 'json',
    null = "list"
  )

  return(config_contents)
}


# A function to check config files for some basic errors and stop the run if something's not right
check_config = function(config.dat) {
  # check for the proper field names
  if (!identical(sort(names(config.dat)),sort(c('series', 'nodes_and_edges_directory', 'inputs_tech_path', 'inputs_resources_path', 'gcam_path', 'gcam_queries')))) {
    print("Error: headers in config file are invalid. Must be 'series', 'nodes_and_edges_directory', 'inputs_tech_path', 'inputs_resources_path', 'gcam_path', 'gcam_queries'. See documentation for an example.")
    return(list("error" = TRUE))
  }
  # check for the proper field contents
  if (!is.na(config.dat$nodes_and_edges_directory)) {
    if (substr(config.dat$nodes_and_edges_directory, nchar(config.dat$nodes_and_edges_directory) - 3, nchar(config.dat$nodes_and_edges_directory)) == '.csv') {
      print("Error: contents in cfg$nodes_and_edges_directory must be a path to a folder or directory, not a csv file. See documentation for an example.")
      return(list("error" = TRUE))
    }
  }
  # check for the proper file format for inputs tech
  if (!(is.na(config.dat$inputs_tech_path))) {
    if (config.dat$inputs_tech_path != "NA") {
      if (substr(config.dat$inputs_tech_path, nchar(config.dat$inputs_tech_path) - 3, nchar(config.dat$inputs_tech_path)) != '.csv') {
        print("Error: contents in cfg$inputs_tech_path must be a path to a csv file and end in the extension '.csv'. See documentation for an example.")
        return(list("error" = TRUE))
      }
      if (grepl("resources", config.dat$inputs_tech_path, fixed = TRUE)) {
        print("Error: Inputs Resources file was input into the Inputs Tech field. Please select the correct Inputs Tech csv file.")
        return(list("error" = TRUE))
      }
    }
  }
  # check for the proper file format for inputs resources
  if (!(is.na(config.dat$inputs_resources_path))) {
    if (config.dat$inputs_resources_path != "NA") {
      if (substr(config.dat$inputs_resources_path, nchar(config.dat$inputs_resources_path) - 3, nchar(config.dat$inputs_resources_path)) != '.csv') {
        print("Error: contents in cfg$inputs_resources_path must be a path to a csv file and end in the extension '.csv'. See documentation for an example.")
        return(list("error" = TRUE))
      }
      if (grepl("tech", config.dat$inputs_resources_path, fixed = TRUE)) {
        print("Error: Inputs Tech file was input into the Inputs Resources field. Please select the correct Inputs Resources csv file.")
        return(list("error" = TRUE))
      }
    }
  }
  # check for a usable GCAM database in the assigned location
  if (!is.na(config.dat$gcam_path)) {
    if (config.dat$gcam_path != "NA") {
      if (!file.exists(paste0(config.dat$gcam_path,"/output/database_basexdb"))) {
        print(paste0("Error: no GCAM database found at ", config.dat$gcam_path, "/output/database_basexdb. Try completing a GCAM model run to generate the output database."))
        return(list("error" = TRUE))
      }
    }
  }
  # check for the proper format of the query xml file
  if (!is.na(config.dat$gcam_queries)) {
    if (config.dat$gcam_queries != "NA") {
      if (substr(config.dat$gcam_queries, nchar(config.dat$gcam_queries) - 3, nchar(config.dat$gcam_queries)) != '.xml') {
        print("Error: contents in cfg$gcam_queries must be the name of a GCAM query file and end in the extension '.xml'. See documentation for an example.")
        return(list("error" = TRUE))
      }
    }
  }
  
  # return the config file unchanged if all tests have been passed
  return(config.dat)
}

# A function to read in a config file and instruct the tool on the proper procedures to take
# If the tool finds the files nodes_detailed.csv and edges_detailed.csv, it will read these files and launch the tool
# If the tool finds the files inputs_tech.csv and inputs_resources.csv, it will begin the data processing pipeline to generate the nodes and edges files, then launch the tool
# If the tool cannot find any of the files above, it will query GCAM to generate the raw data files, begin the data processing pipeline to generate the nodes and edges files, then launch the tool
# Otherwise, it will return an error code to signify to the tool that something is wrong...
check_for_inputs = function(cfg) {
  # if the user filled in the nodes and edges directory, check for files there
  if ((!(is.na(cfg$nodes_and_edges_directory))) & (!(cfg$nodes_and_edges_directory == "NA"))) {
    # if the correct files exist, then we're good
    if (file.exists(paste0(cfg$nodes_and_edges_directory,"/nodes_detailed.csv")) & file.exists(paste0(cfg$nodes_and_edges_directory,"/edges_detailed.csv"))) {
      print("Found nodes and edges files. Opening app...")
      return(1)
    }
    # if not, don't do anything and move on to the next step. We might have enough info to build the missing files
    print(paste0("Nodes and edges files not found at ", cfg$nodes_and_edges_directory, ". Attempting to construct nodes and edges dataframe."))
  }
  # next check for the inputs_tech and inputs_resources csvs If these files exist, use them to build the nodes and edges csvs
  if ((!(is.na(cfg$inputs_tech_path))) & (!(is.na(cfg$inputs_resources_path))) & (!(cfg$inputs_tech_path == "NA")) & (!(cfg$inputs_resources_path == "NA"))) {
    if (file.exists(cfg$inputs_tech_path) & file.exists(cfg$inputs_resources_path)) {
      # build nodes and edges so the tool can run
      print(paste0("Constructing nodes and edges dataframes using ", cfg$inputs_tech_path, " and ", cfg$inputs_resources_path))
      get_logits = !(cfg$gcam_path == "NA")
      calc_nodes_and_edges(cfg, get_logits)
      if (is.na(cfg$nodes_and_edges_directory) | cfg$nodes_and_edges_directory == "NA") {
        cfg$nodes_and_edges_directory = './processed_data'
      }
      # update the config file so this only needs to happen once
      configr::write.config(
        cfg,
        file.path = "io_flows.cfg",
        write.type = 'json'
      )
      return(1)
    } else if ((!is.na(cfg$gcam_path)) & (!(cfg$gcam_path == "NA"))) {
      # copy the query file from the io_flows directory to the GCAM directory if necessary
      if (!file.exists(paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries))) {
        file.copy(paste0("/queries/",cfg$gcam_queries), paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries))
      } 
      if (!file.exists(paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries))) {
        print(paste("ERROR. GCAM query file", cfg$gcam_queries, "cannot be found in the location ", cfg$gcam_path, "/output/queries/"))
        return(-1)
      }
      # if the user has given us a directory to the data files but the files are missing, construct them using the gcam database
      print("Querying GCAM...")
      query_path = paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries)
      # if not, we need to query gcam to build these data files and write to this location
      conn = rgcam_setup(cfg$gcam_path)
      inputs = rgcam_query(conn, query_path)
      inputs_tech = inputs$inputs_tech
      inputs_resources = inputs$inputs_resources
      tech_filename = paste0("./raw_data/", cfg$series, "_inputs_tech.csv")
      resources_filename = paste0("./raw_data/", cfg$series, "_inputs_resources.csv")
      write.csv(inputs_tech, tech_filename, row.names = False)
      write.csv(inputs_resources, resources_filename, row.names = False)
      # update the config file so this only needs to happen once
      cfg$inputs_tech_path = tech_filename
      cfg$inputs_resources_path = resources_filename
      if (is.na(cfg$nodes_and_edges_directory) | cfg$nodes_and_edges_directory == "NA") {
        cfg$nodes_and_edges_directory = './processed_data'
      }
      configr::write.config(
        cfg,
        file.path = "io_flows.cfg",
        write.type = 'json'
      )
      print("Calculating Nodes and Edges...")
      calc_nodes_and_edges(cfg, TRUE)
      return(1)
    } else {
      print("ERROR. Inputs_tech.csv and inputs_resources.csv not found. GCAM database unable to be queried.")
      return(-1)
    }
  } else if ((!(is.na(cfg$gcam_path))) & (!(cfg$gcam_path == "NA"))) {
    if (!(cfg$gcam_path == "NA")) {
      # copy the query file from the io_flows directory to the GCAM directory if necessary
      if (!file.exists(paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries))) {
        file.copy(paste0("./queries/",cfg$gcam_queries), paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries))
      } 
      if (!file.exists(paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries))) {
        print(paste("ERROR. GCAM query file", cfg$gcam_queries, "cannot be found in the location ", cfg$gcam_path, "/output/queries/"))
        return(-1)
      }
      # if inputs tech and inputs resources are not listed, we also need to query gcam here
      # look for gcam output, query it to build inputs tech and resources, then do the whole thing
      print("Querying GCAM...")
      query_path = paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries)
      # if not, we need to query gcam to build these data files and write to the default location
      conn = rgcam_setup(cfg$gcam_path)
      inputs = rgcam_query(conn, query_path)
      inputs_tech = inputs$inputs_tech
      inputs_resources = inputs$inputs_resources
      tech_filename = paste0("./raw_data/", cfg$series, "_inputs_tech.csv")
      resources_filename = paste0("./raw_data/", cfg$series, "_inputs_resources.csv")
      write.csv(inputs_tech, tech_filename, row.names = FALSE)
      write.csv(inputs_resources, resources_filename, row.names = FALSE)
      
      # update the config file so this only needs to happen once
      cfg$inputs_tech_path = tech_filename
      cfg$inputs_resources_path = resources_filename
      if (is.na(cfg$nodes_and_edges_directory) | cfg$nodes_and_edges_directory == "NA") {
        cfg$nodes_and_edges_directory = './processed_data'
      }
      configr::write.config(
        cfg,
        file.path = "io_flows.cfg",
        write.type = 'json'
      )
      print("Calculating Nodes and Edges...")
      calc_nodes_and_edges(cfg, TRUE)
      return(1)
    } else if (!(file.exists(paste0(cfg$gcam_path,"/output/queries/", cfg$gcam_queries)))) {
      print(paste("ERROR. GCAM query file", cfg$gcam_queries, "cannot be found in the location ", cfg$gcam_path, "/output/queries/"))
      return(-1)
    }
    else {
      # Should never reach this catch all
      print(paste("ERROR. Not enough information provided in config file. Please check documentation for requirements."))
      return(-1)
    }
  } else {
    print(paste("ERROR. Error constructing config file. Please check documentation for requirements."))
    return(-1)
  }
}
