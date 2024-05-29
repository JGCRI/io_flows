# This script defines functions to search the GCAM directory for csv files containing relevant 
# information on logit exponents and compiles them into a single dataframe 
# for use by the io_flows visualization tool

library(dplyr)
library(tidyr)

# a function to count the number of commented lines at the beginning of a logits file
# any line starting with a # will be ignored
# returns the number of lines to be skipped when using the read.csv() function's 'skip' parameter
skip_lines = function(path) {
  file = read.csv(path, header = FALSE, nrows = 20)
  counter = 0
  for (i in 1:nrow(file)) {
    if (substr(file[i,1],1,1) == "#") {
      counter = counter + 1
    } else {
      break
    }
  }
  return(counter)
}

# A function to look through logit directories and files in the GCAM database and extract relevant information
# Returns a dataframe with sectors, subsectors, and logits that can be joined with the nodes dataframe from the IO tool
process_logits = function(cfg) {
  wd = getwd()
  setwd(cfg$gcam_path)
  # These files are duplicates or do not contain relevant information
  files_to_ignore = c("A_LandNode_logit.csv", "A_LandNode_logit_irr.csv", "A54.tranSubsector_logit.csv",
                      "A61.subsector_shrwt.csv", "A23.elecS_sector_vertical.csv", "A232.structure.csv",
                      "A23.elecS_sector.csv", "A23.elecS_subsector_logit.csv",
                      "RTI_peatland_categories.csv", "RTI_peatland_data.csv", "RTI_peatland_EFs.csv",
                      "A_unlimited_water_price.csv")
  directories_to_ignore = c("input/gcamdata/inst/extdata/gcam-usa", 
                            "input/gcamdata/inst/extdata/mi_headers", 
                            "input/gcamdata/inst/extdata/tests", 
                            "input/gcamdata/inst/extdata/socioeconomics")
  
  # search through directories to compile a list of csvs to read
  files_to_read = c()
  directories_to_search = list.dirs("input/gcamdata/inst/extdata", recursive = FALSE)
  for (directory in directories_to_search) {
    if (!(directory %in% directories_to_ignore)) {
      files = list.files(directory)
      for (file in files) {
        if (!(file %in% files_to_ignore) & substr(file, nchar(file) - 3, nchar(file)) == ".csv") {
          path = paste0(directory,"/",file)
          # open file and check to see if it includes logits
          dta = read.csv(path, skip = skip_lines(path), nrows = 0) # count the number of commented lines at the beginning of the file so we know where the header is
          # if this column header is present, then add it to the list of files we're interested in, otherwise ignore it
          if ("logit.exponent" %in% colnames(dta)) {
            files_to_read = c(files_to_read, path)
          }
        }
      }
    }
  }
  #print(files_to_read)
  
  # loop through files and extract logits
  i = 1
  # empty data frame to store data
  logits = data.frame()
  # loop through all logit files
  for (file in files_to_read) {
    # print(file)
    dta = read.csv(text = readLines(file, warn = FALSE), skip = skip_lines(file)) # count the number of commented lines at the beginning of the file (start with a #)
    dta = dta %>% filter(!(is.na(logit.exponent)))
    # these files come in a few distinct formats - for each, make sure we're extracting the relevant columns
    if ('AgSupplySector' %in% colnames(dta)) {
      if ('AgSupplySubsector' %in% colnames(dta)) {
        dta = dta %>% select(AgSupplySector, AgSupplySubsector, logit.exponent)
        colnames(dta) = c("sector", "subsector", "logit")
      } else {
        dta = dta %>% select(AgSupplySector, logit.exponent) %>%
          mutate(subsector = "None")
        colnames(dta) = c("sector", "logit", "subsector")
      }
    } else if ('tranSubsector' %in% colnames(dta)) {
      dta = dta %>% select(supplysector, tranSubsector, logit.exponent)
      colnames(dta) = c("sector", "subsector", "logit")
    } else {
      if ('subsector' %in% colnames(dta)) {
        dta = dta %>% select(supplysector, subsector, logit.exponent)
        colnames(dta) = c("sector", "subsector", "logit")
      } else {
        dta = dta %>% select(supplysector, logit.exponent) %>% 
          mutate(subsector = "None")
        colnames(dta) = c("sector", "logit", "subsector")
      }
    }

    # add in this data to the growing data frame
    if (nrow(logits) == 0) {
      logits = dta
    } else {
      logits = rbind(logits, dta)
    }
    i = i + 1
  } # end of loop through logit files
  
  # remove duplicates
  logits = logits %>% unique()
  
  ## Manual tweaks to sector/subsector names to improve the join rate
  
  ## LEVEL ONE
  
  # the water logits are missing the C/W distinction
  # irr handled separately because it's regional
  logits_water = logits %>% filter(grepl('water_td_', sector))
  logits_water_C = logits_water %>% mutate(sector = paste(sector, '_C', sep = ""))
  logits_water_W = logits_water %>% mutate(sector = paste(sector, '_W', sep = ""))
  logits = rbind(logits, logits_water_C, logits_water_W)
  
  # scavenging is a naming mismatch
  logits_scavenging = logits %>% filter(grepl('Scavenging_Other',sector))
  logits_scavenging = logits_scavenging %>% mutate(sector = paste(sector, '_Rsrc', sep = ""))
  logits = rbind(logits, logits_scavenging)
  
  # some of the electricity schemes are messy
  logits_elec = logits %>% 
    filter(sector == 'electricity' & subsector %in% c('biomass', 'coal', 'gas', 'geothermal','refined liquids'))
  logits_elec = rbind(logits_elec, logits_elec)
  logits_elec$sector = c("elec_coal (IGCC)","elec_gas (steam/CT)","elec_refined liquids (steam/CT)","elec_biomass (IGCC)","elec_geothermal",
                         "elec_coal (conv pul)","elec_gas (CC)","elec_refined liquids (CC)","elec_biomass (conv)","elec_geothermal")
  logits_elec$subsector = "None"
  logits_elec = logits_elec %>% unique() # one duplicate (intentionally)
  logits = rbind(logits, logits_elec)
  
  # some of the labels need to be lower case
  logits_case = logits %>%
    filter(sector %in% c('Poultry','Pork')) %>%
    mutate(sector = tolower(sector))
  
  logits = rbind(logits, logits_case)
  
  ## LEVEL TWO
  
  # some of the labels need to be lower case
  logits_case = logits %>%
    filter(subsector %in% c('Poultry','Pork')) %>%
    mutate(subsector = tolower(subsector))
  logits = rbind(logits, logits_case)
  
  # deal with the regionality
  regional_crops = c("Corn","FiberCrop","FodderGrass","FodderHerb","MiscCrop","OilCrop",
                     "OtherGrain","PalmFruit","Rapeseed","Rice","RootTuber","Soybean",
                     "SugarCrop","Wheat","biomassGrass","biomassTree")
  logits_regional_crops = logits %>%
    filter(subsector %in% regional_crops) %>%
    filter(sector == subsector) %>%
    mutate(subsector = paste(subsector, "_X", sep = ""))
  logits = rbind(logits, logits_regional_crops)
  
  traded_crops = c("traded DDGS","traded beef","traded biomass","traded corn","traded corn ethanol",
                   "traded dairy","traded fibercrop","traded misccrop","traded oilcrop",
                   "traded oilcrop biodiesel","traded oilcrop feedcake","traded oilcrop oil",
                   "traded othergrain","traded palmfruit","traded palmfruit biodiesel",
                   "traded palmfruit feedcake","traded palmfruit oil","traded pork","traded poultry",
                   "traded rapeseed","traded rapeseed biodiesel","traded rapeseed feedcake","traded rapeseed oil",
                   "traded refined oil","traded rice","traded root_tuber","traded sheepgoat",
                   "traded soybean","traded soybean biodiesel","traded soybean feedcake","traded soybean oil",
                   "traded sugar cane ethanol","traded sugarcrop","traded wheat")
  logits_traded_crops = logits %>% 
    filter(subsector %in% traded_crops) %>%
    filter(sector == subsector) %>%
    mutate(subsector = paste("X ", subsector, sep = ""))
  logits = rbind(logits, logits_traded_crops)
  
  logits_water2 = logits %>%
    filter(grepl("water_td",sector)) %>%
    mutate(subsector = "X")
  logits = rbind(logits, logits_water2)
  
  logits_grasstree = logits %>% 
    filter(sector == 'biomass', subsector %in% c('biomass_Grass', 'biomass_Tree')) %>%
    mutate(subsector = paste(subsector, "_X", sep = ""))
  logits = rbind(logits, logits_grasstree)
  
  setwd(wd)
  
  # read in a list of logits that will be added to the list
  # I've filled many of these in based on manual inspection of the data
  # The user can add more by editing the csv below
  #logits_manual = read.csv("./resources/logits_manual.csv", header = TRUE, stringsAsFactors = FALSE)
  #logits = rbind(logits, logits_manual)
  
  logits = logits %>% unique()
  
  # write output
  write.csv(logits, "./processed_data/full_logits_table.csv", row.names = FALSE)
  
  return(logits)
}


  
