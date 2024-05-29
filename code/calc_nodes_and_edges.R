# This script defines functions to perform the data processing and preparation process for the IO flows tool
# It reads in two files -- inputs_resources.csv and inputs_tech.csv -- and performs operations to generate a list of nodes and edges for the network

library(dplyr)
library(tidyr)
library(configr)
library(tcltk)

# load the logits functions
source("./code/process_logits.R")

# A function to transform the raw input files into lists of nodes and edges usable in the visualization tool
# This includes removing duplicate data, handling location names, definining sectors, subsectors, and technologies
# Inputs:
#   cfg - a config file defined by the user - includes locations of input files
#   get_logits - a boolean variable that controls whether logit values are read in and joined to the nodes list (TRUE) or not (FALSE)
# Outputs:
#   a list of nodes and a list of edges - these are returned by the functions and also written to memory in a location determined by the config file
calc_nodes_and_edges = function(cfg, get_logits) {
  # read inputs_tech.csv file
  inputs_tech = read.csv(cfg$inputs_tech_path)
  inputs_tech = inputs_tech %>% filter(scenario %in% c("BASE", "base"))
  
  # read inputs_resources.csv file
  inputs_resources = read.csv(cfg$inputs_resources_path)
  inputs_resources = inputs_resources %>% filter(scenario %in% c("BASE","base"))
  
  # this is a list of regions to look for in node names
  # all region names will identified will be replaced by an "X" to reduce the size of the data frame and the network
  regions_full = read.csv("./resources/region_list_full.csv", header = T, stringsAsFactors = F)
  
  # read in logit values for each node
  if (get_logits) {
    if (file.exists("./processed_data/full_logits_table.csv")) {
      #logits = read.csv("./processed_data/full_logits_table.csv", header = T, stringsAsFactors = F)  
      logits = process_logits(cfg)
      } else {
      logits = process_logits(cfg)
    }
  } else {
    logits = data.frame(sector = NA, subsector = NA, logit = NA)
  }
  
  ### Text processing to reduce the number of nodes and edges by replacing regions with "X"
  
  # start by removing unnecessary rows/columns
  inputs_tech = inputs_tech %>% 
    select(sector, subsector, technology, input) %>%
    unique()
  
  # some of our regions are really short and common letter combinations ("Ob", "Po"), so I'm replacing words with these letters and will add them back in later
  words_to_replace = c("poultry","pork", "import", "cooling pond", "gasoline pool", "global solar")
  replacements = c("XXXX","ZZZZ","VVVV","JJJJ","QQQQ","KKKK")
  
  # replace the problematic words above with placeholders
  for (i in 1:length(words_to_replace)) {
    inputs_tech = inputs_tech %>% mutate(
      input = case_when(grepl(words_to_replace[i], input, ignore.case = TRUE) ~ gsub(words_to_replace[i], replacements[i], input, ignore.case = TRUE), TRUE ~ input),
      sector = case_when(grepl(words_to_replace[i], sector, ignore.case = TRUE) ~ gsub(words_to_replace[i], replacements[i], sector, ignore.case = TRUE), TRUE ~ sector),
      subsector = case_when(grepl(words_to_replace[i], subsector, ignore.case = TRUE) ~ gsub(words_to_replace[i], replacements[i], subsector, ignore.case = TRUE), TRUE ~ subsector),
      technology = case_when(grepl(words_to_replace[i], technology, ignore.case = TRUE) ~ gsub(words_to_replace[i], replacements[i], technology, ignore.case = TRUE), TRUE ~ technology)
    )
  }
  
  # now loop through the list of regions and replace all occurrences with an "X"
  regions = regions_full$regions
  for (r in regions) {
    inputs_tech = inputs_tech %>% mutate(
      input = case_when(grepl(r, input) ~ gsub(r, "X", input), TRUE ~ input),
      sector = case_when(grepl(r, sector) ~ gsub(r, "X", sector), TRUE ~ sector),
      subsector = case_when(grepl(r, subsector) ~ gsub(r, "X", subsector), TRUE ~ subsector),
      technology = case_when(grepl(r, technology) ~ gsub(r, "X", technology), TRUE ~ technology)
    )
  }
  
  # reverse the common letter combinations replacements
  for (i in 1:length(words_to_replace)) {
    inputs_tech = inputs_tech %>% mutate(
      input = case_when(grepl(replacements[i], input) ~ gsub(replacements[i], words_to_replace[i], input), TRUE ~ input),
      sector = case_when(grepl(replacements[i], sector) ~ gsub(replacements[i], words_to_replace[i], sector), TRUE ~ sector),
      subsector = case_when(grepl(replacements[i], subsector) ~ gsub(replacements[i], words_to_replace[i], subsector), TRUE ~ subsector),
      technology = case_when(grepl(replacements[i], technology) ~ gsub(replacements[i], words_to_replace[i], technology), TRUE ~ technology)
    )
  }
  
  # remove duplicate rows (reducing across regions and also years)
  inputs_tech = inputs_tech %>% unique()
  
  # we don't care about these rows so get rid of them
  inputs_tech = inputs_tech %>%
    filter(!grepl("-Floor", input))
  
  # To add double-clickability (patent-pending), we need to label all the nodes/edges as level 1 (shows up always) or level 2 (shows up on double-click) or level 3 (shows up on double-click of a subsector)
  # The level 1 connections represent flows from input->sector
  # The level 2 connections represent flows into subsectors and can be broken down into two types:
  #     upstream - inputs->subsectors
  #     downstream - subsectors->sectors
  # The level 3 connections represent flows into technologies and can be broken down into two types:
  #     upstream - input->technology
  #     downstream - technology -> subsector
  # In the graph, this manifests itself as new subsector nodes popping up in between the inputs and sector of interest (or new tech nodes between inputs and subsectors) with those connections being replaced by new ones
  inputs_tech_df_level1 = inputs_tech %>%
    transmute(
      from = input,
      to = sector
    ) %>%
    unique() %>%
    mutate(level = "1")
  
  # level 2a edges going from inputs to subsectors 
  inputs_tech_df_level2_upstream = inputs_tech %>%
    transmute(
      input,
      sector, # bringing this in for naming conventions then dropping it later
      subsector
    ) %>% 
    unique() %>%
    # create longer unique names for nodes that will be searchable but not displayed
    mutate(subsector = paste(sector,">>",subsector,sep = "")) %>%
    select(-sector) %>%
    unique() %>%
    rename(from = input, to = subsector) %>%
    mutate(level = "2a")
    
  # level 2b edges going from subsector to sector
  # (these will be relabeled level 2 later but I need to distinguish between them in the next step)
  inputs_tech_df_level2_downstream = inputs_tech %>%
    transmute(
      subsector,
      sector,
      input
    ) %>% 
    unique() %>%
    mutate(subsector = paste(sector,">>",subsector,sep = "")) %>%
    select(-input) %>%
    unique() %>%
    rename(from = subsector, to = sector) %>%
    mutate(level = "2b")
  
  # level 3a edges will be going from inputs to techs
  inputs_tech_df_level3_upstream = inputs_tech %>%
    transmute(
      input,
      sector, # bringing this in for water name issues then dropping it later
      subsector,
      technology
    ) %>% 
    unique() %>%
    mutate(technology = paste(sector,">>",subsector,">>",technology,sep = "")) %>%
    select(-sector,-subsector) %>%
    unique() %>%
    rename(from = input, to = technology) %>%
    mutate(level = "3a")
  
  # level 3b edges going from technology to subsector
  # (these will be relabeled level 3 later but I need to distinguish between them in the next step)
  inputs_tech_df_level3_downstream = inputs_tech %>%
    transmute(
      technology,
      subsector,
      sector,
      input
    ) %>% 
    unique() %>%
    mutate(technology = paste(sector,">>",subsector,">>",technology,sep = "")) %>%
    mutate(subsector = paste(sector,">>",subsector,sep = "")) %>%
    select(-input, -sector) %>%
    unique() %>%
    rename(from = technology, to = subsector) %>%
    mutate(level = "3b")
  
  # clean up inputs_resources
  inputs_resources_df = inputs_resources %>% 
    transmute(
      input,
      sector = resource
    ) %>% 
    unique()
  
  # the resources data doesn't have the same level of detail, so we're leaving it the same
  inputs_resources_df_level1 = inputs_resources_df %>%
    transmute(
      from = input,
      to = sector
    ) %>%
    mutate(level = "1")
     
  # concatenate cleaned dfs
  df_levels = rbind(inputs_tech_df_level1, inputs_tech_df_level2_upstream, inputs_tech_df_level2_downstream, 
                     inputs_tech_df_level3_upstream, inputs_tech_df_level3_downstream, inputs_resources_df_level1)
  
  
  # separate to create nodes and edges
  lvl1_df = df_levels %>% filter(level == "1")
  lvl2a_df = df_levels %>% filter(level == "2a")
  lvl2b_df = df_levels %>% filter(level == "2b")
  lvl3a_df = df_levels %>% filter(level == "3a")
  lvl3b_df = df_levels %>% filter(level == "3b")
  
  # level 1 nodes will be shown by default on the graph -- these are the inputs and sectors
  lvl1_nodes = unique(c(lvl1_df$from, lvl1_df$to))
  # level 2 nodes (I'll relabel lvl 2a/b to lvl 2 momentarily) will only be shown upon request (double-click) -- these are the subsectors
  lvl2_nodes = unique(c(lvl2b_df$from, lvl2a_df$to))
  # level 3 notes will only be shown upon request -- these are the technologies
  lvl3_nodes = unique(c(lvl3b_df$from, lvl3a_df$to))
  
  # create the full data frame of nodes, relabeling levels to be 1,2,3 as defined above
  nodes_df_levels = data.frame(label = c(lvl1_nodes,lvl2_nodes,lvl3_nodes), 
                               level = c(rep(1,length(lvl1_nodes)),rep(2,length(lvl2_nodes)),rep(3,length(lvl3_nodes)))) %>%
    unique() %>%
    mutate(id = row_number()) %>%
    # keep the long labels for searching, create short labels for displaying
    mutate(title_long = label) %>%
    mutate(label = gsub("..*>>", "", label))
  
  # join with logits data
  if (get_logits) {
    nodes_temp_level1 = nodes_df_levels %>% filter(level == 1) %>% 
      left_join(
        distinct(logits, sector, .keep_all = TRUE) %>% filter(subsector == "None") %>%
          select(sector, logit),
        by = c("label" = "sector")
      ) %>% distinct(id, .keep_all = TRUE) # sometimes there are multiple logits for the same sector - keep the first
    
    # slightly different join for level 2
    nodes_temp_level2 = nodes_df_levels %>% filter(level == 2) %>% 
      left_join(
        distinct(logits, sector, .keep_all = TRUE) %>% filter(subsector != "None") %>%
          mutate(subsector_long = paste(sector,">>",subsector,sep = "")) %>%
          select(subsector_long, logit),
        by = c("title_long" = "subsector_long")
      )
    
    # slightly different join for level 3
    nodes_temp_level3 = nodes_df_levels %>% filter(level == 3) %>%
      mutate(logit = NA)
  } else {
    nodes_temp_level1 = nodes_df_levels %>% filter(level == 1) %>% mutate(logit = NA)
    nodes_temp_level2 = nodes_df_levels %>% filter(level == 2) %>% mutate(logit = NA)
    nodes_temp_level3 = nodes_df_levels %>% filter(level == 3) %>% mutate(logit = NA)
  }
  
  # combine them again, now with logits
  nodes_df_levels = rbind(nodes_temp_level1,nodes_temp_level2, nodes_temp_level3)
  colnames(nodes_df_levels)[5] = "title"
  
  # format logit labels
  nodes_df_levels = nodes_df_levels %>% 
    mutate(title = ifelse(is.na(title),"NA",title)) %>%
    mutate(title = paste("Logit: ", title, sep = "")) %>%
    mutate(label_logits = paste(label,substr(title,8,length(title)),sep = ": ")) %>%
    mutate(label_nologits = label)

  # now we're done with nodes, let's set up the edges dataframe with the same levels scheme
  edges_df_levels = df_levels %>%
    mutate(level = ifelse(level == "3a" | level == "3b", 3,
                          ifelse(level == "2b" | level == "2a",2,
                          ifelse(level == "1",1,level)))) %>%
    # join with nodes to get the proper ids, labels
    left_join(distinct(select(nodes_df_levels,c(title_long,id))), by = c('from' = 'title_long')) %>%
    left_join(distinct(select(nodes_df_levels,c(title_long,id))), by = c('to' = 'title_long')) %>%
    mutate(title = "supplemental info")
  
  # clean up a bit
  colnames(edges_df_levels) = c('from_name','to_name','level','from','to', 'title')
  colnames(nodes_df_levels)[2] = "level"
  # grapes
  
  # write nodes and edges to csv
  if (!(is.na(cfg$nodes_and_edges_directory)) & cfg$nodes_and_edges_directory != "NA") {
    write.csv(nodes_df_levels, file = paste0(cfg$nodes_and_edges_directory, "/nodes_detailed.csv"), row.names = FALSE)
    write.csv(edges_df_levels, file = paste0(cfg$nodes_and_edges_directory, "/edges_detailed.csv"), row.names = FALSE)
  } else {
    write.csv(nodes_df_levels, file = "./processed_data/nodes_detailed.csv", row.names = FALSE)
    write.csv(edges_df_levels, file = "./processed_data/edges_detailed.csv", row.names = FALSE)
  }
  
  # also return a list object with both nodes and edges
  return(list(
    "nodes" = nodes_df_levels,
    "edges" = edges_df_levels
  ))
}