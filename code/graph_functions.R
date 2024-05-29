# This script defines several functions that will be called by the visualization
# tool to construct the network for a given state

library(dplyr)

# A function to load the network data from selected edge.csv and nodes.csv files
# Returns g, a list of nodes and edges, that represents the graph
load_graph = function(directory="."){
  if (is.na(directory) | directory == "NA") {
    directory = "./processed_data"
  }
  # check for both the nodes and edges files and read them in if they exist
  if (file.exists(paste(directory, "nodes_detailed.csv", sep="/")) & file.exists(paste(directory, "edges_detailed.csv", sep="/"))) {
    g = list("nodes" = read.csv(paste(directory, "nodes_detailed.csv", sep="/")),
         "edges" = read.csv(paste(directory, "edges_detailed.csv", sep="/")),
         "error" = FALSE)  
  } else { # error catch -> alternate return code
    print("Error: Nodes and edges data does not exist. Please run calc_nodes_and_edges.R to generate this data and ensure the files are in the correct directory.")
    g = list("nodes" = NA, "edges" = NA, "error" = TRUE)
  }
  return(g)
}

# A function to identify and return a list of all of the UPSTREAM nodes (within [upstreamMaxDist] edges) of the focus node. 
# Upstream nodes are at the tail ends of directed edges
# Inputs:
#   edges - a dataframe with all edges in the graph
#   focusNode - the current sector of focus selected by the user; all distances will be relative to this node
#   upstreamMaxDist (default: 100) - defined by the user, maximum number of edges between nodes for which the node will be included in the graph
#   node_level (default: 1) - determines whether sectors (levels 1-3), subsectors (level 2-3), technologies (level 3) are visible in the graph
# Returns:
#   nodeList - a vector of nodes within upstreamMaxDist edges of focusNode
upstream_nodes = function(edges, focusNode, upstreamMaxDist=100, node_level=1){
  nodeList = c(as.integer(focusNode)) # add focusNode to list by default
  #error handling for the case of max dist =0 (or negative)
  if (upstreamMaxDist < 1){
    return(nodeList)
  }
  # loop through numbers of edges and add nodes to the list if they're within the defined distance
  for (i in 1:upstreamMaxDist){
    nodeList0 = nodeList
    # nodes are included if they're adjacent to nodes already included in the list
    nodeList = union(nodeList, unique(filter(edges, ((to %in% nodeList0) & (level == node_level)))$from))
    # stop if we didn't add any more nodes to the list
    if(setequal(nodeList, nodeList0)){
      break
    }
  }
  return(nodeList)
}

# Returns a list of all of the DOWNSTREAM nodes (within [downstreamMaxDist] edges) of the focus node. 
# Downstream nodes are at the leading ends of directed edges
# Inputs:
#   edges - a dataframe with all edges in the graph
#   focusNode - the current sector of focus selected by the user; all distances will be relative to this node
#   downstreamMaxDist (default: 100) - defined by the user, maximum number of edges between nodes for which the node will be included in the graph
#   node_level (default: 1) - determines whether sectors (levels 1-3), subsectors (level 2-3), technologies (level 3) are visible in the graph
# Returns:
#   nodeList - a vector of nodes within downstreamMaxDist edges of focusNode
downstream_nodes = function(edges, focusNode, downstreamMaxDist=100, node_level=1){
  nodeList = c(as.integer(focusNode))
  # switch to and from, then use the function defined above. Symmetry!
  edges = edges %>% 
    rename("fro" = to, "to" = from) %>% 
    rename("from" = fro)
  return(upstream_nodes(edges, focusNode, downstreamMaxDist,node_level))
}

# A function to build a network around a chosen focus node given upstream and downstream distance limits
# Inputs:
#   g - an unfiltered graph object with a nodes dataframe and an edges dataframe included
#   focusNode - the current sector of focus selected by the user; all distances will be relative to this node
#   upstreamMaxDist (default: 20) - defined by the user, maximum number of edges between nodes for which the node will be included in the graph
#   downstreamMaxDist (default: 20) - defined by the user, maximum number of edges between nodes for which the node will be included in the graph
# Outputs:
#   g - an updated graph object filtered based on the focusNode and upstream and downstream distances
selected_subgraph = function(g, focusNode, upstreamMaxDist=20, downstreamMaxDist=20){
  # identify relevant upstream and downstream nodes
  upNodes   = upstream_nodes(g$edges, focusNode, upstreamMaxDist, node_level = 1)
  downNodes = downstream_nodes(g$edges, focusNode, downstreamMaxDist, node_level = 1)
  subNodes = union(upNodes, downNodes)
  
  # filter nodes and edges
  g$edges = g$edges %>% 
    filter((from %in% subNodes & to %in% subNodes) & level == 1)
  g$nodes = g$nodes %>% 
    filter((id %in% subNodes) & (level == 1)) %>% 
    # add label column for upstream and downstream nodes
    mutate(group = case_when(id == focusNode ~ "Focus Sector",
                             id %in% upNodes ~ "Upstream Sectors",
                             id %in% downNodes ~ "Downstream Sectors")) 
  return(g)
}

# A function to expand sectors and subsectors to include more nodes/edges on the graph
# Inputs
#   fullgraph - unfiltered network with all nodes and edges
#   subgraph - a filtered network based on the focusNode and the upstream and downstream distances defined by the user
#   focusNode - the current sector of focus selected by the user; all distances will be relative to this node
#   expandSectors - a vector of sectors to be expanded to show subsectors, selected by the user
#   expandSubsectors - a vector of subsectors to be expanded to show technologies, selected by the user
#   expand_level (default: 2) - select level 2 to expand only sectors to show subsectors; select level 3 to expand sectors to show subsectors and subsectors to show technologies
# Outputs:
#   g - an updated graph object (with nodes and edges dataframes) that has been filtered and expanded per the user-defined parameters
add_to_subgraph_doubleclick = function(fullgraph, subgraph, focusNode, expandSectors, expandSubsectors, expand_level = 2) {
  # shorthand for relevant nodes, edges
  subgraph_nodes = subgraph$nodes
  subgraph_edges = subgraph$edges
  fullgraph_nodes = fullgraph$nodes
  fullgraph_edges = fullgraph$edges
  
  ## Expand Sectors first, we always do this for level 2 or 3
  
  # remove edges that went from input to sector
  subgraph_edges = subgraph_edges %>% filter(!(to %in% expandSectors))
  
  # join with the subgraph to keep track of labels for coloring purposes
  labels_frame = fullgraph_nodes %>% 
          left_join(select(subgraph_nodes,c(id,group)), by = c("id" = "id")) %>%
          filter(id %in% c(expandSectors)) %>%
          arrange(id)
  labels = labels_frame$group

  # add new nodes (subsectors) these will be upstream of the sector by 1 and have level 2
  subsector_list = c()
  labels_list = c()
  ix = 1
  # for each node we want to expand, identify its upstream nodes so they can be added to the graph 
  for (node in sort(unique(c(expandSectors)))) {
    upNodes2 = upstream_nodes(fullgraph_edges, node, upstreamMaxDist = 1, node_level = 2)
    upNodes2 = upNodes2[upNodes2 != node] # otherwise will be double counted
    subsector_list = c(subsector_list, upNodes2)
    # keep track of labels too
    labels_list = c(labels_list,rep(labels[ix],length(upNodes2)))
    ix = ix + 1
  }
  labels_frame2 = data.frame(subsector = subsector_list, color_label = labels_list)

  # label every node
  # error catch in case of empty labels dataframe
  if (length(labels_frame2) == 0) {
    # this will be an empty dataframe - mostly we just need to avoid joining empty dataframes
    new_nodes = fullgraph_nodes %>%
      filter(id %in% subsector_list) %>%
      mutate(group = "Upstream Subsectors")
  } else {
    new_nodes = fullgraph_nodes %>%
      filter(id %in% subsector_list) %>%
      left_join(labels_frame2, by = c("id" = "subsector")) %>%
      mutate(group = ifelse(color_label %in% c("Upstream Sectors","Focus Sector"), "Upstream Subsectors",
                            "Downstream Subsectors")) %>%
      select(-color_label)
  }

  # add new edges: 2 types
  # type 1: from subsector to focusnode
  # type 2: from input to subsector
  new_edges = fullgraph_edges %>%
    filter(level == 2 &
             ((to %in% expandSectors & from %in% new_nodes$id) | # type 1
              (from %in% subgraph_nodes$id & to %in% new_nodes$id))) # type 2
  
  full_edges = rbind(subgraph_edges, new_edges)
  full_nodes = rbind(subgraph_nodes, new_nodes)
  
  # if there's a node with no edges, drop it
  nodes_with_edges = unique(c(full_edges$from, full_edges$to))
  full_nodes = full_nodes %>% filter(id %in% nodes_with_edges)
  
  # we've got our new graph!
  g = list("nodes" = full_nodes,
           "edges" = full_edges)

  ## Then expand subsectors if necessary (expand_level 3)
  if (expand_level == 3) {
    subgraph = g
    subgraph_nodes = subgraph$nodes
    subgraph_edges = subgraph$edges
    
    # remove edges that went from input to subsector
    subgraph_edges = subgraph_edges %>% filter(!(to %in% expandSubsectors))
    
    # join for labeling again
    labels_frame = fullgraph_nodes %>% 
      left_join(select(subgraph_nodes,c(id,group)),by = c("id" = "id")) %>%
      filter(id %in% c(expandSubsectors)) %>%
      arrange(id)
    labels = labels_frame$group
    
    # add new nodes (technologies) these will be upstream of the subsectors by 1 and have level 3
    technology_list = c()
    labels_list = c()
    ix = 1
    for (node in sort(expandSubsectors)) {
      upNodes2 = upstream_nodes(fullgraph_edges, node, upstreamMaxDist = 1, node_level = 3)
      upNodes2 = upNodes2[upNodes2 != node] # otherwise will be double counted
      technology_list = c(technology_list, upNodes2)
      labels_list = c(labels_list, rep(labels[ix],length(upNodes2)))
      ix = ix + 1
    }
    
    labels_frame3 = data.frame(technology = technology_list, color_label = labels_list)

    if (length(labels_frame3) == 0) {
      # this will be an empty dataframe - mostly we just need to avoid joining empty dataframes
      new_nodes = fullgraph_nodes %>%
        filter(id %in% technology_list) %>%
        mutate(group = "Upstream Technologies")
    } else {
      new_nodes = fullgraph_nodes %>% 
        filter(id %in% technology_list) %>%
        left_join(labels_frame3, by = c("id" = "technology")) %>%
        mutate(group = ifelse(color_label %in% c("Upstream Subsectors","Focus Sector"), "Upstream Technologies",
                              "Downstream Technologies")) %>%
        select(-color_label)
      #mutate(group = "Technologies")
    }
    
    # add new edges: 2 types
    # type 1: from technology to expand nodes
    # type 2: from input to technology
    new_edges = fullgraph_edges %>% 
      filter(level == 3 & 
               ((to %in% expandSubsectors & from %in% new_nodes$id) | # type 1
                (from %in% subgraph_nodes$id & to %in% new_nodes$id))) # type 2
    
    # if there's a node with no edges, drop it
    nodes_with_edges = unique(c(full_edges$from, full_edges$to))
    full_nodes = full_nodes %>% filter(id %in% nodes_with_edges)
    
    full_edges = rbind(subgraph_edges, new_edges)
    full_nodes = rbind(subgraph_nodes, new_nodes)
    g = list("nodes" = full_nodes,
             "edges" = full_edges)
  }
  
  # we're done!
  return(g)
}

# An all-in-one function to build the graph and then expand sectors using the above functions
# Inputs
#   fullgraph - unfiltered network with all nodes and edges
#   focusNode - the current sector of focus selected by the user; all distances will be relative to this node
#   expandSectors - a vector of sectors to be expanded to show subsectors, selected by the user
#   expandSubsectors - a vector of subsectors to be expanded to show technologies, selected by the user
#   expand_level - (default: 2) select level 2 to expand only sectors to show subsectors; select level 3 to expand sectors to show subsectors and subsectors to show technologies
#   subsectors (default: FALSE) - a boolean that should be TRUE if subsectors/technologies are to be shown and FALSE otherwise
#   upstreamMaxDist (default: 20) - defined by the user, maximum number of edges between nodes for which the node will be included in the graph
#   downstreamMaxDist (default: 20) - defined by the user, maximum number of edges between nodes for which the node will be included in the graph
# Outputs:
#   subgraph - an updated graph object (with nodes and edges dataframes) that has been filtered and expanded per the user-defined parameters
construct_graph = function(fullgraph, focusNode, expandSectors = c(), expandSubsectors = c(), expand_level = 2, subsectors = FALSE, upstreamMaxDist = 20, downstreamMaxDist = 20) {
  # filter the network based on the focusNode and upstream and downstream distances
  subgraph = selected_subgraph(fullgraph,focusNode,upstreamMaxDist,downstreamMaxDist)
  # then doubleclick to expand sectors/subsectors if necessary
  if (subsectors) {
    subgraph = add_to_subgraph_doubleclick(fullgraph,subgraph,focusNode,expandSectors,expandSubsectors, expand_level)
  }
  return(subgraph)
}

# A utility function to remove a node with a given id (and all adjacent edges) from the network
remove_node = function(g, node_to_remove){
  # remove edges that start or terminate in the passed in node
  g$edges = g$edges %>% 
    filter(from != node_to_remove | to != node_to_remove)
  
  g$nodes = g$nodes %>% 
    filter(id != node_to_remove)
  
  return(g)
}

