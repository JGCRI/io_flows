# This script defines the RShiny functionality for the IO Flows visualization tools
# It sets up the layout of the tool, displays the graph, and defines the interactive functionality 

library(visNetwork)
library(shiny)
library(shinyWidgets)
library(rgcam)

# load in all other defined functions
source("./code/graph_functions.R")
source("./code/calc_nodes_and_edges.R")
source("./code/rgcam_functions.R")
source("./code/config_functions.R")

# This function defines and handles all of the RShiny functionality
# It takes a return code from the data processing steps as input and will only run if the return code is viable
# Inputs:
#   return code - should be 1 to run, -1 indicates an error in a previous step
#   cfg - a config file defined by the user
#   g - a graph object that includes a list of nodes and edges. Note: can also include an error code to cause the function to break
# Outputs:
#   Does not return anything if run is successful but launches the visualization RShiny tool
#   Returns a -1 error code if unsuccessful
run_app = function(return_code, cfg, g) {
  # check for errors first
  if (return_code == -1) {
    return(-1)
  } 
  if (g$error) {
    print("Error.") 
    return(-1)
  }
  
  # set up the ui for the shiny app
  ui = fluidPage(
  
    titlePanel(paste0("Model Sector Input / Output Flows: GCAM ", cfg$series)),
  
    visNetworkOutput("network", height = "60vh"),
    hr(),
    
    # set up all the buttons and toggles
    fluidRow(
      column(1,
             # this button controls whether subsectors and technologies will be shown
             radioButtons(inputId = "expandr",
                          label = "Show Subsectors/Techs",
                          choices = list("No" = FALSE,
                                         "Yes" = TRUE)),
             # this button controls whether logits will be displayed as part of the label text
             radioButtons(inputId = "logits",
                         label = "Display logits",
                         choices = list("No" = FALSE,
                                        "Yes" = TRUE))),
      column(3,
             # this dropdown allows the user to select the focus node
             pickerInput(
               inputId = "focusNode",
               label = "Sector of Focus",
               choices = (filter(g$nodes,level == 1) %>% arrange(title_long))$title_long,
               selected = "regional oil"
             ),
             # this dropdown allows the user to select which sectors will be expanded
             pickerInput(
               inputId = "expandNodes",
               label = "Sectors to Expand",
               choices = c("a", "b", "c"),
               multiple = TRUE,
               options = list(
                 `deselect-all-text` = "None",
                 `select-all-text` = "All",
                 `actions-box` = TRUE,
                 `liveSearch` = TRUE
               ),
               selected = c("regional oil")
             ),
             # this dropdown allows the user to select which subsectors will be expanded
             pickerInput(
               inputId = "expandSubsectors",
               label = "Subsectors to Expand",
               choices = c("a","b","c"),
               multiple = TRUE,
               options = list(
                 `deselect-all-text` = "None",
                 `select-all-text` = "All",
                 `actions-box` = TRUE,
                 `liveSearch` = TRUE
               ), selected = c("None")
             )),
      column(5,
             # these sliders allow the users to select the maximum distance from the focus sector to be included in the graph
             sliderInput(inputId = 'upstreamMaxDist',
                         label = 'Max Upstream Node Distance',
                         min = 0, max = 8, value = 2, step = 1),
             sliderInput(inputId = 'downstreamMaxDist',
                         label = 'Max Downstream Node Distance',
                         min = 0, max = 8, value = 2, step = 1))
  ))
  
  # now build in all of the functionality
  server = function(input, output, session) {
      # this variable controls whether or not we should be showing subsectors on the graph
      d_subsector = reactiveValues(data = FALSE)
      # a list to keep track of which sectors are expanded
      r_expandSectors = reactiveValues(data = c())
      # a list to keep track of which subsectors are expanded
      r_expandSubsectors = reactiveValues(data = c())
      # are we expanding sectors or subsectors?
      r_expandLevel = reactiveValues(data = 2)
      r_expand = reactiveValues(data = FALSE)
      
      # when the expand radio button, the focus node, or the expand nodes are updated...
      observeEvent(c(input$expandr, input$focusNode, input$expandNodes), {
        r_expand$data = input$expandr
        # if we want to show expanded data
        if (r_expand$data == TRUE & length(r_expandSectors$data) > 0) {
            # update the lists of nodes to expand (and look whether we're expanding subsectors too)
            r_expandSectors$data = g$nodes[g$nodes$title_long %in% input$expandNodes,"id"]
            d_subsector$data = TRUE
            if (length(r_expandSubsectors$data) > 0) {
              r_expandLevel$data = 3
            } else {
              r_expandLevel$data = 2
            }
        } else {
          # if not, just update the list of nodes to expand
          r_expandSectors$data = g$nodes[g$nodes$title_long %in% input$expandNodes,"id"]
          if (length(r_expandSubsectors$data) > 0) {
            r_expandLevel$data = 3
          } else {
            r_expandLevel$data = 2
          }
  
          # update the dropdowns to show the right choices
          r_expandSubsectors$data = c()
          id_choices_sub = c()
          for (node in r_expandSectors$data) {
            id_choices_in = upstream_nodes(g$edges, node, 1, node_level = 2)
            id_choices_in = id_choices_in[id_choices_in != node]
            id_choices_sub = c(id_choices_sub, id_choices_in)
          }
          name_choices_sub = g$nodes[g$nodes$id %in% id_choices_sub, "title_long"]
          updatePickerInput(
            session,
            inputId = "expandSubsectors",
            choices = name_choices_sub,
            selected = NULL
          )
          d_subsector$data = FALSE
        }
      })
  
      # when the user changes the distance sliders, update everything to reflect those changes
      observeEvent(c(input$upstreamMaxDist, input$downstreamMaxDist), {
        # update the dropdowns to show only what's on the screen
        focusid = g$nodes[g$nodes$title_long==input$focusNode,"id"]
        id_choices = c(upstream_nodes(g$edges, focusid, input$upstreamMaxDist, node_level = 1),
                       downstream_nodes(g$edges, focusid, input$downstreamMaxDist, node_level = 1))
        name_choices = g$nodes[g$nodes$id %in% id_choices,"title_long"]
        updatePickerInput(
          session,
          inputId = "expandNodes",
          choices = name_choices,
          selected = g$nodes[g$nodes$id %in% r_expandSectors$data,"title_long"]
        )
  
        # same for subsectors
        id_choices_sub = c()
        for (node in r_expandSectors$data) {
          id_choices_in = upstream_nodes(g$edges, node, 1, node_level = 2)
          id_choices_in = id_choices_in[id_choices_in != node]
          id_choices_sub = c(id_choices_sub, id_choices_in)
        }
        name_choices_sub = g$nodes[g$nodes$id %in% id_choices_sub, "title_long"]
        updatePickerInput(
          session,
          inputId = "expandSubsectors",
          choices = name_choices_sub,
          selected = g$nodes[g$nodes$id %in% r_expandSubsectors$data,"title_long"]
        )
      })
  
      # when the user changes the focus node, update the graph and the dropdowns
      observeEvent(c(input$focusNode),{
        # update the focus node dropdown
        focusid = g$nodes[g$nodes$title_long==input$focusNode,"id"]
        id_choices = c(upstream_nodes(g$edges, focusid, input$upstreamMaxDist, node_level = 1),
                       downstream_nodes(g$edges, focusid, input$downstreamMaxDist, node_level = 1))
        name_choices = g$nodes[g$nodes$id %in% id_choices,"title_long"]
        updatePickerInput(
          session,
          inputId = "expandNodes",
          choices = name_choices,
          selected = c(input$focusNode)
        )
  
        # update the subsector dropdown
        id_choices_sub = c()
        for (node in r_expandSectors$data) {
          id_choices_in = upstream_nodes(g$edges, node, 1, node_level = 2)
          id_choices_in = id_choices_in[id_choices_in != node]
          id_choices_sub = c(id_choices_sub, id_choices_in)
        }
        name_choices_sub = g$nodes[g$nodes$id %in% id_choices_sub, "title_long"]
        updatePickerInput(
          session,
          inputId = "expandSubsectors",
          choices = name_choices_sub,
          selected = g$nodes[g$nodes$id %in% r_expandSubsectors$data,"title_long"]
        )
      })
  
      # keep track of upstream and downstream distances
      r_up = eventReactive(input$upstreamMaxDist, {
        input$upstreamMaxDist
      })
      r_down = eventReactive(input$downstreamMaxDist, {
        input$downstreamMaxDist
      })
  
      # keep track of the focus node
      r_focus = eventReactive(input$focusNode, {
        d_subsector$data = FALSE
        g$nodes[g$nodes$title_long==input$focusNode,"id"]
      })
  
      # if the user hits the expand button, update the dropdown menus accordingly
      observeEvent(input$expandNodes,{
        r_expandSectors$data = g$nodes[g$nodes$title_long %in% input$expandNodes,"id"]
  
        id_choices_sub = c()
        for (node in r_expandSectors$data) {
          id_choices_in = upstream_nodes(g$edges, node, 1, node_level = 2)
          id_choices_in = id_choices_in[id_choices_in != node]
          id_choices_sub = c(id_choices_sub, id_choices_in)
        }
        name_choices_sub = g$nodes[g$nodes$id %in% id_choices_sub, "title_long"]
        updatePickerInput(
          session,
          inputId = "expandSubsectors",
          choices = name_choices_sub,
          selected = g$nodes[g$nodes$id %in% r_expandSubsectors$data,"title_long"]
        )
      })
  
      # if the user changes the subsectors dropdown, update the corresponding list
      observeEvent(input$expandSubsectors, {
        if (is.null(input$expandSubsectors)) {
          r_expandSubsectors$data = c()
        } else {
          r_expandSubsectors$data = g$nodes[g$nodes$title_long %in% input$expandSubsectors,"id"]
        }
      }, ignoreNULL = FALSE)
      
      # All right, let's actually visualize this thing!
      output$network = renderVisNetwork({
        # build the network according to all the parameters set above
        # modify the data slightly if we want to display logits as part of the node name
        if (input$logits) {
          g$nodes$label = g$nodes$label_logits
          delay = 10000
        } else {
          g$nodes$label = g$nodes$label_nologits
          delay = 200
        }
        
        # build the graph
        g.focus2 = construct_graph(
            g,
            focusNode = r_focus(),
            expandSectors = r_expandSectors$data,
            expandSubsectors = r_expandSubsectors$data,
            expand_level = r_expandLevel$data,
            subsectors = d_subsector$data,
            upstreamMaxDist = r_up(),
            downstreamMaxDist = r_down()
          )
        
        # weird bug where sometimes the nodes are not unique
        g.focus2$nodes = g.focus2$nodes %>% unique()
        g.focus2$edges = g.focus2$edges %>% unique()
        
        # network visualization options
        visNetwork(g.focus2$nodes,
                   g.focus2$edges,
                   width = "100%") %>%
          visEdges(arrows  = list(to = list(enabled = TRUE, scaleFactor = 1.5))) %>%
          visGroups(groupname = "Focus Sector",
                    shape = "box",
                    color = list(background = "#ecffb1",
                                 border = "black",
                                 highlight = "#EA8AE2",
                                 hover = list(background = "#ecffb1", border = "#000000")),
                    x = 0,
                    y = 0,
                    physics = FALSE,
                    font = list(
                      size = 18,
                      bold = TRUE
                    ),
                    shadow = list(enabled = FALSE)) %>%
          # formatting of nodes by group
          visGroups(groupname = "Upstream Sectors",
                    shape = "ellipse",
                    color = list(background = "#e18b7d",
                                 border = "black",
                                 highlight = "#74a9cf",
                                 hover = list(background = "#e18b7d", border = "#000000"))) %>%
          visGroups(groupname = "Downstream Sectors",
                    shape = "ellipse",
                    color = list(background = "#7eb5d6",
                                 border = "black",
                                 highlight = "#99D9B9",
                                 hover = list(background = "#7eb5d6", border = "#000000"))) %>% 
          visGroups(groupname = "Upstream Subsectors",
                    shape = "ellipse",
                    color = list(background = "#f6b294",
                                 border = "black",
                                 highlight = "#74a9cf",
                                 hover = list(background = "#f6b294", border = "#000000"))) %>%
          visGroups(groupname = "Downstream Subsectors",
                    shape = "ellipse",
                    color = list(background = "#bad7e9",
                                 border = "black",
                                 highlight = "#74a9cf",
                                 hover = list(background = "#bad7e9", border = "#000000"))) %>%
          visGroups(groupname = "Upstream Technologies",
                    shape = "ellipse",
                    color = list(background = "#fddbc7",
                                 border = "black",
                                 highlight = "#74a9cf",
                                 hover = list(background = "#fddbc7", border = "#000000"))) %>%
          visGroups(groupname = "Downstream Technologies",
                    shape = "ellipse",
                    color = list(background = "#e7f1f7",
                                 border = "black",
                                 highlight = "#74a9cf",
                                 hover = list(background = "#e7f1f7", border = "#000000"))) %>%
          
          #ensure that the output graph is identical for given inputs
          visLayout(randomSeed = 42) %>%
          # highlight nearest nodes and edges on hover
          visOptions(highlightNearest = list(
            enabled = TRUE,
            hover = TRUE
          )) %>%
          # allow dragging and selecting
          visInteraction(dragNodes = TRUE, dragView = TRUE, selectable = FALSE, 
                         selectConnectedEdges = TRUE,tooltipDelay = delay) %>%
          # custom sorted legend
          visLegend(
            useGroups = FALSE,
            main = "Legend",
            addNodes = data.frame(
              label = c("Upstream Technologies", "Upstream Subsectors", "Upstream Sectors", "Focus Sector", "Downstream Technologies", "Downstream Subsectors", "Downstream Sectors"),
              shape = c("ellipse", "ellipse", "ellipse", "box", "ellipse", "ellipse", "ellipse"),
              color.background = c("#fddbc7", "#f6b294", "#e18b7d", "#ecffb1", "#e7f1f7", "#bad7e9", "#7eb5d6"),
              color.border = c("black")
            )
          ) %>%
          # set up the physics parameters for locating nodes on the screen
          visPhysics(enabled = !as.logical(FALSE),
                       solver = 'forceAtlas2Based',
                       forceAtlas2Based = list(
                         gravitationalConstant = -35,
                         centralGravity = 0.01,
                         springLength = 70,
                         springConstant = 0.08,
                         damping = 1,
                         avoidOverlap = 0.5
                       )
                     ) %>%
          # add an "export to pdf" button
          visExport(type = "pdf", name = "io_flows_network")
      })
  }

  # run it
  shinyApp(ui = ui, server = server)
}

### Below is the code that actually gets run when the user hits the "Run App" button
# We look for a config, create one if it does not exist, then check it for errors. 
# If no errors are found, then the graph is loaded based on parameters in the config and the tool is launched

# prompt user to create a config if one does not exist
config_name = "io_flows.cfg"
if (file.exists(config_name)) {
  config.dat = configr::read.config(config_name)
} else {
  config.dat = write_config()
}

# check the config for some basic errors
config.dat = check_config(config.dat)
# if there's an error, rename the file - 
# this will allow the user to see what went wrong but won't stop the code from constructing a new config file on the next run
if ("error" %in% names(config.dat)) {
  file.rename(config_name, "io_flows_ERROR.cfg")
}
# if that passes, let's try to run the app
if (!("error" %in% names(config.dat))) { # error check
  return_code = check_for_inputs(config.dat)
  
  if (return_code == 1) {
    g = load_graph(directory=config.dat$nodes_and_edges_directory)
    g$nodes$font.size = 18 # set label font size
  } else {
    g = list(error = TRUE)
  }
  
  # Go!
  run_app(return_code, config.dat, g)
}
