library(shiny)
library(shinydashboard)
library(igraph)
library(dplyr)
library(stringr)
library(networkD3)
library(bipartite)
library(bipartiteD3)
library(r2d3) #for renderD3() Shiny input function and d3Output()
library(purrr)
library(RColorBrewer)
library(DT)
library(heatmaply)
library(shinycssloaders) # for animated loading icon
library(rsconnect)
library(markdown)
library(jsonlite)
library(shinyalert)

source("custom_bipartited3_functions.R") # removeSource(BP_JS_Writer_a) to remove function from environment

## FUNCTION DEFINITIONS ##


# Abbreviate input strings to match input edgelist
abbr_input <- function(input){
  if(input == "Narratives") {
    abbr <- "nrs"
  } else if(input == "Features") {
    abbr <- "fts"
  } else if(input == "Attributes") {
    abbr <- "ats"
  } else if(input == "Events in Soviet History") {
    abbr <- "sh"
  } else if(input == "Events in pre-Soviet History") {
    abbr <- "psh"
  } else if(input == "All Groups and Locations") {
    abbr <- "ag"
  } else if(input == "All Tajik Groups and Locations") {
    abbr <- "tj"
  } else if(input == "All Non-Tajik Groups and Locations") {
    abbr <- "ntj"
  } else {
    textOutput("Please enter a category")
  }
  return(abbr)
}

# Read in edgelist
read_edgelist <- function(input_1, input_2){ # input_1 = category, input_2 = group
  filepath <- paste0("./input/", abbr_input(input_1), "_", abbr_input(input_2), "_edgelist.csv")
  edgelist <- read.csv2(filepath, header=T, row.names=NULL)
  return(edgelist)
}

# Read in unipartite edgelist
read_unipartite_edgelist <- function(input_1, input_2){ # input_1 = category, input_2 = group
  filepath <- paste0("./input/", abbr_input(input_1), "_", abbr_input(input_1), "_", abbr_input(input_2),"_edgelist.csv")
  edgelist <- read.csv2(filepath, header=T, row.names=NULL)
  return(edgelist)
}

# Read in nodelist
read_nodelist <- function(input_2){
  filepath <- paste0("./input/", abbr_input(input_2), "_nodelist.csv")
  nodelist <- read.csv2(filepath, header=T, row.names=NULL)
  return(nodelist)
}

# Add weights to edgelist by counting recurring edges and combine issues and other 
# attributes into one vector. Note that summarise()` has grouped output by 'source'. Override using the `.groups` argument.
make_weighted <- function(edgelist) {
  edgelist$startpos <- NULL
  edgelist$weight <- 1
  weighted_edgelist <- edgelist %>% 
    group_by(source, target, group) %>% 
    summarise(issues = Reduce(paste, as.character(issue)), weight=sum(weight), timeframe=Reduce(paste, as.character(tempi)), past_present_relation=Reduce(paste, as.character(past_present_relation)), statement_types=Reduce(paste, as.character(type_of_statement))) 
  return(weighted_edgelist)
} 

# Important: reverse order of columns by changing "group_by" order
make_unipartite_weighted <- function(edgelist, input) { # input = category
  edgelist$startpos <- NULL
  edgelist$weight <- 1
  if(input == "Features"){
    weighted_edgelist <- edgelist %>% 
      group_by(source, target, group, feature) %>% 
      summarise(relation=Reduce(paste, as.character(relation)), issues = Reduce(paste, as.character(issue)), weight=sum(weight), timeframe=Reduce(paste, as.character(tempi)), past_present_relation=Reduce(paste, as.character(past_present_relation)), statement_types=Reduce(paste, as.character(type_of_statement))) 
  }else if(input == "Attributes"){
    weighted_edgelist <- edgelist %>% 
      group_by(source, target, group, attribute) %>% 
      summarise(feature=Reduce(paste, as.character(feature)), issues = Reduce(paste, as.character(issue)), weight=sum(weight), timeframe=Reduce(paste, as.character(tempi)), past_present_relation=Reduce(paste, as.character(past_present_relation)), statement_types=Reduce(paste, as.character(type_of_statement))) 
  } else if(input == "Narratives"){
    weighted_edgelist <- edgelist %>% 
      group_by(source, target, group, narrative) %>% 
      summarise(issues = Reduce(paste, as.character(issue)), weight=sum(weight), timeframe=Reduce(paste, as.character(tempi)), past_present_relation=Reduce(paste, as.character(past_present_relation)), statement_types=Reduce(paste, as.character(type_of_statement))) 
  } else { # "Events in Soviet History" or "Events in pre-Soviet History"
    weighted_edgelist <- edgelist %>% 
      group_by(source, target, group, event_or_period) %>% 
      summarise(issues = Reduce(paste, as.character(issue)), weight=sum(weight), timeframe=Reduce(paste, as.character(tempi)), past_present_relation=Reduce(paste, as.character(past_present_relation)), statement_types=Reduce(paste, as.character(type_of_statement))) 
  }
  return(weighted_edgelist)
} 


## UI ##
ui <- fluidPage(
  # custom CSS for Notification button in Bigraph panel
  tags$head(
    tags$style(
      HTML(".shiny-notification {
             position:fixed;
             top: calc(20%);
             left: calc(50%);
             }
             "
      )
    )
  ),
  headerPanel("Soviet Ethnography"),
  sidebarLayout(
  sidebarPanel(
      # reformulate "bipartite" = "two-mode", unipartite = "one-mode"?
      helpText("Choose a category and a group to show 
               a bipartite graph of group-category co-occurrences. 
               Select 'unipartite' to display a unipartite projection
               of the selected category for the selected group(s).
               See 'About' for background and more instructions."),
      selectInput(inputId="category", 
              label="Choose category or select from list",
              choices=c("Narratives", 
                        "Features", 
                        "Attributes", 
                        "Events in Soviet History", 
                        "Events in pre-Soviet History")),
      # align checkboxGroupInput in columns
      fluidRow(
        column(
          width = 6,
          checkboxGroupInput(
            inputId = "category_2",
            label=NULL,
            choices = c("modernization / socialist progress", 
                        "slijanie / sblizhenie", 
                        "Sovietization", 
                        "control over nature",
                        "closing gap between city and countryside")
          )
        ),
        
        column(
          width = 6,#4 for three columns
          checkboxGroupInput(
            inputId = "category_3",
            label = NULL,
            choices = c("battle against religion", 
                        "battle against perezhitki / survivals", 
                        "liberation of women", 
                        "failed modernization", 
                        "socialist internationalism")
          )
        )
      ),
      selectInput(inputId="group",
                         label="Choose group or select from list",
                         choices=c("All Groups and Locations", # Actually this is groups and places
                                   "All Tajik Groups and Locations", 
                                   "All Non-Tajik Groups and Locations"),
                         selected="All Groups and Locations"), #"Tajik Locations","Non-Tajik Locations","Tajik Groups","Non-Tajik Groups","Pamir groups", "Uzbeks", "Groups outside Tajikistan", "Groups in Tajikistan", "Geographical location"
      # align checkboxGroupInput in columns
      fluidRow(
        column(
          width = 6,
          checkboxGroupInput(
            inputId = "group_2",
            label=NULL,
            choices = c("tadzhiki", #Tajiks
                        "uzbeki", 
                        "Pamir groups", 
                        "Kazakhs",
                        "Kyrgyz", 
                        "Central Asian titular nations", 
                        "Historical")
          )
        ),
        
        column(
          width = 6,#4 for three columns
          checkboxGroupInput(
            inputId = "group_3",
            label = NULL,
            choices = c("gornye tadzhiki", 
                        "ravninnye tadzhiki", 
                        "Central Asia", 
                        "Soviet Union", 
                        "Afghanistan", 
                        "Iran", 
                        "women")
          )
        )
      ),
        
      textInput(inputId="years", 
                label="Enter year (e.g. 1955 or 1964,1972; for the 1960s: 196)",), 
      checkboxInput(inputId="unipartite", label="Unipartite (group-group co-occurrences)", value=FALSE), #(one-mode network)
   
  ),
  
  
  mainPanel(
    # output functions wrapped in shinycssloaders::withSpinner() adds animation while loading
    tabsetPanel(
      id="tabsetPanel",
      tabPanel("Network", forceNetworkOutput("network", width = "70em",height = "60em")),
      tabPanel("Edges", dataTableOutput(outputId = "table")),
      tabPanel("Bigraph", d3Output("bipartite", width = "110em",height = "150em")), 
      tabPanel("Heatmap", shinycssloaders::withSpinner(plotlyOutput("heatmap", width = "120em",height = "150em", inline=F), hide.ui=FALSE)), # width = "80em",height = "80em", oder width = "80em",height = "50em"
      tabPanel("About", includeMarkdown("about.md"))
   )
  )
  )
)


## SERVER ##

server <- function(input, output, session) {  

  # reactively remove bipartite graph tabsetPanel if unipartite option is selected
  observeEvent(input$unipartite, {
    if(input$unipartite){
      hideTab("tabsetPanel", "Bigraph")
    } else {
      showTab("tabsetPanel", "Bigraph")
    }
    
  })
  
  # reactively change input: if group is selected, remove choices from group_2 and group_3   
  observeEvent(input$group, {
    updateCheckboxGroupInput(session, "group_2",
                             selected=character(0),)
    updateCheckboxGroupInput(session, "group_3",
                             selected=character(0),)
  })
  
  # reactively change input: if new category is selected,change choices for category_2 and category_3   
  observeEvent(input$category, {
    if(input$category == "Features") {
      updateCheckboxGroupInput(session, "category_2",
                             selected=character(0),
                             choices=c("byt", "housing", "clothing"),)
      updateCheckboxGroupInput(session, "category_3",
                             selected=character(0),
                             choices=c("perezhitki", "physical features", "religion"),)
    } else if (input$category == "Attributes") {
      updateCheckboxGroupInput(session, "category_2",
                               selected=character(0),
                               choices=c("modern", "peredovyj", "isolated"),)
      updateCheckboxGroupInput(session, "category_3",
                               selected=character(0),
                               choices=c("otstalyj", "active", "changing"),)
    } else {
      updateCheckboxGroupInput(session, "category_2",
                               selected=character(0),
                               choices = c("modernization/socialist progress", 
                                           "slijanie/sblizhenie", 
                                           "Sovietization", 
                                           "control over nature",
                                           "closing gap between city and countryside"),)
      updateCheckboxGroupInput(session, "category_3",
                               selected=character(0),
                               choices = c("battle against religion", 
                                           "battle against 'perezhitki'", 
                                           "liberation of women", 
                                           "failed modernization", 
                                           "socialist internationalism"),)
    }

  })
 

  # Create reactive data. Filter edgelist by selected input categories, groups and 
  # years. Unlike filter_by_group, the category name does not exactly match the 
  # input category name, so string matching is employed instead of exact match.
  filtered_edgelist <- reactive({
    input_years <- str_replace_all(input$years,",", "|")
    if (!is.null(input$group_2) || !is.null(input$group_3)) {
      input_groups <- c(input$group_2, input$group_3)
      if(!is.null(input$category_2) || !is.null(input$category_3)){
        input_categories <- c(input$category_2, input$category_3)
        if(input$unipartite == FALSE) {
          edgelist <- read_edgelist("All Groups and Locations", input$category)
          # filter by category 
          edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist$target))
          # filter by group
          edgelist<- subset(edgelist, group %in% input_groups | source %in% input_groups)
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_weighted(edgelist)
        } else {
          edgelist <- read_unipartite_edgelist("All Groups and Locations", input$category)
          # filter by category
          edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist[,8]))
          # filter by group
          # return all rows where both groups in the group column match the input_groups vector
          edgelist$matches <- edgelist$group %>%
            str_split(", ") %>%
            map(is.element, input_groups) %>%
            map(sum) %>%
            unlist
          edgelist <- edgelist[edgelist$matches == 2,] 
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_unipartite_weighted(edgelist, input$category)
        }
      } else { # no category selected
        if(input$unipartite == FALSE) {
          edgelist <- read_edgelist("All Groups and Locations", input$category)
          edgelist<- subset(edgelist, group %in% input_groups | source %in% input_groups)
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_weighted(edgelist)
        } else {
          edgelist <- read_unipartite_edgelist("All Groups and Locations", input$category)
          # return all rows where both groups in the group column match the input_groups vector
          edgelist$matches <- edgelist$group %>%
            str_split(", ") %>%
            map(is.element, input_groups) %>%
            map(sum) %>%
            unlist
          edgelist <- edgelist[edgelist$matches == 2,] 
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_unipartite_weighted(edgelist, input$category)
        }
      }
    } else if(!is.null(input$category_2) || !is.null(input$category_3)){
      input_categories <- c(input$category_2, input$category_3)
      if (!is.null(input$group_2) || !is.null(input$group_3)) {
        input_groups <- c(input$group_2, input$group_3)
        if(input$unipartite == FALSE) {
          edgelist <- read_edgelist("All Groups and Locations", input$category) #edgelist <- read_edgelist(input$group, input$category)
          # filter by group
          edgelist<- subset(edgelist, group %in% input_groups | source %in% input_groups)
          # filter by category
          edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist$target))#grepl(source, input_categories, fixed=T)
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_weighted(edgelist)
        } else {
          edgelist <- read_unipartite_edgelist("All Groups and Locations", input$category) #edgelist <- read_unipartite_edgelist(input$group, input$category)
          # filter by group
          edgelist$matches <- edgelist$group %>%
                  str_split(", ") %>%
                  map(is.element, input_groups) %>%
                  map(sum) %>%
                  unlist
          edgelist <- edgelist[edgelist$matches == 2,]
          # filter by category
          edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist[,8])) # edgelist[,8] always contains attribute, feature, narrative, event_or_period - depending on input category
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_unipartite_weighted(edgelist, input$category)
        } 
      } else {
        if(input$unipartite == FALSE) {
          edgelist <- read_edgelist("All Groups and Locations", input$category) #edgelist <- read_edgelist(input$group, input$category)
          edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist$target))#grepl(source, input_categories, fixed=T)
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_weighted(edgelist)
        } else {
          edgelist <- read_unipartite_edgelist("All Groups and Locations", input$category) #edgelist <- read_unipartite_edgelist(input$group, input$category)
          edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist[,8])) # edgelist[,8] always contains attribute, feature, narrative, event_or_period - depending on input category
          # filter by years 
          edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
          # add edge weights
          edgelist <- make_unipartite_weighted(edgelist, input$category)
        }
      }
    } else { # no categories or groups selected: read in unfiltered edgelist
      if(input$unipartite == FALSE) {
        edgelist <- read_edgelist(input$group, input$category)
        # filter by years 
        edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
        # add edge weights
        edgelist <- make_weighted(edgelist)
      } else {
        edgelist <- read_unipartite_edgelist(input$group, input$category)
        # filter by years 
        edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
        # add edge weights
        edgelist <- make_unipartite_weighted(edgelist, input$category)
      }
    }
  })
  
    
    output$network <- renderForceNetwork ({
      d <- filtered_edgelist()
      if(input$unipartite == FALSE){
        # clean edgelist for input into igraph functions: keep only source, target and weight columns
        d <- subset(d, select = c(source, target, weight))
        # make graph object
        net <- graph_from_data_frame(d, directed = FALSE, vertices = NULL)
        # igraph to networkD3 object
        x_d3 <- igraph_to_networkD3(net)
        # add class attribute to make graph bipartite 
        nodelist <- read_nodelist(input$category)
        x_d3$nodes$class <-  is.element(x_d3$nodes$name,nodelist$label)
        net <- set_vertex_attr(net, "type", value = x_d3$nodes$class) # dont need this here??
        x_d3$links$value = E(net)$weight # this is unnecessary? not used later?
        # create networkD3 object for bipartite graph
        y_d3 <- igraph_to_networkD3(net, group=x_d3$nodes$class)
        colourscale <- "d3.scaleOrdinal().range([d3.schemeCategory10[0],d3.schemeCategory10[4]])"#JS("d3.scaleOrdinal(d3.schemeCategory10);")
        charge <- -2^6 #-2^5
          
      } else {
        # clean edgelist for input into igraph functions
        d <- subset(d, select = c(source, target, weight))
        # make graph object
        net <- graph_from_data_frame(d, directed = FALSE, vertices = NULL)
        # igraph to networkD3 object
        y_d3 <- igraph_to_networkD3(net, group=V(net)$name)
        colourscale <- "d3.scaleOrdinal().range([d3.schemeCategory10[0]])" # '#1f77b4'one color only
        charge <- -2^5 #-2^8
      }
      # draw force directed network
      forceNetwork(
        Links = y_d3$links,
        Nodes = y_d3$nodes,
        Source = 'source',
        Target = 'target',
        NodeID = 'name',
        Value = 'value', #new
        colourScale = colourscale,# JS("d3.scaleOrdinal(d3.schemeCategory10);"),##1f77b4 
        charge= charge, 
        fontSize = 9, #8
        fontFamily = "helvetica",#"serif",
        Group = 'group',
        opacity = 0.8,  
        opacityNoHover = 1, # always display node labels
        zoom=TRUE # enable zoom
      )
    })
  
  output$table <- DT::renderDataTable({
    d <- filtered_edgelist()
    datatable(d)
  })
  

  output$heatmap <- renderPlotly({
    d <- filtered_edgelist()
    
    # make graph: weight will be addded automatically as edge attribute E(net)$weight
    net <- graph_from_data_frame(d, directed = FALSE, vertices = NULL)
    
    if(input$unipartite == FALSE){
      # make bipartite: add "type" attribute
      x_d3 <- igraph_to_networkD3(net)
      nodelist <- read_nodelist(input$category)
      x_d3$nodes$class <-  is.element(x_d3$nodes$name,nodelist$label)
      net <- set_vertex_attr(net, "type", value = x_d3$nodes$class)
      # get incidence matrix 
      m <- as_incidence_matrix(
        net,
        types = NULL,
        attr = "weight",
        names = TRUE,
        sparse = FALSE 
      )

    } else {
      # get adjacency matrix 
      m <- as_adjacency_matrix(
        net, 
        type = "both",
        attr = "weight",
        names = TRUE,
        sparse = FALSE 
      )
      
    }
    # plot heatmap
    plotly <- colorbar(heatmaply(m,
                       colors = colorRampPalette(brewer.pal(9,"YlGnBu")),
                       grid_gap = 0,
                       Rowv = F,
                       Colv = F,
                       symm=T,
                       margins = c(100,100,0,0),
                       label_names = c("Column", "Row", "Weight"),
                       plot_method = "plotly", colorbar_len = 0.7),
             nticks = max(m)+1) %>%
      layout(autosize = TRUE, margin = list(l = 0, r = 0, b = 0, t = 20, pad = 4)) # move output down
      
  })
  

  output$bipartite <- renderD3({
    # This output is only displayed if either specific groups, categories or years are selected.
    # Halt execution and display error message if too many groups or categories are selected
    exists <- is.null(input$group_2) && is.null(input$group_3) && is.null(input$category_2) && is.null(input$category_3) && (is.null(input$years) || input$years == "")
    if(exists && input$tabsetPanel == "Bigraph") {
      showNotification(id="reduceselection","Too many groups or categories selected.", type = "message", duration=NULL)
    } else {
      removeNotification(id="reduceselection")
    }
    req(!exists)
    
    # get filtered edgelist
    d <- filtered_edgelist()
    # write js script using customized bipartiteD3::BP_JS_Writer() function
    # maybe this does not have to be rewritten each time??
    d <- subset(d, select = c(source, target, weight))
    # convert dataframe to incidence matrix using bipartite igraph object
    net <- graph_from_data_frame(d, directed = FALSE, vertices = NULL)
    x_d3 <- igraph_to_networkD3(net)
    nodelist <- read_nodelist(input$category)
    x_d3$nodes$class <-  is.element(x_d3$nodes$name,nodelist$label)
    net <- set_vertex_attr(net, "type", value = x_d3$nodes$class)
    # get incidence matrix
    m <- as_incidence_matrix(
      net,
      types = NULL,
      attr = "weight",
      names = TRUE,
      sparse = FALSE )
    
    # from bipartite_D3 function: transforms incidence matrix to dataframe format
    # accepted by BP_JS_Writer function as input. List2DF, Array2DF, Matrix2DF are
    # from the bipartiteD3 package
    PrimaryLab ="Group or Location"
    SecondaryLab = "Category"
    SiteNames= c("Co-occurrence")
    df<-NULL
    if(tibble::is_tibble(m)){
      d<- as.data.frame(m)
    }
    if(is.data.frame(m)){
      df<- m
    }else{
      if(is.list(m)){
        df<-List2DF(m, PrimaryLab,SecondaryLab,SiteNames) 
      }else{
        dimensions<-length(dim(m))
        if(dimensions ==3){
          df<-Array2DF(m, PrimaryLab,SecondaryLab,SiteNames) 
        }
        if(dimensions==2){
          df<-Matrix2DF(m, PrimaryLab,SecondaryLab,SiteNames) 
        }
      }
    }

    # set colors: will be grey scale in this case
    mycolors <- YlGnBu(50) # number of colors
    # write .js script containing instructions for input into r2d3 function
    # using customized BP_JS_Writer() function from bipartiteD3 package
    BP_JS_Writer_a(df,
                   colouroption="manual",   
                   HighlightLab = 'Unlinked',
                   HighlightCol = '#3366CC',
                   monoChromeCol = 'rgb(56,43,61)',
                   ColourBy = 1,
                   NamedColourVector = mycolors,
                   SortPrimary=NULL,
                   SortSecondary=NULL,
                   mp = c(1,1),
                   MinWidth=1,
                   Pad=4,
                   IndivFigSize= c(300, 1000),
                   BarSize = 35,
                   Orientation = 'vertical',
                   EdgeMode = 'smooth',
                   AxisLabels= c("Group or Location", "Category"),
                   FigureLabel = 'Co-Occurrences',
                   BoxLabPos = c(100, 100),
                   IncludePerc = TRUE,
                   PercentageDecimals =0,
                   PercPos = NULL,
                   CSS_Output_Supress = FALSE,
                   PRINT=FALSE

      )
    
    # convert dataframe to d3 friendly data format (JSON object of arrays)
    colnames(df) <- NULL
    jdata <- jsonlite::toJSON(df)
    # from bipartite_D3 function: creates the visualization based on input data, which 
    # is changed reactively, and .js script containing visualization instructions
    #LoadVisJS()
    r2d3::r2d3(data =jdata, script = "JSBP.js", 
               d3_version = '5',
               dependencies ="vizjs.js") 

  })
  
}

shinyApp(ui = ui, server = server)


