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
library(shinyalert) # for notifications

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
  } else if(input == "Events, Periods in Soviet History") {
    abbr <- "sh"
  } else if(input == "Events, Periods in pre-Soviet History") {
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

# Read in bipartite edgelist
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


# Return input group and all its subgroups (input group is value in group column, not source column, in this case)
filter_groups_bipartite <- function(edgelist, input_groups) {
  if(length(input_groups) > 0) { # input_groups not empty: either group_2 or group_3 was selected and !is.null(input$group_2) || !is.null(input$group_3)
    edgelist_a <- subset(edgelist, grepl(paste(input_groups,collapse="|"), edgelist$group)) 
    edgelist_b <- subset(edgelist, grepl(paste(input_groups,collapse="|"), edgelist$source)) 
    edgelist <- rbind(edgelist_a, edgelist_b)
    return(edgelist)
  } else {
    return(edgelist)
  }
}


# Helper function for filter_groups_unipartite(). Takes the length of the current 
# input_groups vector as input (i.e. length(c(input$groups_2, input$groups_3)))
# and returns a vector of indices. This vector may be used to drop rows from a 
# dataframe created by expand.grid() function. expand.grid() calculates the Cartesian 
# product of the elements of two lists and returns a dataframe in which rows represent
# all combinations of elements from the two lists, which includes elements with the 
# same index position, i.e vector_1[[1]] and vector_2[[1]]. In the filter_groups_unipartite()
# function, vector_1 and vector_2 are vectors of indices of rows in the input edgelist 
# which matched matched the search group input. vector_1[[1]] and vector_2[[1]] thus contain
# indices of rows from the input edgelist that matched with the same element in the 
# input_groups vector. However, when more than one group is selected, the edgelist 
# is filtered for rows where different elements in the input_groups vector were matched.
helper <- function(n) {
  vec <- c()
  for(i in 1:n){
    vec <- c(vec, n*(n-(n-(i-1)))+i)
  }
  return(vec)
}

# Return all rows where either source or group 1 in group column and either target or 
# group 2 in group column match the input_groups vector.
# If one group is selected, display all edges between subgroups of the selected category
# If more than one group is selected, display only edges between subgroups of different main groups. 
# F.e.: group 1 = tadzhiki, group 2 = uzbeki. Edges between tadzhiki Seravshana and uzbeki Samarkanda 
# are displayed, but not edges between tadzhiki Seravshana and tadzhiki Darvaza.
filter_groups_unipartite <- function(edgelist, input_groups) {
  # temporarily split up "group" column into two: "group" column contains two groups, 
  # the first corresponds to the source column, the second to the target column. 
  temp <- str_split_fixed(edgelist$group, ",", 2)
  if(length(input_groups) == 1) { # only one category selected
    group_matches_1 <- grep(input_groups, temp[,1]) # return row indices
    group_matches_2 <- grep(input_groups, temp[,2]) # return row indices
    source_matches <- grep(input_groups, edgelist$source)# return row indices
    target_matches <- grep(input_groups, edgelist$target)# return row indices
    if(!is.null(group_matches_1) && !is.null(group_matches_2)) { 
      matches <- intersect(group_matches_1, group_matches_2)
    } else if(!is.null(group_matches_1) && !is.null(target_matches)) {
      matches <- intersect(group_matches_1, target_matches)
    } else if(!is.null(group_matches_2) && !is.null(source_matches)) {
      matches <- intersect(group_matches_2, source_matches)
    } else if(!is.null(source_matches) && !is.null(target_matches)) {
      matches <- intersect(source_matches, target_matches)
    } else {
      return(edgelist)
    }
    edgelist <- edgelist[matches, ]
    return(edgelist)
  } else if(length(input_groups) > 1) { # more than one category selected
    # get list of list of all row indices that matched each of the input groups. 
    # i.e. for input_groups <- c("tadzhiki", "uzbeki"), group_matches is a list 
    # of two lists: The first list contains indices of rows from the input edgelist, 
    # where "tadzhiki" returned a match for the first element in the "group" column. 
    group_matches_1 <- map(input_groups, grep, temp[,1])  # return row indices
    group_matches_2 <- map(input_groups, grep, temp[,2])
    source_matches <- map(input_groups, grep, edgelist$source)  # return row indices
    target_matches <- map(input_groups, grep, edgelist$target)
    # call helper function to remove all combinations between matches for the 
    # same input group (see helper function specification)
    indices <- helper(length(input_groups))
    # get df of row indices of all combinations between group_matches_1 and group_matches_2
    group_group_combinations <- expand.grid(group_matches_1, group_matches_2) 
    group_group_combinations <- group_group_combinations[-indices, ]
    # get df of row indices of all combinations between group_matches_1 and target_matches 
    group_target_combinations <- expand.grid(group_matches_1, target_matches) 
    group_target_combinations <- group_target_combinations[-indices, ]
    # get df of row indices of all combinations between group_matches_2 and source_matches 
    source_group_combinations <- expand.grid(group_matches_1, target_matches) 
    source_group_combinations <- source_group_combinations[-indices, ]
    # get df of row indices of all combinations between source_matches and target_matches
    source_target_combinations <- expand.grid(source_matches, target_matches) 
    source_target_combinations <- source_target_combinations[-indices, ]
    # apply intersect() rowwise to both columns in the dataframe of combinations, return unique matches
    matches <- unlist(map2(group_group_combinations$Var1, group_group_combinations$Var2, intersect))
    matches <- c(matches, unlist(map2(group_target_combinations$Var1, group_target_combinations$Var2, intersect)))
    matches <- c(matches, unlist(map2(source_group_combinations$Var1, source_group_combinations$Var2, intersect)))
    matches <- c(matches, unlist(map2(source_target_combinations$Var1, source_target_combinations$Var2, intersect)))
    matches <- unique(matches)
    if(!is.null(matches)) {
      edgelist <- edgelist[matches, ]
      return(edgelist)
    } else {
      return(edgelist)
    }
  } else {
    return(edgelist)
  }
}


filter_categories_bipartite <- function(edgelist, input_categories) {
  if(length(input_groups) > 0) {
    edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist$target))
    return(edgelist)
  } else {
    return(edgelist)
  }
}

filter_categories_unipartite <- function(edgelist, input_categories) {
  if(length(input_groups) > 0) {
    edgelist <- subset(edgelist, grepl(paste(input_categories,collapse="|"), edgelist[,9])) # edgelist[,9] always contains attribute, feature, narrative, event_or_period - depending on input category
    return(edgelist)
    } else {
      return(edgelist)
  }
}


# Add weights to bipartite edgelist by counting recurring edges and combine 
# issues and other attributes into one vector. 
make_weighted <- function(edgelist) {
  edgelist$startpos <- NULL
  edgelist$weight <- 1
  weighted_edgelist <- edgelist %>% 
    group_by(source, target, group) %>% 
    summarise(issues = Reduce(paste, as.character(issue)), weight=sum(weight), timeframe=Reduce(paste, as.character(tempi)), past_present_relation=Reduce(paste, as.character(past_present_relation)), statement_types=Reduce(paste, as.character(type_of_statement))) 
  return(weighted_edgelist)
} 

# Add weights to unipartite edgelist by counting recurring edges and combine 
# issues and other attributes into one vector depending on input category
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
      summarise(issues = Reduce(paste, as.character(issue)), weight=sum(weight), timeframe=Reduce(paste, as.character(tempi)), past_present_relation=Reduce(paste, as.character(past_present_relation)), statement_types=Reduce(paste, as.character(type_of_statement))) # feature=Reduce(paste, as.character(feature)), 
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
  
  tags$head(
    tags$style(
      # suppress error messages in output window
      type="text/css",
      ".shiny-output-error { visibility:hidden; }",
      ".shiny-output-error:before { visibility: hidden; }",
      # custom CSS for Notification button in Bigraph panel
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
                        "Events, Periods in Soviet History", 
                        "Events, Periods in pre-Soviet History")),
      
      conditionalPanel(condition='input.category == "Features" && input.mode != "Bipartite"',
        checkboxInput(inputId="similarity", label="Only display similarity", value=FALSE)),

      
      # align checkboxGroupInput in columns
      fluidRow(
        column(
          width = 6,
          checkboxGroupInput(
            inputId = "category_2",
            label=NULL,
            choices = c("modernization", 
                        "Sovietization", 
                        "sblizhenie", 
                        "control over nature",
                        "closing gap between city and countryside", 
                        "socialist internationalism")
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
                        "development of national in form, socialist in content culture",
                        "assimilation")
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
                        "kazakhi",
                        "kirgizy",
                        "turkmeny",
                        "Central Asian titular nations", 
                        "Historical groups",
                        "Historical locations",
                        "Tajikistan",
                        "Uzbekistan",
                        "Yaghnob", 
                        "Northern Tajikistan", 
                        "Southern Tajikistan", 
                        "Tajiks outside the Tajik SSR",
                        "women")
          )
        ),
        
        column(
          width = 6,#4 for three columns
          checkboxGroupInput(
            inputId = "group_3",
            label = NULL,
            choices = c("gornye tadzhiki", 
                        "ravninnye tadzhiki",
                        "plainland regions",
                        "mountainous regions",
                        "Zeravshan", 
                        "Ferghana",
                        "Soviet Union", 
                        "russkij narod / Russians",
                        "tjurki",
                        "Afghanistan", 
                        "Iran",
                        "Darvaz",
                        "Karategin",
                        "Kuljab",
                        "urban population",
                        "rural population")
          )
        )
      ),
        
      textInput(inputId="years", 
                label="Enter year (e.g. 1955 or 1964,1972; for the 1960s: 196)",), 
     
      radioButtons(inputId="mode", label="Choose mode", choices=c("Bipartite", "Unipartite (group-group co-occurrences)"), inline=T), 
      
  ),
  
  
  mainPanel(
    # output functions wrapped in shinycssloaders::withSpinner() adds animation while loading
    tabsetPanel(
      id="tabsetPanel",
      tabPanel("Network", forceNetworkOutput("network", width = "70em",height = "60em")),
      tabPanel("Edges", dataTableOutput(outputId = "table")),
      tabPanel("Bigraph", d3Output("bipartite", width = "110em",height = "150em")), 
      tabPanel("Heatmap", shinycssloaders::withSpinner(plotlyOutput("heatmap", width = "120em",height = "130em", inline=F), hide.ui=FALSE)), # height = 150em
      tabPanel("About", includeMarkdown("about.md"))
   )
  )
  )
)


## SERVER ##

server <- function(input, output, session) {  
  # print generalized error message in case of system errors
  options(shiny.sanitize.errors = TRUE)
  
  # reactively remove bipartite graph tabsetPanel if unipartite option is selected
  observeEvent(input$mode, {
    if(input$mode != "Bipartite"){
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
    # reset "Only display similarity" input choice whenever a user switches between main categories
    updateCheckboxInput(session, "similarity",
                        value=FALSE)
    if(input$category == "Features") {
      updateCheckboxGroupInput(session, "category_2",
                             selected=character(0),
                             choices=c("byt / bytovyj uklad", "clothing", "food / cooking", "housing", "kul'tura / kul'turnye tradicii",
                                       "material'naja kul'tura", "dukhovnaja kul'tura",
                                       "language / spoken","language / terminology", "prazdniki", "folklore",
                                       "intermarriages", "weddings", "child birth", "family life","death / funerals",
                                       "medicine / healthcare" #, "antropologicheskij tip", "etnicheskie priznaki", "antropologicheskij sostav"
                                       ),)
      updateCheckboxGroupInput(session, "category_3",
                             selected=character(0),
                             choices=c("perezhitki", "religion", "beliefs / myths",
                                       '"racial type" / rasovyj tip / rasa', #"physical features,  anthropometry,  anthroposcopy"
                                       "ethnogenesis", "etnos / ethnos", 
                                       #"genetic features", "anthroposcopic measures", "craniological features", "physical features",
                                      "gender relations / polozhenie zhensshiny",
                                      "family structure", "social formation", "mode of production",
                                      "obraz zhizni", "kolkhozes", "agriculture",
                                      "identity / self-description", "external identification"),)
    } else if (input$category == "Attributes") {
      updateCheckboxGroupInput(session, "category_2",
                               selected=character(0),
                               choices=c("modern / sovremennyj", "progressive / peredovyj", "progressive / progressivnyj","developed / razvito", 
                                         "suppressed / ugnetennyj / exploited", "unequal", "egalitarian / liberated",
                                         "undemocratic / hierarchical / despotic", "democratic", "collectivist",
                                         "atheist / non-spiritual / materialist",
                                         "educated", "obespechenno", "urban", "kul'turno",
                                         "flawed / in need of improvement", 
                                         "Muslim", "Zoroastrian", "religious", "superstitious",
                                         "feudal","patriarchal", "matriarchal",
                                         "loyal / trustworthy", "disloyal / suspect"),)
      updateCheckboxGroupInput(session, "category_3",
                               selected=character(0),
                               choices=c("otstalyj", "archaic / arkhaichno", "primitive / primitivno",
                                         "inhospitable / adverse conditions", "hospitable", "isolated / otdalennyj / trudnodostupnyj", "zamknutyj",
                                         "active", "passive", "evropeoidno", '"chisto" / pure / clean',
                                         "avtokhtonno", "unique / locally specific / svoeobraznyj", "diverse / mnogoobrazno",
                                         "traditional / narodnyj / nacional'nyj", "mixed / coexisting",
                                         "vanishing / being replaced or erased", "threatened / attacked / in need of protection",
                                         "changing / in a process of change",
                                         "growing", "ustoichivo / stable", 
                                         "peaceful", "belligerent"),)
    } else if (input$category == "Events, Periods in Soviet History") {
      updateCheckboxGroupInput(session, "category_2",
                               selected=character(0),
                               choices=c("forced resettlements late 1920s-1930s",
                                         "forced resettlements 1940s-early 1950s",
                                         "forced resettlements 1960s - 1970s",
                                         "construction of Bol'shoj Ferganskij kanal 1938-1940",
                                         "process urbanizacii / urbanizacija"),)
      updateCheckboxGroupInput(session, "category_3",
                               selected=character(0),
                               choices=c("nacional'no-gosudarstvennye razmezhevanija v Srednej Azii 1920x", 
                                         "perekhod k osedlosti Sredneaziatskikh kochevnikov nachalo XXv.", 
                                         "process nacional'noj konsolidacii tadzhikskogo naroda",
                                         "gody kollektivizacii"),)
    } else if (input$category == "Events, Periods in pre-Soviet History") { 
      updateCheckboxGroupInput(session, "category_2",
                               selected=character(0),
                               choices=c("prisoedinenie Central'noj / Srednej Azii k Rossii", 
                                         "prisoedinenie Vostochnoj Bukhary k Rossii", 
                                         "prisoedinenie Kokandskogo khanstva k Rossii"),)
      updateCheckboxGroupInput(session, "category_3",
                               selected=character(0),
                               choices=c("pre-Soviet migration from highlands to lowlands", 
                                         "process osedanija kochevnikov v konce XIX - nachale XX vv.", 
                                         "Arabskoe zavoevanie Srednej Azii i vnedrenie islama"),)
    } else { #input$category == "Narratives"
      updateCheckboxGroupInput(session, "category_2",
                               selected=character(0),
                               choices = c("modernization", 
                                           "Sovietization", 
                                           "sblizhenie",
                                           "control over nature",
                                           "closing gap between city and countryside",
                                           "socialist internationalism"),)
      updateCheckboxGroupInput(session, "category_3",
                               selected=character(0),
                               choices = c("battle against religion", 
                                           "battle against perezhitki / survivals", 
                                           "liberation of women",
                                           "development of national in form, socialist in content culture",
                                           "assimilation"),)
    }

  })
 

  # Create reactive data. Filter edgelist by selected input categories, groups and 
  # years. Unlike filter_by_group, the category name does not exactly match the 
  # input category name, so string matching is employed instead of exact match.
  filtered_edgelist <- reactive({
    # input choices into vectors 
    input_years <- str_replace_all(input$years,",", "|")
    input_groups <- c(input$group_2, input$group_3) # if neither group_2 nor group_3 are selected, input_groups is an empty vector
    input_categories <- c(input$category_2, input$category_3) # if neither category_2 nor category_3 are selected, input_groups is an empty vector
    # read and filter edgelist depending on input mode
    if(input$mode == "Bipartite") {
      edgelist <- read_edgelist(input$group, input$category)
      edgelist <- filter_groups_bipartite(edgelist, input_groups)
      edgelist <- filter_categories_bipartite(edgelist, input_categories)
    } else {
      edgelist <- read_unipartite_edgelist(input$group, input$category)
      edgelist <- filter_groups_unipartite(edgelist, input_groups)
      edgelist <- filter_categories_unipartite(edgelist, input_categories)
      if(input$similarity) { #  && input$category == "Features"
        # filter for similarity relation
        edgelist <- subset(edgelist, grepl("similarity", edgelist[,10]))
      }
    }
    # filter by years: same for bipartite and unipartite modes 
    edgelist <- subset(edgelist, grepl(input_years, edgelist$issue))
    
  })
  
  # Create reactive data. Add edge weights to dataframe filtered_edgelist
  weighted_edgelist <- reactive({
    edgelist <- filtered_edgelist()
    if(input$mode == "Bipartite") {
      edgelist <- make_weighted(edgelist)
    } else {
      edgelist <- make_unipartite_weighted(edgelist, input$category)
    }
  })
  
    
  output$network <- renderForceNetwork ({
      
      d <- weighted_edgelist()
      
      # create bipartite / unipartite igraph object and convert to networkD3 object
      if(input$mode == "Bipartite"){
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
    d <- weighted_edgelist()
    datatable(d)
  })
  

  output$heatmap <- renderPlotly({
    
    d <- weighted_edgelist()
    
    # make graph object: weight will be added automatically as edge attribute E(net)$weight
    net <- graph_from_data_frame(d, directed = FALSE, vertices = NULL)
    
    # transform input dataframe to incidence/adjacency matrix
    if(input$mode == "Bipartite"){
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
    
    # Display output only if either specific groups, categories or years are selected.
    # Halt execution and display error message if too many groups or categories are selected
    exists <- is.null(input$group_2) && is.null(input$group_3) && is.null(input$category_2) && is.null(input$category_3) && (is.null(input$years) || input$years == "")
    if(exists && input$tabsetPanel == "Bigraph") {
      showNotification(id="reduceselection","Too many groups or categories selected.", type = "message", duration=NULL)
    } else {
      removeNotification(id="reduceselection")
    }
    # shiny::validate(
    #   need(!exists, "Please select a data set")
    # )
    req(!exists)
    
    # get filtered edgelist
    d <- weighted_edgelist()
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
    # from bipartite_D3 function: create the visualization based on input data, which 
    # is changed reactively, and .js script containing visualization instructions
    #LoadVisJS() #uncomment this in case of error "viz not found"
    r2d3::r2d3(data =jdata, script = "JSBP.js", 
               d3_version = '5',
               dependencies ="vizjs.js") 

  })
  
}

shinyApp(ui = ui, server = server)

