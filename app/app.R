###############################Install Related Packages #######################
if (!require("shiny")) {
  install.packages("shiny")
  library(shiny)
}
if (!require("shinydashboard")) {
  install.packages("shinydashboard")
  library(shinydashboard)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library(lubridate)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require("lubridate")) {
  install.packages("lubridate")
  library("lubridate")
}
if (!require("usmap")) {
  install.packages("usmap")
  library("usmap")
}
if (!require("shinyWidgets")) {
  install.packages("shinyWidgets")
  library("shinyWidgets")
}

## -----------------------------------Import data-----------------------------------------
data <- read.csv("DisasterDeclarationsSummaries.csv")
data_clean <- na.omit(data)

top_disasters <- c("Biological", "Flood", "Hurricane", "Severe Ice Storm", 
                   "Severe Storm(s)", "Snowstorm")

filtered_data <- data_clean %>%
  filter(incidentType %in% top_disasters)

# Filter data for New York State and convert date format
ny_data <- filter(data_clean, state == "NY") %>%
  mutate(Year = year(ymd_hms(declarationDate)))

# Get the range of all years
years_range <- range(ny_data$Year)

cleaned_data <- read.csv('cleaned_data.csv')



#########################################################################
##
##                                UI
#########################################################################

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Disaster Summaries"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Beginning", tabName = "begin", icon = icon("compass")),
                        menuItem("Introduction", tabName = "intro", icon = icon("home")),
                        menuItem("Disaster Types", tabName = "type", icon = icon("square-poll-vertical")),
                        menuItem("House Incident Map", tabName = "damage", icon = icon("city")),
                        menuItem("Recovery Programs", tabName = "program", icon = icon("chart-pie"))
                      )
                    ),
                    
                    dashboardBody(
                      tags$head(
                        tags$style(HTML("
      #page { 
        background-size: cover; 
        height: 100vh; 
      }
      .first-bg { 
        background-image: url('https://assets-lbmjournal-com.s3.us-east-2.amazonaws.com/2023/09/PLM-Featured-Image.jpg'); 
      }
      .second-bg { 
        background-image: url('https://raw.githubusercontent.com/aqkelly/project2-shinyapp-group12/main/doc/figs/pic.jpg'); 
      }

    "))
                      ),
                      
                      tabItems(
                        # ------------------ Beginning ---------------------------------------------------
                        tabItem(tabName = "begin", div(id = "page", class = 'first-bg', 
                                                       fluidRow(
                                                         absolutePanel(
                                                           style = "background-color: white",
                                                           top = "45%",
                                                           left = "25%",
                                                           right = "25%",
                                                           height = 150,
                                                           tags$p(
                                                             style = "padding: 5%; background-color: white; font-family: alegreya; font-size: 120%",
                                                             "The image presents a powerful tableau of nature's most dramatic and formidable expressions. The skies and earth converge in a quartet of tumultuous events, each scene a testament to the immense energy and raw power of the natural world."
                                                           ))))
                        ),
                        # ------------------ Introduction ---------------------------------------------------
                        
                        tabItem(tabName = "intro", 
                                fluidPage(
                                  fluidRow(box(width = 15, title = "Introduction", status = "primary",
                                               solidHeader = TRUE, 
                                               h3("Disaster Declarations Summaries"),
                                               h5("We are aiming to develop a mobile application specifically designed 
                        for government agencies, enabling them to swiftly summarize and analyze 
                        historical disaster data. This application will facilitate these agencies 
                        in conducting comprehensive analyses of past disasters, which, in turn, 
                        will inform their decision-making processes regarding funding allocations 
                        and budgeting strategies. By leveraging this data, the government can make
                        more informed decisions, optimizing the allocation of resources to areas most
                        in need and improving disaster response and preparedness strategies."))),
                                  
                                  fluidRow(box(width = 15, title = "Targeted User & Usage", status = "primary", 
                                               solidHeader=TRUE,
                                               HTML(
                                                 "<h5> Government agenct </h5>
                       <h5> <p><li>summarize the historical disaster data</a></li></h5>
                       <h5><li>making funding desicion</a></li></h5>
                       <h5><li>budgeting</a></li></h5>"
                                               ))),
                                  
                                  fluidRow(box(width = 15, title = "Reference", status = "primary", 
                                               solidHeader=TRUE,
                                               HTML(
                                                 "<h3> Data Sources </h3>
                       <h5> <p><li>Disaster Declarations Summaries: <a href='https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2'>Link</a></li></h4>
                       <h5><li>Housing Assistance Program Data - Owners: <a href='https://www.fema.gov/openfema-data-page/housing-assistance-program-data-owners-v2'> Link</a></li></h4>"
                                               )
                                  )),
                                  
                                  h5("By Jiaqi Lu, Tianyi Jiang, Yuqi Liu, Guanbiao Li"))),
                        
                        # ------------------ Disaster Types ------------------------------------------------
                        tabItem(tabName = "type",
                                h2("Distribution of Top 5 Disasters by State and Year", align = 'center'),
                                fluidPage(
                                  h5("The following graph count the top 5 Disasters: Biological, 
                Flood, Hurricane, Severe Ice Storm, Severe Storm(s), Snowstorm in different states."),
                                  sidebarLayout(
                                    sidebarPanel(
                                      selectInput("state", "Choose a state:",
                                                  choices = unique(filtered_data$state)),
                                      sliderInput("yearRange", label = "Select Year Range:",
                                                  min = min(filtered_data$fyDeclared, na.rm = TRUE), 
                                                  max = max(filtered_data$fyDeclared, na.rm = TRUE), 
                                                  value = c(min(filtered_data$fyDeclared, na.rm = TRUE), 
                                                            max(filtered_data$fyDeclared, na.rm = TRUE)), 
                                                  step = 1)),
                                    mainPanel(
                                      plotOutput("disasterPlot"))
                                  )
                                  )),
                        
                        # ------------------ Recovery Program ----------------------------------------------
                        tabItem(tabName = "program",
                                h2("Distribution of Aid Programs in New York State", align = 'center'),
                                fluidPage(
                                  sidebarLayout(
                                    sidebarPanel(
                                      # Add time range selector
                                      sliderInput("yearRange1",
                                                  "Select Year Range:",
                                                  min = years_range[1], max = years_range[2],
                                                  value = years_range, # Default to full range
                                                  step = 1, sep = ''),
                                      h4("Program Introduction"),
                                      h5("IH: Individual Housing"),
                                      h5("IA: Individual Assistance"),
                                      h5("PA: Public Assistance"),
                                      h5("HM: Hazard Mitigation")
                                    ),
                                    mainPanel(plotOutput("programPlot"))))
                        ),
                        
                        # ------------------ Recovery Program ----------------------------------------------
                        tabItem(tabName = "damage",
                                h2("New York Region - Total Number of Incidents by County", align = 'center'),
                                sidebarLayout(
                                  sidebarPanel(
                                    radioButtons("mapType", "Choose Map Type:",
                                                 choices = list("Total Damage" = "total_damage",
                                                                "Affected Individuals" = "num_damage_owners",
                                                                "IHP Approved" = "total_approvedIhp_owners"),
                                                 selected = "total_damage")
                                  ),
                                  mainPanel(
                                    plotOutput("selectedMap")
                                  ))
                        )
                      )
                    )
)

#########################################################################
##
##                                Server
#########################################################################

server <- function(input, output) {
  
  output$disasterPlot <- renderPlot({
    # Filter data based on input
    state_data <- filtered_data %>%
      filter(state == input$state, fyDeclared >= input$yearRange[1], fyDeclared <=
               input$yearRange[2])
    
    disaster_count <- state_data %>%
      group_by(incidentType) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
    
    ggplot(disaster_count, aes(x = reorder(incidentType, Count), y = Count, fill = incidentType)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = paste("Disaster Distribution in", input$state, "from", 
                         input$yearRange[1], "to", input$yearRange[2]),
           x = "Disaster Type",
           y = "Number of Disasters") +
      coord_flip()
    
  })
  
  # Filter data based on selected time range
  filtered_data1 <- reactive({
    ny_data %>%
      filter(Year >= input$yearRange1[1], Year <= input$yearRange1[2])
  })
  
  # Render the pie chart
  output$programPlot <- renderPlot({
    # Summarize the counts for each aid program
    program_counts <- filtered_data1() %>%
      summarise(IH = sum(ihProgramDeclared, na.rm = TRUE),
                IA = sum(iaProgramDeclared, na.rm = TRUE),
                PA = sum(paProgramDeclared, na.rm = TRUE),
                HM = sum(hmProgramDeclared, na.rm = TRUE)) %>%
      pivot_longer(cols = IH:HM, names_to = "Program", values_to = "Count") %>%
      mutate(Percentage = Count / sum(Count) * 100,
             Label = ifelse(Percentage > 0, paste(Program, "\n", round(Percentage, 1), "%"), ""))
    
    # Draw the pie chart
    ggplot(program_counts, aes(x = "", y = Count, fill = Program)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      theme_void() +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5)) +
      labs(title = paste("Aid Programs Distribution in New York State", 
                         input$yearRange1[1], "-", input$yearRange1[2]),
           x = NULL, y = NULL, fill = "Program")
  })
  
  
    output$selectedMap <- renderPlot({
    
    # Determine which map to plot based on selected input
    value_col <- switch(input$mapType,
                        "total_damage" = "total_damage",
                        "num_damage_owners" = "num_damage_owners",
                        "total_approvedIhp_owners" = "total_approvedIhp_owners")
    
    title <- switch(input$mapType,
                    "total_damage" = "Total Damage per County",
                    "num_damage_owners" = "Number of People Affected per County",
                    "total_approvedIhp_owners" = "Total Approved IHP Owners per County")
    
    subtitle <- switch(input$mapType,
                       "total_damage" = "Total Damage",
                       "num_damage_owners" = "Affected Individuals",
                       "total_approvedIhp_owners" = "IHP Approved")
    
    # Plotting the map
    plot_usmap(data = cleaned_data, values = value_col, include = c("NY"), color = "blue") +
      scale_fill_continuous(low = "yellow", high = "red", name = subtitle, labels = scales::comma) +
      labs(title = "Impact on New York Counties", subtitle = title) +
      theme(legend.position = "right")
  })
  
  observeEvent(input$regen, {
    data() # This will trigger a new generation of data
  })
}


shinyApp(ui = ui, server = server)