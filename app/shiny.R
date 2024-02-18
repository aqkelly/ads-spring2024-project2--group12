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

df_select <- read.csv('cleaned_data.csv')
incidents_per_county <- df_select %>%
  group_by(county, fipsCountyCode) %>%
  summarise(incidentCount = n()) %>%
  arrange(desc(incidentCount))

incidents_per_county <- incidents_per_county %>%
  mutate(state = "NY")

incidents_per_county$fips <- sprintf("%02d%03d", 36, incidents_per_county$fipsCountyCode)

plot_usmap(data = incidents_per_county, values = "incidentCount", include = c("NY"), color = "blue") + 
  scale_fill_continuous(low = "yellow", high = "red", name = "Total Incidents", labels = scales::comma) + 
  labs(title = "New York Region", subtitle = "Total Number of Incidents by County") +
  theme(legend.position = "right")

#########################################################################
##
##                                UI
#########################################################################

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Disaster Summaries"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("home")),
      menuItem("Disaster Types", tabName = "type", icon = icon("square-poll-vertical")),
      menuItem("Recovery Programs", tabName = "program", icon = icon("chart-pie")),
      menuItem("House Damage", tabName = "damage", icon = icon("city"))
    )
  ),
  
  dashboardBody(
    tabItems(
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
        
        fluidRow(box(width = 15, title = "Targeted User", status = "primary", 
                     solidHeader=TRUE,
                     h5("We want to create an app for government agency for them to quickly 
                        summarize the historical disaster data, and use these data to do further
                        analysis such as funding desicion and budgeting."
                        ))),
        
        fluidRow(box(width = 15, title = "Reference", status = "primary", 
                     solidHeader=TRUE,
                     HTML(
                       "<h3> Data Sources </h3>
                       <h5> <p><li>Disaster Declarations Summaries: <a href='https://www.fema.gov/openfema-data-page/disaster-declarations-summaries-v2'>Link</a></li></h4>
                       <h5><li>Housing Assistance Program Data - Owners: <a href='https://www.fema.gov/openfema-data-page/housing-assistance-program-data-owners-v2'> Link</a></li></h4>"
                     ),
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
              ))),
      
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
                                step = 1, sep = '')),
                  mainPanel(plotOutput("programPlot"))))
              ),
      
      # ------------------ Recovery Program ----------------------------------------------
      tabItem(tabName = "damage",
              h2("New York Region - Total Number of Incidents by County", align = 'center'),
              plotOutput("incidentsMap")
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
  
  
  output$incidentsMap <- renderPlot({ # Use renderPlot for plotting
    # Your data preparation steps should go here
    # Ensure incidents_per_county is defined and contains the expected data
    
    # Placeholder for the plotting logic, assuming incidents_per_county is ready
    # You should replace the below example with your actual plotting code
    plot_usmap(data = incidents_per_county, values = "incidentCount", include = c("NY"), color = "blue") + 
      scale_fill_continuous(low = "yellow", high = "red", name = "Total Incidents", labels = scales::comma) + 
      labs(title = "New York Region", subtitle = "Total Number of Incidents by County") +
      theme(legend.position = "right")
  })
  
  observeEvent(input$regen, {
    data() # This will trigger a new generation of data
  })
}


shinyApp(ui = ui, server = server)