
library(shiny)
library(dplyr)
library(ggplot2)

# Montgomery County, MD Crime Stats Dataset
df_0 <- read.csv("https://query.data.world/s/7jqlkphcnfizaq2fiocyiug527i3r6", 
                 header=TRUE, stringsAsFactors=FALSE)

# Dataset is large enough to remove any NA rows
df <- na.omit(df_0)

# Extract month and year from timestamp, then create Year_month column
# Filter for year_month before 201906 so that it's exactly 3 years of data
df$year <- substr(df$Start.Date.Time, 7, 10)
df$month <- substr(df$Start.Date.Time, 1, 2)
df$year_mo <- as.numeric(paste(df$year, df$month, sep = ""))
df <- subset(x = df, subset = year_mo <= 201906) 

# Summarize number of crimes by type by year
df_crime_type_summ <- df %>% 
  group_by(Crime.Name1, year_mo) %>% 
  count() %>% 
  na.omit()

# Remove empty crime types
df_crime_type_summ <- df_crime_type_summ[!df_crime_type_summ$Crime.Name1 == "", ]

# Total number of victims per place of crime
df_byPlace <- df %>% 
  group_by(Place) %>% 
  summarise(totalVictims = sum(Victims)) %>% 
  select(Place, totalVictims) %>% 
  arrange(desc(totalVictims))

# Remove empty months
df_month <- df[!df$month == "", ]

# Find number of victims by zip code
df_zip <- df %>% 
  group_by(Zip.Code, year) %>% 
  summarise(totVictims = sum(Victims))

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Montgomery County, Maryland Crime Statistics"),
  
  # 
  sidebarLayout(
    sidebarPanel(
      
      checkboxGroupInput(inputId = "crime_type",
                         label = "Choose crime type(s):",
                         choices = c("Crime Against Society",
                                     "Crime Against Property",
                                     "Crime Against Person",
                                     "Other",
                                     "Not a Crime"),
                         selected = c("Crime Against Society",
                                      "Crime Against Property",
                                      "Crime Against Person",
                                      "Other",
                                      "Not a Crime")),
      
      br(),
      
      helpText("Note that 2017 and 2018 are the only years with a full 12 months
               of data, so please keep that in mind when making comparisons."),
      radioButtons(inputId = "yr",
                   label = "Choose the year:",
                   choices = c("2016", "2017", "2018", "2019"),
                   selected = "2018"),
      br(),
      textInput("zip", "Enter the Zip Code:", value = "20910"),
      
      numericInput(inputId = "topN",
                   label = "Choose top N:",
                   value = 10,
                   min = 1, step = 1),
      br(),
      
      helpText("Click the button below to download the original dataset."),
      downloadButton(outputId = "downloadData", label = "Download")
      ),
    
    # Show plots and data table
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Yearly Trends",
                           br(), 
                           p("The graph below shows the number of crimes per month and by year
                             so you can make year-over-year comparisons."),
                           br(),
                           plotOutput("monthBarChart"),
                           br(),
                           p("To understand the drivers of any year-over-year changes,
                             we can look at a plot depicting the number of crimes over time,
                             broken out by the crime classification. You can choose the crime
                             types to display on the sidebar menu."),
                           br(),
                           plotOutput("linePlot")),
                  
                  tabPanel("Crimes by Area",
                           br(),
                           p("The graph below shows the number of crimes by police district.
                             You can choose the year on the sidebar menu."),
                           plotOutput("barGraph"),
                           br(),
                           textOutput("victimsZip")),
                  
                  tabPanel("Place of Crimes",
                           br(),
                           p("The table below shows the places with the greatest number of 
                             victims, sorted in descending order. You can choose the number of 
                             top places you would like to see using the input on the 
                             sidebar menu."),
                           DT::dataTableOutput("victimsByPlace"))
      )
    )
  )
)

# Define server logic 
server <- function(input, output, session) {
  
  # Filter crime type summary by user input 
  filtered_crimetype <- reactive({
    df_crime_type_summ %>% filter(Crime.Name1 %in% input$crime_type)
  })
  
  # Filter for year by user input
  filtered_byYear <- reactive({
    df %>% filter(year == input$yr)
  })
  
  # Number of victims by chosen zip code and year
  output$victimsZip <- renderText({
    validate(
      need(input$zip %in% df_zip$Zip.Code, "Please enter a valid zip code.")
    )
    zip_index <- which(df_zip$Zip.Code == input$zip & df_zip$year == input$yr)
    paste("The number of victims in the zip code", input$zip, "and year", input$yr, "is",
          df_zip[zip_index, 3])
  })
  
  # Update the default value to be the zip code with the max number of victims for the chosen year
  # Note that this default only changes for the year 2016 -
  # For 2017-2019, the same zip code has the most victims
  observe({
    filtered_zip <- df_zip %>% filter(year == input$yr)
    max_index <-  which.max(filtered_zip$totVictims)
    new_value <- filtered_zip$Zip.Code[max_index]
    
    updateTextInput(session, inputId = "zip", 
                    value = new_value)
  })
  
  # Axis sizes
  plot_theme = theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16))
  
  # Bar graph showing number of crimes per month with year-over-year comparison 
  output$monthBarChart <- renderPlot({
    ggplot(data = df_month, aes(x = month, fill = year)) +
      geom_bar(position = position_dodge()) +
      ylab("Number of Crimes") +
      ggtitle("Number of Crimes by Month, Year-over-Year") +
      plot_theme
      
  })
  
  # Line plot showing number of crimes per YYYYMM by crime type
  output$linePlot <- renderPlot({
    ggplot(data = filtered_crimetype(), aes(x = year_mo,
                                            y = n,
                                            group = Crime.Name1,
                                            color = Crime.Name1)) +
      geom_line(size = 1) +
      xlab("Year_Month (YYYYMM)") + ylab("Number of Crimes") +
      guides(color=guide_legend("Crime Type")) +
      ggtitle("Number of Crimes by Crime Type vs. Time") +
      plot_theme
  })
  
  # Bar graph showing number of crimes per police district
  output$barGraph <- renderPlot({
    ggplot(data = filtered_byYear(), aes(x = Police.District.Name),
           fill = Police.District.Name) +
      geom_bar() +
      xlab("Police District") + ylab("Number of Crimes") +
      ggtitle("Number of Crimes per Police District") +
      guides(fill = FALSE) +
      theme(axis.text.x = element_text(size = 14, angle = 90, hjust = 1),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            plot.title = element_text(size = 16))
  })
  
  # Data table showing the top N places with the most victims
  output$victimsByPlace <- DT::renderDataTable(
    DT::datatable(data = df_byPlace[1:input$topN, ], 
                  options = list(pageLength = 20))
  )
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$yr, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_byYear(), file, row.names = FALSE)
    }
  ) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

