library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rgdal)
library(leaflet)
library(shinythemes)
library(shinydashboard)
library(reshape2)
library(plotly)
library(tibble)
require(leaflet.extras)
require(readxl)
require(stringr)
require(RColorBrewer)

ckanSQL <- function(url) {
  # Make the Request
  r <- RETRY("GET", URLencode(url))
  # Extract Content
  c <- content(r, "text")
  # Basic gsub to make NA's consistent with R
  json <- gsub('NaN', 'NA', c, perl = TRUE)
  # Create Dataframe
  data.frame(jsonlite::fromJSON(json)$result$records)
}

# Unique values for Resource Field
ckanUniques <- function(id, field) {
  url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20DISTINCT(%22", field, "%22)%20from%20%22", id, "%22")
  c(ckanSQL(URLencode(url)))
}

Ngh <- sort(ckanUniques("7f438bd0-71c7-4997-a5b8-f12894599215", "Neighborhood")$Neighborhood)
Total <- sort(ckanUniques("7f438bd0-71c7-4997-a5b8-f12894599215","'Estimate..Total....No.Social.Security.income'")$Estimate..Total....No.Social.Security.income)
Ssi<- sort(ckanUniques("7f438bd0-71c7-4997-a5b8-f12894599215","'Estimate..Total.'")$Estimate..Total.)           


#create ui
ui <- navbarPage("Pitssburgh Neighborhood Income",
                 theme = shinytheme("cosmo"),
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              # Different neighborhood select
                              selectInput("NghSelect",
                                          "Choose the Nehborhood:",
                                          choices = Ngh,
                                          selected=c("Allentown", "Arlington","Banksville","East Hills", "East Liberty", "Beechview", "Bedford Dwellings", "Beltzhoover", "Bluff")),
                              #Total Income without ssi Select
                              sliderInput("InSelect",
                                          "Estimate Income without SSI:",
                                          min = min(Total, na.rm = T),
                                          max = max(Total, na.rm = T),
                                          value = c("1400", "6000"),
                                          step = 1),
                              #total income select
                              sliderInput("SsiSelect",
                                          "Social Security Income:",
                                          min=min(Ssi,na.rm=T),
                                          max=max(Ssi,na.rm=T),
                                          value=c("200","6000"),
                                          step=1),
                              #reset button
                              actionButton("reset", "Reset Your Selection", icon = icon("refresh")
                                           )
                            ),
                            # Output plot
                            mainPanel(
                              plotlyOutput("plot1"),
                              plotlyOutput("plot2"),
                              plotlyOutput("plot3")
                            )
                          )
                          ),
                 #tab panel
                 tabPanel("Table",
                          inputPanel(
                            downloadButton("downloadData","Download the Social Security Income Data in Pittsburgh")),
                          fluidPage(DT::dataTableOutput("table")
                                    )
                          )
                 )
# Define server logic
server <- function(input, output, session = session) {
  Ngh.filter <- ifelse(length(input$NghSelect) > 0, paste0("%20AND%20%22Neighborhood%22%20IN%20(%27", paste0(gsub(" " ,"%20", input$NghSelect), collapse = "%27,%27"), "%27)"), "")
  IcInput <- reactive({
    # Build API Query with proper encodes
    url <- paste0("https://data.wprdc.org/api/3/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%227f438bd0-71c7-4997-a5b8-f12894599215da%22%20WHERE%20%22Estimate..Total....No.Social.Security.income%22%20%3E=%20%27", input$InSelect[1], "%27%20AND%20%22Estimate..Total....No.Social.Security.income%22%20%3C=%20%27", input$InSelect[2], "%27", Ngh.filter)
    print(url)
    # Load and clean data
    No.filter <- ckanSQL(url) %>%
      mutate(Estimate..Total....No.Social.Security.income = as.numeric(Estimate..Total....No.Social.Security.income))
    return(No.filter)
  })
  SsiInput <- reactive({
    # Build API Query with proper encodes
    url2 <- paste0("https://data.wprdc.org/api/action/datastore_search_sql?sql=SELECT%20*%20FROM%20%227f438bd0-71c7-4997-a5b8-f12894599215da%22%20WHERE%20%22Estimate..Total.%22%20%3E=%20%27", input$SsiSelect[1], "%27%20AND%20%22Estimate..Total.%22%20%3C=%20%27", input$SsiSelect[2], "%27", Ngh.filter)
    print(url2)
    # Load and clean data
    Ssi.filter <- ckanSQL(url2) %>%
      mutate(Estimate..Total. = as.numeric(Estimate..Total.))
    return(Ssi.filter)
  })
  # #income without ssi reactive
  # icInput <- reactive({
  #   no.filter <- income %>%
  #     # Slider income without social security Filter
  #     filter(Estimate..Total....No.Social.Security.income >= input$InSelect[1] & Estimate..Total....No.Social.Security.income <= input$InSelect[2])
  #   if (length(input$NghSelect) > 0 ) {
  #     no.filter <- subset(no.filter, Neighborhood %in% input$NghSelect)
  #   }
  #   return(no.filter)
  # })
  # #income with ssi reactive
  # ssiInput <- reactive({
  #   ssi.filter <- income %>%
  #     filter(Estimate..Total. >= input$SsiSelect[1] & Estimate..Total. <= input$SsiSelect[2])
  #   if (length(input$NghSelect) > 0 ) {
  #     ssi.filter <- subset(ssi.filter, Neighborhood%in% input$NghSelect)
  #   }
  #   return(ssi.filter)
  # })
  #plot1 , use plotly compare two income in each neighborhood
  output$plot1<-renderPlotly({
    dat1<- IcInput()
    dat2<-SsiInput()
    dat1$Neighborhood<-factor(dat1$Neighborhood)
    dat2$Neighborhood<-factor(dat2$Neighborhood)
    p1<-plot_ly(dat1,x=~Neighborhood,y=~Estimate..Total....No.Social.Security.income,name="With SSI",type="bar")
    p2<-plot_ly(dat2,x=~Neighborhood,y=~Estimate..Total.,name = "Without SSI",type="bar")
    subplot(p1,p2)
    })
  #create plot about the distribution of without SSI
  output$plot2 <- renderPlotly(({
    dat3<-IcInput()
    ggplotly(
      ggplot(data =dat3, aes(x=Estimate..Total.))+
        geom_density(fill="blue",alpha=.6)+
        xlab("Total Income") +
        ylab("Density") +
        ggtitle("Distribution of Total Income with social Security Income")
    )
  })
  )
  #create plot about the distribton of SSI
  output$plot3 <- renderPlotly(({
    dat4<-SsiInput()
    ggplotly(
      ggplot(data =dat4, aes(x=Estimate..Total....No.Social.Security.income))+
        geom_density(fill="pink",alpha=.6)+
        xlab("SSI Income") +
        ylab("Density") +
        ggtitle("Distribution of Total Income without Social Security Income")
    )
  })
  )
  #table oupt
  output$table <- DT::renderDataTable({
    no.filter <- icInput()
    subset(no.filter, select = c(Neighborhood, Estimate..Total.,Estimate..Total....No.Social.Security.income))
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("income in Pittsburgh", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(obInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session,"NghSelect",selected = c("Allentown", "Arlington","Banksville","East Hills", "East Liberty", "Beechview", "Bedford Dwellings", "Beltzhoover", "Bluff"))
    updateSliderInput(session, "InSelect", value = c("1400", "6000"))
    updateSliderInput(session, "SsiSelect", value = c("200", "6000"))
    showNotification("Hey! You did it! You have successfully reset the filters", type = "message")
  })
}
# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
