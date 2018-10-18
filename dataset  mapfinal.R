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

Ngbh.load <-readOGR('https://opendata.arcgis.com/datasets/dbd133a206cc4a3aa915cb28baa60fd4_0.geojson')
Ball.load <-readOGR('https://opendata.arcgis.com/datasets/87f3b2bdfa6646319a56e86521480efd_0.geojson')
income<- read.csv('C:/Users/csvan/Documents/GitHub/SC_Project2/ScGH_project2/ssi.csv',head=TRUE)

#create ui
ui <- navbarPage("Higher income, more Ballfileds?",
                 theme = shinytheme("cosmo"),
                 # tabPanel("Title",
                 #          mainPanel(
                 #            imageOutput("ballimg")
                 #          )),
                 #create map panel
                 tabPanel("Ball fileds in Allegheny",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("NgbhSelect",
                                          "Neighborhood",
                                          levels(Ngbh.load$hood),
                                          selected = c("Allegheny Center","Banksville"),
                                          selectize = T,
                                          multiple = T)
                            ),
                            mainPanel(
                              # Using Shiny JS
                              shinyjs::useShinyjs(),
                              # Style the background and change the page
                              tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                         body {background-color: #e2dede;}"),
                              # Map Output
                              leafletOutput("leaflet")
                              )
                          )),
                 tabPanel("Income Distribution",
                          sidebarLayout(
                            sidebarPanel(
                              # Different neighborhood select
                              selectInput("NghSelect",
                                          "Choose the Nehborhood:",
                                          choices = sort(unique(income$Neighborhood)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected=c("Allentown", "Arlington","Banksville","East Hills", "East Liberty", "Beechview", "Bedford Dwellings", "Beltzhoover", "Bluff")),
                              #Total Income without ssi Select
                              sliderInput("InSelect",
                                          "Estimate Income without SSI:",
                                          min = min(income$Estimate..Total....No.Social.Security.income, na.rm = T),
                                          max = max(income$Estimate..Total....No.Social.Security.income, na.rm = T),
                                          value = c("1400", "6000"),
                                          step = 1),
                              #total income select
                              sliderInput("SsiSelect",
                                          "Social Security Income:",
                                          min=min(income$Estimate..Total.,na.rm=T),
                                          max=max(income$Estimate..Total.,na.rm=T),
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
  pal <- colorFactor(c("navy", "red"), domain = Ball.load$active)
  output$leaflet <- renderLeaflet({
    # Load green infrastructure filtered data
    #greenInf <- greenInfInputs()
    Ngbh<-NgbhInputs()
    leaflet() %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group =" Street",options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles("Stamen.Toner", group="Toner",options = providerTileOptions(noWrap = TRUE)) %>%
      setView(-80,40.45,zoom=12) %>%
      #add polygon layer
      addPolygons(data = Ngbh) %>%
      addCircleMarkers(data=Ball.load,radius=6,color=~pal(Ball.load$active),stroke=FALSE,fillOpacity = 0.5)%>%
      #add layers control
      addLayersControl(
        baseGroups = c("Toner", "Street"),
        options = layersControlOptions(collapsed = FALSE))
  })
  NgbhInputs <- reactive({
    Ngbh <- Ngbh.load 
    if (length(input$NgbhSelect) > 0) {
      Ngbh<- subset(Ngbh, hood %in% input$NgbhSelect)
    }
    return(Ngbh)
  })
  #income without ssi reactive
  icInput <- reactive({
    no.filter <- income %>%
      # Slider income without social security Filter
      filter(Estimate..Total....No.Social.Security.income >= input$InSelect[1] & Estimate..Total....No.Social.Security.income <= input$InSelect[2])
    if (length(input$NghSelect) > 0 ) {
      no.filter <- subset(no.filter, Neighborhood %in% input$NghSelect)
    }
    return(no.filter)
  })
  #income with ssi reactive
  ssiInput <- reactive({
    ssi.filter <- income %>%
      filter(Estimate..Total. >= input$SsiSelect[1] & Estimate..Total. <= input$SsiSelect[2])
    if (length(input$NghSelect) > 0 ) {
      ssi.filter <- subset(ssi.filter, Neighborhood%in% input$NghSelect)
    }
    return(ssi.filter)
  })
  # output$ballimg <-renderImage()
  #plot1 , use plotly compare two income in each neighborhood
  output$plot1<-renderPlotly({
    dat1<- icInput()
    dat2<-ssiInput()
    dat1$Neighborhood<-factor(dat1$Neighborhood)
    dat2$Neighborhood<-factor(dat2$Neighborhood)
    p1<-plot_ly(dat1,x=~Neighborhood,y=~Estimate..Total....No.Social.Security.income,name="With SSI",type="bar")
    p2<-plot_ly(dat2,x=~Neighborhood,y=~Estimate..Total.,name = "Without SSI",type="bar")
    subplot(p1,p2)
    })
  #create plot about the distribution of without SSI
  output$plot2 <- renderPlotly(({
    dat3<-icInput()
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
    dat4<-ssiInput()
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
