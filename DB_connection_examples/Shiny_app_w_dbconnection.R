#Loading the needed packages  ----
library(ggplot2)
library(dplyr)
library(magrittr)
library(sf)
library(mapdata)
library(scales)
library(RODBC)
library(viridis)
library(scales)

#Setting up database connections  ----
Sys.setenv(ORACLE_HOME="/ora1/app/oracle/product/11.2.0/dbhome_1")
sole <- RODBC::odbcConnect(dsn="sole", uid="ajones", pwd="PASSWORD", believeNRows=FALSE)
sqlQuery(sole,"SET ROLE ALL")

#Pulling map polygons  ----
usa <- ggplot2::map_data('state') %>% 
  filter(region %in% c("maine", "vermont", "new hampshire", 
                       "massachusetts", "connecticut", "rhode island",
                       "new york", "pennsylvania", "new jersey", 
                       "delaware", "district of columbia", "maryland",
                       "west virginia", "virginia", "north carolina"))

canada <- map_data("worldHires", "Canada")

# Define the UI  ----
ui <- fluidPage(
  
  # App title ----
  br(),
  fluidRow(
    column(1,ofset=5,tags$img(src = "CR_image_Final_transp.png",height="100%", width="100%",align='Right')),
    column(6,
           titlePanel("Historical GTE Data Explorer"),
           mainPanel(h4("Description:"),
                     h5("Data collected by CRB partners. Bottom temperatures are clipped to show only the time when the gear is on the bottom. Most of the participants have been mobile gear vessels. This is just an example of a way that data could be shared!"))
    )),
  # Action button to refresh data ----
  
  # Layout with input and output definitions ----
  br(),
  fluidRow(
    #column(0.25,''),
    column(2,''),
    column(1,actionButton('go','Go get the data!',align='left'))),
  br(),
  fluidRow(
    #column(,''),
    column(3,
           # Input: Slider for the number of bins ----
           sliderInput("year",
                       label = "Years to display",
                       min = 2010,
                       max = 2019,
                       value = c(2011,2011),
                       sep = '',
                       dragRange=TRUE)),
    column(3,
           sliderInput("month",
                       label = "Months to display",
                       min = 1,
                       max = 12,
                       value = c(1,1),
                       sep = '',
                       pre='M ',
                       dragRange=TRUE))
  ),
  br(),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    fluidRow(
      
      column(width=8, offset=0.75,
             
      tabsetPanel(type = "tabs",
                tabPanel("No. temp. records", plotOutput(outputId = "mapPlot1",height = 700)),
                tabPanel("Mean temp.", plotOutput(outputId = "mapPlot2",height = 700)),
                tabPanel("Temp stand.dev.", plotOutput(outputId = "mapPlot3",height = 700)),
                tabPanel("No. vessels reporting", plotOutput(outputId = "mapPlot4",height = 700))#,
                #tabPanel("Vessel distributions", plotOutput(outputId = "mapPlot5",height = 700))
        )
      )
    )
  )
)

#Setting up the server side  ----
server <- function(input, output) {
  GTE_JOIN_Q <- reactiveValues(Query=NULL)
  GTE_JOIN_SUB <- reactiveValues(Q_DF=NULL)
  
  observeEvent(input$go, {
    #Making thte SQL query text ----
    GTE_JOIN_Q$Query <- paste("select * from NERS.GTE_JOIN where EXTRACT(YEAR FROM GPS_DATETIME) <= (",max(input$year),")",
                              " AND EXTRACT(YEAR FROM GPS_DATETIME) >= (",min(input$year),")",
                              " AND EXTRACT(MONTH FROM GPS_DATETIME) >= (",min(input$month),")",
                              " AND EXTRACT(MONTH FROM GPS_DATETIME) <= (",max(input$month),")",
                              sep="")
    
    #Wrapping the SQL query in a progress bar ----
    withProgress(message='Querying database',{
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.25)
      }
      GTE_JOIN_SUB$Q_DF <- sqlQuery(sole,GTE_JOIN_Q$Query)
    })
  })
  
  output$mapPlot1 <- renderPlot({
    
    #Wrapping the plotting in a progress bar ----
    withProgress(message='Plotting Results',{
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.25)
      }
      
      #Making plot limits ----
      lons = c(-76, -66)
      lats = c(35, 44)
      
      #Formatting the plot ----
      validate(
        need(GTE_JOIN_SUB$Q_DF, "\n \n \t Please select a time period")
      )
      ggplot(data=GTE_JOIN_SUB$Q_DF) +
        stat_bin_2d(aes(y=LATITUDE,
                        x=LONGITUDE,fill = stat(count)),binwidth = c(0.1666666,0.1666666),alpha=0.5) +
        geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
        geom_polygon(data = canada, aes(x=long, y = lat, group = group)) +
        coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
        scale_fill_viridis_c(trans = log10_trans(),breaks=c(1,10,100,1000,10000,100000,1000000)) +
        labs(x='Longitude',y='Latitude',fill='Count',title=' ',subtitle = ' ')
    })
  })
  
  output$mapPlot2 <- renderPlot({
    
    #Wrapping the plotting in a progress bar ----
    withProgress(message='Plotting Results',{
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.25)
      }
      
      #Making plot limits ----
      lons = c(-76, -66)
      lats = c(35, 44)
      
      #Formatting the plot ----
      validate(
        need(GTE_JOIN_SUB$Q_DF, "Please select a time period")
      )
      ggplot(data=GTE_JOIN_SUB$Q_DF) +
        stat_summary_2d(aes(y=LATITUDE,
                        x=LONGITUDE,z=TEMP),fun='mean',binwidth = c(0.1666666,0.1666666),alpha=0.5) +
        geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
        geom_polygon(data = canada, aes(x=long, y = lat, group = group)) +
        coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
        scale_fill_viridis_c(option = 'plasma') +
        labs(x='Longitude',y='Latitude',fill='Temp (C)',title=' ',subtitle = ' ')
    })
  })  
  
  output$mapPlot3 <- renderPlot({
    
    #Wrapping the plotting in a progress bar ----
    withProgress(message='Plotting Results',{
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.25)
      }
      
      #Making plot limits ----
      lons = c(-76, -66)
      lats = c(35, 44)
      
      #Formatting the plot ----
      validate(
        need(GTE_JOIN_SUB$Q_DF, "Please select a time period")
      )
      ggplot(data=GTE_JOIN_SUB$Q_DF) +
        stat_summary_2d(aes(y=LATITUDE,
                            x=LONGITUDE,z=TEMP),fun='sd',binwidth = c(0.1666666,0.1666666),alpha=0.5) +
        geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
        geom_polygon(data = canada, aes(x=long, y = lat, group = group)) +
        coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
        scale_fill_viridis_c(option = 'magma') +
        labs(x='Longitude',y='Latitude',fill='Temp (C)',title=' ',subtitle = ' ')
    })
  }) 
  
  output$mapPlot4 <- renderPlot({
    
    #Wrapping the plotting in a progress bar ----
    withProgress(message='Plotting Results',{
      for (i in 1:10) {
        incProgress(1/10)
        Sys.sleep(0.25)
      }
      
      #Making plot limits ----
      lons = c(-76, -66)
      lats = c(35, 44)
      
      #Formatting the plot ----
      validate(
        need(GTE_JOIN_SUB$Q_DF, "Please select a time period")
      )
      ggplot(data=GTE_JOIN_SUB$Q_DF) +
        stat_summary_2d(aes(y=LATITUDE,
                            x=LONGITUDE,z=VP_NUM), fun= function(x) length(unique(x)), binwidth = c(0.1666666,0.1666666),alpha=0.5) +
        geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
        geom_polygon(data = canada, aes(x=long, y = lat, group = group)) +
        coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
        scale_fill_viridis_c(option = '') +
        labs(x='Longitude',y='Latitude',fill="No. Ves.",title=' ',subtitle = ' ')
    })
  })
  
  # output$mapPlot5 <- renderPlot({
  #   
  #   #Wrapping the plotting in a progress bar ----
  #   withProgress(message='Plotting Results',{
  #     for (i in 1:10) {
  #       incProgress(1/10)
  #       Sys.sleep(0.25)
  #     }
  #     
  #     #Making plot limits ----
  #     lons = c(-76, -66)
  #     lats = c(35, 44)
  #     
  #     #Formatting the plot ----
  #     validate(
  #       need(GTE_JOIN_SUB$Q_DF, "Please select a time period")
  #     )
  #     ggplot(data=GTE_JOIN_SUB$Q_DF %>% mutate(YEAR=as.numeric(substr(GPS_DATETIME,1,4)))) +
  #       stat_density_2d(aes(x=LONGITUDE,y=LATITUDE,fill = ..level.. ),geom = "polygon" ) + 
  #       geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  #       geom_polygon(data = canada, aes(x=long, y = lat, group = group)) +
  #       coord_sf(xlim = lons, ylim = lats,crs="+proj=longlat +datum=WGS84") +
  #       facet_wrap(~YEAR) +
  #       scale_fill_viridis_c(option = '')
  #   })
  # })
  
}

shinyApp(ui, server)