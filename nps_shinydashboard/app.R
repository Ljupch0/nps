
#nps_shinydashboard
#author: Ljupcho Naumov
#site: Ljupcho.com

################ Libraries #################

library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(NPS)
library(leaflet)
library(geojsonio)
library(scales)
library(DT)

######### Data Cleaning & Prep ############

sheets_deauth()
nps_data <- read_sheet("https://docs.google.com/spreadsheets/d/1OG2i1gIv0J9bZMxRr8O2SHYxQsu27LjRbBUBnaZ7zvM")

#variable name cleaning
names(nps_data) <- tolower(names(nps_data))
nps_data <- rename(nps_data,nps=`how likely are you to recommend this product to a friend or colleague?`,country=`country of respondent`) 

#NPS Calculations
nps_score <- round(nps(nps_data$nps)*100, digits=2)
nps_data$nps_category <- npc(nps_data$nps)

#Variables as factors, for better filtering in Data Tables
nps_data$country <- as.factor(nps_data$country)
nps_data$gender <- as.factor(nps_data$gender)


#New age group variable
nps_data$age_groups <- cut(nps_data$age, c(17,24,34,49,69,99))

#mapping data
europe <- geojsonio::geojson_read("../europe.geo.json", what = "sp")
pal <- colorNumeric("Greens", domain = europe@data$n, na.color="white")
country_count <- nps_data %>% group_by(country) %>% count()
europe@data <- left_join(europe@data, country_count, by=c("sovereignt"="country"))

labels <- sprintf(
    "<strong>%s</strong><br/>%g responses",
    europe@data$sovereignt, europe@data$n
) %>% lapply(htmltools::HTML)

#Number of weeks for which there is data, used in generating timeseries histogram
weeks=as.integer((max(nps_data$date)-min(nps_data$date))/7)

#################  UI  ####################

ui <- dashboardPage(skin="purple",
        dashboardHeader(title="Net Promoter Score"),
        dashboardSidebar(disable = TRUE),
        dashboardBody(
            fluidRow(
                tags$head(tags$style(HTML(".small-box {height: 138px}"))),
                column(2, shinydashboard::valueBoxOutput("nps_score", width=NULL)),
                column(2, shinydashboard::valueBoxOutput("number_responces", width=NULL)),
                column(8, box(title = "Overall Distribution",
                              width=NULL, 
                              plotOutput("distribution",height = 75)))
            ),
            fluidRow(
                column(5, box(title = "NPS over Time",
                              footer = "Each bar represents approximately 1 week of responses.",
                              width=NULL,
                              plotOutput("timeseries", height=320))),
                column(7, box(title = "Response Distribution by Rating",
                              width=NULL,
                              plotOutput("response_distribution", height=360)))
            ),
            fluidRow(
                column(4, box(title = "Demographics: Country",
                              width=NULL,
                              leafletOutput("map"))),
                column(4, box(title = "Demographics: Age",
                              width=NULL,
                              plotOutput("age_distribution"))),
                column(4, box(title = "Demographics: Gender",
                              width=NULL,
                              plotOutput("gender_pie")))
            ),
            fluidRow(
                column(12, box(title = "Raw Data",
                              width=NULL,
                              dataTableOutput("data"),
                              style = "height:560px; overflow-y: scroll;"))
            )
        )
)




#############   Server    ################

server <- function(input, output) {
    
    output$nps_score <- renderValueBox({
        shinydashboard::valueBox(
            nps_score,
            "Overall NPS Score",
            color = ifelse(nps_score>0, "green", "red"),
            width=NULL,
            icon=icon("bullhorn")
        )
    })
    
    output$number_responces <- renderValueBox(
        shinydashboard::valueBox(
            nrow(nps_data),
            "Number of Survey Responses",
            color="blue",
            width=NULL,
            icon=icon("users")
        )
    )
    
    output$distribution <- renderPlot({
        ggplot(nps_data)+
            geom_bar(
                mapping = aes(x="", fill = factor(nps_category, levels = c("Promoter", "Passive","Detractor"))),
                position = "fill",
                width = 0.65)+
            scale_fill_manual(values = c("springgreen3","gold", "firebrick"))+
            labs(fill="")+
            coord_flip()+
            theme_minimal()+
            theme(
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                panel.grid.major.y = element_blank(),
                axis.text.x = element_text(color = "grey20", size = 10),
                text = element_text(size=13)
            )
    })
    
    output$timeseries <- renderPlot({
        ggplot(nps_data)+
            geom_histogram(
                mapping = aes(x=date, fill = factor(nps_category,levels = c("Promoter", "Passive","Detractor"))),
                position = "fill",
                bins=weeks,
                color="white",
                size=1.2)+
            scale_fill_manual(values = c("springgreen3","gold", "firebrick"))+
            guides(fill=FALSE)+
            theme_minimal()+
            theme(
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.x = element_text(color = "grey20", size = 10),
                text = element_text(size=13)
            )  
    })
    
    output$data <- renderDataTable({
        datatable(nps_data,
                  extensions = 'Buttons',
                  options=list(paging = FALSE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               info = FALSE),
                  #colnames = c(),
                  filter = list(position = 'top'), 
                  fillContainer = FALSE,
                  rownames = FALSE
                  )
    })
    
    
    output$gender_pie <- renderPlot({
        ggplot(nps_data)+
            geom_bar(aes(x="", y=gender, fill=gender), stat="identity")+
            coord_polar("y", start=0)+
            scale_fill_manual(values = c("salmon", "lightskyblue", "mediumpurple"))+
            labs(fill="Gender")+
            theme_minimal()+
            theme(
                axis.text.x=element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.major.y = element_blank(),
                axis.title.x=element_blank(),
                axis.title.y=element_blank(),
                text = element_text(size=15)
            )
    })
    
    
    output$age_distribution <- renderPlot({
        ggplot(nps_data, aes(x=age_groups))+
            geom_bar(aes(fill=age_groups))+
            geom_text(stat='count', aes(label=..count..), vjust= -0.2)+
            scale_fill_brewer(palette = "Blues")+
            guides(fill=FALSE)+
            xlab("Age Groups")+
            ylab("")+
            theme_minimal()+
            theme(
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                axis.line.y=element_blank(),
                axis.text.y=element_blank(),
                text = element_text(size=15)
            )
    })
    
    output$map <- renderLeaflet({
        leaflet(data=europe) %>% 
            addTiles %>% 
            setView(5.995606, 47.003610, zoom=4) %>% 
            addProviderTiles(provider="Esri.WorldGrayCanvas") %>% 
            addPolygons(
                fillColor = ~pal(europe@data$n),
                weight = 2,
                opacity = 1,
                color = "grey",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                    weight = 4,
                    color = "DimGray",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels)
    })
    
    output$response_distribution <- renderPlot({
        ggplot(data=nps_data)+
            geom_bar(mapping=aes(x=nps, fill=nps_category))+
            scale_fill_manual(values= c("firebrick","gold","springgreen3"))+
            guides(fill=FALSE)+
            geom_text(stat='count', aes(x=nps,label=..count..), vjust= -0.3)+
            scale_x_continuous(breaks = seq(min(nps_data$nps), max(nps_data$nps), by = 1))+
            labs(y=NULL, x=NULL)+
            theme_minimal()+
            theme(
                panel.grid.major.y=element_blank(),
                panel.grid.minor.y=element_blank(),
                panel.grid.minor.x=element_blank(),
                panel.grid.major.x=element_blank(),
                axis.text.y = element_blank(),
                axis.text.x = element_text(size=11)
            )
    })

    
}

shinyApp(ui, server)