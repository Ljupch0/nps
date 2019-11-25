
#nps_shinydashboard
#author: Ljupcho Naumov
#site: Ljupcho.com

################ Libraries #################

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(NPS)
library(leaflet)
library(geojsonio)

######### Data Cleaning & Prep ############

sheets_deauth()
nps_data <- read_sheet("https://docs.google.com/spreadsheets/d/1OG2i1gIv0J9bZMxRr8O2SHYxQsu27LjRbBUBnaZ7zvM")

#variable name cleaning
names(nps_data) <- tolower(names(nps_data))
nps_data <- rename(nps_data,nps=`how likely are you to recommend trivago to a friend or colleague?`,country=`country of respondent`) 

#NPS Calculations
nps_score <- round(nps(nps_data$nps)*100, digits=2)
nps_data$nps_category <- npc(nps_data$nps)

#New age group variable
nps_data$age_groups <- cut(nps_data$age, c(17,24,34,49,69,99))

#mapping data

europe <- geojsonio::geojson_read("D:/R/nps/europe.geo.json", what = "sp")
pal <- colorNumeric("Greens", domain = europe@data$n, na.color="white")
country_count <- nps_data %>% group_by(country) %>% count()
europe@data <- left_join(europe@data, country_count, by=c("sovereignt"="country"))

labels <- sprintf(
    "<strong>%s</strong><br/>%g respondents",
    europe@data$sovereignt, europe@data$n
) %>% lapply(htmltools::HTML)

#################  UI  ####################

ui <- dashboardPage(
    dashboardHeader(title="Net Promoter Scores"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 120px}"))),
            column(3, shinydashboard::valueBoxOutput("nps_score", width=NULL)),
            column(9, plotOutput("distribution", height = 120))),
        fluidRow(
            column(4, plotOutput("gender_pie")),
            column(4, plotOutput("age_distribution")),
            column(4, leafletOutput("map"))
        )
            

    )
)




#############   Server    ################

server <- function(input, output) {
    
    output$nps_score <- renderValueBox({
        shinydashboard::valueBox(
            nps_score,
            "NPS Score",
            color = "green",
            width=NULL 
        )
    })
    
    output$distribution <- renderPlot({
        ggplot(nps_data)+
            geom_bar(
                mapping = aes(x="", fill = factor(nps_category, levels = c("Promoter", "Passive","Detractor"))),
                position = "fill",
                width = 0.45)+
            scale_fill_manual(values = c("springgreen3","gold", "firebrick"))+
            coord_flip()+
            labs(fill="")+
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
            setView(15.089348, 48.343510, zoom=4) %>% 
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
    
    
}

shinyApp(ui, server)