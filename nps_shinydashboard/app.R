
#nps_shinydashboard
#author: Ljupcho Naumov
#site: Ljupcho.com

################ Libraries #################

library(shiny)
library(shinydashboard)
library(dplyr)
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

file <- system.file("europe.geo.json", package = "geojsonio")
europe <- geojsonio::geojson_read(file, what = "sp")
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
    dashboardSidebar(include=FALSE),
    dashboardBody(
        fluidRow(infoBoxOutput("nps_score")),
        fluidRow(),
        fluidRow()
    )
)




#############   Server    ################

server <- function(input, output) {
    
    output$nps_score <- renderInfoBox({
        infoBox(
            nps_score, "NPS Score",
            color = if (nps_score>=50){
                "green"
            } else if (nps_score<50 | nps_score > 0){
                "yellow"
            } else {
                "red"
            }
        )
    })
    
    
    
    
    
}

shinyApp(ui, server)