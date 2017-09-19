library(shiny)

## Load, format, subset, WQD File
## itrcDataFull <- read.csv("toolExData2.csv", stringsAsFactors=FALSE,
##                      sep=",", header=TRUE, check.names=FALSE)

itrcDataFull <- read.csv("Component Sheet 6-12-17 doug H.csv", stringsAsFactors=FALSE,
                     sep=",", header=TRUE, check.names=FALSE)

## Subset the data to workable table size for now
##itrcData <- itrcDataFull[,c(1,2,5,8,11,12,13,14,15,16)]
itrcData <- itrcDataFull

## Hyperlink practice names placeholder info sheets
itrcData$Practice <- paste0("<a href=", itrcData$doc, " target='blank' >",
                         itrcData$Practice,"</a>")
itrcData$notesDesc <- "None"
## Drop doc name field, write to global
itrcData <<- subset(itrcData, select=-c(doc))


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="ITRC BMP Tool",
               tabPanel("Pollutant Filtering"),
               includeCSS("styles.css"),
               h2("**DRAFT/PROTOTYPE**", align="center"),
               h2("ITRC Stormwater BMP Applicability / Evaulation Tool"),
               h3("Here is the tool..."),
               sidebarLayout(
                   sidebarPanel(
                       h5("Select the pollutants of concern from the drop down box, below.  The table will update to show only Stormwater Practices that have been determined to have meaningfull removal potential for the selected pollutant(s)."),
                       br(),

                       selectInput(inputId="pollutants",
                                   label="Select Pollutant(s): ",
                                   choices=names(itrcData[2:37]),
                                   multiple=TRUE,
                                   selectize=TRUE),
                       h5("How were pollutant removal potentials established?")
                   ),
                   mainPanel(
                       DT::dataTableOutput("results")
                   )
               )
               )
)

