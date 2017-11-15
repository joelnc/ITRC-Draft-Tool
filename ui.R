library(shiny)

## Load, format, subset, WQD File
## itrcDataFull <- read.csv("toolExData2.csv", stringsAsFactors=FALSE,
##                      sep=",", header=TRUE, check.names=FALSE)

itrcDataFull <- read.csv("Component sheet 6-12-17 doug H.csv", stringsAsFactors=FALSE,
                     sep=",", header=TRUE, check.names=FALSE)
itrcDataFull2 <- read.csv("Copy of Component sheet 11-2-17.csv", stringsAsFactors=FALSE,
                     sep=",", header=TRUE, check.names=FALSE)

## Subset the data to workable table size for now
##itrcData <- itrcDataFull[,c(1,2,5,8,11,12,13,14,15,16)]
itrcData <- itrcDataFull
itrcData2nd <- itrcDataFull2

## Hyperlink practice names placeholder info sheets
itrcData$Practice <- paste0("<a href=", itrcData$doc, " target='blank' >",
                         itrcData$Practice,"</a>")
itrcData$notesDesc <- "None"
itrcData2nd$Practice <- paste0("<a href=", itrcData2nd$doc, " target='blank' >",
                         itrcData2nd$Practice,"</a>")
itrcData2nd$notesDesc <- "None"

## Drop doc name field, write to global
itrcData <<- subset(itrcData, select=-c(doc))
itrcData2nd <<- subset(itrcData2nd, select=-c(doc))


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="ITRC BMP Tool",
               tabPanel("Pollutant Filtering"),
               includeCSS("styles.css"),
               h2("**DRAFT/PROTOTYPE**", align="center"),
               h2("ITRC Stormwater BMP Applicability / Evaulation Tool"),
               h6("Note: The table is currently populated with dummy data.  This is for functionality and testing purposes only.", color="red"),
               column(4,
                      br(),
                      wellPanel(
                          selectizeInput(inputId="pollutants",
                                         label="Select Pollutant(s): ",
                                         choices=list(
                                             Sediments=names(itrcData2nd[c(2:6)]),
                                             Metals=names(itrcData2nd[c(7:21)]),
                                             Nutrients=names(itrcData2nd[c(23,24,27:30)]),
                                             Other=names(itrcData2nd[c(22,25,26,31:38)])),
                                         multiple=TRUE),
                          h5("Select the pollutants of concern from the drop down box, above.  The table will update to show only Stormwater Practices that have been determined to have meaningfull removal potential for the selected pollutant(s).")
                      ),
                      wellPanel(
                          HTML('<button data-toggle="collapse" data-target="#secScr" class="button" style="horizontal-align:middle"><span><b>Secondary Screening</b></span></button>'),
                          tags$div(id = 'secScr',  class="collapse",
                                   h5("In case the returned list is still a bit too long..."),
                                   checkboxInput(inputId="frz",
                                                 label="Unaffected By Freezing"),
                                   checkboxInput(inputId="ard",
                                                 label="Compatible With Arid Condtions"),
                                   checkboxInput(inputId="ung",
                                                 label="Can Be Installed Underground"),
                                   checkboxInput(inputId="cont",
                                                 label="Design Unaffected By Contaminated Soils"),
                                   checkboxInput(inputId="hwt",
                                                 label="High Water Table"),
                                   checkboxInput(inputId="htss",
                                                 label="High TSS Loads"),
                                   checkboxInput(inputId="hct",
                                                 label="Hydrologic Control"),
                                   checkboxInput(inputId="inf",
                                                 label="Water In The Ground")
                                   )
                      ),
                      wellPanel(
                          HTML('<button data-toggle="collapse" data-target="#demo2" class="button" style="horizontal-align:middle"><span>Pollutant Removal Determinations</span></button>'),
                          tags$div(id = 'demo2',  class="collapse",
                                   h5("Determinations about whether a particular practice potentially removes a given pollutant were made by the ITRC Stormwater Work Team."),
                                   h5("In general determinations were made using empricial data where available, while also relying on knowledge of unit processes affecting pollutant removal potential in the absence of empirical evidence.  Important caveats, limitations, and potential site constraints are discussed in the Practice Information Sheets, and througout the Team Document.")


                                   )
                      )
                      ),
               column(8,
                      DT::dataTableOutput("results")
                      )
               )
)
