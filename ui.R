library(DT)
library(shinythemes)

## Load File
itrcDataFull <- read.csv("Copy of Component sheet 11-2-17_pol.csv", stringsAsFactors=FALSE,
                          sep=",", header=TRUE, check.names=FALSE)
itrcDataFull2 <- read.csv("Copy of Component sheet 11-2-17.csv", stringsAsFactors=FALSE,
                          sep=",", header=TRUE, check.names=FALSE)

## Subset the data to workable table size for now
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
               theme = shinytheme("paper"),
               tabPanel("Pollutant and Secondary Filtering",
                        includeCSS("styles.css"),
                        h1("**DRAFT/PROTOTYPE**", align="center"),
                        h3("ITRC Stormwater BMP Applicability / Evaulation Tool", align="center"),
                        h4("Note: The table is currently populated with dummy data.  This is for functionality and testing purposes only."),
                        column(5,
                               br(),
                               wellPanel(
                                   h5("Pollutant Screening"),
                                   selectizeInput(inputId="pollutants",
                                                  label="Select Pollutant(s): ",
                                                  choices=list(
                                                      Sediments=names(itrcData2nd[c(2:6)]),
                                                      Metals=names(itrcData2nd[c(7:21)]),
                                                      Nutrients=names(itrcData2nd[c(23,24,27:30)]),
                                                      Other=names(itrcData2nd[c(22,25,26,31:38)])),
                                                  multiple=TRUE),
                                   h6("Select the pollutants of concern from the drop down box, above.  The table will update to show only Stormwater Practices that have been determined to have meaningfull removal potential for the selected pollutant(s).")
                               ),
                               wellPanel(
                                   HTML('<button data-toggle="collapse" data-target="#demo2" class="button" style="horizontal-align:middle"><span><b>Pollutant Removal Determinations</b></span></button>'),
                                   tags$div(id = 'demo2',  class="collapse",
                                            h6("Determinations about whether a particular practice potentially removes a given pollutant were made by the ITRC Stormwater Work Team."),
                                            h6("In general determinations were made using empricial data where available, while also relying on knowledge of unit processes affecting pollutant removal potential in the absence of empirical evidence.  Important caveats, limitations, and potential site constraints are discussed in the Practice Information Sheets, and througout the Team Document.")


                                            )
                               ),
                               wellPanel(
                                   HTML('<button data-toggle="collapse" data-target="#secScr" class="button" style="horizontal-align:middle"><span><b>Secondary Screening Criteria</b></span></button>'),
                                   tags$div(id = 'secScr',  class="collapse",
                                            h6("Selecting any of the criteria below with further restrict the list of returned practices categories."),
                                            tags$div(title="Practices that will are not likely to experience substantially reduced pollutant removal performance during periods of freezing air temperatures.",
                                                     checkboxInput(inputId="frz",
                                                                   label="Performance Unaffected By Freezing Conditions")
                                                     ),
                                            tags$div(title="Practices that are suitable for use in arid climates and can meet expected performance levels without the need for irrigation.",
                                                     checkboxInput(inputId="ard",
                                                                   label="Performance Unaffected By Arid Condtions")
                                                     ),
                                            tags$div(title="Practices that can potentially be installed underground.",
                                                     checkboxInput(inputId="ung",
                                                                   label="Can Be Installed Underground")
                                                     ),
                                            tags$div(title="Practices for which design and operation are largely unaffected by the presence of contaminated soils on site.",
                                                     checkboxInput(inputId="cont",
                                                                   label="Design Unaffected By Contaminated Soils")
                                                     ),
                                            tags$div(title="",
                                                     checkboxInput(inputId="hgw",
                                                                   label="Performance Unaffected By High Groundwater")
                                                     ),
                                            tags$div(title="",
                                                     checkboxInput(inputId="htss",
                                                                   label="Performance Unaffected By High TSS Loads")
                                                     ),
                                            tags$div(title="Practices with the potential to meet or contribute to regulatory requiments for volume storage and / or peak rate reduction.",
                                                     checkboxInput(inputId="hct",
                                                                   label="Can Provide Storage / Peak Flow Control")
                                                     ),
                                            tags$div(title="Practices with the potential to meet or contribute to regulatory requiments for groundwater recharge and / or volume reduction.",
                                                     checkboxInput(inputId="inf",
                                                                   label="Can Contribute to Volume Reduction / Recharge")
                                                     ),
                                            tags$div(title="Potentially meets most of the criteria to be classified as Green Stormwater Infrastructure per EPA's definition.",
                                                     checkboxInput(inputId="gsi",
                                                                   label="Green Infrastructure")
                                                     ),
                                            tags$div(title="Suitable for use near airports (i.e., lack of open air standing water).",
                                                     checkboxInput(inputId="air",
                                                                   label="Safe For Airport Use")
                                                     )
                                            )
                               )
                               ),
                        column(7,
                               DT::dataTableOutput("results")
                               )),
               tabPanel("Pollutant Filtering",
                        includeCSS("styles.css"),
                        h1("**DRAFT/PROTOTYPE**", align="center"),
                        h3("ITRC Stormwater BMP Applicability / Evaulation Tool", align="center"),
                        h4("Note: The table is currently populated with dummy data.  This is for functionality and testing purposes only."),
                        column(5,
                               br(),
                               wellPanel(
                                   h5("Pollutant Screening"),
                                   selectizeInput(inputId="pollutants0",
                                                  label="Select Pollutant(s): ",
                                                  choices=list(
                                                      Sediments=names(itrcData[c(2:6)]),
                                                      Metals=names(itrcData[c(7:21)]),
                                                      Nutrients=names(itrcData[c(23,24,27:30)]),
                                                      Other=names(itrcData[c(22,25,26,31:38)])),
                                                  multiple=TRUE),
                                   h6("Select the pollutants of concern from the drop down box, above.  The table will update to show only Stormwater Practices that have been determined to have meaningfull removal potential for the selected pollutant(s).")
                               ),
                               wellPanel(
                                   HTML('<button data-toggle="collapse" data-target="#demo2" class="button" style="horizontal-align:middle"><span><b>Pollutant Removal Determinations</b></span></button>'),
                                   tags$div(id = 'demo2',  class="collapse",
                                            h6("Determinations about whether a particular practice potentially removes a given pollutant were made by the ITRC Stormwater Work Team."),
                                            h6("In general determinations were made using empricial data where available, while also relying on knowledge of unit processes affecting pollutant removal potential in the absence of empirical evidence.  Important caveats, limitations, and potential site constraints are discussed in the Practice Information Sheets, and througout the Team Document.")
                                            )
                               )
                               ),
                        column(7,
                               DT::dataTableOutput("results0")
                               )
                        )
               )
)
