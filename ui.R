library(shiny)

## Load, format, subset, WQD File
itrcDataFull <- read.csv("toolExData.csv", stringsAsFactors=FALSE,
                     sep=",", header=TRUE, check.names=FALSE)

## Subset the data to workable table size for now
itrcData <- itrcDataFull[,c(1,2,5,8,11,12,13,14,15,16)]

## Hyperlink practice names placeholder info sheets
itrcData$Practice <- paste0("<a href=", itrcData$doc, " target='blank' >",
                         itrcData$Practice,"</a>")

##itrcData$IDB <- '<a href="#", onclick="alert(\'Hello World\');">Helloo</a>'
##itrcData$IDB <- '<a href="#", onclick="alert(', itrcData$doc,' );">Helloo</a>')
itrcData$IDB <- paste0("<a href=", "test2.html", " target='blank' >",
                         itrcData$IDB,"</a>")

## Drop doc name field, write to global
itrcData <<- subset(itrcData, select=-c(doc))


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="ITRC BMP Tool",
               selected="Data",
               br(),
               h2("**DRAFT/PROTOTYPE**", align="center"),
               h2("ITRC Stormwater BMP Applicability / Evaulation Tool"),
               fluidRow(
                   h3("Select Criteria", align="center"),
                   column(6,
                          wellPanel(style="background-color: #D2DDE4",
                                    selectInput(
                                    inputId="pollutants",
                                    label="Select Pollutant(s): ",
                                    choices=names(itrcData[2:4]),
                                    multiple=TRUE,
                                    selectize=TRUE)
                                    )
                          ),
                   column(6,
                          wellPanel(style="background-color: #D2DDE4;",
                                    includeCSS("styles.css"),
                                    h4("Does data need to meet these conditions? (Select...)"),
                                    h5("(*If none selected, all practices will be shown)"),
                                    fluidRow(
                                        column(6,
                                           checkboxInput(inputId="TPL",
                                                             label="3rd Party Lab?",
                                                             value=FALSE)
                                           ),
                                        column(6,
                                               checkboxInput(inputId="refJ",
                                                             label="Ref. Journ?",
                                                             value=FALSE)
                                               )
                                    ),
                                    fluidRow(
                                        column(6,
                                               checkboxInput(inputId="TPF",
                                                         label="3rd Party Field",
                                                         value=FALSE)
                                               ),
                                        column(6,
                                               checkboxInput(inputId="IDB",
                                                             label="Int. DB? ",
                                                             value=FALSE)
                                               )
                                    ),
                                    fluidRow(
                                        column(6,
                                               checkboxInput(inputId="Vend",
                                                             label="Vendor Data?",
                                                             value=FALSE)
                                               )
                                    )
                                    )
                          )
               ),
               hr(),
               DT::dataTableOutput("results2")
               )
)
