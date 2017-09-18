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

## Add Modal Text
for (i in 1:nrow(itrcData)) {
    ifelse(itrcData$IDB[i]==TRUE,
           itrcData$isIn[i] <- paste("<font color='green'><b> IS </b></font>"),
           itrcData$isIn[i] <- paste("<font color='red'><b> IS NOT </b></font>"))

    ifelse(itrcData$Sediment[i]=="Yes",
           itrcData$stat1[i] <- paste("<font color='green'><b> DOES </b></font>"),
           itrcData$stat1[i] <- paste("<font color='red'><b> DOES NOT </b></font>"))
    ifelse(itrcData$Sediment[i]=="Yes",
           itrcData$meaning1[i] <- paste("<font color='green'><b> DOES </b></font>"),
           itrcData$meaning1[i] <- paste("<font color='red'><b> DOES NOT </b></font>"))

    ifelse(itrcData$P[i]=="Yes",
           itrcData$stat2[i] <- paste("<font color='green'><b> DOES </b></font>"),
           itrcData$stat2[i] <- paste("<font color='red'><b> DOES NOT </b></font>"))
    ifelse(itrcData$P[i]=="Yes",
           itrcData$meaning2[i] <- paste("<font color='green'><b> DOES </b></font>"),
           itrcData$meaning2[i] <- paste("<font color='red'><b> DOES NOT </b></font>"))

    ifelse(itrcData$Bacteria[i]=="Yes",
           itrcData$stat3[i] <- paste("<font color='green'><b> DOES </b></font>"),
           itrcData$stat3[i] <- paste("<font color='red'><b> DOES NOT </b></font>"))
    ifelse(itrcData$Bacteria[i]=="Yes",
           itrcData$meaning3[i] <- paste("<font color='green'><b> DOES </b></font>"),
           itrcData$meaning3[i] <- paste("<font color='red'><b> DOES NOT </b></font>"))

}

polMap <<- list(dtRow=c(2,3,4), dfRow=c(11,13,15))

## Drop doc name field, write to global
itrcData <<- subset(itrcData, select=-c(doc))


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="ITRC BMP Tool",
               tabPanel("Pollutant Filtering"),
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
               ),
               tabPanel("Previous Version",
                        selected="Data",
                        br(),
                        h2("**DRAFT/PROTOTYPE**", align="center"),
                        h2("ITRC Stormwater BMP Applicability / Evaulation Tool"),
                        fluidRow(
                            h3("Select Criteria", align="center"),
                            column(6,
                                   wellPanel(style="background-color: #D2DDE4",
                                                 selectInput(
                                                     inputId="pollutants2",
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
                        h6("Note: The table is currently populated with dummy data.  This is for functionality and testing purposes only.", color="red"),
                        hr(),
                        DT::dataTableOutput("results3")
                        )
               )
)

