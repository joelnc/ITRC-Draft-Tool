library(shiny)
library(tuple)
library(DT)

## Put dataSubset1() and 2() into the same reactive call
##... as is filtering doesn't work correctly backwards and forward
##... see also conditionalPanel()

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {


        ## Pollutant only filtering
        dataSubset1 <- reactive({
            #### If no pollutnats
            if (is.null(input$pollutants)) {
                pollFilt <- itrcData2nd
            ## First extract based on that
            } else if (length(input$pollutants)==1) {
                pollFilt <- itrcData2nd[which(itrcData2nd[ ,input$pollutants]!='No'), ]

            #### If multiple Pollutants
            } else if (length(input$pollutants)>1) {
                ## pollFilt <- itrcData2nd[which(itrcData2nd[ ,input$pollutants]!='X'), ]

                temp <- NULL # init. container
                for (i in 1:length(input$pollutants)) { # loop over cols. to filter
                    for (j in 1:nrow(itrcData2nd)) { # loop over rows in given col.
                        ## If given row isn't an 'X', record its row index
                        if (itrcData2nd[j, input$pollutants[i]]!='No') {
                            temp <- c(temp, j)
                        }
                    }
                }
                pollFilt <-itrcData2nd[temp[tuplicated(temp, n=length(input$pollutants))], ]
            }
        })

        ## Secondary filters
        dataSubset2 <- reactive({
            if (all(c(input$frz, input$ard, input$ung, input$cont,
                      input$hwt, input$htss, input$hct, input$inf) == FALSE)) {
                          pollFilt2 <- dataSubset1()
            } else {
                ds <- dataSubset1()
                ##filts <- construct list of yes columns from input$...
                filts <- NULL
                if (input$frz) filts <- c(filts, 39)
                if (input$ard) filts <- c(filts, 40)
                if (input$ung) filts <- c(filts, 41)
                if (input$cont) filts <- c(filts, 42)
                if (input$hwt) filts <- c(filts, 43)
                if (input$htss) filts <- c(filts, 44)

                filtCont <- NULL
                for (j in 1:length(filts)) { # loop over cols. to filter
                    for (i in 1:nrow(ds)) { # loop over rows in given col.
                        ## If given row isn't an 'X', record its row index
                        if (ds[i, filts[j]]!='No') {
                            filtCont <- c(filtCont, i)
                        }
                    }
                }

                if (length(filts==1)) {
                    pollFilt2 <- ds[which(ds[ ,filtCont]!='No'), ]
                } else {
                    pollFilt2 <-ds[filtCont[tuplicated(filtCont, n=length(filts))], ]
                }
            }


        })



        ## Secondary filters combined
        dataSubset3 <- reactive({

            ## If secondarys all false, run through pollutnats....
            if (all(c(input$frz, input$ard, input$ung, input$cont,
                      input$hwt, input$htss, input$hct, input$inf) == FALSE)) {

                ## Replace this simple call with a primary call
                ##pollFilt2 <- dataSubset1()

                ## If no pollutnats
                if (is.null(input$pollutants)) {
                    pollFilt <- itrcData2nd
                    ## First extract based on that
                } else if (length(input$pollutants)==1) {
                    pollFilt <- itrcData2nd[which(itrcData2nd[ ,input$pollutants]!='No'), ]

                ## If multiple Pollutants
                } else if (length(input$pollutants)>1) {
                    ## pollFilt <- itrcData2nd[which(itrcData2nd[ ,input$pollutants]!='X'), ]

                    temp <- NULL # init. container
                    for (i in 1:length(input$pollutants)) { # loop over cols. to filter
                        for (j in 1:nrow(itrcData2nd)) { # loop over rows in given col.
                            ## If given row isn't an 'X', record its row index
                            if (itrcData2nd[j, input$pollutants[i]]!='No') {
                                temp <- c(temp, j)
                            }
                        }
                    }
                    pollFilt <-itrcData2nd[temp[tuplicated(temp, n=length(input$pollutants))], ]
                }



            ##################################################################
            ## Else if some secondarys true...
            } else {
                ##ds <- dataSubset1()

                ## If no pollutnats
                if (is.null(input$pollutants)) {
                    pollFilt <- itrcData2nd
                    ## First extract based on that
                } else if (length(input$pollutants)==1) {
                    pollFilt <- itrcData2nd[which(itrcData2nd[ ,input$pollutants]!='No'), ]

                ## If multiple Pollutants
                } else if (length(input$pollutants)>1) {
                    ## pollFilt <- itrcData2nd[which(itrcData2nd[ ,input$pollutants]!='X'), ]

                    temp <- NULL # init. container
                    for (i in 1:length(input$pollutants)) { # loop over cols. to filter
                        for (j in 1:nrow(itrcData2nd)) { # loop over rows in given col.
                            ## If given row isn't an 'X', record its row index
                            if (itrcData2nd[j, input$pollutants[i]]!='No') {
                                temp <- c(temp, j)
                            }
                        }
                    }
                    pollFilt <-itrcData2nd[temp[tuplicated(temp, n=length(input$pollutants))], ]
                }


                ##filts <- construct list of yes columns from input$...
                filts <- NULL
                if (input$frz) filts <- c(filts, 39)
                if (input$ard) filts <- c(filts, 40)
                if (input$ung) filts <- c(filts, 41)
                if (input$cont) filts <- c(filts, 42)
                if (input$hwt) filts <- c(filts, 43)
                if (input$htss) filts <- c(filts, 44)

                filtCont <- NULL
                for (j in 1:length(filts)) { # loop over cols. to filter
                    for (i in 1:nrow(pollFilt)) { # loop over rows in given col.
                        ## If given row isn't an 'X', record its row index
                        if (pollFilt[i, filts[j]]!='No') {
                            filtCont <- c(filtCont, i)
                        }
                    }
                }

                if (length(filts==1)) {
                    pollFilt <- pollFilt[which(pollFilt[ ,filtCont]!='No'), ]
                } else {
                    pollFilt <- pollFilt[pollFilt[tuplicated(filtCont, n=length(filts))], ]
                }
            }
        })






        ## Pollutant only table
        output$results <- DT::renderDataTable(
                                   DT::datatable(
                                           dataSubset2()[,c("Practice", "notesDesc")],
                                           escape=FALSE,
                                           select="single",
                                           rownames=FALSE,
                                           options = list(pageLength = 25,
                                                          searching=FALSE,
                                                          paging=FALSE)
                                       )
                               )



   })## Done


        ## ## 'flags' df, holds T/F values indicating which data to show
        ## ## Initialize as all False
        ## flags1 <- itrcData[1,2:4] # as in Sed., Bact., TP
        ## flags1[flags1!=FALSE] <- FALSE

        ## ## Flags for data conditions
        ## flags2 <- data.frame(ThirdPartyLab=FALSE, ThirdPartyField=FALSE, IDB=FALSE,
        ##                  RefJournal=FALSE, VendorData=FALSE)


        ## ## Combined
        ## dataSubset0 <- reactive({
        ##     #### If no pollutnats
        ##     if (is.null(input$pollutants)) {
        ##         pollFilt <- itrcData
        ##         ## AND no check boxes
        ##         if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
        ##                   input$Vend)==FALSE)) {
        ##             ## Ret all data
        ##             returnData <- pollFilt
        ##         } else {
        ##             ## (Re-)Set flag values whenever inputs change
        ##             if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
        ##             else  flags2$ThirdPartyLab <- FALSE

        ##             if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
        ##             else flags2$ThirdPartyField <- FALSE

        ##             if (input$IDB==TRUE) flags2$IDB <- TRUE
        ##             else flags2$IDB <- FALSE

        ##             if (input$refJ==TRUE) flags2$RefJournal <- TRUE
        ##             else flags2$RefJournal <- FALSE

        ##             if (input$Vend==TRUE) flags2$VendorData <- TRUE
        ##             else flags2$VendorData <- FALSE

        ##             ## Extract colnames for conditional TRUE
        ##             showThese <- colnames(flags2[which(flags2==TRUE)])

        ##             ## If just one box is checked, simple subset
        ##             if (length(showThese)==1) {
        ##                 returnData <- pollFilt[which(pollFilt[ ,showThese]==TRUE), ]
        ##             ## More than one box, attrocious loop
        ##             } else {
        ##                 temp <- NULL ## init. container
        ##                 for (i in 1:length(showThese)) { ## loop over cols. to filter
        ##                     for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
        ##                         ## If given row is a TRUE, record its row index
        ##                         if (pollFilt[j, showThese[i]]==TRUE) {
        ##                             temp <- c(temp, j)
        ##                         }
        ##                     }
        ##                 }
        ##                 ## Return subset of practices where all checkbox values = TRUE
        ##                 returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
        ##             }
        ##         }

        ##     #### If 1 pollutnant
        ##     ## First extract based on that
        ##     } else if (length(input$pollutants)==1) {
        ##         pollFilt <- itrcData[which(itrcData[ ,input$pollutants]!='No'), ]

        ##             if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
        ##                       input$Vend)==FALSE)) {
        ##                 returnData <- pollFilt
        ##             } else {
        ##                 ## (Re-)Set flag values whenever inputs change
        ##                 if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
        ##                 else  flags2$ThirdPartyLab <- FALSE

        ##                 if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
        ##                 else flags2$ThirdPartyField <- FALSE

        ##                 if (input$IDB==TRUE) flags2$IDB <- TRUE
        ##                 else flags2$IDB <- FALSE

        ##                 if (input$refJ==TRUE) flags2$RefJournal <- TRUE
        ##                 else flags2$RefJournal <- FALSE

        ##                 if (input$Vend==TRUE) flags2$VendorData <- TRUE
        ##                 else flags2$VendorData <- FALSE

        ##                 ## Extract colnames for conditional TRUE
        ##                 showThese <- colnames(flags2[which(flags2==TRUE)])

        ##                 ## If just one box is checked, simple subset
        ##                 if (length(showThese)==1) {
        ##                     returnData <- pollFilt[which(pollFilt[ ,showThese]==TRUE), ]
        ##                 ## More than one box, attrocious loop
        ##                 } else {
        ##                     temp <- NULL ## init. container
        ##                     for (i in 1:length(showThese)) { ## loop over cols. to filter
        ##                         for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
        ##                             ## If given row is a TRUE, record its row index
        ##                             if (pollFilt[j, showThese[i]]==TRUE) {
        ##                                 temp <- c(temp, j)
        ##                             }
        ##                         }
        ##                     }
        ##                     ## Return subset of practices where all checkbox values = TRUE
        ##                     returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
        ##                 }
        ##             }

        ##     #### If multiple Pollutants
        ##     } else if (length(input$pollutants)>1) {
        ##         ## pollFilt <- itrcData[which(itrcData[ ,input$pollutants]!='X'), ]

        ##         temp <- NULL # init. container
        ##         for (i in 1:length(input$pollutants)) { # loop over cols. to filter
        ##             for (j in 1:nrow(itrcData)) { # loop over rows in given col.
        ##                 ## If given row isn't an 'X', record its row index
        ##                 if (itrcData[j, input$pollutants[i]]!='No') {
        ##                     temp <- c(temp, j)
        ##                 }
        ##             }
        ##         }

        ##         ## Return subset of practices where all checkbox values = TRUE
        ##         pollFilt <- itrcData[temp[tuplicated(temp, n=length(input$pollutants))], ]

        ##             if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
        ##                       input$Vend)==FALSE)) {
        ##               returnData <- pollFilt
        ##             } else {
        ##                 ## (Re-)Set flag values whenever inputs change
        ##                 if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
        ##                 else  flags2$ThirdPartyLab <- FALSE

        ##                 if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
        ##                 else flags2$ThirdPartyField <- FALSE

        ##                 if (input$IDB==TRUE) flags2$IDB <- TRUE
        ##                 else flags2$IDB <- FALSE

        ##                 if (input$refJ==TRUE) flags2$RefJournal <- TRUE
        ##                 else flags2$RefJournal <- FALSE

        ##                 if (input$Vend==TRUE) flags2$VendorData <- TRUE
        ##                 else flags2$VendorData <- FALSE

        ##                 ## Extract colnames for conditional TRUE
        ##                 showThese <- colnames(flags2[which(flags2==TRUE)])

        ##                 ## If just one box is checked, simple subset
        ##                 if (length(showThese)==1) {
        ##                     returnData <- pollFilt[which(pollFilt[ ,showThese]==TRUE), ]
        ##                 ## More than one box, attrocious loop
        ##                 } else {
        ##                     temp <- NULL ## init. container
        ##                     for (i in 1:length(showThese)) { ## loop over cols. to filter
        ##                         for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
        ##                             ## If given row is a TRUE, record its row index
        ##                             if (pollFilt[j, showThese[i]]==TRUE) {
        ##                                 temp <- c(temp, j)
        ##                             }
        ##                         }
        ##                     }
        ##                     ## Return subset of practices where all checkbox values = TRUE
        ##                     returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
        ##                 }
        ##             }
        ##     }

        ## })


        ## ## Display the subset
        ## output$results2 <- DT::renderDataTable(
        ##                            DT::datatable(
        ##                                    dataSubset0()[,1:9],
        ##                                    escape=FALSE,
        ##                                    select="single",
        ##                                    rownames=FALSE,
        ##                                    options= list(columnDefs = list(list(className = 'dt-center', targets = 1:8))),
        ##                                    callback=JS("var tips = ['',
        ##                                                         'Including TSS, SSC, and Turbidity',
        ##                                                         'Total or Fractional P',
        ##                                                         'May include Fecal Coliform, E. Coli, and Enterrococcus',
        ##                                                         'Indicates whether 3rd labratory testing informs preformance expectations',
        ##                                                         'Indicates whether 3rd field testing informs preformance expectations',
        ##                                                         'Indicates whether the International SW BMP Database includes data on this practice',
        ##                                                         'Indicates whether journal articles support this practices performance expectations',
        ##                                                         'Indicates whether vendor data supports this practices performance expectations'],
        ##                                                 header = table.columns().header();
        ##                                             for (var i = 0; i < tips.length; i++) {
        ##                                               $(header[i]).attr('title', tips[i]);
        ##                                             }
        ##                                             ")
        ##                                )
        ##                        )

        ## ## observe event 1
        ## observeEvent(input$results2_cell_clicked, {

        ##     info <- input$results2_cell_clicked

        ##     if (is.null(info$value)) {
        ##         return()
        ##     } else if (!is.null(info$value) & info$col %in% c(1,2,3)) {
        ##         showModal(
        ##             modalDialog(
        ##                 htmlOutput("text")
        ##             )
        ##         )
        ##     }
        ## })

        ## ##polMap <<- list(dtRow=c(2,3,4), dfRow=c(11,13,15))
        ## ## This seems unnecessary as as an observer. The above call should do a fresh pull
        ## ## when clicked. Investigate
        ## observeEvent(input$results2_cell_clicked, {

        ##     info <- input$results2_cell_clicked
        ##     ##browser()

        ##     output$text <- renderUI({
        ##         if (dataSubset0()[info$row, info$col+1]=="Yes") {
        ##             aPart <- paste("<font color='green'><b>",
        ##                            "YES", "</b></font>")
        ##         } else {
        ##             aPart <- paste("<font color='red'><b>",
        ##                            "NO", "</b></font>")
        ##         }

        ##         bPart <- paste("Practice: <b>", dataSubset0()[info$row,"Practice"],
        ##                        "</b> potentially treats for pollutant <b>",
        ##                        names(itrcData)[info$col+1], "</b>.")
        ##         cPart <- "<u><b>IBMPDB</b></u>"
        ##         dPart <- paste("<li> Data for this practice ",
        ##                        dataSubset0()[info$row,"isIn"],
        ##                        "included in the ISWBMPDB.</li>")
        ##         ePart <- paste("<li> The ISWBMPDB ",
        ##                        dataSubset0()[info$row, polMap$dfRow[polMap$dtRow==info$col+1]],
        ##                        "show a statistically significant effect for this combo.</li>")
        ##         fPart <- paste("<li> The ISWBMPDB ",
        ##                        dataSubset0()[info$row, polMap$dfRow[polMap$dtRow==info$col+1]],
        ##                        "show a meaningful effect for this combo.</li>")
        ##         gPart <- "<b><u>Unit Processes</u></b> <li>Say something about how knowledge of unit processes affected our assessment</li>"
        ##         hPart <- paste("<b><u> ", dataSubset0()[info$row, "Practice"], "and Pollutants Similar To",
        ##                        names(itrcData)[info$col+1], "</u></b> <li>Say something relevant to this heading</li>")

        ##         if (dataSubset0()[info$row,"IDB"]==TRUE) {
        ##             HTML(paste(aPart, br(), br(), bPart, br(), br(),cPart, br(),
        ##                        dPart, ePart, fPart, br(), gPart, br(), hPart))
        ##         } else {
        ##             HTML(paste(aPart, br(), br(), bPart, br(), br(),cPart, br(),
        ##                        dPart, br(), gPart, br(), hPart))

        ##         }
        ##     })
        ## })


        ## ## This seems unnecessary as as an observer. The above call should do a fresh pull
        ## ## when clicked. Investigate
        ## observeEvent(input$results2_cell_clicked, {

        ##     info <- input$results2_cell_clicked
        ##     ##browser()

        ##     output$text <- renderUI({
        ##         if (itrcData[info$row, info$col+1]=="Yes") {
        ##             aPart <- paste("<font color='green'><b>",
        ##                            "YES", "</b></font>")
        ##         } else {
        ##             aPart <- paste("<font color='red'><b>",
        ##                            "NO", "</b></font>")
        ##         }

        ##         bPart <- paste("Practice: <b>", itrcData$Practice[info$row],
        ##                        "</b> potentially treats for pollutant <b>",
        ##                        ##names(itrcData)[polMap$dfRow[polMap$dtRow==info$col]], "</b>.")
        ##                        names(itrcData)[info$col+1], "</b>.")
        ##         cPart <- "<u><b>IBMPDB</b></u>"
        ##         dPart <- paste("<li> Data for this practice ",
        ##                        itrcData$isIn[info$row],
        ##                        "included in the ISWBMPDB.</li>")
        ##         ePart <- paste("<li> The ISWBMPDB ",
        ##                        itrcData[info$row, polMap$dfRow[polMap$dtRow==info$col+1]],
        ##                        "show a statistically significant effect for this combo.</li>")
        ##         fPart <- paste("<li> The ISWBMPDB ",
        ##                        itrcData[info$row, polMap$dfRow[polMap$dtRow==info$col+1]],
        ##                        "show a meaningful effect for this combo.</li>")
        ##         gPart <- "<b><u>Unit Processes</u></b> <li>Say something about how knowledge of unit processes affected our assessment</li>"
        ##         hPart <- paste("<b><u> ", itrcData$Practice[info$row], "and Pollutants Similar To",
        ##                        names(itrcData)[info$col+1], "</u></b> <li>Say something relevant to this heading</li>")

        ##         if (itrcData$IDB[info$row]==TRUE) {
        ##             HTML(paste(aPart, br(), br(), bPart, br(), br(),cPart, br(),
        ##                        dPart, ePart, fPart, br(), gPart, br(), hPart))
        ##         } else {
        ##             HTML(paste(aPart, br(), br(), bPart, br(), br(),cPart, br(),
        ##                        dPart, br(), gPart, br(), hPart))

        ##         }
        ##     })
        ## })



