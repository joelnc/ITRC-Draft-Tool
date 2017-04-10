library(shiny)
library(tuple)

## Put dataSubset1() and 2() into the same reactive call
##... as is filtering doesn't work correctly backwards and forward
##... see also conditionalPanel()


## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

        ## 'flags' df, holds T/F values indicating which data to show
        ## Initialize as all False
        flags1 <- itrcData[1,2:4] # as in Sed., Bact., TP
        flags1[flags1!=FALSE] <- FALSE

        ## Flags for data conditions
        flags2 <- data.frame(ThirdPartyLab=FALSE, ThirdPartyField=FALSE, IDB=FALSE,
                         RefJournal=FALSE, VendorData=FALSE)


        ## Combined
        dataSubset0 <- reactive({
            ## If no pollutnats
            if (is.null(input$pollutants)) {
                pollFilt <- itrcData
                ## AND no check boxes
                if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
                          input$Vend)==FALSE)) {
                    ## Ret all data
                    returnData <- pollFilt
                } else {
                    ## (Re-)Set flag values whenever inputs change
                    if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
                    else  flags2$ThirdPartyLab <- FALSE

                    if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
                    else flags2$ThirdPartyField <- FALSE

                    if (input$IDB==TRUE) flags2$IDB <- TRUE
                    else flags2$IDB <- FALSE

                    if (input$refJ==TRUE) flags2$RefJournal <- TRUE
                    else flags2$RefJournal <- FALSE

                    if (input$Vend==TRUE) flags2$VendorData <- TRUE
                    else flags2$VendorData <- FALSE

                    ## Extract colnames for conditional TRUE
                    showThese <- colnames(flags2[which(flags2==TRUE)])

                    ## If just one box is checked, simple subset
                    if (length(showThese)==1) {
                        returnData <- pollFilt[which(pollFilt[ ,showThese]==TRUE), ]
                    ## More than one box, attrocious loop
                    } else {
                        temp <- NULL ## init. container
                        for (i in 1:length(showThese)) { ## loop over cols. to filter
                            for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
                                ## If given row is a TRUE, record its row index
                                if (pollFilt[j, showThese[i]]==TRUE) {
                                    temp <- c(temp, j)
                                }
                            }
                        }
                        ## Return subset of practices where all checkbox values = TRUE
                        returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
                    }
                }

            ## If 1 pollutnant
            ## First extract based on that
            } else if (length(input$pollutants)==1) {
                pollFilt <- itrcData[which(itrcData[ ,input$pollutants]!='X'), ]

                    if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
                              input$Vend)==FALSE)) {
                        returnData <- pollFilt
                    } else {
                        ## (Re-)Set flag values whenever inputs change
                        if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
                        else  flags2$ThirdPartyLab <- FALSE

                        if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
                        else flags2$ThirdPartyField <- FALSE

                        if (input$IDB==TRUE) flags2$IDB <- TRUE
                        else flags2$IDB <- FALSE

                        if (input$refJ==TRUE) flags2$RefJournal <- TRUE
                        else flags2$RefJournal <- FALSE

                        if (input$Vend==TRUE) flags2$VendorData <- TRUE
                        else flags2$VendorData <- FALSE

                        ## Extract colnames for conditional TRUE
                        showThese <- colnames(flags2[which(flags2==TRUE)])

                        ## If just one box is checked, simple subset
                        if (length(showThese)==1) {
                            returnData <- pollFilt[which(pollFilt[ ,showThese]==TRUE), ]
                        ## More than one box, attrocious loop
                        } else {
                            temp <- NULL ## init. container
                            for (i in 1:length(showThese)) { ## loop over cols. to filter
                                for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
                                    ## If given row is a TRUE, record its row index
                                    if (pollFilt[j, showThese[i]]==TRUE) {
                                        temp <- c(temp, j)
                                    }
                                }
                            }
                            ## Return subset of practices where all checkbox values = TRUE
                            returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
                        }
                    }


            ## If multiple Pollutants
            } else {
                if (length(input$pollutants)>1) {
                    pollFilt <- itrcData[which(itrcData[ ,input$pollutants]!='X'), ]

                    if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
                              input$Vend)==FALSE)) {
                      returnData <- pollFilt
                    } else {
                        ## (Re-)Set flag values whenever inputs change
                        if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
                        else  flags2$ThirdPartyLab <- FALSE

                        if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
                        else flags2$ThirdPartyField <- FALSE

                        if (input$IDB==TRUE) flags2$IDB <- TRUE
                        else flags2$IDB <- FALSE

                        if (input$refJ==TRUE) flags2$RefJournal <- TRUE
                        else flags2$RefJournal <- FALSE

                        if (input$Vend==TRUE) flags2$VendorData <- TRUE
                        else flags2$VendorData <- FALSE

                        ## Extract colnames for conditional TRUE
                        showThese <- colnames(flags2[which(flags2==TRUE)])

                        ## If just one box is checked, simple subset
                        if (length(showThese)==1) {
                            returnData <- pollFilt[which(pollFilt[ ,showThese]==TRUE), ]
                        ## More than one box, attrocious loop
                        } else {
                            temp <- NULL ## init. container
                            for (i in 1:length(showThese)) { ## loop over cols. to filter
                                for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
                                    ## If given row is a TRUE, record its row index
                                    if (pollFilt[j, showThese[i]]==TRUE) {
                                        temp <- c(temp, j)
                                    }
                                }
                            }
                            ## Return subset of practices where all checkbox values = TRUE
                            returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
                        }
                    }
                }
            }
        })



        ## Display the subset
        output$results2 <- DT::renderDataTable(
                                   DT::datatable(
                                           dataSubset0(),
                                       options(list(pageLength = 25)),
                                       escape =FALSE,
                                       rownames=FALSE
                                       )
        )
    })## Done


        ## ## Pollutant filtering subset
        ## dataSubset1 <- reactive({

        ##     ## If nothing selected, RETURN ALL
        ##     if (is.null(input$pollutants)) {
        ##         a0 <- itrcData
        ##     } else {
        ##         ## If just one box is checked, simple subset
        ##         if (length(input$pollutants)==1) {
        ##             a0 <- itrcData[which(itrcData[ ,input$pollutants]!='X'), ]
        ##         }

        ##         ## More than 1, loop over pollutants and rows... find better way
        ##         } else {
        ##             temp0 <- NULL # init. container
        ##             for (i0 in 1:length(input$pollutants)) { # loop over cols. to filter
        ##                 for (j0 in 1:nrow(itrcData)) { # loop over rows in given col.
        ##                     ## If given row isn't an 'X', record its row index
        ##                     if (itrcData[j0, input$pollutants[i0]]!='X') {
        ##                         temp0 <- c(temp0, j0)
        ##                     }
        ##                 }
        ##             }

        ##             ## Return subset of practices where all checkbox values = TRUE
        ##             a0 <- itrcData[temp0[tuplicated(temp0, n=length(input$pollutants))], ]
        ##         }
        ##     }
        ## })


        ## ## Data subset function
        ## dataSubset2 <- reactive({

        ##     ## If nothing selected, RETURN ALL
        ##     if (all(c(input$TPL, input$TPF, input$IDB, input$refJ,
        ##               input$Vend)==FALSE)) {
        ##         a1 <- dataSubset1()

        ##     ## Else, do a conditional subset, display results
        ##     } else {

        ##         ## (Re-)Set flag values whenever inputs change
        ##         if (input$TPL==TRUE) flags2$ThirdPartyLab <- TRUE
        ##         else  flags2$ThirdPartyLab <- FALSE

        ##         if (input$TPF==TRUE)  flags2$ThirdPartyField <- TRUE
        ##         else flags2$ThirdPartyField <- FALSE

        ##         if (input$IDB==TRUE) flags2$IDB <- TRUE
        ##         else flags2$IDB <- FALSE

        ##         if (input$refJ==TRUE) flags2$RefJournal <- TRUE
        ##         else flags2$RefJournal <- FALSE

        ##         if (input$Vend==TRUE) flags2$VendorData <- TRUE
        ##         else flags2$VendorData <- FALSE

        ##         ## Extract colnames for conditional TRUE
        ##         showThese <- colnames(flags2[which(flags2==TRUE)])

        ##         ## If just one box is checked, simple subset
        ##         if (length(showThese)==1) {
        ##             #a0 <- dataSubset1()
        ##             #browser()
        ##             a1 <- dataSubset1()[which(dataSubset1()[ ,showThese]==TRUE), ]

        ##         ## More than one box, attrocious loop
        ##         } else {
        ##             a0 <- dataSubset1()
        ##             temp <- NULL ## init. container
        ##             for (i in 1:length(showThese)) { ## loop over cols. to filter
        ##                 for (j in 1:nrow(itrcData)) { ## loop over rows in given col.
        ##                     ## If given row is a TRUE, record its row index
        ##                     if (itrcData[j, showThese[i]]==TRUE) {
        ##                         temp <- c(temp, j)
        ##                     }
        ##                 }
        ##             }
        ##             ## Return subset of practices where all checkbox values = TRUE
        ##             a1 <- a0[temp[tuplicated(temp, n=length(showThese))], ]
        ##         }
        ##     }
        ## })


