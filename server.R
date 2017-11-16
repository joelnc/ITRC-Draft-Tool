library(shiny)
library(tuple)
library(DT)
library(shinyBS)

## Put dataSubset1() and 2() into the same reactive call
##... as is filtering doesn't work correctly backwards and forward
##... see also conditionalPanel()

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {

        ## 'flags' df, holds T/F values indicating which data to show
        ## Initialize as all False
        flags1 <- itrcData2nd[1,2:38]
        flags1[flags1!=FALSE] <- FALSE

        ## Flags for data conditions
        flags2 <- data.frame(frz=FALSE, ard=FALSE, ung=FALSE,
                             cont=FALSE, hgw=FALSE)


        ## Combined
        dataSubset0 <- reactive({
            #### If no pollutnats
            if (is.null(input$pollutants)) {
                pollFilt <- itrcData2nd
                ## AND no check boxes
                if (all(c(input$frz, input$ard, input$ung, input$cont,
                          input$hgw, input$htss, input$hct, input$inf, input$gsi)==FALSE)) {
                    ## Ret all data
                    returnData <- pollFilt
                } else {
                    ## (Re-)Set flag values whenever inputs change
                    if (input$frz==TRUE) flags2$frz <- TRUE
                    else  flags2$frz <- FALSE

                    if (input$ard==TRUE)  flags2$ard <- TRUE
                    else flags2$ard <- FALSE

                    if (input$ung==TRUE) flags2$ung <- TRUE
                    else flags2$ung <- FALSE

                    if (input$cont==TRUE) flags2$cont <- TRUE
                    else flags2$cont <- FALSE

                    if (input$hgw==TRUE) flags2$hgw <- TRUE
                    else flags2$hgw <- FALSE

                    if (input$htss==TRUE) flags2$htss <- TRUE
                    else flags2$htss <- FALSE

                    if (input$hct==TRUE) flags2$hct <- TRUE
                    else flags2$hct <- FALSE

                    if (input$inf==TRUE) flags2$inf <- TRUE
                    else flags2$inf <- FALSE

                    if (input$gsi==TRUE) flags2$gsi <- TRUE
                    else flags2$gsi <- FALSE

                    ## Extract colnames for conditional TRUE
                    showThese <- colnames(flags2[which(flags2==TRUE)])

                    ## If just one box is checked, simple subset
                    if (length(showThese)==1) {
                        returnData <- pollFilt[which(pollFilt[ ,showThese]=="Yes"), ]
                    ## More than one box, attrocious loop
                    } else {
                        temp <- NULL ## init. container
                        for (i in 1:length(showThese)) { ## loop over cols. to filter
                            for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
                                ## If given row is a TRUE, record its row index
                                if (pollFilt[j, showThese[i]]=="Yes") {
                                    temp <- c(temp, j)
                                }
                            }
                        }
                        ## Return subset of practices where all checkbox values = TRUE
                        returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
                    }
                }

            #### If 1 pollutnant
            ## First extract based on that
            } else if (length(input$pollutants)==1) {
                pollFilt <- itrcData2nd[which(itrcData2nd[ ,input$pollutants]!='No'), ]


                if (all(c(input$frz, input$ard, input$ung, input$cont,
                          input$hgw, input$htss, input$hct, input$inf, input$gsi)==FALSE)) {
                    returnData <- pollFilt
                } else {
                    ## (Re-)Set flag values whenever inputs change
                    if (input$frz==TRUE) flags2$frz <- TRUE
                    else  flags2$frz <- FALSE

                    if (input$ard==TRUE)  flags2$ard <- TRUE
                    else flags2$ard <- FALSE

                    if (input$ung==TRUE) flags2$ung <- TRUE
                    else flags2$ung <- FALSE

                    if (input$cont==TRUE) flags2$cont <- TRUE
                    else flags2$cont <- FALSE

                    if (input$hgw==TRUE) flags2$hgw <- TRUE
                    else flags2$hgw <- FALSE

                    if (input$htss==TRUE) flags2$htss <- TRUE
                    else flags2$htss <- FALSE

                    if (input$hct==TRUE) flags2$hct <- TRUE
                    else flags2$hct <- FALSE

                    if (input$inf==TRUE) flags2$inf <- TRUE
                    else flags2$inf <- FALSE

                    if (input$gsi==TRUE) flags2$gsi <- TRUE
                    else flags2$gsi <- FALSE

                    ## Extract colnames for conditional TRUE
                    showThese <- colnames(flags2[which(flags2==TRUE)])

                    ## If just one box is checked, simple subset
                    if (length(showThese)==1) {
                        returnData <- pollFilt[which(pollFilt[ ,showThese]=="Yes"), ]
                        ## More than one box, attrocious loop
                    } else {
                        temp <- NULL ## init. container
                        for (i in 1:length(showThese)) { ## loop over cols. to filter
                            for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
                                ## If given row is a TRUE, record its row index
                                if (pollFilt[j, showThese[i]]=="Yes") {
                                    temp <- c(temp, j)
                                }
                            }
                        }
                        ## Return subset of practices where all checkbox values = TRUE
                        returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
                    }
                }


            #### If multiple Pollutants
            } else if (length(input$pollutants)>1) {
                ## pollFilt <- itrcData[which(itrcData[ ,input$pollutants]!='X'), ]

                temp <- NULL # init. container
                for (i in 1:length(input$pollutants)) { # loop over cols. to filter
                    for (j in 1:nrow(itrcData2nd)) { # loop over rows in given col.
                        ## If given row isn't an 'X', record its row index
                        if (itrcData2nd[j, input$pollutants[i]]!='No') {
                            temp <- c(temp, j)
                        }
                    }
                }

                ## Return subset of practices where all checkbox values = TRUE
                pollFilt <- itrcData2nd[temp[tuplicated(temp, n=length(input$pollutants))], ]

                    if (all(c(input$frz, input$ard, input$ung, input$cont,
                          input$hgw, input$htss, input$hct, input$inf, input$gsi)==FALSE)) {
                      returnData <- pollFilt
                    } else {
                        ## (Re-)Set flag values whenever inputs change
                        if (input$frz==TRUE) flags2$frz <- TRUE
                        else  flags2$frz <- FALSE

                        if (input$ard==TRUE)  flags2$ard <- TRUE
                        else flags2$ard <- FALSE

                        if (input$ung==TRUE) flags2$ung <- TRUE
                        else flags2$ung <- FALSE

                        if (input$cont==TRUE) flags2$cont <- TRUE
                        else flags2$cont <- FALSE

                        if (input$hgw==TRUE) flags2$hgw <- TRUE
                        else flags2$hgw <- FALSE

                        if (input$htss==TRUE) flags2$htss <- TRUE
                        else flags2$htss <- FALSE

                        if (input$hct==TRUE) flags2$hct <- TRUE
                        else flags2$hct <- FALSE

                        if (input$inf==TRUE) flags2$inf <- TRUE
                        else flags2$inf <- FALSE


                        if (input$gsi==TRUE) flags2$gsi <- TRUE
                        else flags2$gsi <- FALSE

                        ## Extract colnames for conditional TRUE
                        showThese <- colnames(flags2[which(flags2==TRUE)])

                        ## If just one box is checked, simple subset
                        if (length(showThese)==1) {
                            returnData <- pollFilt[which(pollFilt[ ,showThese]=="Yes"), ]
                        ## More than one box, attrocious loop
                        } else {
                            temp <- NULL ## init. container
                            for (i in 1:length(showThese)) { ## loop over cols. to filter
                                for (j in 1:nrow(pollFilt)) { ## loop over rows in given col.
                                    ## If given row is a TRUE, record its row index
                                    if (pollFilt[j, showThese[i]]=="Yes") {
                                        temp <- c(temp, j)
                                    }
                                }
                            }
                            ## Return subset of practices where all checkbox values = TRUE
                            returnData <- pollFilt[temp[tuplicated(temp, n=length(showThese))], ]
                        }
                    }
            }
        })

        ## Pollutant only table
        output$results <- DT::renderDataTable(
                                   DT::datatable(
                                           dataSubset0()[,c("Practice", "notesDesc")],
                                           escape=FALSE,
                                           select="single",
                                           rownames=FALSE,
                                           options = list(pageLength = 25,
                                                          searching=FALSE,
                                                          paging=FALSE)
                                       )
                               )



   })## Done
