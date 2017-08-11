### load packages
library(shiny)
library(data.table)
library(ggplot2)
library(RCurl)
library(DT)

### read in data
link <- "https://raw.githubusercontent.com/sppandlkk/control_chart/master/data/episode_data.csv"
dataUse <- read.csv(url(link))
### or use local version
#dataUse <- read.csv("../data/episode_data.csv")

### deal with data
dataUse$procDate <- as.Date(dataUse$procDate, "%Y-%m-%d")

### sort by procedure data
dataUse <- dataUse[order(dataUse$procDate), ]

### parameter setting
grp <- "grp"
hospID <- "hospID"
physID <- "physID"
procDate <- "procDate"
obsAdverseOutcome <- "obsAdverseOutcome"
preAdverseOutcome <- "preAdverseOutcome"
obsCost <- "obsCost"
preCost <- "preCost"
DIAG_PFX <- "diag_"
POA_PFX <- "poa_"
PROC_PFX <- "proc_"


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Control Chart for Adverse Outcomes and Costs"),
   
   #### First row
   fluidRow(
       column(2,
              selectInput("grpIdx", 
                          label    = "Choose surgical group: ",
                          choices  = c("1: CABG" = 1,  
                                       "2: Hip Replacement" = 2,
                                       "3: Brain Surgery" = 3
                                       ), 
                          selected = 1)
       ),  
       column(2,
              selectInput("hospId", 
                          label    = "Choose Hospital: ",
                          choices  = c("Hosp_1" = "Hosp_1",  
                                       "Hosp_2" = "Hosp_2",
                                       "Hosp_3" = "Hosp_3",
                                       "Hosp_4" = "Hosp_4"
                                       ),
                          selected = "Hosp_1")
       ),  
       column(2,
              selectInput("physId", 
                          label    = "Choose Physician: ",
                          choices  = c("All"    = "All",
                                       "Phys_A" = "Phys_A",  
                                       "Phys_B" = "Phys_B",
                                       "Phys_C" = "Phys_C"
                          ),
                          selected = "All")
       ),
       column(2,
              selectInput("type",
                          label = "Type of control chart",
                          choices = c("Adverse Outcomes" = "RAAdverseOutcome",
                                      "Cost"             = "RACost"),
                         selected = "RAAdverseOutcome")
       )
   ),  
   
   #### Second row
   fluidRow(
       column(2,
              selectInput("p1_year", 
                          label    = "Phase I start year",
                          choices  = c(2015:2017), 
                          selected = 2015)
       ),
       
       column(2,
              selectInput("p1_month",
                          label    = "Phase I start month",
                          choices  = c("January"   = "01",
                                       "Feburary"  = "02",
                                       "March"     = "03",
                                       "April"     = "04",
                                       "May"       = "05",
                                       "June"      = "06",
                                       "July"      = "07",
                                       "August"    = "08",
                                       "September" = "09",
                                       "October"   = "10",
                                       "November"  = "11",
                                       "December"  = "12"),
                          selected = "01")
       ),
       
       column(2,
              selectInput("p2_year",
                          label    = "Phase II start year",
                          choices  = c(2016, 2017),
                          selected = 2016)
       ),
       
       column(2,
              selectInput("p2_month",
                          label = "Phase II start month",
                          choices  = c("January"   = "01",
                                       "Feburary"  = "02",
                                       "March"     = "03",
                                       "April"     = "04",
                                       "May"       = "05",
                                       "June"      = "06",
                                       "July"      = "07",
                                       "August"    = "08",
                                       "September" = "09",
                                       "October"   = "10",
                                       "November"  = "11",
                                       "December"  = "12"),
                          selected = "07")
       )
   ),
   
   #### third row: plot and data output
   fluidRow(
       column(7,
              h2("Control chart"),
              plotOutput("plot",
                         width  = 650,
                         height = 300,
                         click  = "plotClick")
       ),
       column(3,
              h2("Analysis for the selected time point"),
              plotOutput("followUpPlot",
                         width  = 475,
                         height = 325)
       )
       
   ),
   
   #### third row: plot and data output
   fluidRow(
       column(3,
              selectInput("plotType", "what type of file to download for the plot",
                          choice = c(".pdf", ".png"))
       ),
       column(3,
              h1(" "),
              downloadButton("downloadPlot", "Download")
       )
   ), 
   
   fluidRow(
       column(2,
              selectInput("nDiag",
                          label    = "Show the number of diagnosis codes",
                          choices  = 0:5,
                          selected = 0),
              checkboxInput("showNonPoa", "Highlight non-POA", FALSE),
              selectInput("nProc",
                          label    = "Show the number of procedure codes",
                          choices  = 0:3,
                          selected = 0)
       ),
       
       column(3,
              h2("data source for control chart"),
              dataTableOutput("allData"),
              selectInput("tableType", "what type of file to download for the data",
                          choice = c(".csv")),
              downloadButton("downloadTable", "Download")
       )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
    #### Prepare data as reactive function
    CreateData <- reactive({
        
        ### restrict to group and hosp
        dataRes <- dataUse[dataUse[, grp] == input$grpIdx &
                           dataUse[, hospID] == as.character(input$hospId), ]
        
        ### apply physician restriction if not "all"
        if (input$physId != "All") {
            dataRes <- dataRes[dataRes[, physID] == as.character(input$physId), ]
        }
        
        ### roll up cases by year month
        ### first create the year month field
        dataRes$yearMonth <- format(dataRes[, procDate], "%Y%m")
        
        ### restrict on phase I
        dataRes <- dataRes[as.numeric(dataRes$yearMonth) >= 
                           as.numeric(paste0(input$p1_year, input$p1_month)), ]
        
        ### compute the avg rate and cost
        avgAdverseOutcome <- mean(dataRes[, obsAdverseOutcome])
        avgCost <- mean(dataRes[, obsCost])
        
        ### set O=E
        dataRes[, preAdverseOutcome] <- dataRes[, preAdverseOutcome] * (avgAdverseOutcome/ mean(dataRes[, preAdverseOutcome]))
        dataRes[, preCost] <- dataRes[, preCost] * (avgCost/ mean(dataRes[, preCost]))
        
        ### aggregate by yearmonth
        dataTable <- data.table(dataRes[, c("yearMonth", 
                                            obsAdverseOutcome, 
                                            preAdverseOutcome,
                                            obsCost,
                                            preCost)])
        
        dataAgg <- dataTable[, lapply(.SD, sum), by = "yearMonth"]
        dataAgg <- as.data.frame(dataAgg)
        
        ### create newDate
        dataAgg$newDate <- as.Date(paste0(dataAgg[, "yearMonth"], "01"), "%Y%m%d")
        
        ### risk adjust
        dataAgg$RAAdverseOutcome <- (dataAgg[, obsAdverseOutcome]/dataAgg[, preAdverseOutcome]) *avgAdverseOutcome
        dataAgg$RACost <- (dataAgg[, obsCost]/dataAgg[, preCost]) * avgCost
        
        return(list(dataAgg = dataAgg,
                    dataRes = dataRes))
    })
        
    ### plot XMR chart
    output$plot <- renderPlot({
       
        dataAgg <- CreateData()[["dataAgg"]] 
        ### create phase I flag
        dataAgg$phaseI <- dataAgg$yearMonth < as.numeric(paste0(input$p2_year,
                                              input$p2_month))
        PlotXmrControlChart(dataIn = dataAgg,
                            phaseI = "phaseI",
                            var = input$type)

   })
   
    ### add a download function
    output$downloadPlot <- downloadHandler(
        filename = function() {
            physDesc <- ""
            if (input$physId != "All") {
                physDesc <- paste(" ", input$physId)
            }
            paste0(input$type,
                 " Control Chart for ",
                 input$hospId,
                 physDesc,
                 "  ( group ",
                 input$grpIdx,
                 " )",
                 input$plotType)
            },
       
       content = function(file){
           
           dataAgg <- CreateData()[["dataAgg"]] 
           ### create phase I flag
           dataAgg$phaseI <- dataAgg$yearMonth < as.numeric(paste0(input$p2_year,
                                                                   input$p2_month))
           ### set up type
           if(input$plotType == ".pdf"){
               pdf(file, width = 13, height = 6)
           } else {
               set_res  <- 300
               png(file,
                   width  = (650 / 72) * set_res,
                   height = (300 / 72) * set_res,
                   res = set_res)
           }
           
           PlotXmrControlChart(dataIn = dataAgg,
                               phaseI = "phaseI",
                               var = input$type)
           dev.off()
       }
   )
   
    #### C) follow_up analysis
    output$followUpPlot <- renderPlot({
       
        dataRes <- CreateData()[["dataRes"]] 
        dataAgg <- CreateData()[["dataAgg"]]
       
        ### now we need to add a function to let user pick the point
        ### compute the distance
        comDist <- abs(as.numeric(dataAgg[, "newDate"]) - (input$plotClick$x - 15) ) 
       
        ### initialize to no follow up plot
        if ((is.null(input$plotClick$x))) {
           
            return()           
            #### if click too far from points, return nothing
        } else if (min(comDist) > 10) {
           
            return()           
        ### if user picks one point then do the follow up plot
        } else {
            selectIndex <- which.min(comDist)
            selectYearMonth   <- dataAgg[selectIndex, "yearMonth"]
            out    <- dataRes[ (dataRes[, "yearMonth"] == selectYearMonth), ]
           
            ### Generate plot for ao
            if (as.character(input$type) == "RAAdverseOutcome") {
                ### name variable for ggplot
                out$obsAdverseOutcome <- out[, obsAdverseOutcome]
                out$physID <- out[, physID]
                
                p <- ggplot(out, aes(procDate, preAdverseOutcome))    
                p <- p + geom_point(aes(shape  = factor(obsAdverseOutcome),
                                        colour = factor(physID)), size=4)
            ### Generate plot for cost
            } else {
                ### name variable for ggplot
                out$obsCost <- out[, obsCost]
                out$physID <- out[, physID]
                
                p <- ggplot(out, aes(procDate, obsCost))    
                p <- p + geom_point(aes(colour = factor(physID)), size=4)
            }
            return(p)
        }
    })
   

   #### output the data to the shiny panel
   prepareData <- reactive({
       ### first call CreateData
       dataRes <- CreateData()[["dataRes"]] 
       dataAgg <- CreateData()[["dataAgg"]] 
       
       ### decide which field to keep
       finalOut <- c("grp", 
                     physID, 
                     procDate,
                     obsAdverseOutcome,
                     preAdverseOutcome,
                     obsCost,
                     preCost)
       
       if (input$nDiag != 0) {
           finalOut <- c(finalOut, paste0(DIAG_PFX, 1:as.numeric(input$nDiag) ))
           finalOut <- c(finalOut, paste0(POA_PFX, 1:as.numeric(input$nDiag) ))
       }
       
       if (input$nProc != 0) {
           finalOut <- c(finalOut, paste0(PROC_PFX, 1:as.numeric(input$nProc) ))
       }
       
       ### now we need to add a function to let user pick the point
       ### compute the distance
       comDist <- abs(as.numeric(dataAgg[, "newDate"]) - (input$plotClick$x - 15) ) 
       
       ### initialize to all cases
       
       if ((is.null(input$plotClick$x))) {
           
           out <- dataRes[, finalOut]
           
       #### Output all data, if click too far from points
       } else if (min(comDist) > 10) {
           
           out <- dataRes[, finalOut]
           
       ### if user picks one point then just output that point
       } else {
           selectIndex <- which.min(comDist)
           selectYearMonth   <- dataAgg[selectIndex, "yearMonth"]
           out    <- dataRes[ (dataRes[, "yearMonth"] == selectYearMonth), finalOut]
           
       }
       return(out)
   })
   
   #### output the data to the shiny panel
   output$allData <- renderDataTable({
       
        showTable <- prepareData()
       
        ### showing 50 records as default
        options(DT.options = list(pageLength = 50))
       
        ### create initial table
        iniTable <- datatable(showTable, 
                              rownames = FALSE, 
                              filter = "top"
        ) %>% formatCurrency(c(obsCost, preCost), digit = 0
        ) %>% formatRound(c(preAdverseOutcome), digit = 3
        )
       
        ### if showPoa is selected, the highlight the non-POA information 
        if (input$showNonPoa & input$nDiag >= 1) {
            
            ### if only the first diag is shown, then no need to do anything
            ### principal diag is always POA
            if (as.numeric(input$nDiag) == 1) {
                outTable <- iniTable
               
                ### if more than 1 diag are shown, highlight the secondary when
                ### poa is 0
            } else if (as.numeric(input$nDiag) >= 2) {
               
                ### find out the location for poa columns
                poaField <- grep(POA_PFX, names(showTable))
               
                ### if user wants to highlight, then drop POA columns
                iniTable <- datatable(showTable, 
                                      rownames = FALSE,
                                      filter = "top",
                                      options = list(columnDefs = list(list(visible = FALSE,
                                                                            ### because I am not showing rownames so I need to subtract by 1
                                                                            targets = poaField - 1 ))))
               
                outTable <- iniTable %>% formatCurrency(c(obsCost, preCost), digit = 0
                                   ) %>% formatRound(c(preAdverseOutcome), digit = 3
                                   ### highlight secondary based on POA = 0
                                   ) %>% formatStyle(paste0(DIAG_PFX, 2:as.numeric(input$nDiag)),
                                                     paste0(POA_PFX,  2:as.numeric(input$nDiag)),
                                                     backgroundColor = styleEqual(c(0, 1), 
                                                     c("yellow", "white"))
                                   )
            }
            ### if user is not interested in poa, then show the regular table
        } else {
            outTable <- iniTable
        }
       
        return(outTable)
    })
   
    

    #### add a download function for user to download the table
    output$downloadTable <- downloadHandler(
        filename = function() {
            physDesc <- ""
            if (input$physId != "All") {
                physDesc <- paste(" ", input$physId)
            }
            paste0(input$type,
                   " Report for ",
                   input$hospId,
                   physDesc,
                   "  ( group ",
                   input$grpIdx,
                   " )",
                   input$tableType)
            },
        content = function(file) {
            write.table(prepareData(),
                        file,
                        sep = ",",
                        row.names = FALSE
            )}
    )
})

### Define function for xmr chart
PlotXmrControlChart <- function(dataIn,
                                phaseI,
                                var) {
    
    ### restrict to phaseI
    dataPhaseI <- dataIn[dataIn[, phaseI], ]
    center <- mean(dataPhaseI[, var])
    movingRange <- diff(dataPhaseI[, var])
    movingAverage <- mean(abs(movingRange))
    UCL <- center + (1.66) * movingAverage
    LCL <- center - (2.66) * movingAverage
    
    ### guess the range of y-axis
    yRange <- range(1.2*dataIn[, var],
                    0.9*dataIn[, var],
                    LCL - 0.2 * movingAverage,
                    LCL + 0.2 * movingAverage)
    
    ### plot xmr
    plot(dataIn$newDate + 15,
         dataIn[, var],
         type = 'o',
         ylim = yRange,
         pch  = 19,
         cex  = 0.6,
         ylab = var,
         cex.lab = 1.25,
         xlab = "")
    
    title(paste0("XMR Control Chart for ",
                 var),
          line = 2.5, cex.main = 1.5)
    
    #### Add background coloring between UCL and LCL
    lim <- par("usr")
    rect(lim[1], UCL, lim[2], lim[4], col = '#4A708B11')
    rect(lim[1], lim[3], lim[2], LCL, col = '#4A708B11')
    
    #### draw lines for center, UCL and LCL
    abline(h = center)
    abline(h = UCL)
    abline(h = LCL)
    
    ### adding information
    mtext(paste("Center = ", round(center, 2),
                ", UCL = ", round(UCL, 2),
                ", LCL = ", round(LCL, 2), sep = ""), line=3, side=1)
    
    #### draw a line between phase I and II
    abline(v = dataIn$newDate[which.min(dataIn[, phaseI])] )

    ### highlight outliers
    outlierIndex <- which((dataIn[, var] < LCL) | (dataIn[, var] > UCL))
    if(length(outlierIndex) > 0){
        points(dataIn$newDate[outlierIndex] + 15,
               dataIn[outlierIndex, var],
               col = "red",
               cex = 1.5,
               lwd = 2)
    }
    
}

# Run the application 
shinyApp(ui = ui, server = server)

