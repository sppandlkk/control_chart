### load packages
library(shiny)
library(data.table)

### read in data
setwd("/Users/sppandlkk/GitHub/control_chart/")
dataUse <- read.table("./data/episode_data.csv",
                      header = TRUE,
                      check = FALSE,
                      sep = ",",
                      stringsAsFactors = FALSE)
### deal with data
dataUse$procDate <- as.Date(dataUse$procDate, "%Y-%m-%d")

### sort by procedure data
dataUse <- dataUse[order(dataUse$procDate), ]


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
                          selected = 2017)
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
                          selected = "01")
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
              checkboxInput("poa", "Highlight POA", FALSE),
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
        dataRes <- dataUse[dataUse$grp == input$grpIdx &
                           dataUse$hospID == as.character(input$hospId), ]
        
        ### apply physician restriction if not "all"
        if (input$physId != "All") {
            dataRes <- dataRes[dataRes$physID == as.character(input$physId), ]
        }
        
        
        ### roll up cases by year month
        ### first create the year month field
        dataRes$yearMonth <- format(dataRes$procDate, "%Y%m")
        
        ### restrict on phase I
        dataRes <- dataRes[as.numeric(dataRes$yearMonth) >= 
                           as.numeric(paste0(input$p1_year, input$p1_month)), ]
        
        ### compute the avg rate and cost
        avgAdverseOutcome <- mean(dataRes$obsAdverseOutcome)
        avgCost <- mean(dataRes$obsCost)
        
        ### set O=E
        dataRes$preAdverseOutcome <- dataRes$preAdverseOutcome * (avgAdverseOutcome/ mean(dataRes$preAdverseOutcome))
        dataRes$preCost <- dataRes$preCost * (avgCost/ mean(dataRes$preCost))
        
        
        
        ### aggregate by yearmonth
        dataTable <- data.table(dataRes[, c("yearMonth", 
                                            "obsAdverseOutcome", 
                                            "preAdverseOutcome",
                                            "obsCost",
                                            "preCost")])
        
        dataAgg <- dataTable[, lapply(.SD, sum), by = "yearMonth"]
        dataAgg <- as.data.frame(dataAgg)
        
        ### create newDate
        dataAgg$newDate <- as.Date(paste0(dataAgg[, "yearMonth"], "01"), "%Y%m%d")
        
        ### risk adjust
        dataAgg$RAAdverseOutcome <- (dataAgg$obsAdverseOutcome/dataAgg$preAdverseOutcome) *avgAdverseOutcome
        dataAgg$RACost <- (dataAgg$obsCost/dataAgg$preCost) * avgCost
        
        return(list(dataAgg = dataAgg,
                    dataRes = dataRes))
    })
        
        
    
    
   output$plot <- renderPlot({
       
        dataAgg <- CreateData()[["dataAgg"]] 
        ### create phase I flag
        dataAgg$phaseI <- dataAgg$yearMonth < as.numeric(paste0(input$p2_year,
                                              input$p2_month))
        PlotXmrControlChart(dataIn = dataAgg,
                            phaseI = "phaseI",
                            var = input$type)

   })
   
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
       } else if (min(comDist) > 7) {
           
           return()           
       ### if user picks one point then do the follow up plot
       } else {
           selectIndex <- which.min(comDist)
           selectYearMonth   <- dataAgg[selectIndex, "yearMonth"]
           out    <- dataRes[ (dataRes[, "yearMonth"] == selectYearMonth), ]
           
           # Generate plot for ao
           if (as.character(input$type) == "RAAdverseOutcome") {
               p <- ggplot(out, aes(procDate, preAdverseOutcome))    
               p <- p + geom_point(aes(shape  = factor(obsAdverseOutcome),
                                       colour = factor(physID)), size=4)
           } else {
               p <- ggplot(out, aes(procDate, obsCost))    
               p <- p + geom_point(aes(
                                       colour = factor(physID)), size=4)
           }
           return(p)
           
       }
       
       
       #### At start when no selection, output everything
       if ((is.null(input$plot_click$x))) {
           
           #### Output all data, if click too far from points
       } else if (min((data_source[, 1] - input$plot_click$x)^2) > 0.25) {
           return()
       } else {
           select_index <- which.min((data_source[,1] - input$plot_click$x)^2)
           select_eid   <- data_source[ select_index, "roll_up_eid"]
           final_out    <- data_raw[ (data_raw$roll_up_eid == select_eid) ,]
           final_out[, !names(final_out) == "roll_up_eid"]
           
           cat_type <- rep(" Routine", nrow(final_out))
           cat_type[as.logical(final_out[, input$dpdt_var])] <- "Adverse Event"
           
           data_for_bx <- data.frame(
               pred_ao_rate   = final_out[, paste("p_", input$dpdt_var, sep="")],
               cat_type       = cat_type,
               phys           = final_out[, "surg_phys_1"],
               procedure_date = final_out[, "PROCEDURE_DATE"])
           
           # Generate plot
           p <- ggplot(data_for_bx, aes(procedure_date, pred_ao_rate))
           p <- p + geom_point(aes(shape  = factor(cat_type),
                                   colour = factor(phys)), size=4)
           p
           
       }
   })
   
   
   
   
   
   #### output the data to the shiny panel
   prepareData <- reactive({
       dataRes <- CreateData()[["dataRes"]] 
       dataAgg <- CreateData()[["dataAgg"]] 
       
       ### decide which field to keep
       finalOut <- colnames(dataRes)[c(2, 4:11)]
       
       if (input$nDiag != 0) {
           finalOut <- c(finalOut, paste0("diag_", 1:as.numeric(input$nDiag) ))
       }
       
       if (input$nProc != 0) {
           finalOut <- c(finalOut, paste0("proc_", 1:as.numeric(input$nProc) ))
       }
       
       ### now we need to add a function to let user pick the point
       ### compute the distance
       comDist <- abs(as.numeric(dataAgg[, "newDate"]) - (input$plotClick$x - 15) ) 
       
       ### initialize to all cases
       
       if ((is.null(input$plotClick$x))) {
           
           out <- dataRes[, finalOut]
           
       #### Output all data, if click too far from points
       } else if (min(comDist) > 7) {
           
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
       prepareData()
   })

   #### add a download function for user to download the table
   output$downloadTable <- downloadHandler(
       filename = function(){
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
       content = function(file){
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
                                var
                                ) {
    
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

