timeAnalysis <- function(input, output, session, hrv.data){
  
  output$graphic <- renderPlot({
    tryCatch({
      PlotNIHR(hrv.data)
    },
    error = function(e) {
      cat("Error: plot failed\n")
      stop(safeError(e))
    })
  })
  
  CHARACTERISTICS = c("Size", "Interval", "SDNN (msec.)", "SDANN (msec.)",
                      "SDNNIDX (msec.)", "pNN50 (%)", "SDSD (msec.)",
                      "r-MSSD (msec.)", "IRRR (msec.)", "MADRR (msec.)",
                      "TINN (msec.)", "HRV index")
  VALUES = c(input$size, input$interval, hrv.data$TimeAnalysis[[1]]$SDNN,
             hrv.data$TimeAnalysis[[1]]$SDANN,
             hrv.data$TimeAnalysis[[1]]$SDNNIDX,
             hrv.data$TimeAnalysis[[1]]$pNN50,
             hrv.data$TimeAnalysis[[1]]$SDSD,
             hrv.data$TimeAnalysis[[1]]$rMSSD,
             hrv.data$TimeAnalysis[[1]]$IRRR,
             hrv.data$TimeAnalysis[[1]]$MADRR,
             hrv.data$TimeAnalysis[[1]]$TINN,
             hrv.data$TimeAnalysis[[1]]$HRVi)
  
  output$time_analysis <- renderTable({
    df <- t(data.frame(VALUES))
    colnames(df) <- CHARACTERISTICS
    df
  })
  
}