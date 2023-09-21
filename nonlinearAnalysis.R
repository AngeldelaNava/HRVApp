timeLag <- function(input, output, session, hrv.data, kTimeLag){
  output$time_lag <- renderPlot({
    CalculateTimeLag(
      hrv.data, method = "first.minimum", lagMax = input$lagMax,
      doPlot = TRUE)
  })
}

embeddingDim <- function(input, output, session, hrv.data, kTimeLag,
                         kEmbeddingDim){
  output$emb_dim <- renderPlot({
    CalculateEmbeddingDim(
      hrv.data, numberPoints = input$numberPoints, timeLag = kTimeLag,
      maxEmbeddingDim = input$maxEmbeddingDim, doPlot = TRUE)
  })
}

correlationDimensionCalculation <- function(output, hrv.data){
  output$corr_plot <- renderPlot({
    PlotCorrDim(hrv.data, indexNonLinearAnalysis = 1)
  })
}

correlationDimensionEstimation <- function(input, output, kEmbeddingDim,
                                           hrv.data){
  output$corr_plot <- renderPlot({
    EstimateCorrDim(
      hrv.data, indexNonLinearAnalysis = 1,
      regressionRange = c(input$minRegC, input$maxRegC),
      useEmbeddings = (kEmbeddingDim - 1):(kEmbeddingDim + 2),
      doPlot = TRUE)
  })
}
correlatonStatistic <- function(output, hrv.data){
  output$corr_text <- renderText({
    hrv.data$NonLinearAnalysis[[1]]$correlation$statistic
  })
}

lyapunovCalculation <- function(output, hrv.data){
  output$lya_plot <- renderPlot({
    PlotMaxLyapunov(hrv.data, indexNonLinearAnalysis = 1)
  })
}

lyapunovEstimation <- function(input, output, kEmbeddingDim, hrv.data){
  output$lya_plot <- renderPlot({
    EstimateMaxLyapunov(
      hrv.data, indexNonLinearAnalysis = 1,
      regressionRange = c(input$minRegL, input$maxRegL),
      useEmbeddings = (kEmbeddingDim):(kEmbeddingDim + 2),
      doPlot = TRUE)
  })
}

lyapunovStatistic <- function(output, hrv.data){
  output$lya_text <- renderText({
    hrv.data$NonLinearAnalysis[[1]]$lyapunov$statistic
  })
}