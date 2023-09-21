source("hrv.R")
source("tabsControl.R")
source("inputOutputControl.R")

server <- function(input, output, session) {
  
  observe({
    tabsControl(input, output, session)
  })
  
  observe({
  inputOutputControl(input, output, session, hrv.data)
  })
}