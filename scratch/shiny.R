library(shiny)

ui <- fillPage(
  plotOutput("tsplot", height = "100%", width = "100%")
)

server <- function(input, output, session) {
  ts_data <- dummyts

  i1 <- 1
  i2 <- nrow(ts_data)
  t1 <- ts_data$t[i1]
  t2 <- ts_data$t[i2]

  max_res <- 1e3
  i_decimated <- floor(seq(i1, i2, length.out = min(i2 - i1 + 1, max_res)))
  zoom_data <- ts_data[i_decimated, ]

  output$tsplot <- renderPlot(plot(y ~ t, zoom_data, type = "l"))
}

shinyApp(ui, server)
