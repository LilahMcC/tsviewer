library(ggplot2)
library(patchwork)
library(shiny)

ui <- fillPage(
  plotOutput("tsplot", height = "100%", width = "100%")
)

server <- function(input, output, session) {
  ts_data <- dummyts
  ts_data$y2 <- dummyts$y * 100

  i1 <- 1
  i2 <- nrow(ts_data)
  t1 <- ts_data$t[i1]
  t2 <- ts_data$t[i2]

  max_res <- 1e3
  i_decimated <- floor(seq(i1, i2, length.out = min(i2 - i1 + 1, max_res)))

  zoom_data <- ts_data[i_decimated, ]

  p1 <- ggplot(zoom_data, aes(t, y)) +
    geom_line() +
    theme_minimal()
  p2 <- ggplot(zoom_data, aes(t, y2)) +
    geom_line() +
    theme_minimal()
  p <- p1 | p2

  output$tsplot <- renderPlot(p)
}

shinyApp(ui, server)
