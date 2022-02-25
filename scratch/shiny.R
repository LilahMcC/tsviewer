library(ggplot2)
library(patchwork)
library(shiny)

ts_data <- dummyts
ts_data$y2 <- dummyts$y * 100

ui <- fillPage(
  fillRow(
    plotOutput("tsplot", height = "100%", width = "100%"),
    height = "90%"
  ),
  fillRow(
    div(),
    sliderInput("tsslider",
                label = NULL,
                min = min(ts_data$t),
                max = max(ts_data$t),
                value = range(ts_data$t),
                width = "100%"),
    div(),
    flex = c(1, 10, 1),
    height = "10%"
  )
)

server <- function(input, output, session) {
  i1 <- 1
  i2 <- nrow(ts_data)
  t1 <- ts_data$t[i1]
  t2 <- ts_data$t[i2]

  max_res <- 1e3
  i_decimated <- floor(seq(i1, i2, length.out = min(i2 - i1 + 1, max_res)))
  i_zoomed <- floor(seq(i1, i2, length.out = min(i2 - i1 + 1, max_res)))

  ts_decimated <- ts_data[i_decimated, ]
  ts_zoomed <- ts_data[i_decimated, ]

  p1 <- ggplot(ts_decimated, aes(t, y)) +
    geom_line() +
    theme_minimal()
  p2 <- ggplot(ts_zoomed, aes(t, y)) +
    geom_line() +
    theme_minimal()
  p3 <- ggplot(ts_zoomed, aes(t, y2)) +
    geom_line() +
    theme_minimal()
  p <- p1 / p2 / p3 + plot_layout(heights = c(2, 6, 2))

  output$tsplot <- renderPlot(p)
}

shinyApp(ui, server)
