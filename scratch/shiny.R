library(ggplot2)
library(patchwork)
library(shiny)

ts_data <- dummyts
ts_data$y2 <- dummyts$y * 100

ui <- fillPage(
  fillRow(
    fillCol(
      plotOutput("ts0", height = "100%", width = "100%"),
      plotOutput("ts1", height = "100%", width = "100%", click = "tsclick1"),
      plotOutput("ts2", height = "100%", width = "100%", click = "tsclick2"),
      flex = c(2, 7, 3)
    ),
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
    textOutput("tstime"),
    flex = c(1, 7, 1, 3),
    height = "10%"
  )
)

server <- function(input, output, session) {
  max_res <- 1e3
  i_decimated <- floor(
    seq(1, nrow(ts_data), length.out = min(nrow(ts_data), max_res))
  )
  ts_decimated <- ts_data[i_decimated, ]

  ts_zoomed <- reactive({
    i1 <- which.min(abs(ts_data$t - input$tsslider[1]))
    i2 <- which.min(abs(ts_data$t - input$tsslider[2]))
    i_zoomed <- floor(
      seq(i1, i2, length.out = min(i2 - i1 + 1, max_res))
    )
    ts_data[i_zoomed, ]
  })

  tclick <- reactiveVal()
  to_posixct <- function(x) as.POSIXct(x, tz = "UTC", origin = "1970-01-01")
  observe(if (!is.null(input$tsclick1)) {
    tclick(to_posixct(input$tsclick1$x))
  })
  observe(if (!is.null(input$tsclick2)) {
    tclick(to_posixct(input$tsclick2$x))
  })
  click_data <- reactiveVal(ts_data[0,])
  observe({
    if (length(tclick()) > 0) {
      if (tclick() > min(ts_zoomed()$t) && tclick() < max(ts_zoomed()$t)) {
        click_data(
          data.frame(
            t = tclick(),
            y = approx(ts_zoomed()$t, ts_zoomed()$y, xout = tclick())$y,
            y2 = approx(ts_zoomed()$t, ts_zoomed()$y2, xout = tclick())$y
          )
        )
      }
    }
  })

  output$ts0 <- renderPlot({
    ggplot(ts_decimated, aes(t, y)) +
      geom_line() +
      annotate("rect",
               xmin = ts_zoomed()$t[1],
               xmax = ts_zoomed()$t[nrow(ts_zoomed())],
               ymin = -Inf,
               ymax = Inf,
               fill = "black",
               color = NA,
               alpha = 0.25) +
      theme_minimal()
  })

  output$ts1 <- renderPlot({
    ggplot(ts_zoomed(), aes(t, y)) +
      geom_line() +
      geom_point(data = click_data(), color = "red", size = 4) +
      coord_cartesian(xlim = range(ts_zoomed()$t)) +
      theme_minimal()
  })

  output$ts2 <- renderPlot({
    ggplot(ts_zoomed(), aes(t, y2)) +
      geom_line() +
      geom_point(data = click_data(), color = "red", size = 4) +
      coord_cartesian(xlim = range(ts_zoomed()$t)) +
      theme_minimal()
  })

  output$tstime <- renderText({
    if (!is.null(tclick())) {
      format(tclick(), "%Y-%m-%d %H:%M:%S")
    } else {
      ""
    }
  })
}

shinyApp(ui, server)
