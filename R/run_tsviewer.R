#' Run the time series viewer
#'
#' @param prh A PRH object. See catsr::read_nc().
#'
#' @export
#' @importFrom magrittr %>%
run_tsviewer <- function(prh) {
  ts_data <- prh %>%
    dplyr::transmute(t = dt, depth = -p, ygyro = gw[, 2], speed)
  runApp(list(
    ui = shiny_ui(ts_data),
    server = shiny_server(ts_data)
  ))
}

#' Shiny UI (internal)
#'
#' @return Shiny UI object
#' @import shiny
#' @noRd
shiny_ui <- function(ts_data) {
  fillPage(
    fillRow(
      fillCol(
        plotOutput("depth1",
                   height = "100%", width = "100%",
                   brush = "depth1_brush"),
        plotOutput("depth2",
                   height = "100%", width = "100%",
                   brush = "depth2_brush"),
        plotOutput("depth3",
                   height = "100%", width = "100%",
                   click = "depth3_click"),
        plotOutput("speed",
                   height = "100%", width = "100%",
                   click = "speed_click"),
        plotOutput("ygyro",
                   height = "100%", width = "100%",
                   click = "ygyro_click"),
        flex = c(1, 1, 2, 2, 2)
      ),
      height = "90%"
    ),
    fillRow(
      textOutput("tstime"),
      height = "10%"
    )
  )
}

#' Shiny server (internal)
#'
#' @param ts_data data frame with columns t, depth, ygyro, and speed.
#'
#' @return Shiny server function
#' @import ggplot2
#' @noRd
shiny_server <- function(ts_data) {
  function(input, output, session) {
    to_posixct <- function(x) as.POSIXct(x, tz = "UTC", origin = "1970-01-01")

    # Decimation for overall depth profile
    max_res <- 1e4
    i_decimated <- floor(
      seq(1, nrow(ts_data), length.out = min(nrow(ts_data), max_res))
    )
    data_decimated <- ts_data[i_decimated, ]

    # Decimation and zooming
    i_zoom1 <- reactiveValues(i1 = 1, i2 = nrow(ts_data))
    # observe(if (!is.null(input$depth1_brush)) {
    #   i_zoom1$i <- which.min(abs(input$depth1_brush$x))
    # })
    data_zoomed1 <- reactive({
      i_zoomed <- floor(
        seq(i_zoom1$i1,
            i_zoom1$i2,
            length.out = min(i_zoom1$i2 - i_zoom1$i1 + 1, max_res))
      )
      ts_data[i_zoomed, ]
    })
    i_zoom2 <- reactiveValues(i1 = 1, i2 = nrow(ts_data))
    data_zoomed2 <- reactive({
      i_zoomed <- floor(
        seq(i_zoom2$i1,
            i_zoom2$i2,
            length.out = min(i_zoom2$i2 - i_zoom2$i1 + 1, max_res))
      )
      ts_data[i_zoomed, ]
    })


    # Time at click
    tclick <- reactiveVal()
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

    output$depth1 <- renderPlot({
      ggplot(data_decimated, aes(t, depth)) +
        geom_line() +
        annotate("rect",
                 xmin = data_zoomed1()$t[1],
                 xmax = data_zoomed1()$t[nrow(data_zoomed1())],
                 ymin = -Inf,
                 ymax = Inf,
                 fill = "black",
                 color = NA,
                 alpha = 0.25) +
        theme_minimal()
    })

    output$depth2 <- renderPlot({
      ggplot(data_zoomed1(), aes(t, depth)) +
        geom_line() +
        annotate("rect",
                 xmin = data_zoomed2()$t[1],
                 xmax = data_zoomed2()$t[nrow(data_zoomed1())],
                 ymin = -Inf,
                 ymax = Inf,
                 fill = "black",
                 color = NA,
                 alpha = 0.25) +
        coord_cartesian(xlim = range(data_zoomed1()$t)) +
        theme_minimal()
    })

    output$depth3 <- renderPlot({
      ggplot(data_zoomed2(), aes(t, depth)) +
        geom_line() +
        # geom_point(data = click_data(), color = "red", size = 4) +
        coord_cartesian(xlim = range(data_zoomed2()$t)) +
        theme_minimal()
    })

    output$speed <- renderPlot({
      ggplot(data_zoomed2(), aes(t, speed)) +
        geom_line() +
        # geom_point(data = click_data(), color = "red", size = 4) +
        coord_cartesian(xlim = range(data_zoomed2()$t)) +
        theme_minimal()
    })

    output$ygyro <- renderPlot({
      ggplot(data_zoomed2(), aes(t, ygyro)) +
        geom_line() +
        # geom_point(data = click_data(), color = "red", size = 4) +
        coord_cartesian(xlim = range(data_zoomed2()$t)) +
        theme_minimal()
    })

    output$tstime <- renderText({
      if (!is.null(tclick())) {
        format(tclick(), "%Y-%m-%d %H:%M:%OS1")
      } else {
        ""
      }
    })
  }
}
