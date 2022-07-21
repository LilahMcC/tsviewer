#' Run the time series viewer
#'
#' @param df `[data.frame]` With depth, acceleration, and derived variables.
#'   Must have columns: \itemize{ \item{DateTime `[POSIXct]`} \item{acc_x
#'   `[double]`} \item{acc_y `[double]`} \item{acc_z `[double]`} \item{anorm
#'   `[double]`} \item{pitch `[double]`} \item{roll `[double]`} \item{depth
#'   `[double]`} }
#' @param id `[character(1)]` Identifier for this animal.
#' @param tz `[character(1)]` Timezone  for this animal.
#'
#' @export
#' @importFrom magrittr %>%
run_tsviewer <- function(df, id, tz) {
  ts_data <- df %>%
    dplyr::transmute(t = DateTime,
                     anorm = anorm,
                     depth = -depth,
                     pitch = pitch/pi*180,
                     roll = roll/pi*180)
  runApp(list(
    ui = shiny_ui(ts_data, id),
    server = shiny_server(ts_data)
  ))
}

#' Shiny UI (internal)
#'
#' @return Shiny UI object
#' @import shiny
#' @noRd
shiny_ui <- function(ts_data, id) {
  fillPage(
    fillRow(
      fillCol(
        plotOutput("depth1",
                   height = "100%", width = "100%",
                   brush = "depth1_brush"),
        plotOutput("depth2",
                   height = "100%", width = "100%",
                   brush = "depth2_brush"),
        plotOutput("anorm",
                   height = "100%", width = "100%",
                   click = "anorm_click"),
        plotOutput("pitch",
                   height = "100%", width = "100%",
                   click = "pitch_click"),
        plotOutput("roll",
                   height = "100%", width = "100%",
                   click = "roll_click"),
        flex = c(1, 1, 2, 2, 2)
      ),
      height = "90%"
    ),
    fillRow(
      column(width = 12, align = "center", textOutput("tstime")),
      height = "10%"
    ),
    tags$head(tags$style("#tstime{color: red;
                                  font-size: 18px;}")),
    title = id
  )
}

#' Shiny server (internal)
#'
#' @param ts_data data frame with columns t, depth, anorm, pitch, and roll.
#'
#' @return Shiny server function
#' @import ggplot2
#' @noRd
shiny_server <- function(ts_data) {
  function(input, output, session) {
    to_posixct <- function(x) {
      as.POSIXct(x, tz = lubridate::tz(ts_data$t), origin = "1970-01-01")
    }

    # Decimation for overall depth profile
    max_res <- 1e4
    i_decimated <- floor(
      seq(1, nrow(ts_data), length.out = min(nrow(ts_data), max_res))
    )
    data_decimated <- ts_data[i_decimated, ]

    # Decimation and zooming
    i_zoom1 <- reactiveValues(i1 = 1, i2 = nrow(ts_data))
    observe(if (!is.null(input$depth1_brush)) {
      xmin <- max(input$depth1_brush$xmin, min(ts_data$t))
      xmax <- min(input$depth1_brush$xmax, max(ts_data$t))
      i <- approx(ts_data$t,
                  seq_along(ts_data$t),
                  xout = c(xmin, xmax),
                  method = "constant")$y
      i_zoom1$i1 <- i[1]
      i_zoom1$i2 <- i[2]
      i_zoom2$i1 <- i[1]
      i_zoom2$i2 <- i[2]
    })
    data_zoomed1 <- reactive({
      i_zoomed <- floor(
        seq(i_zoom1$i1,
            i_zoom1$i2,
            length.out = min(i_zoom1$i2 - i_zoom1$i1 + 1, max_res))
      )
      ts_data[i_zoomed, ]
    })

    i_zoom2 <- reactiveValues(i1 = 1, i2 = nrow(ts_data))
    observe(if (!is.null(input$depth2_brush)) {
      xmin <- max(input$depth2_brush$xmin, min(ts_data$t))
      xmax <- min(input$depth2_brush$xmax, max(ts_data$t))
      i <- approx(ts_data$t,
                  seq_along(ts_data$t),
                  xout = c(xmin, xmax),
                  method = "constant")$y
      i_zoom2$i1 <- i[1]
      i_zoom2$i2 <- i[2]
    })
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
    observe(
      for (click in c("anorm_click", "pitch_click", "roll_click")) {
        if (!is.null(input[[click]])) {
          tclick(to_posixct(input[[click]]$x))
        }
      }
    )
    click_data <- reactiveVal(ts_data[0,])
    observe({
      if (length(tclick()) > 0 &&
          tclick() > min(data_zoomed2()$t) &&
          tclick() < max(data_zoomed2()$t)) {
            i <- approx(data_zoomed2()$t,
                        seq_along(data_zoomed2()$t),
                        xout = tclick(),
                        method = "constant")$y
            click_data(data_zoomed2()[i, ])
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

    output$anorm <- renderPlot({
      ggplot(data_zoomed2(), aes(t, anorm)) +
        geom_line() +
        geom_point(data = click_data(), color = "red", size = 4) +
        coord_cartesian(xlim = range(data_zoomed2()$t), ylim = c(-2, 2)) + #setting limits on y axis seems to not be working
        theme_minimal()
    })

    output$pitch <- renderPlot({
      ggplot(data_zoomed2(), aes(t, pitch)) +
        geom_line() +
        geom_point(data = click_data(), color = "red", size = 4) +
        coord_cartesian(xlim = range(data_zoomed2()$t), ylim = c(-60, 60)) + # here too
        theme_minimal()
    })

    output$roll <- renderPlot({
      ggplot(data_zoomed2(), aes(t, roll)) +
        geom_line() +
        geom_point(data = click_data(), color = "red", size = 4) +
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
