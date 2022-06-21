#' Run the time series viewer
#'
#' @param prh A PRH object. See catsr::read_nc().
#' @param resume Resume progress flag. If `TRUE`, searches for deployment
#'   workbook in the Phase III shared Google Drive folder.
#'
#' @export
#' @importFrom magrittr %>%
run_tsviewer <- function(prh, resume = FALSE) {
  ts_data <- prh %>%
    dplyr::transmute(t = dt, depth = -p, ygyro = gw[, 2], speed)
  if (resume) {
    if (!googlesheets4::gs4_has_token()) {
      googledrive::drive_auth(
        scopes = "https://www.googleapis.com/auth/drive.readonly"
      )
    }
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    phaseiii <- read_phaseiii(attr(prh, "whaleid"))
    phaseiii$motionlessstart <- lubridate::force_tz(
      phaseiii$motionlessstart,
      attr(prh, "tz")
    )
    phaseiii$motionlessend <- lubridate::force_tz(
      phaseiii$motionlessend,
      attr(prh, "tz")
    )
  } else {
    phaseiii <- data.frame(
      deployid = character(),
      motionlessid = double(),
      motionlessstart = double(),
      motionlessend = double(),
      duration_s = double()
    )
  }
  runApp(list(
    ui = shiny_ui(ts_data, attr(prh, "whaleid")),
    server = shiny_server(ts_data, attr(prh, "tz"), phaseiii)
  ))
}

#' Shiny UI (internal)
#'
#' @return Shiny UI object
#' @import shiny
#' @noRd
shiny_ui <- function(ts_data, deployid) {
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
      column(width = 12, align = "center", textOutput("tstime")),
      height = "10%"
    ),
    tags$head(tags$style("#tstime{color: red;
                                  font-size: 18px;}")),
    title = deployid
  )
}

#' Shiny server (internal)
#'
#' @param ts_data data frame with columns t, depth, ygyro, and speed.
#'
#' @return Shiny server function
#' @import ggplot2
#' @noRd
shiny_server <- function(ts_data, ts_tz, phaseiii) {
  function(input, output, session) {
    to_posixct <- function(x) as.POSIXct(x, tz = ts_tz, origin = "1970-01-01")

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
    phaseiii_zoomed1 <- reactive({
      phaseiii %>%
        dplyr::filter(
          motionlessstart < ts_data$t[i_zoom1$i2],
          motionlessend > ts_data$t[i_zoom1$i1]
        ) %>%
        dplyr::mutate(
          motionlessstart = pmax(motionlessstart, ts_data$t[i_zoom1$i1]),
          motionlessend = pmax(motionlessend, ts_data$t[i_zoom1$i2])
        )
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
    phaseiii_zoomed2 <- reactive({
      phaseiii %>%
        dplyr::filter(
          motionlessstart < ts_data$t[i_zoom2$i2],
          motionlessend > ts_data$t[i_zoom2$i1]
        ) %>%
        dplyr::mutate(
          motionlessstart = pmax(motionlessstart, ts_data$t[i_zoom2$i1]),
          motionlessend = pmin(motionlessend, ts_data$t[i_zoom2$i2])
        )
    })


    # Time at click
    tclick <- reactiveVal()
    observe(
      for (click in c("depth3_click", "speed_click", "ygyro_click")) {
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
        geom_vline(aes(xintercept = motionlessstart),
                   phaseiii,
                   color = "red") +
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
        geom_vline(aes(xintercept = motionlessstart),
                   phaseiii_zoomed1(),
                   color = "red") +
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
        geom_rect(aes(xmin = motionlessstart,
                      xmax = motionlessend),
                  phaseiii_zoomed2(),
                  inherit.aes = FALSE,
                  ymin = -Inf,
                  ymax = Inf,
                  fill = "red",
                  alpha = 0.25) +
        geom_point(data = click_data(), color = "red", size = 4) +
        coord_cartesian(xlim = range(data_zoomed2()$t)) +
        theme_minimal()
    })

    output$speed <- renderPlot({
      ggplot(data_zoomed2(), aes(t, speed)) +
        geom_line() +
        geom_rect(aes(xmin = motionlessstart,
                      xmax = motionlessend),
                  phaseiii_zoomed2(),
                  inherit.aes = FALSE,
                  ymin = -Inf,
                  ymax = Inf,
                  fill = "red",
                  alpha = 0.25) +
        geom_point(data = click_data(), color = "red", size = 4) +
        coord_cartesian(xlim = range(data_zoomed2()$t)) +
        theme_minimal()
    })

    output$ygyro <- renderPlot({
      ggplot(data_zoomed2(), aes(t, ygyro)) +
        geom_line() +
        geom_rect(aes(xmin = motionlessstart,
                      xmax = motionlessend),
                  phaseiii_zoomed2(),
                  inherit.aes = FALSE,
                  ymin = -Inf,
                  ymax = Inf,
                  fill = "red",
                  alpha = 0.25) +
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
