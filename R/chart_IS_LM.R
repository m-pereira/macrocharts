

#' Create a shiny app with IS-LM chart
#' @return It returns a interactive \link[shiny]{shinyApp}
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom plotly ggplotly
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyOutput
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @import shinythemes
#' @import shinydashboard
#' @import ggthemes
#' @examples
#'\dontrun{
#'chart_IS_LM()
#'}
chart_IS_LM <- function() {


  ### UI function  -------------------
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("cerulean"),

    # Application design
    shiny::titlePanel("specification IS-LM"),
    shiny::fluidRow(shiny::column(11, shiny::h4("Parameters for IS-LM withouth change"))),
    shiny::fluidRow(
      shiny::column(2, shiny::sliderInput("breaks", shiny::withMathJax("$$\\hspace{1cm}\\textbf{breaks on axis}$$"), value = 0.1, min = 0.001, max = 1)), # slides
      shiny::column(1, shiny::sliderInput("c1", shiny::withMathJax("$$\\hspace{1cm}\\textbf{c}$$"), value = 0.8, min = 0, max = 1)), # propensao marginal a consumir
      shiny::column(1, shiny::sliderInput("t1", shiny::withMathJax("$$\\hspace{1cm}\\textbf{t}$$"), value = 0.25, min = 0, max = 1)), # Imposto de Income proporcional
      shiny::column(1, shiny::sliderInput("I_01", shiny::withMathJax("$$\\hspace{2cm} I_0$$"), value = 900, min = 0, max = 2000)),
      shiny::column(1, shiny::sliderInput("b1", shiny::withMathJax("$$\\hspace{2cm}\\textbf{b}$$"), value = 50, min = 0, max = 100)),
      shiny::column(1, shiny::sliderInput("G1", shiny::withMathJax("$$\\hspace{2cm} G_0$$"), value = 800, min = 0, max = 1600)),
      shiny::column(1, shiny::sliderInput("k1", shiny::withMathJax("$$\\hspace{1cm}\\textbf{k}$$"), value = 0.25, min = 0, max = 1)), # sensibilidade da demanda por saldos reais em relacao ao nivel de Income # L= 0,25 Y - 62,5i
      shiny::column(1, shiny::sliderInput("h1", shiny::withMathJax("$$\\hspace{1cm}\\textbf{h}$$"), value = 62.5, min = 0, max = 100)), # sensibilidade da demanda por saldos reais em relacao a Interest Rate
      shiny::column(1, shiny::sliderInput("MP1", shiny::withMathJax("$$\\hspace{2cm}\\bar{M}/P$$"), value = 500, min = 0, max = 1000)) # oferta real de moeda
    ),
    shiny::fluidRow(shiny::column(9, shiny::h4("Parameters for IS-LM with change"))),
    shiny::fluidRow(
      #      shiny::column(1, shiny::sliderInput("x_axis_0", shiny::withMathJax("$$\\hspace{1cm}\\textbf{axis}$$"), value = 20, min = 0, max = 40)), # slides
      shiny::column(1, shiny::sliderInput("c", shiny::withMathJax("$$\\hspace{1cm}\\textbf{c}$$"), value = 0.8, min = 0, max = 1)),
      shiny::column(1, shiny::sliderInput("t", shiny::withMathJax("$$\\hspace{1cm}\\textbf{t}$$"), value = 0.25, min = 0, max = 1)),
      shiny::column(1, shiny::sliderInput("I_0", shiny::withMathJax("$$\\hspace{2cm} I_0$$"), value = 900, min = 0, max = 2000)),
      shiny::column(1, shiny::sliderInput("b", shiny::withMathJax("$$\\hspace{2cm}\\textbf{b}$$"), value = 50, min = 0, max = 100)),
      shiny::column(1, shiny::sliderInput("G", shiny::withMathJax("$$\\hspace{2cm} G_0$$"), value = 800, min = 0, max = 1600)),
      shiny::column(1, shiny::sliderInput("k", shiny::withMathJax("$$\\hspace{1cm}\\textbf{k}$$"), value = 0.25, min = 0, max = 1)), # sensibilidade da demanda por saldos reais em relacao ao nivel de Income # L= 0,25 Y - 62,5i
      shiny::column(1, shiny::sliderInput("h", shiny::withMathJax("$$\\hspace{1cm}\\textbf{h}$$"), value = 62.5, min = 0, max = 100)), # sensibilidade da demanda por saldos reais em relacao a Interest Rate
      shiny::column(1, shiny::sliderInput("MP", shiny::withMathJax("$$\\hspace{2cm}\\bar{M}/P$$"), value = 500, min = 0, max = 1000)) # oferta real de moeda
    ),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Dynamic do IS-LM",
        shiny::fluidRow(
          shiny::column(6, shiny::dataTableOutput("TABLE")),
          shiny::column(6, plotly::plotlyOutput("GrafTotal"))
        )
      ),
      shiny::tabPanel(
        "specification IS'-LM'",
        shiny::fluidRow(shiny::column(12, plotly::plotlyOutput("Graindiv")))
      )
    )
  )

  ### Server function -------------------
  server <- function(input, output, session) {
    # a workaound for warning about visible binding for global variable
    vari <- vari2 <-ID <- NULL

    # some multipliers used to chart
    res <- shiny::reactiveValues()

    shiny::observe({
      x_axis <- seq(from = 0,to = 20,
                    by = input$breaks)
      ### taking the inputs
      # model with change
      res$Income <- (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))
      res$interest <- ((1 / input$h) * ((input$k * (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))) - input$MP))
      # model without change
      res$IncomeS <- (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))
      res$interestS <- ((1 / input$h1) * ((input$k1 * (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))) - input$MP1))
      # income change:

      res$TABLE <- data.frame(
        "Variable" = c("Income", "interest", "c", "t", "I_0", "b", "G", "k", "h", "MP"),
        "Withtouth_change" = c(
          round(res$IncomeS, digits = 2),
          round(res$interestS, digits = 2),
          input$c1,
          input$t1,
          input$I_01,
          input$b1,
          input$G1,
          input$k1,
          input$h1,
          input$MP1
        ),
        "With_change" = c(
          round(res$Income, digits = 2),
          round(res$interest, digits = 2),
          input$c,
          input$t,
          input$I_0,
          input$b,
          input$G,
          input$k,
          input$h,
          input$MP
        ),
        "Delta" =
          c(
            paste0(round((((res$Income - res$IncomeS) / res$IncomeS) * 100), digits = 2), "%"),
            paste0(round((((res$interest - res$interestS) / res$interest) * 100), digits = 2), "%"),
            paste0(round((((input$c - input$c1) / input$c) * 100), digits = 2), "%"),
            paste0(round((((input$t - input$t1) / input$t) * 100), digits = 2), "%"),
            paste0(round((((input$I_0 - input$I_01) / input$I_0) * 100), digits = 2), "%"),
            paste0(round((((input$b - input$b1) / input$b) * 100), digits = 2), "%"),
            paste0(round((((input$G - input$G1) / input$G) * 100), digits = 2), "%"),
            paste0(round((((input$k - input$k1) / input$k) * 100), digits = 2), "%"),
            paste0(round((((input$h - input$h1) / input$h) * 100), digits = 2), "%"),
            paste0(round((((input$MP - input$MP1) / input$MP) * 100), digits = 2), "%")
          )
      )
      ### IS-LM ------------------------------

      ## chart for model with change
      ### calculations
      res$Income <- (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))
      res$interest <- ((1 / input$h) * ((input$k * (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))) - input$MP))
      ### estimating breaks:
      res$eixox <- res$Income * x_axis
      res$eixoy <- res$interest * x_axis
      ### values with curve:
      res$IS <- (((input$I_0 + input$G) / (1 - (input$c * (1 - input$t))) - ((input$b * res$eixoy) / (1 - (input$c * (1 - input$t))))))
      res$LM <- ((res$eixox * (input$k / input$h)) - (input$MP / input$h))
      ## chart the model without change:
      ### calculations:
      res$IncomeS <- (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))
      res$interestS <- ((1 / input$h1) * ((input$k1 * (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))) - input$MP1))
      ### estimating breaks:
      res$eixoxS <- res$IncomeS * x_axis
      res$eixoyS <- res$interestS * x_axis
      ### values the curve:
      res$ISS <- (((input$I_01 + input$G1) / (1 - (input$c1 * (1 - input$t1))) - ((input$b1 * res$eixoyS) / (1 - (input$c1 * (1 - input$t1))))))
      res$LMS <- ((res$eixoxS * (input$k1 / input$h1)) - (input$MP1 / input$h1))
      ### creating the data frame used for ggplot:
      res$DTLM <- data.frame("vari" = c(res$eixox), "vari2" = c(res$LM), "ID" = "LM'")
      res$DTIS <- data.frame("vari" = c(res$IS), "vari2" = c(res$eixoy), "ID" = "IS'")
      res$DTLMS <- data.frame("vari" = c(res$eixoxS), "vari2" = c(res$LMS), "ID" = "LM")
      res$DTISS <- data.frame("vari" = c(res$ISS), "vari2" = c(res$eixoyS), "ID" = "IS")
      res$DATA <- dplyr::bind_rows(res$DTLM, res$DTIS)
      res$DATA1 <- dplyr::bind_rows(res$DATA, res$DTLMS)
      res$DATA2 <- dplyr::bind_rows(res$DATA1, res$DTISS)
      #colnames(res$TABLE) <- c("Variable", "Withtouth Change","With CHange","Delta")

    })
    # render table  ------------------------
    output$TABLE <- shiny::renderDataTable(res$TABLE
      )

    # drawing the graph with displacement
    # fazendo o mapa no ggplot do specification de deslocamento:
    output$GrafTotal <- plotly::renderPlotly({
      p2 <-
        res$DATA2 |>
        ggplot2::ggplot(
          ggplot2::aes(x = vari, y = vari2, colour = ID)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::labs(x = "Income", y = "Interest Rate") +
        ggplot2::scale_colour_discrete("specification") +
        ggplot2::xlim(0.5 * (min(c(res$Income, res$IncomeS))), 1.5 * (max(c(res$Income, res$IncomeS)))) +
        ggplot2::ylim(0.5 * (min(c(res$interest, res$interestS))), 1.5 * (max(c(res$interest, res$interestS)))) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p2)

    })

    output$Graindiv <- plotly::renderPlotly({
      p <-
        dplyr::filter(res$DATA2, ID %in% c("LM'", "IS'")) |>
        ggplot2::ggplot(
          ggplot2::aes(x = vari, y = vari2, colour = ID)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::labs(x = "Income", y = "Interest Rate") +
        ggplot2::scale_colour_discrete("specification") +
        ggplot2::xlim(0.5 * (min(c(res$Income))), 1.5 * (max(c(res$Income)))) +
        ggplot2::ylim(0.5 * (min(c(res$interest))), 1.5 * (max(c(res$interest)))) +
        ggplot2::theme_minimal()

      plotly::ggplotly(p)
    })
  }

  # (Equilibrio no mercado de bens) Y = (A - bi)/(1-c(1-t))
  # (Equilibrio no mercado monetario) i= (1/h) . [ k Y- (M/P)]


  # Run the application
  shiny::shinyApp(ui, server)

}
