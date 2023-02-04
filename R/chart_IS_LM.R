

#' Create a shiny app with IS-LM chart
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @import shiny
#' @import shinythemes
#' @import shinydashboard
#' @import ggthemes
#' @export
#' @examples
#' chart_IS_LM()
#'
chart_IS_LM <- function() {
  time <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2)



  # Define UI for application that draws a histogram
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("cerulean"),

    # Application title
    shiny::titlePanel("Modelo macroeconomico IS-LM"),
    shiny::fluidRow(shiny::column(12, shiny::h4("Parametros do modelo IS-LM sem mudanca"))),
    shiny::fluidRow(
      shiny::column(1, shiny::sliderInput("c1",  shiny::withMathJax("$$\\hspace{1cm}\\textbf{c}$$"), value = 0.8, min = 0, max = 1)), # propensao marginal a consumir
      shiny::column(1, shiny::sliderInput("t1",  shiny::withMathJax("$$\\hspace{1cm}\\textbf{t}$$"), value = 0.25, min = 0, max = 1)), # Imposto de renda proporcional
      shiny::column(2, shiny::sliderInput("I_01",shiny::withMathJax("$$\\hspace{2cm} I_0$$"), value = 900, min = 0, max = 2000)),
      shiny::column(2, shiny::sliderInput("b1",  shiny::withMathJax("$$\\hspace{2cm}\\textbf{b}$$"), value = 50, min = 0, max = 100)),
      shiny::column(2, shiny::sliderInput("G1",  shiny::withMathJax("$$\\hspace{2cm} G_0$$"), value = 800, min = 0, max = 1600)),
      shiny::column(1, shiny::sliderInput("k1",  shiny::withMathJax("$$\\hspace{1cm}\\textbf{k}$$"), value = 0.25, min = 0, max = 1)), # sensibilidade da demanda por saldos reais em relacao ao nivel de renda # L= 0,25 Y - 62,5i
      shiny::column(1, shiny::sliderInput("h1",  shiny::withMathJax("$$\\hspace{1cm}\\textbf{h}$$"), value = 62.5, min = 0, max = 100)), # sensibilidade da demanda por saldos reais em relacao a taxa de juros
      shiny::column(2, shiny::sliderInput("MP1", shiny::withMathJax("$$\\hspace{2cm}\\bar{M}/P$$"), value = 500, min = 0, max = 1000)) # oferta real de moeda
    ),
    shiny::fluidRow(shiny::column(12, shiny::h4("Parametros do modelo IS-LM com mudanca"))),
    shiny::fluidRow(
      shiny::column(1, shiny::sliderInput("c",   shiny::withMathJax("$$\\hspace{1cm}\\textbf{c}$$"), value = 0.8, min = 0, max = 1)),
      shiny::column(1, shiny::sliderInput("t",   shiny::withMathJax("$$\\hspace{1cm}\\textbf{t}$$"), value = 0.25, min = 0, max = 1)),
      shiny::column(2, shiny::sliderInput("I_0", shiny::withMathJax("$$\\hspace{2cm} I_0$$"), value = 900, min = 0, max = 2000)),
      shiny::column(2, shiny::sliderInput("b",   shiny::withMathJax("$$\\hspace{2cm}\\textbf{b}$$"), value = 50, min = 0, max = 100)),
      shiny::column(2, shiny::sliderInput("G",   shiny::withMathJax("$$\\hspace{2cm} G_0$$"), value = 800, min = 0, max = 1600)),
      shiny::column(1, shiny::sliderInput("k",   shiny::withMathJax("$$\\hspace{1cm}\\textbf{k}$$"), value = 0.25, min = 0, max = 1)), # sensibilidade da demanda por saldos reais em relacao ao nivel de renda # L= 0,25 Y - 62,5i
      shiny::column(1, shiny::sliderInput("h",   shiny::withMathJax("$$\\hspace{1cm}\\textbf{h}$$"), value = 62.5, min = 0, max = 100)), # sensibilidade da demanda por saldos reais em relacao a taxa de juros
      shiny::column(2, shiny::sliderInput("MP",  shiny::withMathJax("$$\\hspace{2cm}\\bar{M}/P$$"), value = 500, min = 0, max = 1000)) # oferta real de moeda
    ),
    shiny::tabsetPanel(
      shiny::tabPanel(
        "Dinamica do IS-LM",
        shiny::fluidRow(
          shiny::column(6, shiny::dataTableOutput("TABLE")),
          shiny::column(6, shiny::plotOutput("GrafTotal"))
        )
      ),
      shiny::tabPanel(
        "Modelo IS'-LM'",
        shiny::fluidRow(shiny::column(12, shiny::plotOutput("Graindiv")))
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output, session) {
    res <- shiny::reactiveValues()

    shiny::observe({
      ### OBTENDO OS RESULTADOS DO MODELO
      # modelo com alteracao
      res$RENDA <- (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))
      res$JUROS <- ((1 / input$h) * ((input$k * (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))) - input$MP))
      # modelo sem alteracao:
      res$RENDAS <- (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))
      res$JUROSS <- ((1 / input$h1) * ((input$k1 * (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))) - input$MP1))
      # variacao renda:

      res$TABLE <- data.frame(
        "Variavel" = c("Renda", "Juros", "c", "t", "I_0", "b", "G", "k", "h", "MP"),
        "Sem_alteracao" = c(
          round(res$RENDAS, digits = 2),
          round(res$JUROSS, digits = 2),
          input$c1,
          input$t1,
          input$I_01,
          input$b1,
          input$G1,
          input$k1,
          input$h1,
          input$MP1
        ),
        "Com_alteracao" = c(
          round(res$RENDA, digits = 2),
          round(res$JUROS, digits = 2),
          input$c,
          input$t,
          input$I_0,
          input$b,
          input$G,
          input$k,
          input$h,
          input$MP
        ),
        "Variacao" =
          c(
            paste0(round((((res$RENDA - res$RENDAS) / res$RENDAS) * 100), digits = 2), "%"),
            paste0(round((((res$JUROS - res$JUROSS) / res$JUROS) * 100), digits = 2), "%"),
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


      ### FAZENDO OS GRAFICOS PARA OS MODELOS:
      ## Fazendo para o modelo com alteracao:
      # calculos que geram condicoes:
      res$Renda <- (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))
      res$Juros <- ((1 / input$h) * ((input$k * (((input$I_0 + input$G) + ((input$b / input$h) * input$MP)) / ((1 - (input$c * (1 - input$t))) + ((input$b * input$k) / input$h)))) - input$MP))
      # calculo dos intervalos:
      res$eixox <- res$Renda * time
      res$eixoy <- res$Juros * time
      # valores para a curva:
      res$IS <- (((input$I_0 + input$G) / (1 - (input$c * (1 - input$t))) - ((input$b * res$eixoy) / (1 - (input$c * (1 - input$t))))))
      res$LM <- ((res$eixox * (input$k / input$h)) - (input$MP / input$h))
      ## Fazendo para o modelo sem alteracao:
      # calculos que geram condicoes:
      res$RendaS <- (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))
      res$JurosS <- ((1 / input$h1) * ((input$k1 * (((input$I_01 + input$G1) + ((input$b1 / input$h1) * input$MP1)) / ((1 - (input$c1 * (1 - input$t1))) + ((input$b1 * input$k1) / input$h1)))) - input$MP1))
      # calculo dos intervalos:
      res$eixoxS <- res$RendaS * time
      res$eixoyS <- res$JurosS * time
      # valores para a curva:
      res$ISS <- (((input$I_01 + input$G1) / (1 - (input$c1 * (1 - input$t1))) - ((input$b1 * res$eixoyS) / (1 - (input$c1 * (1 - input$t1))))))
      res$LMS <- ((res$eixoxS * (input$k1 / input$h1)) - (input$MP1 / input$h1))
      ## montando o data frame:
      res$DTLM <- data.frame("vari" = c(res$eixox), "vari2" = c(res$LM), "ID" = "LM'")
      res$DTIS <- data.frame("vari" = c(res$IS), "vari2" = c(res$eixoy), "ID" = "IS'")
      res$DTLMS <- data.frame("vari" = c(res$eixoxS), "vari2" = c(res$LMS), "ID" = "LM")
      res$DTISS <- data.frame("vari" = c(res$ISS), "vari2" = c(res$eixoyS), "ID" = "IS")
      res$DATA <- dplyr::bind_rows(res$DTLM, res$DTIS)
      res$DATA1 <- dplyr::bind_rows(res$DATA, res$DTLMS)
      res$DATA2 <- dplyr::bind_rows(res$DATA1, res$DTISS)
    })

    # aqui nao mexer
    output$TABLE <- shiny::renderDataTable(res$TABLE)

    # fazendo o mapa no ggplot do modelo de deslocamento:
    output$GrafTotal <- shiny::renderPlot({
      res$DATA2 |>
      ggplot2::ggplot(
        ggplot2::aes(x = vari, y = vari2, fill = ID, group = ID)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::geom_line(ggplot2::aes(colour = ID)) +
        ggplot2::labs(x = "Renda", y = "Taxa de juros") +
        ggplot2::scale_colour_discrete("Modelo") +
        ggplot2::xlim(0.5 * (min(c(res$RENDA, res$RENDAS))), 1.5 * (max(c(res$RENDA, res$RENDAS)))) +
        ggplot2::ylim(0.5 * (min(c(res$JUROS, res$JUROSS))), 1.5 * (max(c(res$JUROS, res$JUROSS)))) +
        ggthemes::theme_economist()
    })

    output$Graindiv <- shiny::renderPlot({
      dplyr::filter(res$DATA2, ID %in% c("LM'", "IS'")) |>
      ggplot2::ggplot(
      ggplot2::aes(x = vari, y = vari2, fill = ID, group = ID)) +
        ggplot2::geom_line(linewidth = 1.5) +
        ggplot2::geom_line(ggplot2::aes(colour = ID)) +
        ggplot2::labs(x = "Renda", y = "Taxa de juros") +
        ggplot2::scale_colour_discrete("Modelo") +
        ggplot2::xlim(0.5 * (min(c(res$RENDA))), 1.5 * (max(c(res$RENDA)))) +
        ggplot2::ylim(0.5 * (min(c(res$JUROS))), 1.5 * (max(c(res$JUROS)))) +
        ggthemes::theme_economist()
    })
  }

  # (Equilibrio no mercado de bens) Y = (A - bi)/(1-c(1-t))
  # (Equilibrio no mercado monetario) i= (1/h) . [ k Y- (M/P)]

  # Run the application
  shiny::shinyApp(ui, server)
}


