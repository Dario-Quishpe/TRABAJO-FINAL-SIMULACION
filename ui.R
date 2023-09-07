suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(shinyWidgets))
options(dplyr.summarise.inform = FALSE)

# UI
shinyUI(fluidPage(fluidRow(column(tags$img(src="logo.png", width="160px", height="90px"), width=2), # Logo página principal
                           column(10, h1("Trabajo Final de Simulación", 
                                         style = "text-align:center;color:#9A9A9A;padding:20px;font-size:2.2em"))
                           ),
        navbarPage("Simulación",
                   navbarMenu("Teorema del Límite Central (Distribuciones Discretas)",
                              tabPanel("Distribución Binomial", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias binomial"),
                                       br()
                              ),
                              tabPanel("Distribución Binomial Negativa", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias binomial negativa"),
                                       br()
                              ),
                              tabPanel("Distribución Poisson", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias poisson"),
                                       br()
                              )
                   ),
                   navbarMenu("Teorema del Límite Central (Distribuciones Continuas)",
                              tabPanel("Distribución Exponencial", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias binomial"),
                                       br()
                              ),
                              tabPanel("Distribución Uniforme", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias uniformes"),
                                       br(),
                                       fluidRow(column(3, 
                                                       numericInput("a", "Límite Inferior:", value = -3, min = -20, max = 10),
                                                       numericInput("b", "Límite Superior:", value = 4, min = 0, max = 30),
                                                       numericInput("nunif", "Número de variables:", value = 12, min = 10, max = 1000),
                                                       numericInput("nsim_unif", "Número de simulaciones:", value = 20, min = 20, max = 2000)
                                       ),
                                       column(9, 
                                              fluidRow(
                                                div(tableOutput("tb_unif"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_unif", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       br(),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       fluidRow(
                                         plotOutput("plot_unif", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_unif", "Ingrese el valor de c:", value = 2, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_unif'),
                                                uiOutput('pteo_unif')
                                         )
                                       ),
                                       br()
                              ),
                              tabPanel("Distribución Normal", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias binomial negativa"),
                                       br()
                              ),
                              tabPanel("Distribución de Pareto", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias poisson"),
                                       br()
                              ),
                              tabPanel("Distribución Betha", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias exponenciales"),
                                       br()
                              ),
                              tabPanel("Distribución Gamma", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias normales"),
                                       br()
                              ),
                              tabPanel("Distribución Chi-Cuadrado", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias normales"),
                                       br()
                              ),
                   ),
                   tabPanel("Kolmogorov - Smirnov",
                            h4("Test de Kolmogorov - Smirnov"),
                            fluidRow(
                                  column(5, 
                                         fileInput('file1', 'Seleccione el archivo .xlsx', accept = c(".xlsx")),
                                         div(tableOutput("carga"), style = "font-size:80%"),
                                         tableOutput("archivo")
                                         ),
                                  column(7,
                                         selectInput("variable", "Seleccione la variable:", choices = c("Variable 1", "Variable 2", "Variable 3", "Variable 4")),
                                         h4("Gráfico KS")
                                         )
                            ),
                            br()
                            ),
                   tabPanel("Bootstrap",
                            h4("Técnicas de remuestreo"),
                            br()
                   ),
                   navbarMenu("Procesos Estocásticos",
                              tabPanel("Cadenas en tiempo discreto",
                                       h4("Cadenas en tiempo discreto"),
                                       br()
                              ),
                              tabPanel("Cadenas en tiempo continuo",
                                       h4("Procesos de Nacimiento y Muerte"),
                                       br(),
                                       fluidRow(
                                             column(4,
                                                    sliderInput("lambda", "Intensidad de nacimientos:", value = 3, min = 1, max = 10),
                                                    sliderInput("mu", "Intensidad de fallecimientos:", value = 4, min = 1, max = 10),
                                                    sliderInput("njumps", "Número de saltos del proceso:", value = 10, min = 1, max = 50)
                                                    ),
                                             column(8,
                                                    fluidRow(
                                                          plotOutput("plot_proceso", height = "500px")
                                                    )
                                                    )
                                       ),
                              )
                              )
        )
)
)
