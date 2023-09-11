suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(highcharter))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinythemes))

options(dplyr.summarise.inform = FALSE)

# UI
shinyUI(fluidPage(tags$head( #COLOR DEGRADADO
  tags$style(HTML(
    "
      body {
        background:linear-gradient(to bottom, white, #00FFEC);
        background-attachment:fixed;
        background-size:cover;
        background-repeat:no-repeat;
      }
      "
  )), 
  #para las animaciones
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      shinyjs::useShinyjs()
  
), fluidRow(column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="110px", height="125px")), # Logo página principal
                           column(width=8,style="background:black", h1("TRABAJO GRUPAL FINAL DE SIMULACIÓN-2023A ", 
                                                                       style = "background:#F9EDE9 ;text-align:center;align-items:center;color:'black';padding:30px;font-size:2.2em")),
                           column(width=2,align="center",style="background:#DEE9F9",img(src="https://cem.epn.edu.ec/imagenes/logos_institucionales/big_png/BUHO_EPN_big.png", width="110px", height="125px"))
),

        navbarPage("Simulación",theme=shinytheme("journal"),
                   navbarMenu("Teorema del Límite Central (Distribuciones Discretas)",
                              tabPanel("Distribución Binomial", tags$style("h4 {color: #035FC6; font-family: roman}", id = "tab_binomial"), #identificador para cambiar las pantallas
                                       h4("Simulación de variables aleatorias binomial"),
                                       br(),
                                       fluidRow(column(3,
                                                       numericInput("pbinom","Ingrese el valor de p",value=0.1,min=0.1,max=1),
                                                       numericInput("ensayos","Ingrese el número de ensayos:",value=20,min=1,max=1000),
                                                       numericInput("nbinom","Número de variables:",value=50,min=10,max=1000),
                                                       numericInput("nsim_binom","Número de simulaciones:",value=100,min=20,max=2000)
                                       ),
                                       column(9,
                                              fluidRow(
                                                div(tableOutput("tb_binom"),style="font-size:80%"),
                                                br(),
                                                downloadButton("download_binom","Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_binom1", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_binom1", "Ingrese el valor de c:", value = 2, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_binom1'),
                                                uiOutput('pteo_binom1')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_binom_prob1", height = "500px")
                                       ),
                                       br(),
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_binom2", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_binom2", "Ingrese el valor de c:", value = 90, min = 70, max = 200)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_binom2'),
                                                uiOutput('pteo_binom2')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_binom_prob2", height = "500px")
                                       ),
                                       br(),
                                       
                              ),
                              tabPanel("Distribución Binomial Negativa", tags$style("h4 {color: #035FC6; font-family: roman}", id= "tab_negativa"),
                                       h4("Simulación de variables aleatorias Binomial Negativa"),
                                       br(),
                                       
                                       fluidRow(column(3,
                                                       numericInput("nsim_bn", "Número de simulaciones:", value = 1000, min = 20, max = 2000),
                                                       numericInput("nbn", "Número de variables a simular:", value = 50, min = 10, max = 200),
                                                       numericInput("exi_bn", "Número de éxitos:", value = 7, min = 1, max = 50),
                                                       numericInput("prob_bn", "Probabilidad de éxito:", value = 0.5, min = 0, max = 1)
                                                       
                                       ),
                                       column(9,
                                              fluidRow(
                                                div(tableOutput("tb_bn"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_bn", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       #llamemos a la grafica
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_bn", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       #resultados numericos
                                       fluidRow(
                                         column(3,
                                                numericInput("c_bn", "Ingrese el valor de c:", value = 7, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_bn'),
                                                uiOutput('pteo_bn')
                                         )
                                       ),
                                       br(),
                                       #grafica
                                       fluidRow(
                                         plotOutput("plot_bn_1", height = "500px")
                                       ),
                                       br(),
                                       br(),
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_bn_2", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_bn_1", "Ingrese el valor de c:", value =400 , min = 0, max = 900)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_bn_3'),
                                                uiOutput('pteo_bn_3')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_bn_prob2", height = "500px")
                                       ),
                                       br(),
                             
                                       
                                       
                              ),
                              tabPanel("Distribución Poisson", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias poisson"),
                                       br(),
                                       fluidRow(column(3,
                                                       numericInput("lambdapoisson", "Media:", value = 0.5, min = 0.25, max = 12),
                                                       numericInput("npoisson", "Número de variables:", value = 50, min = 10, max = 1000),
                                                       numericInput("nsim_poisson", "Número de simulaciones:", value = 1000, min = 20, max = 2000)
                                       ),
                                       column(9,
                                              fluidRow(
                                                div(tableOutput("tb_poisson"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_poisson", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_poisson1", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_poisson1", "Ingrese el valor de c:", value = 1, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_poisson1'),
                                                uiOutput('pteo_poisson1')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_poisson_prob1", height = "500px")
                                       ),
                                       br(),
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_poisson2", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_poisson2", "Ingrese el valor de c:", value = 50, min = 0, max = 100)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_poisson2'),
                                                uiOutput('pteo_poisson2')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_poisson_prob2", height = "500px")
                                       ),
                                       br(),
                              ),
                   ),
                   navbarMenu("Teorema del Límite Central (Distribuciones Continuas)",
                              tabPanel("Distribución Exponencial", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias exponenciales"),
                                       br(),
                                       fluidRow(column(3, 
                                                       numericInput("lambdaexp", "Parámetro:", value = 1, min = 0.1, max = 10),
                                                       numericInput("nexp", "Número de variables:", value = 50, min = 10, max = 1000),
                                                       numericInput("nsim_exp", "Número de simulaciones:", value = 1000, min = 20, max = 2000)
                                       ),
                                       column(9, 
                                              fluidRow(
                                                div(tableOutput("tb_exp"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_exp", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_exp1", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_exp1", "Ingrese el valor de c:", value = 1, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_exp1'),
                                                uiOutput('pteo_exp1')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_exp_prob1", height = "500px")
                                       ),
                                       br(),
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_exp2", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_exp2", "Ingrese el valor de c:", value = 50, min = 0, max = 100)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_exp2'),
                                                uiOutput('pteo_exp2')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_exp_prob2", height = "500px")
                                       ),
                                       br(),
                                       
                              ),
                              tabPanel("Distribución Uniforme", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias uniformes"),
                                       br(),
                                       fluidRow(column(3, 
                                                       numericInput("a", "Límite Inferior:", value = -3, min = -20, max = 10),
                                                       numericInput("b", "Límite Superior:", value = 4, min = 0, max = 30),
                                                       numericInput("nunif", "Número de variables:", value = 12, min = 10, max = 1000),
                                                       numericInput("nsim_unif", "Número de simulaciones:", value = 200, min = 20, max = 2000)
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
                                                numericInput("c_unif_1", "Ingrese el valor de c:", value = 0.5, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_unif'),
                                                uiOutput('pteo_unif')
                                         )
                                       ),
                                       br(),
                                       fluidRow(
                                         plotOutput("plot_uni_1", height = "500px")
                                       ),
                                       br(),
                                       
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_uni_2", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("C_unif_2", "Ingrese el valor de c:", value = 9, min = 0, max = 100)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_unif_3'),
                                                uiOutput('pteo_unif_3')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_unif", height = "500px")
                                       ),
                                       br(),
                                       
                                       
                              ),
                              tabPanel("Distribución Normal", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias normales"),
                                       br(),
                                       fluidRow(column(3, 
                                                       numericInput("m_norm", "Media:", value = 0, min = 0, max = 12),
                                                       numericInput("sd_norm", "Varianza:", value = 1, min = 0, max = 10),
                                                       numericInput("nnorm", "Número de variables:", value = 50, min = 10, max = 1000),
                                                       numericInput("nsim_norm", "Número de simulaciones:", value = 1000, min = 20, max = 2000)
                                       ),
                                       column(9, 
                                              fluidRow(
                                                div(tableOutput("tb_norm"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_norm", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_norm1", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_norm1", "Ingrese el valor de c:", value = 1, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_norm1'),
                                                uiOutput('pteo_norm1')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_norm_prob1", height = "500px")
                                       ),
                                       br(),
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_norm2", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_norm2", "Ingrese el valor de c:", value = 50, min = 0, max = 100)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_norm2'),
                                                uiOutput('pteo_norm2')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_norm_prob2", height = "500px")
                                       ),
                                       br(),
                              ),
                              tabPanel("Distribución de Pareto", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias (PARETO)",style = "background:#FEE9DF  ;text-align:center;align-items:center;color:'black';padding:25px;font-size:1.5em",align="center"),
                                       br(),
                                       fluidRow(column(3, 
                                                       numericInput("a_pareto", "Minimo(escala):", value = 2, min = 1, max = 1000),
                                                       numericInput("b_pareto", "Gamma(Forma):", value = 4, min = 2, max = 1000),
                                                       numericInput("npareto", "Número de variables:", value = 25, min = 10, max = 1000),
                                                       numericInput("nsim_pareto", "Número de simulaciones:", value = 200, min = 20, max = 2000)
                                       ),
                                       column(9, 
                                              fluidRow(
                                                div(tableOutput("tb_pareto"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_pareto", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       br(),
                                       h4("Aplicación del Teorema del Límite Central",style = "background:#FEE9DF  ;text-align:center;align-items:center;color:'black';padding:25px;font-size:1.5em",align="center"),
                                       column(12,box(plotOutput("plot_Pareto_ggplot",height = 400), width = 12)),
                                       br(),
                                       
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       
                                       fluidRow(
                                         column(3,
                                                numericInput("c_pareto", "Ingrese el valor de c:", value = 2.8, min = 0, max = 100)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_pareto'),
                                                uiOutput('pteo_pareto')
                                         )
                                       ),
                                       br(),
                               h4(strong("Suma de variables"),style = "background:#FEE9DF  ;text-align:center;align-items:center;color:'black';padding:25px;font-size:1.5em",align="center"),
                               fluidRow(
                                 plotOutput("plot_pareto2", height = "500px")
                               ),
                               br(),
                               h4("Aproximación de probabilidades"),
                               br(),
                               fluidRow(
                                 column(3,
                                        numericInput("c_pareto2", "Ingrese el valor de c:", value = 70, min = 0, max = 100)
                                 ),
                                 column(9,
                                        h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                        uiOutput('pest_pareto2'),
                                        br(),
                                        uiOutput('pteo_pareto2'),
                                 )
                               ),
                               fluidRow(
                                 plotOutput("plot_pareto_prob2", height = "500px")
                               ),
                               br(),
                               
                               

                    
                                
                            
                                       
                              ),
                              tabPanel("Distribución Betha", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias Betha"),
                                       br(),
                                       fluidRow(column(3,
                                                       numericInput("pbetha","Ingrese del parámetro p",value=1,min=0.1,max=20),
                                                       numericInput("qbetha","Ingrese el parámetro q",value=1,min=0.1,max=20),
                                                       numericInput("nbetha", "Número de variables:", value = 50, min = 10, max = 1000),
                                                       numericInput("nsim_betha", "Número de simulaciones:", value = 100, min = 20, max = 2000)
                                                       
                                       ),
                                       column(9, 
                                              fluidRow(
                                                div(tableOutput("tb_betha"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_betha", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_betha1", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_betha1", "Ingrese el valor de c:", value = 1, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_betha1'),
                                                uiOutput('pteo_betha1')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_betha_prob1", height = "500px")
                                       ),
                                       br(),
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_betha2", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_betha2", "Ingrese el valor de c:", value = 50, min = 0, max = 100)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_betha2'),
                                                uiOutput('pteo_betha2')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_betha_prob2", height = "500px")
                                       ),
                                       br(),
                                       
                              ),
                              
                              tabPanel("Distribución Gamma", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables Gamma"),
                                       br(),
                                       fluidRow(column(3, 
                                                       numericInput("alfagam", "Forma:", value = 10, min = 0.1, max = 20),
                                                       numericInput("lambdagam", "Escala:", value = 10, min = 0.1, max = 20),
                                                       numericInput("ngam", "Número de variables:", value = 50, min = 10, max = 1000),
                                                       numericInput("nsim_gam", "Número de simulaciones:", value = 1000, min = 20, max = 2000)
                                       ),
                                       column(9, 
                                              fluidRow(
                                                div(tableOutput("tb_gam"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_gam", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_gam1", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_gam1", "Ingrese el valor de c:", value = 100, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_gam1'),
                                                uiOutput('pteo_gam1')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_gam_prob1", height = "500px")
                                       ),
                                       br(),
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_gam2", height = "500px")
                                       ),
                                       
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_gam2", "Ingrese el valor de c:", value = 5000, min = 0, max = 100)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } {Y} = \\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(Y \\leq c)$$"),
                                                uiOutput('pest_gam2'),
                                                uiOutput('pteo_gam2')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_gam_prob2", height = "500px")
                                       ),
                                       br(),
                              ),
                              tabPanel("Distribución Chi-Cuadrado", tags$style("h4 {color: #035FC6; font-family: roman}"),
                                       h4("Simulación de variables aleatorias Chi-Cuadradas"),
                                       br(),
                                       fluidRow(column(3, 
                                                       numericInput("chi_grados", "Grados de libertad:", value = 10, min = 2, max = 100),
                                                      # numericInput("lambdagam", "Escala:", value = 10, min = 0.1, max = 20),
                                                       numericInput("nchi", "Número de variables:", value = 50, min = 10, max = 1000),
                                                       numericInput("nsim_chi", "Número de simulaciones:", value = 1000, min = 20, max = 2000)
                                       ),
                                       column(9, 
                                              fluidRow(
                                                div(tableOutput("tb_chi"), style = "font-size:80%"),
                                                br(),
                                                downloadButton("download_chi", "Descargar Simulaciones")
                                              )
                                       )
                                       ),
                                       h4("Aplicación del Teorema del Límite Central"),
                                       br(),
                                       h4(strong("Promedio de variables")),
                                       fluidRow(
                                         plotOutput("plot_chi", height = "500px")
                                       ),
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_chi_1", "Ingrese el valor de c:", value = 4, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_chi'),
                                                uiOutput('pteo_chi')
                                         )
                                       ),
                                       fluidRow(
                                         plotOutput("plot_chi_1", height = "500px")
                                       ),
                                       br(),
                                       
                                       h4(strong("Suma de variables")),
                                       fluidRow(
                                         plotOutput("plot_chi_2", height = "500px")
                                       ),
                                       
                                       br(),
                                       h4("Aproximación de probabilidades"),
                                       br(),
                                       fluidRow(
                                         column(3,
                                                numericInput("c_chi_2", "Ingrese el valor de c:", value = 4, min = 0, max = 20)
                                         ),
                                         column(9,
                                                h4("$$\\text{Dado que } \\bar{X} = \\frac{1}{n}\\sum_{i=1}^{n} X_i,\\quad \\text{ se busca calcular }\\quad P(\\bar{X} \\leq c)$$"),
                                                uiOutput('pest_chi'),
                                                uiOutput('pteo_chi')
                                         )
                                       ),
                                       
                                       
                                       fluidRow(
                                         plotOutput("plot_chi_prob2", height = "500px")
                                       ),
                                       br(),
                              ),
                   ),
                   tabPanel("Kolmogorov - Smirnov",
                            h4("Test de Kolmogorov - Smirnov",style = "background:#FEE9DF  ;text-align:center;align-items:center;color:'black';padding:26px;font-size:1.8em",align="center"),
                            fluidRow(
                                  column(4, 
                                         fileInput('file2', 'Seleccione el archivo .xlsx', accept = c(".xlsx")),
                                         div(tableOutput("cargaks"), style = "font-size:50%"),
                                         tableOutput("archivoks")
                                         ),
                                  column(4,
                                         selectInput("variable", "Seleccione la variable:",choice=c("1",
                                                                                                    "2",
                                                                                                    "3",
                                                                                                    "4",
                                                                                                    "5",
                                                                                                    "6",
                                                                                                    "7",
                                                                                                    "8",
                                                                                                    "9",
                                                                                                    "10",
                                                                                                    "11"
                                                                                                    
                                                                                                    )),
                                         h4("Gráfico KS"),
                                         plotOutput("grafica_distribuciones",width ="500px" , height = "350px"),
          
                                         ),
                                  column(4,infoBoxOutput("info_boxKs"), style = "text-align:left;padding:60px;font-size:1.8em"),
                            
                                  
                            ),
                        
                            column(8, box(highchartOutput("grafica_densidades",height = 400), width = 12)),
                            ),
                   tabPanel("Bootstrap",
                            h4("Técnicas de remuestreo"),
                            h4("Intervalos de confianza para la Media"),
                            br(),
                            fluidRow(
                              column(5, 
                                     fileInput('fileboot', 'Seleccione el archivo .xlsx', accept = c(".xlsx")),
                                     div(tableOutput("cargaboot"), style = "font-size:80%")
                                     #tableOutput("archivoboot")
                              ),
                              column(7,
                                     fluidRow(
                                       box(highchartOutput("plot_boot",height = 400), width = 12)
                                     ),
                                     
                              )
                            ),
                            br(),
                            fluidRow(
                              column(3,
                                     numericInput("nivel_confboot","Nivel de confianza del intervalo", value = 95, min = 70, max = 99)
                              )
                            ),
                            br(),
                            fluidRow(
                              uiOutput("IC_trad", height = "500px")
                            ),
                            fluidRow(
                              uiOutput("IC_boot", height = "500px")
                            ),
                            
                   ),
                   navbarMenu("Procesos Estocásticos (tiempo discreto)",
                              tabPanel("Cadenas en tiempo discreto",
                                       h1("Cadenas en tiempo discreto"),
                                       h2(strong("Ejercicio1: Clientes en una empresa:")),
                                       br(),
                                       p(" Tras realizar un estudio de mercado, los
                                         directivos de la empresa XYZ llegan a la siguiente conclusión: cuando
                                         transcurre cada año el 70% de sus clientes siguen siendo fieles,
                                         mientras que el 30% de sus clientes se pasan a la competencia, el 35% de
                                         los clientes de la competencia se pasan a la empresa XYZ, mientras que
                                         el 65% de los que no son clientes permanecen en la competencia. Si la
                                         empresa XYZ hoy tiene 5729 clientes y la competencia 10812, estudie la
                                         evolución de la población de clientes de la empresa XYZ."),
                                       h3(strong("SOLUCION TEORICA")),
                                       br(),
                                       p("$$\\text{Vamos a considerar los siguientes dos estados : E = Empresa XYZ, C = Competencia
                                          y la variable}\\quad{} X_t \\quad{}  \\text{ como: empresa a la que le son fieles los clientes en el año t
                                          }$$"),
                                       br(),
                                      p("$$\\text{Ahora en base a la información que se nos entrega en el enunciado
                                          presentamos el diagrama de la cadena con su respectiva matriz de transición  :}$$"),
                                       br(),
                                      fluidRow(
                                          column(8,box(plotOutput("plot_grafo",height = 600), width =15 ,style="font-size:200%")),
                                          column(4,box(tableOutput("tabla_grafo"),height = 450, aling="left",style = "font-size:200%")),
                                      ),
                                      p("Ahora nosotros conocemos que nuestra cadena es finita , irreducible y además ergódica pues como"),
                                      br(),
                                      p("$$ p_{EE}(1)=0.7>0 \\quad{}\\text{y} \\quad{} p_{CC}(1)=0.65>0 \\quad{} \\text{tenemos que d(E)=d(C)=1} $$"),
                                      br(),
                                      p("y también sabemos que en toda cadena finita ,
                                        irreducible y cerrada todos sus estados son recurrentes(recurrentes
                                        positivos). Así existe un unica distribución estacionaria la cual es
                                        independiente de la distribucion inicial ,no olvidemos que también es
                                        igual a la distribución limite."),
                                      br(),
                                      
                                      p("Con todo esto procedemos a calcular la distribución límite con la
                                        siguiente expreción para cadenas de dos estados:"),
                                      br(),
                                      helpText('$$\\begin{align*}
                                                \\pi_E=\\frac{0.35}{0.3+0.65}=0.538\\text{, }\\quad{}
                                                \\pi_C=\\frac{0.3}{0.3+0.65}=0.461
                                              \\end{align*}$$'),
                                      p("Así obtenemos teoricamente que 0.538 es la aproximación a la proporción
                                          a largo plazo de los clientes de la Empresa XYZ y por otro lados 0.462
                                          es aproximadamente la proporción a largo plazo de los cliente que
                                          corresponden a la competencia."),
                                      br(),
                                      p("Con lo anterior mencionado tenemos que a largo plazo"),
                                      br(),
                                      p("$$16541*(0.538)\\approx 8899 \\quad{} \\text{clientes corresponden a la Empresa xyz y}$$"),
                                      br(),
                                      p("$$16541*(0.462)\\approx 7642 \\quad{} \\text{corresponden a la competencia a largo plazo}$$"),
                                      br(),
                                      h3(strong("SOLUCION MEDIANTE SIMUALCION")),
                                      br(),
                                      p("Mediante la simulacion de 6000 cadenas de markov  y 500 iteraciones a este proceso
                                        obtenemos los siguientes resutados" ),
                                      br(),
                                      column(12, box(highchartOutput("distribucion_estados",height = 400), width = 12)),
                                      br(),
                                      fluidRow(
                                      valueBox(0.534, "Limite simulado de pi_E", icon = icon("star")),
                                      valueBox(0.466, "Limite simulado de pi_c", icon = icon("star"))
                                      ),
                                      br(),
                                      p("De lo cual podemos apreciar que tanto teoricamente y mediante simulación
                                          hemos obtenido los mismos resultados"),
                                      br(),
                                      br(),
                                      h2(strong("Ejercicio2: Contaminacion de la region Amazonica:")),
                                      br(),
                                      p("Para este ejercicio consideramos los siguientes estados:"),
                                      br(),
                                      p(strong("Estado 1: Terrenos limpios")),
                                      br(),
                                      p(strong(" Estado 2: Terrenos con nivel de contaminación medio")),
                                      br(),
                                      p(strong("Estado 3: Terrenos con nivel de contaminación alta")),
                                      br(),
                                      br(),
                                      h3(strong("SOLUCION TEORICA")),
                                      br(),
                                      p("Al momento de analizar el problema que se nos ha planteado se ha llegado
                                        a la conclusion de que la matriz de transicion debe tener esta forma:"),
                                      br(),
                                      column(12,align="center",tableOutput("P"),height = 200,style = "font-size:200%"),
                                      br(),
                                      p("Si hacemos los calculos necesarios para que exista un distribución estacionaria es decir hallar el vector"),
                                      br(),
                                      p("$$ \\Pi=(\\pi_1,\\pi_2,\\pi_3) \\quad{}\\text{ tal que  }\\quad{}
                                        \\Pi P = \\Pi \\quad{} \\text{Nos da que, }\\quad{} \\pi_2<0 \\quad{} \\text{lo cual es absurdo.} $$"),
                                      br(),
                                      p("Por lo tanto vamos a calcular la distribución límite mediante la descomposición de
                                        Jordan, para ellos calculamos los vectores y valores propios de P. Los
                                        cuales son"),
                                      p("$$\\lambda_1=1,\\lambda_2=\\dfrac{-\\sqrt{2}+3}{10},\\lambda_3=\\dfrac{\\sqrt{2}+3}{10}$$"),
                                      br(),
                                      p("$$v_1=(1 , 1 , 1)$$"),
                                      p("$$v_2=(\\frac{-17\\sqrt{2}+26}{98},  \\frac{3\\sqrt{2}-12}{14} , 1)$$"),
                                      p("$$v_3=(\\frac{17\\sqrt{2}+26}{98}, \\frac{-3\\sqrt{2}-12}{14},  1)$$"),
                                      p("realizando el proceso correspondiente de descomposicion de Jordan encontramos "),
                                      br(),
                                      p("$$\\lim_{n\\to\\infty} P^n=C\\lim_{n\\to\\infty}D^nC^{-1}=$$ "),
                                      br(),
                                      column(12,align="center",tableOutput("c_l_c"),height = 200,style = "font-size:200%"),
                                      br(),
                                      p("Asi finalmente teniendo en cuenta que Pi inicial es (1,0,0) "),
                                      br(),
                                      p("$$\\Pi_0\\lim_{n\\to\\infty} P^n=( 0.477, 0.362 , 0.191)$$"),
                                      br(),
                                      p(strong("Esta distribucion limite es la misma sin importar la distribucion inicial")) ,
                                      br(),
                                      h3(strong("SOLUCION MEDIANTE SIMULACION")),
                                      br(),
                                      column(12, box(highchartOutput("RES_2",height = 400), width = 12)),
                                      br(),
                                      fluidRow(
                                        valueBox(0.446, "Limite simulado de pi_1", icon = icon("star")),
                                        valueBox(0.363, "Limite simulado de pi_2", icon = icon("star")),
                                        valueBox(0.1906, "Limite simulado de pi_3", icon = icon("star"))
                                      ),
                                      br(),
                                      br(),
                                      
                                      
                              ),
                   ),
                              
                  navbarMenu("Procesos Estocásticos (tiempo continuo)",
                                            tabPanel("Proceso de Nacimiento y Muerte",
                                              
                                                           h3(strong("Sean las siguientes tasas: ")),
                                                           br(),
                                                           h4(" Tasa de Nacimientos: $$λ_n = 3n+2 , ∀ n≥ 0$$"),
                                                           br(),
                                                           h4(" Tasa de Falleciemitos: $$μ_n = 4n , ∀ n≥ 1$$ "),
                                                           br(),
                                                           h3(strong("SIMULEMOS LA TRAYECTORIA DEL PROCESO")),
                                                           br(),
                                                           fluidRow(
                                                             column(4,
                                                                    sliderInput("tiempo1", "Tiempo:", value = 1, min = 0.5, max = 3)
                                                                    
                                                             ),
                                                             column(8,
                                                                    fluidRow(
                                                                      box(highchartOutput("plot_proceso_NM",height = 400), width = 12)
                                                                    )
                                                             ),
                                                             
                                                             
                                                           ),
                                                  br(),
                                                  
                                              
                                                           h4(strong("La condición necesaria para la existencia de la distribución estacionaria es:")),
                                                           br(),
                                                           h4("$$\\sum_{n=0}^{\\infty}\\frac{\\lambda_0,...,\\lambda_n-1}{\\mu_1,...,\\mu_n}<\\infty$$"),
                                                           br(),
                                                           h4("Entonces, para este caso en particular tenemos: $$\\sum_{n=0}^{\\infty}\\frac{2*5*...*(3n-1)}{4^{n}*n!}$$"),
                                                           br(),
                                                           h4("Ahora como: $$2*5*...*(3n-1)<3*6*....*3n = 3^{n}*n!$$ Entonces, tenemos: $$L=\\sum_{n=0}^{\\infty}\\frac{\\lambda_0,...,\\lambda_n-1}{\\mu_1,...,\\mu_n}< \\sum_{n=0}^{\\infty}\\frac{3^{n}*n!}{4^{n}*n!}=\\sum_{n=0}^{\\infty}(\\frac{3}{4})^{n}=\\frac{\\frac{3}{4}}{1-\\frac{3}{4}}<\\infty$$ Es decir, la serie converge con lo cual 
                                concluimos que el proceso tiene una distribución estacionaria. $$Pi_0=\\frac{1}{1+L} , Pi_n=\\frac{2*5*...*3n-1}{4^{n}n!(1+L)}$$"),
                                                           br(),
                                                           h3(strong("SIMULEMOS LA DISTRIBUCIÓN ESTACIONARIA")),
                                                           br(),
                                                           fluidRow(
                                                             column(4,numericInput("ns1","Seleccione el número de simulaciones a generar",value=100,min=2,max=1000)
                                                                    #numericInput("Tde","Tiempo",value=1,min=0.5,max=3)
                                                                    
                                                             ),
                                                             column(4,
                                                                    sliderInput("Tde", "Tiempo:", value = 1, min = 0.1, max = 2)
                                                                    
                                                             ),
                                                             column(7,"Resultados obtenidos",
                                                                    div(tableOutput("dst1")),
                                                                    br(),
                                                                    downloadButton("download_dst1", "Descargar Simulaciones")
                                                                    
                                                             )
                                                           ),
                                                           h3(strong("EVOLUCIÓN DE LA DISTRIBUCIÓN ESTACIONARIA")),
                                                           br(),
                                                           
                                                           
                                                           column(4,numericInput("est","Seleccione el estado",value=2,min=0,max=13),
                                                                  # numericInput("Tde1","Tiempo",value=0.1,min=0.1,max=2),
                                                                  numericInput("nsd","Seleccione el número de simulaciones a generar",value=100,min=2,max=1000)
                                                           ),
                                                           column(4,
                                                                  sliderInput("Tde1", "Tiempo:", value = 1, min = 0.1, max = 2)
                                                                  
                                                           ),
                                                           
                                                           column(8,
                                                                  fluidRow(
                                                                    box(highchartOutput("plot_distestacionaria",height = 400), width = 12)
                                                                  )
                                                           ),
                                                  br(),
                                                  
                                                  
                                                  
                                       ),
                                       
                                       
                                       tabPanel("Compañía de Seguros", 
                                                
                                                           h4("El número de reclamos diarios (reporte de siniestros) a una compañía
de seguros se modeliza mediante un proceso de Poisson con tasa λ = 6. Por otro
lado, la cuantía de cada reclamo sigue una distribución normal N(600, 32000), y dichas
cuantías son independientes unas de otras."),
                                                           fluidRow(
                                                             h4(strong("a) Estime mediante simulación la probabilidad de que en un período de 2 días la cuantía
total de los reclamos sea inferior a 10000 dólares.")),
                                                             h4("Realizaremos el gráfico del Proceso con respecto al número de días"),
                                                             column(4,
                                                                    sliderInput("ndia", "Número de días:", value = 2, min = 1, max = 365)
                                                                    
                                                             ),
                                                             column(8,
                                                                    fluidRow(
                                                                      box(highchartOutput("plot_proceso_dia",height = 400), width = 12)
                                                                    )
                                                             ),
                                                             br(),
                                                             h4("Realizaremos el gráfico del Proceso con respecto al número de reclamos"),
                                                             column(4,
                                                                    sliderInput("nreclamo", "Número de reclamos:", value = 10, min = 1, max = 20)
                                                                    
                                                             ),
                                                             column(8,
                                                                    fluidRow(
                                                                      box(highchartOutput("plot_proceso_reclamo",height = 400), width = 12)
                                                                    )
                                                             ),
                                                             br(),
                                                             h4("Estimamos la probabilidad para el periodo de días y una cuantia de total de reclamos ingresada por el usuario. (Por defecto será 2 días y $10000)"),
                                                             fluidRow(column(3, 
                                                                             numericInput("ansim", "Número de simulaciones", value=1000, min=1,max=1000),
                                                                             numericInput("andia", "Periodo de días:", value = 2, min = 2, max = 30),
                                                                             numericInput('acuantia',"Cuantia total:", value=10000,min=100,max=100000)
                                                             ),
                                                             column(9,
                                                                    h4("$$P(TReclamos < TCuantia)$$"),
                                                                    uiOutput('proba')
                                                             )
                                                             )
                                                           ),
                                                  br(),
                                                
                                                           h4("b) El número de reclamos diarios (reporte de siniestros) a una compañía
de seguros se modeliza mediante un proceso de Poisson con tasa λ = 6. Por otro
lado, la cuantía de cada reclamo sigue una distribución normal N(600, 32000), y dichas
cuantías son independientes unas de otras."),
                                                           br(),
                                                           h4(strong("1. La probabilidad de que en los próximos 9 reclamos, al menos 2 correspondan a
                         accidentes de tránsito")),
                                                           fluidRow(h4("Graficaremos la función de densidad para la cantidad de próximos reclamos y reclamos de accidentes de tránsito ingresados por el usuario (Por defecto será 9 reclamos y al menos 2 accidentes de tránsito). Y con esto se calculará la probabilidad"),
                                                                    column(4,
                                                                           numericInput("b1nsim","Número de simulaciones",value=100,min=1,max=10000),
                                                                           numericInput("b1preclamo", "Número de reclamos:", value = 9, min = 1, max = 20),
                                                                           numericInput("b1atrans","Número mínimo de accidentes de tránsito:",value=2,min=1,max=20)
                                                                    ),
                                                                    column(8,
                                                                           fluidRow(
                                                                             plotOutput("plot_b1",height = 400)
                                                                           )
                                                                    )
                                                           ),
                                                           fluidRow(
                                                             column(4,h4("$$P(NReclamos \\geq NAtr)$$")),
                                                             column(8, uiOutput('probb1'))
                                                           ),
                                                           br(),
                                                           h4(strong("2. La cuantía total esperada para los próximos 12 reclamos.")),
                                                           fluidRow(h4("Graficaremos la evolución de la cuantía esperada según los reclamos. El usuario podrá elegir para cuantos reclamos quiere obtener el valor de la cuantía total esperada(Por defecto 12 reclamos)"),
                                                                    column(4,
                                                                           numericInput("b2nsim","Número de simulaciones",value=1000,min=1,max=10000),
                                                                           numericInput("b2nreclamo", "Número de reclamos:", value = 12, min = 1, max = 30)
                                                                    ),
                                                                    column(8,
                                                                           fluidRow(
                                                                             box(highchartOutput("plot_cuantia_esperada",height = 400), width = 12)
                                                                           )
                                                                    )
                                                           ),
                                                           fluidRow(
                                                             column(8, uiOutput('cuantiaespb2'))
                                                           ),
                                                           br(),
                                                           h4(strong("3. La probabilidad de que la cuantía total de los reclamos por accidentes de tránsito
sea inferior al 30% de la cuantía total de los reclamos en un día.")),
                                                           fluidRow(h4("El usuario podrá elegir el porcentaje (Por defecto 30%) y el número de días (Por defecto 1 día)"),
                                                                    column(4,
                                                                           numericInput("b23nsim","Número de simulaciones:",value = 100,min=1,max=1000),
                                                                           numericInput("b2porc","Porcentaje",value=0.30,min=0.1,max=1),
                                                                           numericInput("b23dia","Día",value=1,min=1,max=30)
                                                                    ),
                                                                    column(8, uiOutput('prob2b3'))
                                                           ),
                                                  br(),
                                                  
                                                           h4("c) Suponga que la compañía aseguradora cuentan con un capital inicial acumulado que
                               500000 dólares y que por cada reclamo cobra un valor por concepto de deducible de
                               80 dólares."),
                                                           br(),
                                                           h4(strong("1.¿Cuál sería la distribución del estado de cuentas al final del mes (30 días)?")),
                                                           fluidRow(h4("Graficaremos la función de densidad para la cantidad de días (Por defecto será 30 días)"),
                                                                    column(4,
                                                                           numericInput("cnumsim","Número de simulaciones",value=1000,min=1,max=10000),
                                                                           numericInput("cnumdia","Número de días",value=30,min=2,max=360)
                                                                    ),
                                                                    column(8,
                                                                           fluidRow(
                                                                             box(highchartOutput("plot_dist_mes",height = 400), width = 12)
                                                                           )
                                                                    ),
                                                                    column(8,
                                                                           fluidRow(
                                                                             plotOutput("plot_ecdf",height = 500)
                                                                           )
                                                                    ),
                                                                    br(),
                                                                    fluidRow(
                                                                      column(4,uiOutput('prueba_ks'))
                                                                    )
                                                                    
                                                           ),
                                                           br(),
                                                           h4(strong("2.Estime la probabilidad de bancarrota en un período de 12 meses.")),
                                                           fluidRow(h4("Calcularemos la probabilidad de bancarrota en el periodo de días que selecciona el usuario (Por defecto 360 dias, 12 meses)"),
                                                                    column(4,
                                                                           numericInput("c2nsim","Número de simulaciones",value=1000,min=1,max=10000),
                                                                           numericInput("c2ndia","Número de días",value=360,min=2,max=720)
                                                                    ),
                                                                    column(8,uiOutput('prob2c')),
                                                                    column(8,
                                                                           fluidRow(
                                                                             plotOutput("plot_bc2",height = 400)
                                                                           )
                                                                    )
                                                           ),
                                                  br(),
                                                  
                                                  
                                       )
                                       
                              )
                              )
        )
)


