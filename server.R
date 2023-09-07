suppressWarnings(library(shiny))
suppressWarnings(library(readxl))
suppressWarnings(library(writexl))
suppressWarnings(library(data.table))
suppressWarnings(library(dplyr))
suppressWarnings(library(ggplot2))
suppressWarnings(library(stringr))
suppressWarnings(library(bit64))
suppressWarnings(library(highcharter))
suppressWarnings(library(rjson))
suppressWarnings(library(httr))
suppressWarnings(library(readr))
suppressWarnings(library(tidyr))
suppressWarnings(library(xts))
suppressWarnings(library(lubridate))
suppressWarnings(library(formattable))
suppressWarnings(library(kableExtra))
suppressWarnings(library(sparkline))
suppressWarnings(library(htmlwidgets))
suppressWarnings(library(shinyauthr))


#getDependency('sparkline')
options(dplyr.summarise.inform = FALSE)

options(scipen = 999)
dir.p <- getwd() # Directorio principal




# Función que simula una trayectoria del proceso de nacimiento y muerte
trayectoria <- function(lambda, mu, njumps){
      N <- numeric(2*njumps + 2)
      time <- numeric(2*njumps + 2)
      N[1] <- 0
      time[1]<- 0
      
      i <- 2
      repeat {
            time.birth<- (-1/(lambda))*log(runif(1))
            time.death<- (-1/(mu))*log(runif(1))
            if(time.birth < time.death | N[i-1]==0) { 
                  time[i]<- time[i-1]+time.birth-0.001
                  N[i] <- N[i-1]
                  if(i==2*njumps+2) break 
                  else {
                        time[i+1]<- time[i]+0.001 
                        N[i+1]<- N[i]+1
                        i<- i+2 }
            }
            
            if(time.death < time.birth & N[i-1]!=0) { 
                  time[i]<- time[i-1]+time.death-0.001
                  N[i] <- N[i-1] 
                  if(i==2*njumps+2) break
                  else {
                        time[i+1]<- time[i]+0.001 
                        N[i+1]<- N[i]-1
                        i<- i+2 }
            }
      }
      return(list(Saltos=N, Tiempo=time))
}


shinyServer(function(input, output, session){

  
##############################################DISTRIBUCION EXPONECIAL################################
  
  sim_exp <-  reactive({
    req(input$nsim_exp >= 10)
    res_exp <- matrix(nrow = input$nexp, ncol = input$nsim_exp)
    for(j in 1:input$nsim_exp){
      res_exp[, j] <- rexp(input$nexp, rate=input$lambdaexp)
    }
    colnames(res_exp) <- paste0("Simulacion_", 1:input$nsim_exp)
    res_exp
  })
  
  output$tb_exp <- function(){
    res_exp <<- data.table(sim_exp())
    
    res_exp %>% 
      kbl(booktabs = TRUE) %>%
      kable_styling(full_width = F, bootstrap_options = c("condensed"), font_size = 11) %>% 
      #column_spec(1, bold = TRUE, border_right = FALSE, border_left = FALSE) %>% 
      row_spec(0, background = "#33639f", color = "#ffffff") %>% 
      scroll_box(width = "1000px", height = "320px")
  }
  
  output$download_exp <- downloadHandler(
    filename = function(){"Distribucion_Exponencial.xlsx"},
    content = function(file){write_xlsx(as.data.frame(res_exp), path = file)}
  )
  
  output$plot_exp1 <- renderPlot({
    sim_exp <- data.frame(Simulacion = 1:input$nsim_exp, Media = unname(colMeans(sim_exp())))
    
    ggplot(sim_exp, aes(x = Media)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = 1/input$lambdaexp, sd = 1/(input$lambdaexp*sqrt(input$nexp))), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$pest_exp1 <- renderUI({
    sim_exp <- data.table(Simulacion = 1:input$nsim_exp, Media = unname(colMeans(sim_exp())))[, Marca := ifelse(Media < input$c_exp1, 1, 0)]
    x <- unlist(sim_exp[,mean(Marca)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pteo_exp1<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_exp1-1/input$lambdaexp)/(1/(input$lambdaexp*sqrt(input$nexp)))))))
  })
  
  output$plot_exp2 <- renderPlot({
    sim_exp <- data.frame(Simulacion = 1:input$nsim_exp, Suma = unname(colSums(sim_exp())))
    
    ggplot(sim_exp, aes(x = Suma)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$nexp/input$lambdaexp, sd = sqrt(input$nexp)/(input$lambdaexp)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$pest_exp2 <- renderUI({
    sim_exp <- data.table(Simulacion = 1:input$nsim_exp, Suma = unname(colSums(sim_exp())))[, Suma := ifelse(Suma < input$c_exp2, 1, 0)]
    x <- unlist(sim_exp[,mean(Suma)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pteo_exp2<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_exp2-input$nexp/input$lambdaexp)/(sqrt(input$nexp)/(input$lambdaexp))))))
  })
  
  
  
  
  
  
  
##############################################DISTRIBUCION GAMMA#############################################################

  sim_gam <-  reactive({
    req(input$nsim_gam >= 10)
    res_gam <- matrix(nrow = input$ngam, ncol = input$nsim_gam)
    for(j in 1:input$nsim_gam){
      res_gam[, j] <- rgamma(input$ngam, shape=input$alfagam,scale = input$lambdagam)
    }
    colnames(res_gam) <- paste0("Simulacion_", 1:input$nsim_gam)
    res_gam
  })
  
  output$tb_gam <- function(){
    res_gam <<- data.table(sim_gam())
    
    res_gam %>% 
      kbl(booktabs = TRUE) %>%
      kable_styling(full_width = F, bootstrap_options = c("condensed"), font_size = 11) %>% 
      #column_spec(1, bold = TRUE, border_right = FALSE, border_left = FALSE) %>% 
      row_spec(0, background = "#33639f", color = "#ffffff") %>% 
      scroll_box(width = "1000px", height = "320px")
  }
  
  output$download_gam <- downloadHandler(
    filename = function(){"Distribucion_Gamma.xlsx"},
    content = function(file){write_xlsx(as.data.frame(res_gam), path = file)}
  )
  
  output$plot_gam1 <- renderPlot({
    sim_gam <- data.frame(Simulacion = 1:input$nsim_gam, Media = unname(colMeans(sim_gam())))
    
    ggplot(sim_gam, aes(x = Media)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$lambdagam*input$alfagam, sd = sqrt(input$alfagam*input$lambdagam^2)/sqrt(input$ngam)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$pest_gam1 <- renderUI({
    sim_gam <- data.table(Simulacion = 1:input$nsim_gam, Media = unname(colMeans(sim_gam())))[, Marca := ifelse(Media < input$c_gam1, 1, 0)]
    x <- unlist(sim_gam[,mean(Marca)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pteo_gam1<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_gam1-input$lambdagam*input$alfagam)/(1/(sqrt(input$alfagam*input$lambdagam^2)/sqrt(input$ngam)))))))
  })
  
  output$plot_gam2 <- renderPlot({
    sim_gam <- data.frame(Simulacion = 1:input$nsim_gam, Suma = unname(colSums(sim_gam())))
    
    ggplot(sim_gam, aes(x = Suma)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$ngam*input$lambdagam*input$alfagam, sd = sqrt(input$alfagam*input$lambdagam^2)*sqrt(input$ngam)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$pest_gam2 <- renderUI({
    sim_gam <- data.table(Simulacion = 1:input$nsim_gam, Suma = unname(colSums(sim_gam())))[, Suma := ifelse(Suma < input$c_gam2, 1, 0)]
    x <- unlist(sim_gam[,mean(Suma)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pteo_gam2<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_gam2-input$ngam*input$lambdagam*input$alfagam)/(sqrt(input$alfagam*input$lambdagam^2)*sqrt(input$ngam))))))
  })
  
  
  
# Simulaciones 
sim_df <-  reactive({
      req(input$nsim_unif >= 10)
      res_unif <- matrix(nrow = input$nunif, ncol = input$nsim_unif)
      for(j in 1:input$nsim_unif){
            res_unif[, j] <- runif(input$nunif, min = input$a, max = input$b)
      }
      colnames(res_unif) <- paste0("Simulacion_", 1:input$nsim_unif)
      res_unif
})
   

## Tablas con los resultados de la simulación
output$tb_unif <- function(){
      res_unif <<- data.table(sim_df())
      
      res_unif %>% 
            kbl(booktabs = TRUE) %>%
            kable_styling(full_width = F, bootstrap_options = c("condensed"), font_size = 11) %>% 
            #column_spec(1, bold = TRUE, border_right = FALSE, border_left = FALSE) %>% 
            row_spec(0, background = "#33639f", color = "#ffffff") %>% 
            scroll_box(width = "1000px", height = "320px")
}
# Botón de descarga
output$download_unif <- downloadHandler(
      filename = function(){"DistribucionUniforme.xlsx"},
      content = function(file){write_xlsx(as.data.frame(res_unif), path = file)}
)


# Gráfico que contrasta la densidad observada con la teórica
output$plot_unif = renderPlot({
      sim_unif <- data.frame(Simulacion = 1:input$nsim_unif, Media = unname(colMeans(sim_df())))
      
      ggplot(sim_unif, aes(x = Media)) + 
            geom_histogram(aes(y =..density..), breaks = seq((min(sim_unif$Media) - 0.5), (max(sim_unif$Media) + 0.5), by = 0.05), colour = "#e42645", fill = "white") +
            geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
            stat_function(fun = dnorm, args = list(mean = mean(sim_unif$Media), sd = sd(sim_unif$Media)), col = "#1b98e0", size = 1.5) + 
            theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank())
            
})

# Probabilidades estimadas y teóricas
output$pest_unif <- renderUI({
      sim_unif <- data.table(Simulacion = 1:input$nsim_unif, Media = unname(colMeans(sim_df())))[, Marca := ifelse(Media < input$c_unif, 1, 0)]
        x <- unlist(sim_unif[,mean(Marca)])
        h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })

output$pteo_unif <- renderUI({
      h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm(input$c_unif))))
})

# Carga de archivo Excel
output$archivo <- renderTable(input$file1)

output$carga <- function(){
      inFile <- input$file1

      if(is.null(inFile))
            return(NULL)
      file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
      res <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)

      res %>%
            kbl(booktabs = TRUE) %>%
            kable_styling(full_width = F, bootstrap_options = c("condensed"), font_size = 11) %>%
            row_spec(0, background = "#33639f", color = "#ffffff") %>%
            scroll_box(width = "450px", height = "350px")

}

# Gráfico de la trayectoria del proceso de nacimiento y muerte
output$plot_proceso = renderPlot({
      
      res <- trayectoria(input$lambda, input$mu, input$njumps)
      saltos <- res$Saltos
      tiempos <- res$Tiempo
      
      plot(tiempos, saltos, type="l", lty=1, lwd=2, col=4, xlab="Tiempo", ylab="Individuos", 
           panel.first=grid())
})

  
})







