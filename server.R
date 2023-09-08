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



#######################################FUNCIONES INTERVALOS DE CONFIANZA#######################

int_conf_boot<-function(muestra,alfa){
  n <- length(muestra)
  x_barra <- mean(muestra)
  # Remuestreo
  B <- 1000
  remuestra <- numeric(n)
  x_barra_boot <- numeric(B)
  estadistico_boot <- numeric(B)
  for (k in 1:B) {
    remuestra <- sample(muestra, n, replace = TRUE)
    x_barra_boot[k] <- mean(remuestra)
    estadistico_boot[k] <- sqrt(n) * (x_barra_boot[k] - x_barra)
  }
  # Aproximación bootstrap de los ptos críticos
  pto_crit <- quantile(estadistico_boot, c(alfa/2, 1 - alfa/2))
  # Construcción del IC
  ic_inf_boot <- x_barra - pto_crit[2]/sqrt(n)
  ic_sup_boot <- x_barra - pto_crit[1]/sqrt(n)
  return(list(ICI=ic_inf_boot,ICS=ic_sup_boot))
}

int_conf_trad<-function(muestra,alfa){
  n <- length(muestra)
  t<-qt(alfa/2, n-1, lower.tail = F)
  ic_inf<-mean(muestra)-t*sqrt(var(x))/sqrt(n)
  ic_sup<-mean(muestra)+t*sqrt(var(x))/sqrt(n)
  return(list(ICI=ic_inf,ICS=ic_sup))
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
  
  sim_norm <-  reactive({
    req(input$nsim_norm >= 10)
    res_norm <- matrix(nrow = input$nnorm, ncol = input$nsim_norm)
    for(j in 1:input$nsim_norm){
      res_norm[, j] <- rnorm(input$nnorm, mean = input$m_norm,sd = input$sd_norm)
    }
    colnames(res_norm) <- paste0("Simulacion_", 1:input$nsim_norm)
    res_norm
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
  
  output$tb_norm <- function(){
    res_norm <<- data.table(sim_norm())
    
    res_norm %>% 
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
  
  output$download_norm <- downloadHandler(
    filename = function(){"Distribucion_Normal.xlsx"},
    content = function(file){write_xlsx(as.data.frame(res_norm), path = file)}
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
  
  output$plot_norm1 <- renderPlot({
    sim_norm <- data.frame(Simulacion = 1:input$nsim_norm, Media = unname(colMeans(sim_norm())))
    
    ggplot(sim_norm, aes(x = Media)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$m_norm, sd = input$sd_norm/sqrt(input$nnorm)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$pest_exp1 <- renderUI({
    sim_exp <- data.table(Simulacion = 1:input$nsim_exp, Media = unname(colMeans(sim_exp())))[, Marca := ifelse(Media < input$c_exp1, 1, 0)]
    x <- unlist(sim_exp[,mean(Marca)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pest_norm1 <- renderUI({
    sim_norm <- data.table(Simulacion = 1:input$nsim_norm, Media = unname(colMeans(sim_norm())))[, Marca := ifelse(Media < input$c_norm1, 1, 0)]
    x <- unlist(sim_norm[,mean(Marca)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pteo_exp1<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_exp1-1/input$lambdaexp)/(1/(input$lambdaexp*sqrt(input$nexp)))))))
  })
  
  output$pteo_norm1<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_norm1-input$m_norm)/(input$sd_norm/sqrt(input$nnorm))))))
  })
  
  output$plot_exp_prob1<-renderPlot({
    sim_exp <- data.frame(Simulacion = 1:input$nsim_exp, Media = unname(colMeans(sim_exp())))
    dat<-density(sim_exp$Media)
    dat<-data.frame(Media=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Media, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = 1/input$lambdaexp, sd = 1/(input$lambdaexp*sqrt(input$nexp))), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Media<input$c_exp1,Media, input$c_exp1)), fill = "skyblue")

  })
  
  output$plot_norm_prob1<-renderPlot({
    sim_norm <- data.frame(Simulacion = 1:input$nsim_norm, Media = unname(colMeans(sim_norm())))
    dat<-density(sim_norm$Media)
    dat<-data.frame(Media=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Media, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$m_norm, sd = input$sd_norm/sqrt(input$nnorm)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Media<input$c_norm1,Media, input$c_norm1)), fill = "skyblue")
    
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
  
  output$plot_norm2 <- renderPlot({
    sim_norm <- data.frame(Simulacion = 1:input$nsim_norm, Suma = unname(colSums(sim_norm())))
    
    ggplot(sim_norm, aes(x = Suma)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$nnorm*input$m_norm, sd = sqrt(input$nnorm)*input$sd_norm), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$pest_exp2 <- renderUI({
    sim_exp <- data.table(Simulacion = 1:input$nsim_exp, Suma = unname(colSums(sim_exp())))[, Suma := ifelse(Suma < input$c_exp2, 1, 0)]
    x <- unlist(sim_exp[,mean(Suma)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pest_norm2 <- renderUI({
    sim_norm <- data.table(Simulacion = 1:input$nsim_norm, Suma = unname(colSums(sim_norm())))[, Suma := ifelse(Suma < input$c_norm2, 1, 0)]
    x <- unlist(sim_norm[,mean(Suma)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pteo_exp2<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_exp2-input$nexp/input$lambdaexp)/(sqrt(input$nexp)/(input$lambdaexp))))))
  })
  
  output$pteo_norm2<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_norm2-input$nnorm*input$m_norm)/(input$sd_norm*sqrt(input$nnorm))))))
  })
  
  output$plot_exp_prob2<-renderPlot({
    sim_exp <- data.frame(Simulacion = 1:input$nsim_exp, Suma = unname(colSums(sim_exp())))
    dat<-density(sim_exp$Suma)
    dat<-data.frame(Suma=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Suma, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$nexp/input$lambdaexp, sd = sqrt(input$nexp)/(input$lambdaexp)), col = "#1b98e0", size = 1.5) +
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Suma<input$c_exp2,Suma, input$c_exp2)), fill = "skyblue")
    
  })
  
  output$plot_norm_prob2<-renderPlot({
    sim_norm <- data.frame(Simulacion = 1:input$nsim_norm, Suma = unname(colSums(sim_norm())))
    dat<-density(sim_norm$Suma)
    dat<-data.frame(Suma=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Suma, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$nnorm*input$m_norm, sd = sqrt(input$nnorm)*(input$sd_norm)), col = "#1b98e0", size = 1.5) +
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Suma<input$c_norm2,Suma, input$c_norm2)), fill = "skyblue")
    
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
  
  
  output$plot_gam_prob1<-renderPlot({
    sim_gam <- data.frame(Simulacion = 1:input$nsim_gam, Media = unname(colMeans(sim_gam())))
    dat<-density(sim_gam$Media)
    dat<-data.frame(Media=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Media, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$lambdagam*input$alfagam, sd = sqrt(input$alfagam*input$lambdagam^2)/sqrt(input$ngam)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Media<input$c_gam1,Media, input$c_gam1)), fill = "skyblue")
    
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
  
  output$plot_gam_prob2<-renderPlot({
    sim_gam <- data.frame(Simulacion = 1:input$nsim_gam, Suma = unname(colSums(sim_gam())))
    dat<-density(sim_gam$Suma)
    dat<-data.frame(Suma=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Suma, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$ngam*input$lambdagam*input$alfagam, sd = sqrt(input$alfagam*input$lambdagam^2)*sqrt(input$ngam)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Suma<input$c_gam2,Suma, input$c_gam2)), fill = "skyblue")
    
  })

  ##################################### DISTRIBUCION UNIFORME######################################################################################################################  
  
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

# grafico DE LA DENSIDAD 
output$plot_uni_1<-renderPlot({
  sim_df <- data.frame(Simulacion = 1:input$nsim_unif, Media = unname(colMeans(sim_df())))
  dat<-density(sim_df$Media)
  dat<-data.frame(Media=dat$x,y=dat$y)
  ggplot(dat, mapping = aes(x = Media, y = y)) + geom_line()+
    ylab("Densidad") +
    #grafico de la densidad normal
    stat_function(fun = dnorm, args = list(mean = mean(sim_df$Media), sd =sd(sim_df$Media)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_area(mapping = aes(x = ifelse(Media<input$b,Media, input$b)), fill = "skyblue")
  
})




#GRAFICO SUMA DE VARIABLES

output$plot_uni_2 <- renderPlot({
  sim_df <- data.frame(Simulacion = 1:input$nsim_unif, Suma = unname(colSums(sim_df())))
  #NO ME QUEDA BIEN LA NORMAL APROXIMADA 
  ggplot(sim_df, aes(x = Suma)) + 
    geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
    geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
    stat_function(fun = dnorm, args = list(mean = mean(sim_df$Suma), sd = sd(sim_df$Suma)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
})
#PLOT DE PROBABILIDADES
output$pest_unif_3 <- renderUI({
  sim_df <- data.table(Simulacion = 1:input$nsim_unif, Suma = unname(colSums(sim_df())))[, Suma := ifelse(Suma < input$b, 1, 0)]
  x <- unlist(sim_df[,mean(Suma)])
  h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
})
###tengo q cambiar la probabilidad teorica y la simulada
output$pteo_unif_3<- renderUI({
  h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_gam2-input$ngam*input$lambdagam*input$alfagam)/(sqrt(input$alfagam*input$lambdagam^2)*sqrt(input$ngam))))))
})


#GRAFICO DE LA DESNSIDAD

output$plot_unif_prob2<-renderPlot({
  sim_df <- data.frame(Simulacion = 1:input$nsim_unif, Suma = unname(colSums(sim_df())))
  dat<-density(sim_df$Suma)
  dat<-data.frame(Suma=dat$x,y=dat$y)
  ggplot(dat, mapping = aes(x = Suma, y = y)) + geom_line()+
    ylab("Densidad") +
    stat_function(fun = dnorm, args = list(mean = mean(sim_df$Suma), sd = sd(sim_df$Suma)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_area(mapping = aes(x = ifelse(Suma<input$b,Suma, input$b)), fill = "skyblue")
  
})

##################################### DISTRIBUCION CHI-CUADRADO######################################################################################################################  

# Simulaciones 
sim_chi <-  reactive({
  req(input$nsim_chi >= 10)
  res_chi <- matrix(nrow = input$nchi, ncol = input$nsim_chi)
  for(j in 1:input$nsim_chi){
    res_chi[, j] <- rchisq(input$nchi, df= input$chi_grados, ncp=0)
  }
  colnames(res_chi) <- paste0("Simulacion_", 1:input$nsim_chi)
  res_chi
})


## Tablas con los resultados de la simulación
output$tb_chi <- function(){
  res_chi <<- data.table(sim_chi())
  
  res_unif %>% 
    kbl(booktabs = TRUE) %>%
    kable_styling(full_width = F, bootstrap_options = c("condensed"), font_size = 11) %>% 
    #column_spec(1, bold = TRUE, border_right = FALSE, border_left = FALSE) %>% 
    row_spec(0, background = "#33639f", color = "#ffffff") %>% 
    scroll_box(width = "1000px", height = "320px")
}
# Botón de descarga
output$download_chi <- downloadHandler(
  filename = function(){"DistribucionChiCuadrada.xlsx"},
  content = function(file){write_xlsx(as.data.frame(res_chi), path = file)}
)


# Gráfico que contrasta la densidad observada con la teórica
output$plot_chi = renderPlot({
  sim_chi <- data.frame(Simulacion = 1:input$nsim_chi, Media = unname(colMeans(sim_chi())))
  
  ggplot(sim_chi, aes(x = Media)) + 
    geom_histogram(aes(y =..density..), breaks = seq((min(sim_chi$Media) - 0.5), (max(sim_chi$Media) + 0.5), by = 0.05), colour = "#e42645", fill = "white") +
    geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
    stat_function(fun = dnorm, args = list(mean = mean(sim_chi$Media), sd = sd(sim_chi$Media)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
})

# Probabilidades estimadas y teóricas

#no me quedan bien 
output$pest_chi <- renderUI({
  sim_chi <- data.table(Simulacion = 1:input$nsim_chi, Media = unname(colMeans(sim_chi())))[, Marca := ifelse(Media < input$c_chi, 1, 0)]
  x <- unlist(sim_chi[,mean(Marca)])
  h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
})

output$pteo_chi <- renderUI({
  h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm(input$c_chi))))
})


# grafico DE LA DENSIDAD 

#no se hace dinamica
output$plot_chi_1<-renderPlot({
  sim_chi <- data.frame(Simulacion = 1:input$nsim_chi, Media = unname(colMeans(sim_chi())))
  dat<-density(sim_chi$Media)
  dat<-data.frame(Media=dat$x,y=dat$y)
  ggplot(dat, mapping = aes(x = Media, y = y)) + geom_line()+
    ylab("Densidad") +
    #grafico de la densidad normal
    stat_function(fun = dnorm, args = list(mean = mean(sim_chi$Media), sd =sd(sim_chi$Media)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_area(mapping = aes(x = ifelse(Media<input$chi_grados,Media, input$chi_grados)), fill = "skyblue")
  
})




#GRAFICO SUMA DE VARIABLES

output$plot_chi_2 <- renderPlot({
  sim_chi <- data.frame(Simulacion = 1:input$nsim_chi, Suma = unname(colSums(sim_chi())))
  
  ggplot(sim_chi, aes(x = Suma)) + 
    geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
    geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
    stat_function(fun = dnorm, args = list(mean = mean(sim_chi$Suma), sd = sd(sim_chi$Suma)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
})
#PLOT DE PROBABILIDADES

#toca hacer bien esta proba
output$pest_unif_3 <- renderUI({
  sim_chi <- data.table(Simulacion = 1:input$nsim_chi, Suma = unname(colSums(sim_chi())))[, Suma := ifelse(Suma < input$b, 1, 0)]
  x <- unlist(sim_df[,mean(Suma)])
  h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
})
###tengo q cambiar la probabilidad teorica y la simulada
output$pteo_unif_3<- renderUI({
  h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_gam2-input$ngam*input$lambdagam*input$alfagam)/(sqrt(input$alfagam*input$lambdagam^2)*sqrt(input$ngam))))))
})


#GRAFICO DE LA DESNSIDAD
#arreglar el grafico
output$plot_chi_prob2<-renderPlot({
  sim_chi <- data.frame(Simulacion = 1:input$nsim_chi, Suma = unname(colSums(sim_chi())))
  dat<-density(sim_chi$Suma)
  dat<-data.frame(Suma=dat$x,y=dat$y)
  ggplot(dat, mapping = aes(x = Suma, y = y)) + geom_line()+
    ylab("Densidad") +
    stat_function(fun = dnorm, args = list(mean = mean(sim_chi$Suma), sd = sd(sim_chi$Suma)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    geom_area(mapping = aes(x = ifelse(Suma<input$chi_grados,Suma, input$chi_grados)), fill = "skyblue")
  
})



##############################BOOTSTRAP#####################################

# Carga de archivo Excel

  #output$archivoboot <- renderTable(input$fileboot)
  output$cargaboot <- function(){
    inFile <- input$fileboot
  
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

  output$plot_boot<-renderHighchart({
    inFile <- input$fileboot
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    res <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    hchart(res) %>% 
      hc_title(text = "Histograma",align="center",width="25") |> 
      hc_plotOptions(series = list(animation = FALSE)) |> 
      hc_add_theme(hc_theme_bloom())
  })
  
  output$IC_trad <- renderUI({
    inFile <- input$fileboot
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    res <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    x<-int_conf_trad(as.vector(res),1-input$nivel_confboot)
    a<-x$ICI
    b<-x$ICS
    h4(withMathJax(sprintf("Intervalo de confianza tradicional: [%.02f,%.02f]",a,b )))
  })
  
  
  output$IC_boot <- renderUI({
    inFile <- input$fileboot
    if(is.null(inFile))
      return(NULL)
    file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
    res <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    x<-int_conf_boot(as.vector(res),1-input$nivel_confboot)
    a<-x$ICI
    b<-x$ICS
    h4(withMathJax(sprintf("Intervalo de confianza bootstrap: [%.02f,%.02f]",a,b )))
  })

# Gráfico de la trayectoria del proceso de nacimiento y muerte
  output$plot_proceso = renderPlot({
      
      res <- trayectoria(input$lambda, input$mu, input$njumps)
      saltos <- res$Saltos
      tiempos <- res$Tiempo
      
      plot(tiempos, saltos, type="l", lty=1, lwd=2, col=4, xlab="Tiempo", ylab="Individuos", 
           panel.first=grid())
  })

  
})







