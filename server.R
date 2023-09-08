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
############################################FUNCION QUE DETERMINA EL KS################################
#funcion paradeterminar el ks
TestKS <- function(x, y){
    vars <- data.frame(y,x)
    vars_e <- subset(vars,subset=vars[,1]==1)
    vars_f <- subset(vars,subset=vars[,1]==0)
    ks <- suppressWarnings(ks.test(vars_e[,2],vars_f[,2],alternative="two.sided"))
    ks <- round(as.numeric(ks$statistic),4)
  return(ks)
}

shinyServer(function(input, output, session){#######################Server#####################################

  
############################################DISTRIBUCIONES DISCRETAS################################
  


###########################################DISTRIBUCION BINOMIAL####################################  
  
  
  ######################################DISTRIBUCION BINOMIAL########################################
  
  sim_binom <-  reactive({
    req(input$nsim_binom >= 10)
    res_binom <- matrix(nrow = input$nbinom, ncol = input$nsim_binom)
    for(j in 1:input$nsim_binom){
      res_binom[, j] <- rbinom(input$nbinom,input$ensayos,input$pbinom)
    }
    colnames(res_binom) <- paste0("Simulacion_", 1:input$nsim_binom)
    res_binom
  })
  
  
  output$tb_binom <- function(){
    res_binom <<- data.table(sim_binom())
    
    res_binom %>% 
      kbl(booktabs = TRUE) %>%
      kable_styling(full_width = F, bootstrap_options = c("condensed"), font_size = 11) %>% 
      #column_spec(1, bold = TRUE, border_right = FALSE, border_left = FALSE) %>% 
      row_spec(0, background = "#33639f", color = "#ffffff") %>% 
      scroll_box(width = "1000px", height = "320px")
  }
  
  #Boton descarga
  
  output$download_binom <- downloadHandler(
    filename = function(){"Distribucion_Binomial.xlsx"},
    content = function(file){write_xlsx(as.data.frame(res_binom), path = file)}
  )
  
  
  output$plot_binom1 <- renderPlot({
    sim_binom <- data.frame(Simulacion = 1:input$nsim_binom, Media = unname(colMeans(sim_binom())))
    
    ggplot(sim_binom, aes(x = Media)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean =input$ensayos*input$pbinom, sd = sqrt(input$ensayos*input$pbinom*(1-input$pbinom))/sqrt(input$nbinom)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  output$pest_binom1 <- renderUI({
    sim_binom <- data.table(Simulacion = 1:input$nsim_binom, Media = unname(colMeans(sim_binom())))[, Marca := ifelse(Media < input$c_binom1, 1, 0)]
    x <- unlist(sim_binom[,mean(Marca)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  output$pteo_binom1<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_binom1-input$ensayos*input$pbinom)/(sqrt(input$ensayos*input$pbinom*(1-input$pbinom))/sqrt(input$nbinom))))))
  })
  
  ##Probabilidad 1
  
  output$plot_binom_prob1<-renderPlot({
    sim_binom <- data.frame(Simulacion = 1:input$nsim_binom, Media = unname(colMeans(sim_binom())))
    dat<-density(sim_binom$Media)
    dat<-data.frame(Media=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Media, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean =input$ensayos*input$pbinom, sd = sqrt(input$ensayos*input$pbinom*(1-input$pbinom))/sqrt(input$nbinom)), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Media<input$c_binom1,Media, input$c_binom1)), fill = "skyblue")
    
  })
  
  
  ##Suma
  
  output$plot_binom2 <- renderPlot({
    sim_binom <- data.frame(Simulacion = 1:input$nsim_binom, Suma = unname(colSums(sim_binom())))
    
    ggplot(sim_binom, aes(x = Suma)) + 
      geom_histogram(aes(y =..density..),colour = "#e42645", fill = "white") +
      geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean =input$nbinom*input$ensayos*input$pbinom, sd = sqrt(input$nbinom*input$ensayos*input$pbinom*(1-input$pbinom))), col = "#1b98e0", size = 1.5) + 
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
  })
  
  
  #Probabilidad 2 simulada
  
  output$pest_binom2 <- renderUI({
    sim_binom <- data.table(Simulacion = 1:input$nsim_binom, Suma = unname(colSums(sim_binom())))[, Suma := ifelse(Suma < input$c_binom2, 1, 0)]
    x <- unlist(sim_binom[,mean(Suma)])
    h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })
  
  
  output$pteo_binom2<- renderUI({
    h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", pnorm((input$c_binom2-input$nbinom*input$ensayos*input$pbinom)/(sqrt(input$nbinom*input$ensayos*input$pbinom*(1-input$pbinom)))))))
  })
  
#Grafica
  
  output$plot_binom_prob2<-renderPlot({
    sim_binom <- data.frame(Simulacion = 1:input$nsim_binom, Suma = unname(colSums(sim_binom())))
    dat<-density(sim_binom$Suma)
    dat<-data.frame(Suma=dat$x,y=dat$y)
    ggplot(dat, mapping = aes(x = Suma, y = y)) + geom_line()+
      ylab("Densidad") +
      stat_function(fun = dnorm, args = list(mean = input$nbinom*input$ensayos*input$pbinom, sd = sqrt(input$nbinom*input$ensayos*input$pbinom*(1-input$pbinom))), col = "#1b98e0", size = 1.5) +
      theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      geom_area(mapping = aes(x = ifelse(Suma<input$c_binom2,Suma, input$c_binom2)), fill = "skyblue")
    
  })
  
  
  
  
  
  
  
  
  
#############################################DISTRIBUCIONES CONTINUAS###############################  
  
  
  
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
  ##################################### DISTRIBUCION DE PARETO ####################################################################################################################
# Simulaciones pareto  
sim_pareto_<-  reactive({
  req(input$nsim_pareto >= 10)
  res_pareto <- matrix(nrow = input$npareto, ncol = input$nsim_pareto)
  for(j in 1:input$nsim_pareto){
    res_pareto[, j] <- rpareto(input$npareto, input$b_pareto,input$a_pareto)
  }
  colnames(res_pareto) <- paste0("Simulacion_", 1:input$nsim_pareto)
  res_pareto
})

## Tablas con los resultados de la simulación_PAreto
output$tb_pareto <- function(){
  res_pareto <<- data.table(sim_pareto_())
  
  res_pareto %>% 
    kbl(booktabs = TRUE) %>%
    kable_styling(full_width = F, bootstrap_options = c("condensed"), font_size = 11) %>% 
    #column_spec(1, bold = TRUE, border_right = FALSE, border_left = FALSE) %>% 
    row_spec(0, background = "#33639f", color = "#ffffff") %>% 
    scroll_box(width = "1000px", height = "320px")
}
# Botón de descarga
output$download_pareto <- downloadHandler(
      filename = function(){"DistribuciondePareto.xlsx"},
      content = function(file){write_xlsx(as.data.frame(res_pareto), path = file)}
)
# Gráfico que contrasta la densidad observada con la teórica
output$plot_Pareto_ggplot = renderPlot({
  sim_pareto <- data.frame(Simulacion = 1:input$nsim_pareto, Media = unname(colMeans(sim_pareto_())))
  
  ggplot(sim_pareto, aes(x = Media)) + 
    geom_histogram(aes(y =..density..), breaks = seq((min(sim_pareto$Media) - 1), (max(sim_pareto$Media) + 1), by = 0.05), colour = "#e42645", fill = "white") +
    geom_density() + stat_density(geom="line", color = "#e42645", linewidth = 1) + ylab("Densidad") +
    stat_function(fun = dnorm, args = list(mean = mean((sim_pareto$Media)), sd = sd(sim_pareto$Media)), col = "#1b98e0", size = 1.5) + 
    theme_light(base_size = 18) + theme(plot.title = element_text(hjust = 0.5),
                                        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
})
# Probabilidades estimadas y teóricas
output$pest_pareto <- renderUI({
      sim_pareto <- data.table(Simulacion = 1:input$nsim_pareto, Media = unname(colMeans(sim_pareto_())))[, Marca := ifelse(Media < input$c_pareto, 1, 0)]
        x <- unlist(sim_pareto[,mean(Marca)])
        h4(withMathJax(sprintf("La probabilidad buscada es igual a: %.03f", x)))
  })

output$pteo_pareto <- renderUI({
      h4(withMathJax(sprintf("La probabilidad teórica es igual a: %.03f", ppareto(input$c_pareto,input$b_pareto,input$a_pareto))))
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

##############################KS#####################################
# Carga de archivo Excel
output$archivoks <- renderTable(input$file2)

output$cargaks <- function(){
      inFile <- input$file2

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

#Grafica de las distribuciones acumuladas ks
output$grafica_distribuciones <- renderPlot({   
  inFile <- input$file2
  
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
  res <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  eleccion<-as.integer(input$variable)
  
  # Comparativa de funciones de distribución
  buenos <- ecdf(res %>% dplyr::filter(VarDep == 0) %>% pull(colnames(res)[eleccion]))
  malos <- ecdf(res %>% dplyr::filter(VarDep == 1) %>% pull(colnames(res)[eleccion]))
  
  grid_var <- unique(res %>% pull(colnames(res)[eleccion]))
  prob_acumulada_ecdf_b <- buenos(v = grid_var)
  prob_acumulada_ecdf_m <- malos(v = grid_var)
  
  df_ecdf <- data.frame(var = grid_var, buenos = prob_acumulada_ecdf_b, malos = prob_acumulada_ecdf_m) %>%
    pivot_longer(cols = c(buenos, malos), names_to = "Marca", values_to = "ecdf")
  
  grafico_ecdf <- ggplot(data = df_ecdf, aes(x = var, y = ecdf, color = Marca)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("gray60", "orangered1")) +
    labs(color = "Marca", y = "Probabilidad acumulada", x = colnames(res)[eleccion]) +
    theme_bw() +
    theme(legend.position = "bottom", plot.title = element_text(size = 15))
  
  #grafico_ecdf
  abs_dif <-  abs(prob_acumulada_ecdf_b - prob_acumulada_ecdf_m)
  distancia_ks <- max(abs_dif)
  paste("Distancia Kolmogorov–Smirnov:", distancia_ks)
  indice_ks <- which.max(abs_dif)
  grafico_ecdf + 
    geom_segment(aes(x = grid_var[indice_ks], xend = grid_var[indice_ks],
                     y = prob_acumulada_ecdf_b[indice_ks], yend = prob_acumulada_ecdf_m[indice_ks]),
                 arrow = arrow(ends = "both", length = unit(0.2,"cm")), color = "black")

})
### GRafica comparativa de las densidades
output$grafica_densidades<-renderHighchart({
  inFile <- input$file2
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
  res <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  eleccion<-as.integer(input$variable)
  
  vars <- data.frame(res$VarDep,res[,eleccion])
  vars_e <- subset(vars,subset=vars[,1]==1)
  vars_f <- subset(vars,subset=vars[,1]==0)
  hc <- hchart(
    density(vars_e[,2]), type = "area", 
    color = "steelblue", name = "bueno"
  ) %>%
    hc_add_series(
      density(vars_f[,2]), type = "area",
      color = "#B71C1C", 
      name = "malo") |> 
        hc_title(text = "Comparacion de densidades") %>%
        hc_add_theme(hc_theme_economist())
  
})

##  infoKS
output$info_boxKs <- renderInfoBox({
  inFile <- input$file2
  
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath, paste(inFile$datapath, ".xlsx", sep=""))
  res <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  eleccion<-as.integer(input$variable)
  ks<-TestKS(res[,eleccion],res$VarDep)
  infoBox(paste("El ks calculado de la variable ",colnames(res[,eleccion])," es =",ks),  icon = icon("list", lib = "glyphicon"),
          color = "yellow", fill = TRUE)
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







