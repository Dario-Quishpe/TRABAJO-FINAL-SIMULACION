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







