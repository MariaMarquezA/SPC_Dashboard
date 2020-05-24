#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("source/control_charts/T2.R")
source("source/control_charts/CUSUM.R")
source("source/control_charts/getH_w.R")
source("source/utilities/get_utilization.R")
source("source/utilities/print_plots.R")
source("source/utilities/readFromDB.R")

table <- readFromDB(display = FALSE)
t_ic <- table[132:368,]

t_ic <- t_ic[-c(22, 38, 40, 90, 95, 132, 198, 229),]

t_ic <- t_ic[41:229,]
t_ic <- t_ic[-55,]
t_ic <- t_ic[-91,]
t_ic <- t_ic[-187,]
t_oc <- table[368:1132,]

t_bind <- rbind(t_ic, t_oc)
n_bind <- length(t_bind[,1])
c_obs <- (n_bind-50):n_bind

t_dates <- as.data.frame(as.POSIXct(t_bind[,1]))
names(t_dates)[1] <- "d"
tbind <- t_bind[,-1]
t_ic <- t_ic[,-1]
t_oc <- t_oc[,-1]
time_stats <- get_utilization(t_dates, c_obs, 3*mean(diff(t_dates$d)[diff(t_dates$d)<100]))

# Define server logic required to draw a histogram
main <- function(input, output) {
    
    observeEvent(input$b1, {
        date1 <- as.POSIXct(paste(input$date1, format(as.POSIXct(input$time1), "%H:%M:%S")))
        date2 <- as.POSIXct(paste(input$date2, format(as.POSIXct(input$time2), "%H:%M:%S")))
        
        if(date2 > date1){
        c_obs <- which(t_dates$d %in% t_dates$d[t_dates$d>=date1 & t_dates$d<=date2])
        time_stats <- get_utilization(t_dates, c_obs, 3*mean(diff(t_dates$d)[diff(t_dates$d)<100]))
        
        output$label2 <- renderText({ sprintf("Active Time: %.2f min, Total Time: %.2f min, Ulitization: %.2f%%", time_stats[1]/60, time_stats[2]/60, time_stats[3]*100) })
        output$label1 <- renderText({ paste("There are", length(c_obs), "data points in display")})
        output$points <- print_Points(t_bind, c_obs, t_dates$d, input$checkbox)
        output$T2 <- print_T2(t_ic, t_oc, c_obs, t_dates$d, input$checkbox)
        output$CUSUM_p <- print_CUSUM(t_ic, t_oc, c_obs, t_dates$d, input$checkbox, shift =  0.01)
        output$CUSUM_n <- print_CUSUM(t_ic, t_oc, c_obs, t_dates$d, input$checkbox, shift = -0.01)
        }
        else {
            output$label <- renderText({ "Invalid date lapse"})
        }
    })
    
    output$label1 <- renderText({ paste("There are", length(c_obs), "data points in display")})
    output$label2 <- renderText({ sprintf("Active Time: %.2f min, Total Time: %.2f min, Ulitization: %.2f%%", time_stats[1]/60, time_stats[2]/60, time_stats[3]*100) })
    output$points <- print_Points(t_bind, c_obs, t_dates$d, input$checkbox)
    output$T2 <- print_T2(t_ic, t_oc, c_obs, t_dates$d, input$checkbox)
    output$CUSUM_p <- print_CUSUM(t_ic, t_oc, c_obs, t_dates$d, input$checkbox, shift =  0.01)
    output$CUSUM_n <- print_CUSUM(t_ic, t_oc, c_obs, t_dates$d, input$checkbox, shift = -0.01)
}

shinyServer(main)
