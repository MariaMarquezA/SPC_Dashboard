print_Points <- function(t_bind, c_obs, dates, axis){
  renderPlot({
    library(ggplot2)
    if(axis){
      x <- c_obs
      text <- "Sample (Sample no.)"
    } else {
      x<- dates[c_obs]
      text <- "Sample (Date)"
    }
    n = length(c_obs)
    plot = ggplot(t_bind[c_obs,]) + 
      geom_point(mapping = aes(x,P1, color = 'P1')) +
      geom_point(mapping = aes(x,P2, color = 'P2')) +
      geom_point(mapping = aes(x,P3, color = 'P3')) +
      geom_point(mapping = aes(x,P4, color = 'P4')) +
      geom_point(mapping = aes(x,P5, color = 'P5')) +
      geom_point(mapping = aes(x,P6, color = 'P6')) +
      geom_point(mapping = aes(x,P7, color = 'P7')) +
      geom_point(mapping = aes(x,P8, color = 'P8')) +
      geom_point(mapping = aes(x,P9, color = 'P9')) +
      geom_point(mapping = aes(x,P10, color = 'P10')) +
      geom_point(mapping = aes(x,P11, color = 'P11')) +
      geom_point(mapping = aes(x,P12, color = 'P12')) +
      geom_point(mapping = aes(x,P13, color = 'P13')) +
      geom_point(mapping = aes(x,P14, color = 'P14')) +
      geom_point(mapping = aes(x,P15, color = 'P15')) +
      geom_point(mapping = aes(x,P16, color = 'P16')) +
      geom_point(mapping = aes(x,P17, color = 'P17')) +
      geom_point(mapping = aes(x,P18, color = 'P18')) +
      geom_point(mapping = aes(x,P19, color = 'P19')) +
      geom_line(mapping = aes(x,0.4, color = 'red')) +
      geom_line(mapping = aes(x,-0.4, color = 'red')) +
      labs(x = text, y = 'Milimeters', color = 'Points') +
      theme_bw(base_size = 15)
    return(plot)
  })
}

print_T2 <- function(t_ic, t_oc, c_obs, dates, axis){
  renderPlot({
    library(ggplot2)
    T2_data <- T2(t_ic, t_oc)
    n_T2 <- length(T2_data)
    UCL = (((n_T2-1)^2)/n_T2)*(qbeta(.9973,19/2,((n_T2-19-1)/2)))
    
    sample= seq(1,n_T2)
    
    if(axis){
      x <- sample[c_obs]
      text <- "Sample (Sample no.)"
    } else {
      x <- dates[c_obs]
      text <- "Sample (Date)"
    }
    
    graph <- as.data.frame(cbind(sample,T2_data))
    ggplot(graph[c_obs,]) +geom_line(mapping=aes(x=x,y=T2_data)) + 
      geom_line(mapping = aes(x=x,y=UCL), colour = 'tomato', size = 1) +
      labs(x = text, y = expression(T^{2})) +
      theme_bw(base_size = 15)
  })
}

print_CUSUM <- function(t_ic, t_oc, c_obs, dates, axis, shift = 0.01){
  renderPlot({
    library(ggplot2)
    Cn <- CUSUM(t_ic, t_oc, shift = shift)
    H <- getH_w(t_ic, t_oc, shift = shift)
    sample <- seq(1,max(1,length(Cn)))
    
    if(axis){
      x <- sample[c_obs]
      text <- "Sample (Sample no.)"
    } else {
      x <- dates[c_obs]
      text <- "Sample (Date)"
    }
    
    graph <- as.data.frame(cbind(sample,Cn))
    ggplot(graph[c_obs,]) +
      geom_line(mapping=aes(x=x,y=Cn)) +
      geom_line(mapping = aes(x=x,y= H), colour = 'tomato', size =1) +
      labs(x = text, y = "CUSUM")+
      theme_bw(base_size = 15)
  })
}