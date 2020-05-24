#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Dashboard"),
    sidebarPanel(dateInput("date1", "From:", value = "2019-03-26"),
                 timeInput("time1", "Time:", seconds = FALSE, value = strptime("12:00:00", "%H:%M:%S"))),
    sidebarPanel(dateInput("date2", "To:", value = "2019-03-28"),
                 timeInput("time2", "Time:", seconds = FALSE, value = strptime("12:00:00", "%T"))),
    # Show a plot of the generated distribution
    mainPanel(actionButton("b1", "Execute"),
              checkboxInput("checkbox", label = "X axis as samples", value = FALSE),
        textOutput("label1"),
        textOutput("label2"),
        tabsetPanel(
            tabPanel("Points", plotOutput("points")),
            tabPanel("T2", plotOutput("T2")),
            tabPanel("CUSUM+",plotOutput("CUSUM_p")),
            tabPanel("CUSUM-",plotOutput("CUSUM_n"))
        )
    )
))
