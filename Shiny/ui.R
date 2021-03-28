library(shiny)
library(ggplot2)
library(readr)
library(plotly)

mod <- read_rds("../Data/elm.rds")
levs <- mod$factor_levels

employ_max <- 20
employ_min <- 0

receipt_max <- 400
receipt_min <- 0

features <- c("FRANCHISE","RACE_ETH","SECTOR","SEX","VET")

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 60000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 600000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

fluidPage(
  tags$script(inactivity),
  tags$h1("ELM Predictions"),
  sidebarLayout(
    #Discrete variables on the left, 
    #plot with two numeric variables on the axes and posterior mean surface
    #Below that, user selects a value for the two numeric variables
    #second plot shows posterior predictive distribution for all inputs
    sidebarPanel(tags$h2("Features"),
                selectInput("franchise", "Franchise", levs$FRANCHISE),
                selectInput("race_eth", "Race", levs$RACE_ETH),
                selectInput("sector", "Sector", levs$SECTOR),
                selectInput("sex", "Sex", levs$SEX),
                selectInput("vet", "Veteran status",levs$VET)),
    mainPanel(
	        tabsetPanel(
			    tabPanel("Mean predictive surface",
                                    fluidRow(
                                    plotlyOutput("surface"),
                                       column(3, offset=2, numericInput("receipts", "Establishment Receipts", 
                                       	     min=0, max=receipt_max, value=100)),
                                       column(3, offset=1, numericInput("employment", "Establishment employment", 
                                       	     min=0, max=employ_max, value=10)))),
			    tabPanel("Posterior predictive...",
				    br(),
                                    plotOutput("density")
			            ),
			    tabPanel("All factor levels",
				    br(),
				    selectInput("factor", "Variable to facet", features),
			           plotOutput("facet_densities")
			    )
		           )
             )
       )
  )
