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
    t = setTimeout(logout, 6000000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

fluidPage(
  tags$script(inactivity),
  tags$h1("ELM Predictions"),
  sidebarLayout(
    sidebarPanel(tags$h3("Categorical variables"),
                selectInput("franchise", "Franchise", levs$FRANCHISE),
                selectInput("race_eth", "Race", sort(levs$RACE_ETH)),
                selectInput("sector", "Sector", levs$SECTOR),
                selectInput("sex", "Sex", levs$SEX),
                selectInput("vet", "Veteran status",levs$VET)),
    mainPanel(
	        tabsetPanel(
			    tabPanel("Mean predictive surface",
                                    fluidRow(
                                    plotlyOutput("surface"),
				    )),
			    tabPanel("Posterior pred. density",
				    br(),
				    checkboxInput("display.prev", "Display previous density",FALSE),
                                    plotOutput("density")
			            ),
			    tabPanel("Factor violin levels",
				    br(),
				    selectInput("factor", "Variable to facet", list("Franchise"="FRANCHISE",
										    "Race/Ethnicity"="RACE_ETH",
										    "Sector"="SECTOR",
										    "Sex"="SEX",
										    "Veteran"="VET")),
			           plotOutput("facet_densities")
			    )
		           ),
	                   br(),
                           fluidRow(column(3, offset=2, sliderInput("receipts", "Receipts ($1,000)", 
                                       	     min=0, max=receipt_max, value=100)),
                                       column(3, offset=1, sliderInput("employment", "Number of employees", 
                                       	     min=0, max=employ_max, value=10))))
             )
       )
  
