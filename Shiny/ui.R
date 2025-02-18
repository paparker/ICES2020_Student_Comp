library(shiny)
library(ggplot2)
library(readr)
library(plotly)

mod <- read_rds("../Data/elm.rds")
mod$factor_levels$RACE_ETH <- dplyr::recode(mod$factor_levels$RACE_ETH,
"Amer. Indian (Non-Hispanic)"                      = "Amer. Indian",
"Amer. Indian and Alaska Native (Non-Hispanic)"    = "Amer. Indian and Alaska Native",
"Amer. Indian and Pacific Islander (Non-Hispanic)" = "Amer. Indian and Pacific Islander",
"Amer. Indian and Some other race (Non-Hispanic)"  = "Amer. Indian and Some other race",
"Asian (Non-Hispanic)"                      = "Asian",
"Asian and Pacific Islander (Non-Hispanic)" = "Asian and Pacific Islander",
"Asian and Some other race (Non-Hispanic)"  = "Asian and Some other race",
"Black (Non-Hispanic)"                      = "Black",
"Black and Amer. Indian (Non-Hispanic)"     = "Black and Amer. Indian",
"Black and Asian (Non-Hispanic)"            = "Black and Asian",
"Black and Pacific Islander (Non-Hispanic)" = "Black and Pacific Islander",
"Black and Some other race (Non-Hispanic)"  = "Black and Some other race",
"Non-white (Hispanic)"                      = "Non-white (Hispanic)",
"Pacific Islander (Non-Hispanic)"           = "Pacific Islander",
"Pacific Islander and Some other race (Non-Hispanic)" = "Pacific Islander and Some other race",
"Some other race (Non-Hispanic)"         = "Some other race",
"White (Hispanic)"                       = "White (Hispanic)",
"White (Non-Hispanic)"                   = "White (Non-Hispanic)",
"White and Amer. Indian (Non-Hispanic)"  = "White and Amer. Indian",
"White and Asian (Non-Hispanic)"         = "White and Asian",
"White and Black (Non-Hispanic)"         = "White and Black",
"White and Pacific Islander (Non-Hispanic)" = "White and Pacific Islander",
"White and Some other race (Non-Hispanic)"  = "White and Some other race")

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
    t = setTimeout(logout, 60000000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

fluidPage(
  tags$script(inactivity),
  tags$h1("Interactive Visualization of Nonlinear Survey Models", align='center'),
  sidebarLayout(
    sidebarPanel(tags$h3("Categorical variables"),
                selectInput("franchise", "Franchise", levs$FRANCHISE),
                selectInput("race_eth", "Race", sort(levs$RACE_ETH)),
                selectInput("sector", "Sector", levs$SECTOR),
                selectInput("sex", "Sex", levs$SEX),
                selectInput("vet", "Veteran status",levs$VET), width=3),
    mainPanel(
	        tabsetPanel(
			    tabPanel("Mean predictive surface",
                                    fluidRow(
                                         plotlyOutput("surface")
				       )
				    ),
			    tabPanel("Posterior pred. density",
				    br(),
				    checkboxInput("display.prev",
						  "Display previous density",
						  FALSE),
                                    plotOutput("density")
			            ),
			    tabPanel("Violin plots by factor",
				    br(),
				    selectInput("factor", "Variable to facet",
					       	list("Franchise"="FRANCHISE",
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
