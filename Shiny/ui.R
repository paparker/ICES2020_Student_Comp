library(shiny)
library(ggplot2)
library(readr)

mod <- read_rds("../Data/elm.Rds")
levs <- mod$factor_levels

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
    t = setTimeout(logout, 60000);  // time is in milliseconds (1000 is 1 second)
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
                selectInput("eth", "Ethnicity", levs$ETH),
                selectInput("franchise", "Franchise", levs$FRANCHISE),
                selectInput("race", "Race", levs$RACE),
                selectInput("sector", "Sector", levs$SECTOR),
                selectInput("sex", "Sex", levs$SEX),
                selectInput("vet", "Veteran status",levs$VET),
                numericInput("receipts", "Establishment Receipts", 
			     min=0, max=7200000, value=100),
                numericInput("employment", "Establishment employment", 
			     min=0, max=19000, value=100)),
    mainPanel(
      plotOutput("density"))
  )
)
