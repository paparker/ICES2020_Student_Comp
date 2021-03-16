library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

mod <- read_rds("../Data/elm.Rds")
levs <- mod$factor_levels

function(input, output) {
  vals <- reactiveValues()
  observe({
    req(input$eth, input$franchise, input$race, input$sector,
       	input$sex, input$vet, input$receipts, input$employment)

    vals$init <- list(RECEIPTS_NOISY = input$receipts,
			     EMPLOYMENT_NOISY = input$employment,
			     SEX = input$sex,
			     ETH = input$eth,
                             RACE = input$race,
                             VET = input$vet,
                             SECTOR = input$sector,
			     FRANCHISE = input$franchise
			     )

    vals$predvec <- model.matrix(formula(~ (RECEIPTS_NOISY+EMPLOYMENT_NOISY+
					   SEX+ETH+RACE+VET+SECTOR+FRANCHISE)-1),
                                      data=vals$init, xlev=mod$factor_levels)

    vals$Xpred <- plogis(cbind(1,vals$predvec)%*%mod$A)
    vals$preds <- plogis(vals$Xpred%*%t(mod$beta))

    df1 <- data.frame(t(vals$preds))
    colnames(df1)<-"x"

    output$density <- renderPlot(
		         	ggplot(df1, aes(x=x)) +
				geom_histogram(aes(y=stat(count)/sum(count)),
					   fill="#69b3a2", alpha=.75, 
						   color="black") +
				geom_vline(color="red", 
					   aes(xintercept=mean(vals$preds))) +
				labs(title="Posterior density", y="freq")
    )
  })
}
