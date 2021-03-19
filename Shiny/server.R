library(shiny)
library(ggplot2)
library(readr)
library(dplyr)

mod <- read_rds("../Data/elm.Rds")
levs <- mod$factor_levels

employ_center <- 25.06129
employ_scale <- 162.4666
employ_max <- 116.792844
employ_min <- -0.154255

receipt_center <- 5834.202
receipt_scale <- 54251.1
receipt_max <- 132.6086660
receipt_min <- -0.1075407

function(input, output) {
  vals <- reactiveValues()
  observe({
    req(input$eth, input$franchise, input$race, input$sector,
       	input$sex, input$vet, input$receipts, input$employment)

    employ_norm <- (input$employment-employ_center)/employ_scale
    receipts_norm <- (input$receipts-receipt_center)/receipt_scale

    vals$init <- list(RECEIPTS_NOISY = receipts_norm,
			     EMPLOYMENT_NOISY = employ_norm,
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
				xlim(c(0,1)) +
				ylim(c(0,.5)) +
				labs(title="Posterior density", y="freq")
    )

   ngrid <- input$ngrid
   receipt_vals <- seq(receipt_min, receipt_max, length.out=ngrid)
   employ_vals <- seq(employ_min, employ_max, length.out=ngrid)

   rec_emp_mat <- expand.grid(receipt_vals, employ_vals)
   cat_mat <- vals$predvec[-c(1,2)]
   vals$predmat <- as.matrix(cbind(rec_emp_mat, t(cat_mat)))
   vals$Xpred_surf <- plogis(cbind(1,vals$predmat)%*%mod$A)
   vals$predsurf <- plogis(vals$Xpred_surf%*%t(mod$beta)) %>% rowMeans()

   df2 <- data.frame(rec_emp_mat,vals$predsurf)
   colnames(df2) <- c("x","y","z")

   #rescale back to original units
   df2 <- df2 %>% mutate(x=receipt_scale*x+receipt_center)
   df2 <- df2 %>% mutate(y=employ_scale*y+employ_center)
   output$surface <- renderPlot(
        			ggplot(df2) +
        				geom_raster(aes(x=x,y=y,fill=z)) +
					labs(title="Predictive surface",
					     x="Receipts", y="Employment",
					     fill="Mean Response")
    )
  })
}
