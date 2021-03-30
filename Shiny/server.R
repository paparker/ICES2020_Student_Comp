library(shiny)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(ggthemes)
library(plotly)

mod <- read_rds("../Data/elm.rds")
levs <- mod$factor_levels

employ_max <- 20
employ_min <- 0

receipt_max <- 400
receipt_min <- 0

function(input, output) {
  vals <- reactiveValues()
  observe({
    req(input$franchise, input$race_eth, input$sector,
       	input$sex, input$vet, input$receipts, input$employment)

    employ_norm <- log(input$employment+1)
    receipts_norm <- log(input$receipts+1)

    vals$init <- list(RECEIPTS_NOISY = receipts_norm,
			     EMPLOYMENT_NOISY = employ_norm,
			     SEX = input$sex,
                             RACE_ETH = input$race_eth,
                             VET = input$vet,
                             SECTOR = input$sector,
			     FRANCHISE = input$franchise
			     )

    vals$predvec <- model.matrix(formula(~ (RECEIPTS_NOISY+
					    EMPLOYMENT_NOISY+SEX+RACE_ETH+
					    VET+SECTOR+FRANCHISE)-1),
                                      data=vals$init, 
				      xlev=mod$factor_levels)

    vals$Xpred <- plogis(cbind(1,vals$predvec)%*%mod$A)
    vals$preds <- plogis(vals$Xpred%*%t(mod$beta))

    df1 <- data.frame(t(vals$preds))
    colnames(df1)<-"x"
    if(exists("df.prev") & input$display.prev){
	  joint.df <- rbind(cbind(df1,id=1), cbind(df.prev,id=2))
	  joint.df$id <- as.factor(joint.df$id)
          output$density <- renderPlot(
		         	(ggplot(group_by(joint.df,id), mapping=aes(x=x), f) +
				  geom_density(mapping=aes(fill=id, alpha=id)) +
				  geom_vline(color="red", size=.125,
					   aes(xintercept=mean(vals$preds))) +
				  labs(title="Posterior Probability of Primary Income Source",
				     x="Probability", y=NULL)+
				  guides(alpha=FALSE, 
					 fill=guide_legend(override.aes=list(alpha=c(0.3,0.05)))
					 ) +
				  scale_fill_manual(name="Prediction", 
						    labels=c("Current", "Previous"), 
						    values=c("green","red"))+
				  scale_alpha_manual(values=c(0.3, 0.05))+ #make previous pred lighter
				  theme_classic()) 
                      )
         df.prev <<- df1
    }else{
         df.prev <<- df1
         output$density <- renderPlot(
		         	(ggplot() +
				  geom_density(df1, mapping=aes(x=x), fill='green', alpha=0.3) +
				  geom_vline(color="red", size=.125,
					   aes(xintercept=mean(vals$preds))) +
				  labs(title="Posterior Probability of Primary Income Source",
				     x="Probability", y=NULL)+
				  scale_fill_manual(name="Prediction", 
						    labels=c("Current", "Previous"), 
						    values=c("green","red"))+
				  guides(alpha=FALSE,
					 fill=guide_legend(override.aes=list(alpha=c(0.3,0.05)))
					 ) +
				  theme_classic())
                      )
    }

   ngrid <- 110
   receipt_vals <- seq(receipt_min, receipt_max, length.out=ngrid)
   employ_vals <- seq(employ_min, employ_max, length.out=ngrid)

   rec_emp_mat <- expand.grid(receipt_vals, employ_vals)
   cat_mat <- vals$predvec[-c(1,2)]
   vals$predmat <- as.matrix(cbind(log(rec_emp_mat+1), t(cat_mat)))
   vals$Xpred_surf <- plogis(cbind(1,vals$predmat)%*%mod$A)
   vals$predsurf <- plogis(vals$Xpred_surf%*%t(mod$beta)) %>% rowMeans()

   df2 <- data.frame(rec_emp_mat,vals$predsurf)
   colnames(df2) <- c("x","y","z")

   #rescale back to original units
   df2 <- df2 %>% mutate(x=x)
   df2 <- df2 %>% mutate(y=y)
   plotly.tmp <- (ggplot(df2, mapping=aes(text=paste("Receipts:", round(x), "\n", 
						     "Employment:", round(y), "\n",
						     "Mean response:", round(z,2)))) +
       		    geom_raster(mapping=aes(x=x,y=y,fill=z)) +
        	      scale_fill_viridis_c()+
		    	labs(title="Predictive surface",
		    	     x="Receipts", y="Employment",
		    	     fill="Mean Response")+
		    	  geom_hline(color='red1', linetype='dashed', 
					 size=.125, aes(yintercept=input$employment))+
		    	  geom_vline(color='red1', linetype='dashed', 
					 size=.125, aes(xintercept=input$receipts))+
		    	  theme_classic()+
		    	  xlim(c(0,receipt_max))+
		    	  ylim(c(0,employ_max)))

   output$surface <- renderPlotly(
			  ggplotly(plotly.tmp, tooltip = c("text"))
    )

   
    facet_var <- input$factor 
    facet_levs <- mod$factor_levels[[facet_var]]
    facet_n <- length(facet_levs)
    facet_mat <-  do.call("rbind", 
			  replicate(facet_n, vals$init, 
				    simplify = FALSE))
    facet_mat[,facet_var] <- facet_levs
    facet.df <- as.data.frame(facet_mat)
    facet.df <- cbind(facet.df[!sapply(facet.df, is.list)],
                      (t(apply(facet.df[sapply(facet.df, is.list)], 1, unlist))))
    facet.df$RECEIPTS_NOISY <- receipts_norm
    facet.df$EMPLOYMENT_NOISY <- employ_norm

    facet_predvec <- model.matrix(formula(~ (RECEIPTS_NOISY+
					     EMPLOYMENT_NOISY+SEX+RACE_ETH+
					     VET+SECTOR+FRANCHISE)-1),
                                      data=facet.df, 
				      xlev=mod$factor_levels)

    facet_Xpred <- plogis(cbind(1,facet_predvec)%*%mod$A)
    facet_preds <- plogis(facet_Xpred%*%t(mod$beta))
    rownames(facet_preds ) <- facet_levs
    facet.df <- data.frame(names=rownames(facet_preds),facet_preds) %>%
                    pivot_longer(cols=starts_with('X'), values_to='x') %>%
                    select(-name)

    output$facet_densities <- renderPlot(facet.df %>%
					   ggplot() + 
					     geom_violin(mapping=aes(y=reorder(names,x),
								 x=x),
								fill="#69b3a2",
								color="black",
								alpha=.75) +
								labs(y=NULL, x="Probability") +
								theme_classic() 
				         )
  })
}
