lm_autoplot <- function(model, leg="collect", group=NULL){
  data <- model@frame %>%
    ungroup() %>%
    dplyr::mutate(resid=resid(model),
                  fitted = fitted(model),
                  sqrt_abs_resid = sqrt(abs(resid(model))),
                  std_resid = rstudent(model),
                  leverage = hatvalues(model),
                  qqx = qqnorm(rstudent(model), plot.it = F)$x,
                  qqy = qqnorm(rstudent(model), plot.it = F)$y) 
  
  y <- quantile(data$std_resid, c(0.25,0.75))
  x <- qnorm(c(0.25,0.75))
  
  diag1 <- data %>%
    ggplot(aes(fitted,resid,col=group))+
    geom_point(pch=1) +
    labs(title = "Residuals vs Fitted", x="Fitted values", y="Residuals")
  
  diag3 <- data %>%
    ggplot(aes(fitted,sqrt_abs_resid,col=group))+
    geom_point(pch=1) +
    labs(title = "Scale-Location", x="Fitted values", y=expression(sqrt("Standardized Residuals")))
  
  diag2 <- data %>%
    ggplot(aes(x=qqx,y=qqy,col=group))+
    geom_point(pch=1) +
    geom_abline(slope = diff(y)/diff(x), intercept = y[1L] - diff(y)/diff(x) * x[1L])+
    labs(title = "Normal Q-Q", x="Theoretical Quantiles", y="Studentized Residuals")
  
  diag4 <- data %>%
    ggplot(aes(leverage,std_resid,col=group))+
    geom_point(pch=1) +
    labs(title = "Residuals vs Leverage", x="Leverage", y="Studentized Residuals")
  
  if(leg=="keep"){
    plot <- (diag1 + diag2)/(diag3 + diag4)+ plot_layout(guides = leg, heights = c(4,4)) &
      theme_bw() & theme(legend.position = "bottom") 
  } else {
    plot <- (diag1 + diag2)/(diag3 + diag4)/guide_area() + plot_layout(guides = leg, heights = c(4,4,2)) &
      theme_bw() &  theme(legend.position = "bottom") & guides(col=guide_legend(nrow=3,byrow=TRUE))    
  }
  return(plot)
}
