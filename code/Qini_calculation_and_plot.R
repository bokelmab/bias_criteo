## calculate Qini curve
calc_CATE <- function(p_Y, p_W, p_pred, p_cut){
  y_treat <- p_Y[p_pred > quantile(p_pred, p_cut) & p_W == 1]
  y_contr <- p_Y[p_pred > quantile(p_pred, p_cut) & p_W == 0]
  qini_mean <- (mean(y_treat)-mean(y_contr))*length(y_treat)
  qini_sd <- sqrt(var(y_treat)/length(y_treat)+var(y_contr)/length(y_contr))*length(y_treat)
  return(list(qini_mean = qini_mean, qini_sd = qini_sd))
}

plot_qini_curve <- function(p_preds1, p_preds2, p_dt_test){
  
  qini_curve1 <- c()
  qini_curve_mu1 <- c()
  qini_curve2 <- c()
  qini_curve_mu2 <- c()
  for(i_dec in 9:0){
    
    ## Qini curve with original target 
    res_qini1 <- p_dt_test %$% calc_CATE(p_Y = Y, p_W = W, p_pred = p_preds1, p_cut = i_dec/10)
    qini_curve1 <- c(qini_curve1, res_qini1$qini_mean)
    res_qini2 <- p_dt_test %$% calc_CATE(p_Y = Y, p_W = W, p_pred = p_preds2, p_cut = i_dec/10)
    qini_curve2 <- c(qini_curve2, res_qini2$qini_mean)
    
    ## Qini curve and standard deviation with B_mu-adjusted target
    res_qini_mu1 <- p_dt_test %$% calc_CATE(p_Y = Y_mu, p_W = W, p_pred = p_preds1, p_cut = i_dec/10)
    qini_curve_mu1 <- c(qini_curve_mu1, res_qini_mu1$qini_mean)
    res_qini_mu2 <- p_dt_test %$% calc_CATE(p_Y = Y_mu, p_W = W, p_pred = p_preds2, p_cut = i_dec/10)
    qini_curve_mu2 <- c(qini_curve_mu2, res_qini_mu2$qini_mean)
    
  }
  
  ## plot Qini curve with original outcome
  layout(matrix(c(1,2,3,3), ncol=2, byrow=TRUE), heights = c(0.9, 0.1))
  par(mar=c(3, 3, 3, 1), mgp=c(1.5, 0.4, 0))
  plot((0:10)/10 * sum(p_dt_test$W), c(0, qini_curve1), type = 'l', main = 'Qini curve orig', ylim = c(0, max(c(qini_curve1,qini_curve2,qini_curve_mu1,qini_curve_mu2))), ylab = 'incremental gain', xlab = expression('N'[W]*'(s)'), lwd = 3)
  lines((0:10)/10 * sum(p_dt_test$W), c(0, qini_curve2), col = 'green', lwd = 3)
  lines(c(0,1) * sum(p_dt_test$W),c(0, qini_curve1[length(qini_curve1)]), col = 'red', lty = 2, lwd = 3)
 
  
  ## plot Qini curve with adjusted target
  par(mar=c(3, 3, 3, 1), mgp=c(1.5, 0.4, 0))
  plot((0:10)/10 * sum(p_dt_test$W), c(0, qini_curve_mu1), type = 'l', main = 'Qini curve adjusted', ylim = c(0, max(c(qini_curve1,qini_curve2,qini_curve_mu1,qini_curve_mu2))), ylab = 'incremental gain', xlab = expression('N'[W]*'(s)'), lwd = 3)
  lines((0:10)/10 * sum(p_dt_test$W), c(0, qini_curve_mu2), col = 'green', lwd = 3)
  lines(c(0,1) * sum(p_dt_test$W),c(0, qini_curve_mu1[length(qini_curve_mu1)]), col = 'red', lty = 2, lwd = 3)
  
  ##legend
  par(mar=c(1, 1, 0.5, 0))
  plot(1, type = "n", axes=FALSE, xlab="", ylab="")
  plot_colors <- c('black', 'green')
  legend(x = "bottom",inset = 0,
         legend = c('T-learner', 'Response model'), 
         col=plot_colors, lty = c(1,1), lwd=5, cex=1, horiz = TRUE, seg.len = 0.5, text.width = 0.07, bty = 'n')
  plot.new()
  
  
}