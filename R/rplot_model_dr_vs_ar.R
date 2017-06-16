# roxygen2::roxygenise()

#' @title r.plot.ar
#' @export
r.plot.ar <- function(
  score, 
  target, 
  weight = rep(1, length(target)), 
  npoints = 200, 
  main = "Acceptance", 
  sub = NULL, 
  xlab = "cutoff", 
  ylab = "% Acceptance", 
  icol = 1, 
  col = r.color(icol),
  target_value = 1,
  ...
) {
  total_DR = length(which(target==target_value))/length(target)
  total_DR_amount = sum(weight[target == target_value])/sum(weight)
  
  score_seq = quantile(score, probs=seq(0,1,by=1/npoints))
  data = data.frame()
  for(threshold in score_seq) {
    accepted = score < threshold
    iDf = data.frame(
      cutoff = threshold,
      AR = length(which(accepted))/length(score),
      DR = length(which(target[accepted]==target_value))/length(which(accepted)),
      DR_amount = sum(weight[accepted & target == target_value])/sum(weight[accepted])
      )
    data <- rbind(data, iDf)
  }
  
  r.plot.new(x=data$cutoff, y=data$AR, main=main, sub=sub, xlab=xlab, ylab=xlab, ...)
  r.plot.add(x=data$cutoff, y=data$AR, col=col, type="l")
    
  invisible(data)
}

#' @title r.plot.dr
#' @export
r.plot.dr <- function(
  score, 
  target, 
  weight = rep(1, length(target)), 
  npoints = 200, 
  main = "Default", 
  sub = NULL, 
  xlab = "cutoff", 
  ylab = "% Default", 
  icol = 1, 
  col = r.color(icol),
  target_value = 1,
  ...
) {
  total_DR = length(which(target==target_value))/length(target)
  total_DR_amount = sum(weight[target == target_value])/sum(weight)
  
  score_seq = quantile(score, probs=seq(0,1,by=1/npoints))
  data = data.frame()
  for(threshold in score_seq) {
    accepted = score < threshold
    iDf = data.frame(
      cutoff = threshold,
      AR = length(which(accepted))/length(score),
      DR = length(which(target[accepted]==target_value))/length(which(accepted)),
      DR_amount = sum(weight[accepted & target == target_value])/sum(weight[accepted])
      )
    data <- rbind(data, iDf)
  }
  
  if (length(unique(range(weight)))>1) ylab = paste0(ylab, " (weighted)")
  
  r.plot.new(x=data$cutoff, y=data$DR, main=main, sub=sub, xlab="cutoff", ylab=ylab, ...)
  r.plot.add(x=data$cutoff, y=data$DR, col=col, type="l")
  
  invisible(data)
}

#' @title r.plot.dr_vs_ar
#' @export
r.plot.dr_vs_ar <- function(
  score, 
  target, 
  weight = rep(1, length(target)), 
  npoints = 200, 
  main = "DR vs AR", 
  sub = NULL, 
  xlab = "% Acceptance", 
  ylab = "% Default", 
  icol = 1, 
  icol.max = 11,
  col = r.color(icol),
  col.max = r.color(icol.max),
  showMax = TRUE,
  target_value = 1,
  ...
) {
  total_DR = length(which(target==target_value))/length(target)
  total_DR_amount = sum(weight[target == target_value])/sum(weight)
  
  score_seq = quantile(score, probs=seq(0,1,by=1/npoints))
  data = data.frame()
  for(threshold in score_seq) {
    accepted = score < threshold
    iDf = data.frame(
      cutoff = threshold,
      AR = length(which(accepted))/length(score),
      DR = length(which(target[accepted]==target_value))/length(which(accepted)),
      DR_amount = sum(weight[accepted & target == target_value])/sum(weight[accepted])
      )
    data <- rbind(data, iDf)
  }
  
  if (length(unique(range(weight)))>1) ylab = paste0(ylab, " (weighted)")
  
  r.plot.new(x=data$AR, y=data$DR_amount, main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  r.plot.add(x=c(0,1), y=c(total_DR_amount, total_DR_amount), col=rgb(0,0,0,0.8), type="l") 
  if(showMax) r.plot.add(c(0,1-total_DR_amount,1),c(0,0,total_DR_amount), col=col.max, type="l")
  r.plot.add(x=data$AR, y=data$DR_amount, col=col, type="l")  
  
  invisible(data)
}

#' @title r.plot.dr_vs_ar_all
#' @export
r.plot.dr_vs_ar_all <- function(
  score, 
  target, 
  weight = rep(1, length(target)), 
  npoints = 200, 
  main = "DR vs AR", 
  sub = NULL, 
  xlab = "% Acceptance", 
  ylab = "% Default", 
  icol = 1, 
  icol.max = 11,
  col = r.color(icol),
  col.max = r.color(icol.max),
  showMax = TRUE,
  target_value = 1,
  ...
) {
  total_DR = length(which(target==target_value))/length(target)
  total_DR_amount = sum(weight[target == target_value])/sum(weight)
  
  score_seq = quantile(score, probs=seq(0,1,by=1/npoints))
  data = data.frame()
  for(threshold in score_seq) {
    accepted = score < threshold
    iDf = data.frame(
      cutoff = threshold,
      AR = length(which(accepted))/length(score),
      DR = length(which(target[accepted]==target_value))/length(which(accepted)),
      DR_amount = sum(weight[accepted & target == target_value])/sum(weight[accepted])
      )
    data <- rbind(data, iDf)
  }
  
  r.plot.new(x=data$cutoff, y=data$AR, main=main, sub=sub, xlab="cutoff", ylab=xlab, ...)
  r.plot.add(x=data$cutoff, y=data$AR, col=col, type="l")
    
  r.plot.new(x=data$cutoff, y=data$DR, main=main, sub=sub, xlab="cutoff", ylab=ylab, ...)
  r.plot.add(x=data$cutoff, y=data$DR, col=col, type="l")
  
  r.plot.new(x=data$AR, y=data$DR, main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
  r.plot.add(x=c(0,1), y=c(total_DR, total_DR), col=rgb(0,0,0,0.8), type="l") 
  if(showMax) r.plot.add(c(0,1-total_DR,1),c(0,0,total_DR), col=col.max, type="l")
  r.plot.add(x=data$AR, y=data$DR, col=col, type="l")

  if (length(unique(range(weight)))>1) {
    r.plot.new(x=data$AR, y=data$DR_amount, main=main, sub=sub, xlab=xlab, ylab=paste0(ylab, " (weighted)"), ...)
    r.plot.add(x=c(0,1), y=c(total_DR_amount, total_DR_amount), col=rgb(0,0,0,0.8), type="l") 
    if(showMax) r.plot.add(c(0,1-total_DR_amount,1),c(0,0,total_DR_amount), col=col.max, type="l")
    r.plot.add(x=data$AR, y=data$DR_amount, col=col, type="l")
  }
  
  invisible(data)
}

#' @title r.gplot.dr_vs_ar
#' @export
r.gplot.dr_vs_ar <- function(
  score, 
  target, 
  weight = rep(1, length(target)), 
  npoints = 200, 
  threshold = quantile(score, probs = 0.9), 
  title = "DR vs AR",
  show_plots = TRUE
) {
  total_DR = prop.table(table(target))[2]
  total_DR_amount = prop.table(tapply(weight, target, "sum"))[2]
  
  num_loans <- length(weight)
  global_DR <- total_DR %>% round(3)
  global_DR_amount <- total_DR_amount %>% round(3)
  
  cond_threshold <- score < threshold
  
  acceptance <- prop.table(table(cond_threshold))["TRUE"] %>% round(3)
  threshold_DR <- prop.table(table(target[cond_threshold]))[2] %>% round(3)
  threshold_DR_amount <- prop.table(tapply(weight[cond_threshold], target[cond_threshold], "sum"))[2] %>% round(3)
  
  message("\t", "Num loans: ", num_loans)
  message("\t", "Global DR :", global_DR )
  message("\t", "Global DR (weighted): ", global_DR_amount)
  
  message("\n\t", "Threshold: ", threshold)
  message("\t", "Acceptance: ", acceptance)
  message("\t", "DR: ", threshold_DR)
  message("\t", "DR (weighted): ", threshold_DR_amount)
  
  score_seq <- quantile(score, probs=seq(0,1,by=1/npoints))
  scoreDf <- data.frame()
  for(i in score_seq) {
    cond_score <- score < i
    iDf <- data.frame(cutoff = i, 
                      acceptance = sum(cond_score)/length(score),
                      DR = sum(target[cond_score])/length(target[cond_score]),
                      DR_amount = sum(weight[cond_score & target == 1])/sum(weight[cond_score]))
    scoreDf <- rbind(scoreDf, iDf)
  }
  
  g1 <- ggplot(scoreDf, aes(cutoff, acceptance)) + 
    geom_line() + 
    geom_vline(xintercept = threshold, col = "red", alpha = 0.8) +
    xlab("cutoff") + ylab("% Acceptance") + 
    ggtitle(title)
  
  g2 <- ggplot(scoreDf, aes(cutoff, DR_amount)) + 
    geom_line() + 
    geom_vline(xintercept = threshold, col = "red", alpha = 0.8) +
    xlab("cutoff") + ylab("% Default (weighted)") + 
    ggtitle(title)
  
  g3 <- ggplot(scoreDf, aes(acceptance, DR)) + 
    geom_line(data.frame(x=c(0,1), y=c(total_DR, total_DR)), aes(x,y), col="#000000CC") +
    geom_line(data.frame(x=c(0,1-total_DR,1), y=c(0,0,total_DR)), aes(x,y), col="#666666A6") +
    geom_line() + 
    ggplot2::annotate("point", acceptance, threshold_DR, colour = "red", alpha = 0.8) + 
    xlab("% Acceptance") + ylab("% Default") +
    ggtitle(title)
  
  g4 <- ggplot(scoreDf, aes(acceptance, DR_amount)) + 
    geom_line(data.frame(x=c(0,1), y=c(total_DR_amount, total_DR_amount)), aes(x,y), col="#000000CC") +
    geom_line(data.frame(x=c(0,1-total_DR_amount,1), y=c(0,0,total_DR_amount)), aes(x,y), col="#666666A6") +
    geom_line() + 
    ggplot2::annotate("point", acceptance, threshold_DR_amount, colour = "red", alpha = 0.8) + 
    xlab("% Acceptance") + ylab("% Default (weighted)") +
    ggtitle(title)
  
  if (show_plots) {
    print(g1)
    print(g2)
    print(g3)
    print(g4)
  }
  
  invisible(list(data=scoreDf, plot_ar = g1, plot_dr = g2, plot_dr_vs_ar = g3, plot_dr_vs_ar_amount = g4))
}