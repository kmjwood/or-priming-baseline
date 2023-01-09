## Title  : or-priming-baseline functions
## Author : Tina Seabrooke

## APA function for graphs
theme_APA <- theme_bw()+
  theme(
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.border=element_blank(),
    axis.line=element_line(size = 1.5),
    text=element_text(size = 24, face = "bold"),
    axis.text = element_text(face="plain", colour = "black", size=20),
    legend.title=element_blank(),
    legend.text = element_text(size = 20, face = "plain", 
                               margin = margin(r = 10, unit = "pt")),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.position = 'top',
    legend.direction = 'horizontal',
    ## Increase space between x axis and x axis title:
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    ## Increase space surrounding plot:
    plot.margin = unit(c(1, 3, 0.5, 0.5), "lines"),
    plot.title = element_text(face="bold", hjust = 0.5)
  )

## Difference-adjusted w/subj error bars
## Slight update of code from Baguley (2012)
cm.ci <- function(data.frame, conf.level = 0.95, difference = TRUE) {
  #cousineau-morey within-subject CIs
  k = ncol(data.frame)
  if (difference == TRUE) {
    diff.factor = 2^0.5/2
  } else {
    diff.factor = 1
  }
  n <- nrow(data.frame)
  df.stack <- stack(data.frame)
  index <- rep(1:n, k)
  p.means <- tapply(df.stack$values, index, mean)
  norm.df <- data.frame - p.means + (sum(data.frame)/(n * k))
  t.mat <- matrix(, k, 1)
  mean.mat <- matrix(, k, 1)
  for (i in 1:k) t.mat[i, ] <- t.test(norm.df[i])$statistic[1]
  for (i in 1:k) mean.mat[i, ] <- mean(as.vector(t(norm.df[i])))
  c.factor <- (k/(k - 1))^0.5
  moe.mat <- mean.mat/t.mat * qt(1 - (1 - conf.level)/2, n - 1) * c.factor * 
    diff.factor
  ci.mat <- matrix(, k, 3)
  for (i in 1:k) {
    ci.mat[i, 1] <- mean.mat[i] - moe.mat[i]
    ci.mat[i, 2] <- mean.mat[i]
    ci.mat[i, 3] <- mean.mat[i] + moe.mat[i]
  }
  ci <- data.frame(ci.mat)
  ci <- cbind(names(data.frame), ci)
  colnames(ci) <- c("Condition", "lower", "av", "upper")
  ci
}

## Quick function to calculate Cohen's d_z
## Code based on
## http://jakewestfall.org/blog/index.php/2016/03/25/five-different-cohens-d-statistics-for-within-subject-designs/

## Requires a data frame with the following columns
## id - Subject number
## cond - Within-subjects condition (must have exactly 2 levels)
## dv - The dependent variable

cohen.dz <- function(dat) {
  dat$id <- factor(dat$id)
  dat$dv <- as.numeric(dat$dv)
  dat$cond <- factor(dat$cond)
  means <- with(dat, tapply(dv, cond, mean))
  md <- abs(diff(means))
  sub_means <- with(dat, tapply(dv, list(id, cond), mean))
  dz <- md / sd(sub_means[,2] - sub_means[,1])
  names(dz) <- "Cohen's d_z"
  dz
}