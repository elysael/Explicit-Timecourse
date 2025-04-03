plotAligned60 <- function() {
  aligned_data <- getAligned()
  
  group1_c <- do.call(rbind, aligned_data$group1)
  group2_c <- do.call(rbind, aligned_data$group2)
  
  group1avg <- aggregate(group1_c$reachdeviation_deg, by = list(group1_c$cutrial_no), FUN = mean)
  group2avg <- aggregate(group2_c$reachdeviation_deg, by = list(group2_c$cutrial_no), FUN = mean)
  c_data <- rbind(group1avg, group2avg)
  
  plot(-1000,1000,
       type = "n", col = "black",
       main = "Average Reach Deviation during Aligned Trials", xlab = "Trial Number", ylab = "Reach Deviation",
       xlim=c(0,104),  ylim = c(-20,80))
lines(c_data$x, col="cadetblue")
abline(h=0, col="black", lty=3)

}
  

plotRotated60 <- function () {
  
  rotated_data <- getRotated()
  
  group1_combined <- do.call(rbind, group1_rotated)
  group2_combined <- do.call(rbind, group2_rotated)
  
  group1_avg <- aggregate(group1_combined$reachdeviation_deg, by = list(group1_combined$cutrial_no), FUN = mean)
  group2_avg <- aggregate(group2_combined$reachdeviation_deg, by = list(group2_combined$cutrial_no), FUN = mean)
  combined_data <- merge(group1_avg, group2_avg, by = "Group.1", all = TRUE)
  
  # Calculate the average of the deviations, ignoring NAs
  combined_data$avg_deviation <- rowMeans(combined_data[, c("x.x", "x.y")], na.rm = TRUE)
  
  # Plot the combined data
  plot(combined_data$Group.1, combined_data$avg_deviation, type="l", col="purple", 
       xlim=c(89, 232), ylim=c(-20, 80), 
       xlab="Trial Number", ylab="Reach Deviation (degrees)", 
       main="Reach Deviation During Rotation Trials")
  
  abline(h=0,col="black",lty=3)
  
}


plotAfter60 <- function () {
  
  after_data <- getAfter()
  
  g1_combined <- do.call(rbind, group1_after)
  g2_combined <- do.call(rbind, group2_after)
  
  g1_avg <- aggregate(g1_combined$reachdeviation_deg, by = list(g1_combined$cutrial_no), FUN = mean)
  g2_avg <- aggregate(g2_combined$reachdeviation_deg, by = list(g2_combined$cutrial_no), FUN = mean)
  
  combined_d <- rbind(g1_avg, g2_avg)
  combined_d <- combined_d[order(combined_d$Group.1), ] 
  
  
  plot(-1000, 1000, type = "n",
        main = "Reach Deviation After Rotation",xlab = "Trial Number", ylab = "Reach Deviation", 
       xlim = c(209, 256), ylim = c(-5, 80))

    lines(combined_d$Group.1, combined_d$x, col = "deeppink", lwd = 2, pch = 16)  
 
   abline(h=0, col="black", lty=3)
}
  

plotAll60 <- function() {
  
  aligned_data <- getAligned()
  rotated_data <- getRotated()
  after_data <- getAfter()
  

  group1_c <- do.call(rbind, aligned_data$group1)
  group2_c <- do.call(rbind, aligned_data$group2)
  group1_combined <- do.call(rbind, group1_rotated)
  group2_combined <- do.call(rbind, group2_rotated)
  g1_combined <- do.call(rbind, group1_after)
  g2_combined <- do.call(rbind, group2_after)
  
  group1avg <- aggregate(group1_c$reachdeviation_deg, by = list(group1_c$cutrial_no), FUN = mean)
  group2avg <- aggregate(group2_c$reachdeviation_deg, by = list(group2_c$cutrial_no), FUN = mean)
  group1_avg <- aggregate(group1_combined$reachdeviation_deg, by = list(group1_combined$cutrial_no), FUN = mean)
  group2_avg <- aggregate(group2_combined$reachdeviation_deg, by = list(group2_combined$cutrial_no), FUN = mean)
  g1_avg <- aggregate(g1_combined$reachdeviation_deg, by = list(g1_combined$cutrial_no), FUN = mean)
  g2_avg <- aggregate(g2_combined$reachdeviation_deg, by = list(g2_combined$cutrial_no), FUN = mean)
  
  
all_data <- rbind(
  group1avg, group2avg, 
  group1_avg, group2_avg, 
  g1_avg ,  g2_avg)

all_data <- all_data[order(all_data$Group.1), ] 
  
#Get CI
CI <- function(df) {
  aggregate(reachdeviation_deg ~ cutrial_no, 
            data = df, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

CI(group1_c)
CI(group2_c)
CI(g1_combined)
CI(g2_combined)
CI(group1_combined)
CI(group2_combined)

ci_list <- list(
  CI(group1_c),
  CI(group2_c),
  CI(g1_combined),
  CI(g2_combined),
  CI(group1_combined),
  CI(group2_combined)
)

ci_combined <- do.call(rbind, ci_list)
ci_combined <- ci_combined[order(ci_combined$cutrial_no), ]

lo <- ci_combined$reachdeviation_deg[, 1]
hi <- ci_combined$reachdeviation_deg[ ,2]
cutrial_no <- ci_combined$cutrial_no


y_lower_limit <- -20
y_upper_limit <- 80


valid_indices <- lo >= y_lower_limit & lo <= y_upper_limit & hi >= y_lower_limit & hi <= y_upper_limit
lo_filtered <- lo[valid_indices]
hi_filtered <- hi[valid_indices]
cutrial_no_filtered <- cutrial_no[valid_indices]


if (length(lo_filtered) == length(hi_filtered) && length(hi_filtered) == length(cutrial_no_filtered)) {
  plot(-1000, 1000, type = "n",
       main = "Reach Deviation Across All Trials", 
       xlab = "Trial", ylab = "Reach Deviation (degrees)",
       xlim = c(0, 256), ylim = c(-20, 80))
  

#lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
smoothreach <- smooth.spline(all_data$Group.1, all_data$x, spar = 0.02)
lines(smoothreach, col='tomato', lwd=1)
abline(h = 0, col = "black", lty = 3)

lo_smooth_filtered <- smooth.spline(cutrial_no_filtered, lo_filtered, spar = 0.008)
hi_smooth_filtered <- smooth.spline(cutrial_no_filtered, hi_filtered, spar = 0.008)

# Draw the polygon for the filtered CIs
polygon(x = c(lo_smooth_filtered$x, rev(hi_smooth_filtered$x)),
        y = c(lo_smooth_filtered$y, rev(hi_smooth_filtered$y)),
        col = rgb(1, 0.388, 0.278, alpha = 0.2),
        border = NA)
} else {
  stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
}



#####AIMING

plotAlignedAim60 <- function() {
  aligned_data <- getAligned()
  
  group1_c <- do.call(rbind, aligned_data$group1)
  group2_c <- do.call(rbind, aligned_data$group2)

  
  group1avg <- aggregate(group1_c$aimdeviation_deg, by = list(group1_c$cutrial_no), FUN = mean)
  group2avg <- aggregate(group2_c$aimdeviation_deg, by = list(group2_c$cutrial_no), FUN = mean)
  c_data <- rbind(group1avg, group2avg)
  
  plot(-1000,1000,
       type = "n", col = "black",
       main = "Average Aim Deviation during Aligned Trials", xlab = "Trial Number", ylab = "Aiming Strategy",
       xlim=c(0,104),  ylim = c(-20,80))
lines(c_data$x, col="cadetblue")
abline(h=0, col="black", lty=3)

}


plotStrategy60 <- function () {
  
  rotated_data <- getRotated()
  
  group1_combined <- do.call(rbind, group1_rotated)
  group2_combined <- do.call(rbind, group2_rotated)
  
  group1_avg <- aggregate(group1_combined$aimdeviation_deg, by = list(group1_combined$cutrial_no), FUN = mean)
  group2_avg <- aggregate(group2_combined$aimdeviation_deg, by = list(group2_combined$cutrial_no), FUN = mean)
  combined_data <- merge(group1_avg, group2_avg, by = "Group.1", all = TRUE)
  
  # Calculate the average of the deviations, ignoring NAs
  combined_data$avg_deviation <- rowMeans(combined_data[, c("x.x", "x.y")], na.rm = TRUE)
  
  # Plot the combined data
  plot(combined_data$Group.1, combined_data$avg_deviation, type="l", col="purple", 
       xlim=c(89, 232), ylim=c(-20, 80), 
       xlab="Trial Number", ylab="Aim Deviation (degrees)", 
       main="Aiming Strategy During Rotation Trials")
  
  abline(h=0,col="black",lty=3)
  
}


plotAfterAim60 <- function () {
  
  after_data <- getAfter()
  
  g1_combined <- do.call(rbind, group1_after)
  g2_combined <- do.call(rbind, group2_after)
  
  g1_avg <- aggregate(g1_combined$aimdeviation_deg, by = list(g1_combined$cutrial_no), FUN = mean)
  g2_avg <- aggregate(g2_combined$aimdeviation_deg, by = list(g2_combined$cutrial_no), FUN = mean)
  
  combined_d <- rbind(g1_avg, g2_avg)
  combined_d <- combined_d[order(combined_d$Group.1), ] 
  
  
  plot(-1000, 1000, type = "n",
       main = "Aim Deviation After Rotation",xlab = "Trial Number", ylab = "Strategy Change", 
       xlim = c(209, 256), ylim = c(-5, 80))
  
  lines(combined_d$Group.1, combined_d$x, col = "deeppink", lwd = 2, pch = 16)  
  
  abline(h=0, col="black", lty=3)
}


plotAllAim60 <- function () {
  
  aligned_data <- getAligned()
  rotated_data <- getRotated()
  after_data <- getAfter()
  
  
  group1_c <- do.call(rbind, aligned_data$group1)
  group2_c <- do.call(rbind, aligned_data$group2)
  group1_combined <- do.call(rbind, group1_rotated)
  group2_combined <- do.call(rbind, group2_rotated)
  g1_combined <- do.call(rbind, group1_after)
  # g2_combined <- do.call(rbind, group2_after)
  
  group1avg <- aggregate(group1_c$aimdeviation_deg, by = list(group1_c$cutrial_no), FUN = mean)
  group2avg <- aggregate(group2_c$aimdeviation_deg, by = list(group2_c$cutrial_no), FUN = mean)
  group1_avg <- aggregate(group1_combined$aimdeviation_deg, by = list(group1_combined$cutrial_no), FUN = mean)
  group2_avg <- aggregate(group2_combined$aimdeviation_deg, by = list(group2_combined$cutrial_no), FUN = mean)
  g1_avg <- aggregate(g1_combined$aimdeviation_deg, by = list(g1_combined$cutrial_no), FUN = mean)
  g2_avg <- aggregate(g2_combined$aimdeviation_deg, by = list(g2_combined$cutrial_no), FUN = mean)
  
  
  all_data <- rbind(
    group1avg, group2avg, 
    group1_avg, group2_avg, 
    g1_avg ,  g2_avg)
  
  all_data <- all_data[order(all_data$Group.1), ] 
  
  #Get CI
  CI <- function(df) {
    aggregate(aimdeviation_deg ~ cutrial_no, 
              data = df, 
              FUN = function(x) Reach::getConfidenceInterval(x))
  }
  
  CI(group1_c)
  CI(group2_c)
  CI(g1_combined)
 # CI(g2_combined)
  CI(group1_combined)
  CI(group2_combined)
  
  ci_list <- list(
    CI(group1_c),
    CI(group2_c),
    CI(g1_combined),
    CI(group1_combined),
    CI(group2_combined))
  
  ci_combined <- do.call(rbind, ci_list)
  ci_combined <- ci_combined[order(ci_combined$cutrial_no), ]
  
 
  lo <- ci_combined$aimdeviation_deg [, 1]
  hi <- ci_combined$aimdeviation_deg[ ,2]
  cutrial_no <- ci_combined$cutrial_no
  
  
  y_lower_limit <- -20
  y_upper_limit <- 80
  
  
  valid_indices <- lo >= y_lower_limit & lo <= y_upper_limit & hi >= y_lower_limit & hi <= y_upper_limit
  lo_filtered <- lo[valid_indices]
  hi_filtered <- hi[valid_indices]
  cutrial_no_filtered <- cutrial_no[valid_indices]
  
  
  if (length(lo_filtered) == length(hi_filtered) && length(hi_filtered) == length(cutrial_no_filtered)) {
    plot(-1000, 1000, type = "n",
         main = "Aim Deviation Across All Trials", 
         xlab = "Trial", ylab = "Aiming Startegy (degrees)",
         xlim = c(0, 256), ylim = c(-20, 80))
    
    #lines(all_data$Group.1, all_data$x, col = "hotpink", lwd = 2, pch = 16)  
    all_data_clean <- all_data[!is.na(all_data$Group.1) & !is.na(all_data$x) & 
                                 !is.infinite(all_data$Group.1) & !is.infinite(all_data$x), ]
    

    smoothall <- smooth.spline(all_data_clean$Group.1, all_data_clean$x, spar = 0.0001)
    lines(smoothall, col='magenta4', lwd=2)
    abline(h = 0, col = "black", lty = 3)
    
    lo_smooth_filtered <- smooth.spline(cutrial_no_filtered, lo_filtered, spar = 0.008)
    hi_smooth_filtered <- smooth.spline(cutrial_no_filtered, hi_filtered, spar = 0.008)
    
    # Draw the polygon for the filtered CIs
    polygon(x = c(lo_smooth_filtered$x, rev(hi_smooth_filtered$x)),
            y = c(lo_smooth_filtered$y, rev(hi_smooth_filtered$y)),
            col =rgb(0.6, 0, 0.6, alpha = 0.2) ,
            border = NA)
  } else {
    stop("Lengths of lo_filtered, hi_filtered, and cutrial_no_filtered do not match!")
  }
}
}





