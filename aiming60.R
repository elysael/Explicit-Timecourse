#average aim deviation
aim60_path <- 'data/Instructed_summary/aiming60'
all_60data <- list()
aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)

all_aim_deviations <- list()


for (i in 1:length(aim60files)) {
  df <- read.csv(aim60files[i], stringsAsFactors = FALSE)
  all_aim_deviations[[i]] <- df$aimdeviation_deg
}

remove_na <- function(x) {
  return(x[!is.na(x)])  # Keep only non-NA values
}

cleaned_aim <- lapply(all_aim_deviations, remove_na)

combinedaimdev <- do.call(cbind,all_aim_deviations)
average_aim_deviation <- apply(combinedaimdev, 1, mean, na.rm = TRUE)


plot(average_aim_deviation, type = "l", col = "skyblue3", lwd = 2, xlim = c(0, 256), ylim = c(0, 70),
     main = "Average Aim Deviation with a 60 Degree Perturbation", xlab = "Trial", ylab = "Aim Deviation (degrees)")
smooth_line<- smooth.spline(average_aim_deviation)
lines(smooth_line, col = "black", lwd = 2)

abline (h=60, col="black", lwd = 1.5, lty=3)


#each participant
aim60_path <- 'data/Instructed_summary/aiming60'
all_60data <- list()
aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)

all_aim_deviations <- list()


for (i in 1:length(aim60files)) {
  df <- read.csv(aim60files[i], stringsAsFactors = FALSE)
  all_aim_deviations[[i]] <- df$aimdeviation_deg
}

# Remove empty cells (where aiming didnt occur)
remove_na <- function(x) {
  return(x[!is.na(x)])  # Keep only non-NA values
}

cleaned_aim <- lapply(all_aim_deviations, remove_na)

combinedaimdev <- do.call(cbind,all_aim_deviations)


# Plot one participant to see trial by trial
plot(cleaned_aim[[6]], , col = "orchid4", type = "p", lwd = 2,
     xlim = c(0, 256), ylim = c(0, 80),
     main = "Aim Deviation with a 60 Degree Perturbation",
     xlab = "Trial", ylab = "Aim Strategy (degrees)")

abline (h=60, col="black", lwd = 1.5, lty=3)


#8, 6 has really messy data in beginning
#13 too 



#compare reach and aim dev
plot(average_aim_deviation, type = "n", col = "blue", lwd = 2, 
     xlim = c(0, 256), ylim = c(0, 70), 
     main = "Comparison of Aim Deviation for Two Conditions", 
     xlab = "Trial", ylab = "Aim Deviation (degrees)")

#lines(average_reach_deviation, col = "hotpink", lwd = 2)


#we can "smooth out the lines just to compare easier"
smooth_line1 <- smooth.spline(average_aim_deviation)
lines(smooth_line1, col = "darkorange1", lwd = 3)
smooth_line2 <- smooth.spline(average_reach_deviation)
lines(smooth_line2, col = "darkmagenta", lwd = 3)
legend("topright", legend = c("Aim Deviation", "Reach Deviation"), col = c("darkorange1", "darkmagenta"), lwd = 2)


