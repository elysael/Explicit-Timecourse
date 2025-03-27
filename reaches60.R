#Aiming 60

# Create a function that includes all participants under the aiming 60 folder.
get60_Data <- function() {
  aim60_path <- 'data/Instructed_summary/aiming60'
  all_60data <- list()
  aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)
  
  for (file_path in aim60files) {
    df <- read.csv(file_path)
    all_60data[[length(all_60data) + 1]] <- df
  }
  
  return(all_60data)
}

aim60_data <- get60_Data()
print(length(aim60_data))



#COMFIRM LEARNERS---------
data_path <- "data/Instructed_summary/aiming60/"
group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                       "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                       "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                       "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                       "SUMMARY_aiming60_a23b35.csv"))
learners <- 0  


for (file in group1_files) {

  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 8, , drop = FALSE]
  close_to_60 <- sum(rotated$reachdeviation_deg >= 25 & rotated$reachdeviation_deg <= 85, na.rm = TRUE)
  proportion_close_to_60 <- close_to_60 / nrow(rotated)
  
  if (proportion_close_to_60 >= 0.5) {
    learners <- learners + 1
  }
}

print(learners)
#there are 9 out of 9 learners that countered the pertubation with a reach deviation ranging from 25 to 85 degrees

data_path <- "data/Instructed_summary/aiming60/"
group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", "SUMMARY_aiming60_654648.csv", 
                                       "SUMMARY_aiming60_f275ca.csv"))



#now for the group within the new paradigm, which had the perturbation introduced at a different trial
learners <- 0  
for (file in group2_files) {
  df <- read.csv(file, stringsAsFactors = FALSE)
  rotated <- df[df$task_idx == 12, , drop = FALSE]
  close_to_60 <- sum(rotated$reachdeviation_deg >= 30 & rotated$reachdeviation_deg <= 80, na.rm = TRUE)
  proportion_close_to_60 <- close_to_60 / nrow(rotated)
  
  if (proportion_close_to_60 >= 0.5) {
    learners <- learners + 1
  }
  
  
}

print(learners)
#there are 4/4 learners



#REMOVE OUTLIERS 
df1 <- aim60_data[[1]]
df1$reachdeviation_deg[df1$reachdeviation_deg < 0] <- 0  
df1$reachdeviation_deg[df1$reachdeviation_deg > 85] <- 85  

plot(df1$reachdeviation_deg, type="l")

#apply to all participants

for (i in 1:length(aim60_data)) {
  df <- aim60_data[[i]]
  
  # Remove values outside the range 0 to 90 degrees
  df$reachdeviation_deg[df$reachdeviation_deg < -20] <--20
  df$reachdeviation_deg[df$reachdeviation_deg > 85] <- 85
  
}

# Plot the participant's individual data points
  if (i == 1) {
    plot(df$reachdeviation_deg, col = i, main = "Reach Deviation Aiming 60 ", xlab = "Trial", ylab = "Reach Deviation (degrees)")
  } 



#lets take the average and do a line graph
aim60_path <- 'data/Instructed_summary/aiming60'
all_60data <- list()
aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)

all_reach_deviations <- list()


for (i in 1:length(aim60files)) {
  df <- read.csv(aim60files[i], stringsAsFactors = FALSE)
  all_reach_deviations[[i]] <- df$reachdeviation_deg
}

combinedreachdev <- do.call(cbind,all_reach_deviations)

average_reach_deviation <- apply(combinedreachdev, 1, mean, na.rm = TRUE)

plot(average_reach_deviation, type = "l", col = "hotpink2", lwd = 2,   xlim = c(0, 256), ylim = c(-30,60),
     main = "Average Reach Deviation with a 60 Degree Perturbation", xlab = "Trial", ylab = "Reach Deviation (degrees)")
#smooth_line2 <- smooth.spline(average_reach_deviation)
#lines(smooth_line2, col = "black", lwd = 3, lty=3)


abline (h=0, col="black", lwd = 1, lty=3)


