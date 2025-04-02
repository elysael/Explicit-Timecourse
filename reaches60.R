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


#Extract the Aligned Phase 

getAligned <- function () {
  data_path <- "data/Instructed_summary/aiming60/"
  
  # Group 1 file paths
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                         "SUMMARY_aiming60_a23b35.csv"))
  
  # Group 2 file paths
  group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", 
                                         "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv"))
  
  group1_data <- list()
  group2_data <- list()
  
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    df$cutrial_no <- as.integer(df$cutrial_no)
     aligned <- df[df$cutrial_no >= 1 & df$cutrial_no <= 88, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    #print(nrow(aligned)) 
    
    group1_data[[length(group1_data) + 1]] <- aligned
  }
  
  # Extract trials for Group 2 (Trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    aligned <- df[df$cutrial_no %in% c(1:24, 41:56, 65:72, 81:88, 97:104), c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_data[[length(group2_data) + 1]] <- aligned
  }
  return(list(group1 = group1_data, group2 = group2_data))
  #print(nrow(group1_data[[1]]))
  #print(nrow(group2_data[[1]]))
  
  group1_c <- do.call(rbind, aligned_data$group1)
  group2_c <- do.call(rbind, aligned_data$group2)
  
  if (!identical(names(group1_c), names(group2_c))) {
    stop("Column names do not match between groups!")
  }
  
  # Combine both groups into one data frame
  c_data <- rbind(group1_c, group2_c)
  
  plot(c_data_avg$cutrial_no, c_data_avg$reachdeviation_deg,
       type = "n", col = "pink",
       xlab = "Trial Number", ylab = "Rotated Reach Deviation",
       ylim = c(-20,80),
       main = "Average Reach Deviation for Group 1 and Group 2 during Baseline")
 lines(c_data$reachdeviation_deg, col="blue")
  
  
   return(c_data) 
}
aligned_data <- getAligned()


  
  
#Extract the Rotated Phase
getRotated <- function () {
  
  data_path <- "data/Instructed_summary/aiming60/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                         "SUMMARY_aiming60_a23b35.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", 
                                         "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv"))
  
  group1_rotated <- list()
  group2_rotated <- list()
  
  # Group 1 (trial 89 to 208)
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated <- df[df$cutrial_no >= 89 & df$cutrial_no <= 208,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_rotated[[length(group1_rotated) + 1]] <- rotated
  }
  
  # Group 2 (trial 113 to 232)
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    rotated <- df[df$cutrial_no >= 113 & df$cutrial_no <= 232, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_rotated[[length(group2_rotated) + 1]] <- rotated
  }
  
  return(list(group1 = group1_rotated, group2 = group2_rotated))
  #print(nrow(rotated_data$group1[[1]]))
  #print(nrow(rotated_data$group2[[1]]))
  
  group1_combined <- do.call(rbind, group1_rotated)
  group2_combined <- do.call(rbind, group2_rotated)
 
  group1_avg <- aggregate(group1_combined$reachdeviation_deg, by = list(group1_combined$cutrial_no), FUN = mean)
  group2_avg <- aggregate(group2_combined$reachdeviation_deg, by = list(group2_combined$cutrial_no), FUN = mean)
  combined_data <- rbind(group1_avg, group2_avg)
  
 
  plot(group1_avg$Group.1, group1_avg$x, type="n", xlim=c(89, 232), ylim=c(-20, 80), 
       xlab="Trial Number", ylab="Reach Deviation (degrees)", 
       main="Reach Deviation for Group 1 and Group 2")
  
  # lines for Group 1 
  lines(group1_avg$Group.1, group1_avg$x, col="blue")
  
  # lines for Group 2 
  lines(group2_avg$Group.1, group2_avg$x, col="red")
  
  
  combined_data <- merge(group1_avg, group2_avg, by = "Group.1", all = TRUE)
  
  # Calculate the average of the deviations, ignoring NAs
  combined_data$avg_deviation <- rowMeans(combined_data[, c("x.x", "x.y")], na.rm = TRUE)
  
  # Plot the combined data
  plot(combined_data$Group.1, combined_data$avg_deviation, type="l", col="purple", 
       xlim=c(89, 232), ylim=c(-20, 80), 
       xlab="Trial Number", ylab="Average Reach Deviation (degrees)", 
       main="Average Reach Deviation for Group 1 and Group 2")
}
rotated_data <- getRotated()




# To look at one participant's data points...
  if (i == 1) {
    plot(df$reachdeviation_deg, col = i, main = "Reach Deviation Aiming 60 ", xlab = "Trial", ylab = "Reach Deviation (degrees)")
  } 


getAfter <- function() {
  
  data_path <- "data/Instructed_summary/aiming60/"
  
  group1_files <- file.path(data_path, c("SUMMARY_aiming60_1ad447.csv", "SUMMARY_aiming60_7eec53.csv", 
                                         "SUMMARY_aiming60_13d986.csv", "SUMMARY_aiming60_33e532.csv", 
                                         "SUMMARY_aiming60_86f3b3.csv", "SUMMARY_aiming60_98e5cb.csv", 
                                         "SUMMARY_aiming60_4093e8.csv", "SUMMARY_aiming60_a02c67.csv", 
                                         "SUMMARY_aiming60_a23b35.csv"))
  
  group2_files <- file.path(data_path, c("SUMMARY_aiming60_7cd1bd.csv", "SUMMARY_aiming60_3091de.csv", 
                                         "SUMMARY_aiming60_654648.csv", "SUMMARY_aiming60_f275ca.csv"))
  
  group1_after <- list()
  group2_after <- list()
  
  # Group 1 (trial 233 to 256) left hand trials
  for (file in group1_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after <- df[df$cutrial_no >= 233 & df$cutrial_no <= 256,c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group1_after[[length(group1_after) + 1]] <- after
  }
  
  # Group 2 (trial 113 to 232) aftereffect trials
  for (file in group2_files) {
    df <- read.csv(file, stringsAsFactors = FALSE)
    after <- df[df$cutrial_no >= 233 & df$cutrial_no <= 256, c("cutrial_no", "reachdeviation_deg", "aimdeviation_deg"), drop = FALSE]
    group2_after[[length(group2_after) + 1]] <- after
  
  return(list(group1 = group1_after, group2 = group2_after))
    #print(nrow(after_data$group1[[1]]))
    #print(nrow(after_data$group2[[1]])) 
    
    
  }

}
after_data <- getAfter()



totalphase <- c(c_data$reachdeviation_deg, combined_data$reachdeviation_deg)

# Create trial numbers for each segment (aligned phase: 1 to 88, rotated phase: 89 to 232)
trial_numbers_aligned <- 0:(length(c_data$reachdeviation_deg) - 1)  # Aligned phase: trial 0 to 104
trial_numbers_rotated <- (105 + 1):(105 + length(combined_data$reachdeviation_deg))  # Rotated phase: trial 105 to 232

# Combine the trial numbers and data
totalphase <- c(c_data$reachdeviation_deg, combined_data$reachdeviation_deg)

# Plot the total data (combined)
plot(totalphase, type="n", xlim=c(1, 256), ylim=c(-20, 80),
     xlab="Trial Number", ylab="Reach Deviation (degrees)",
     main="Reach Deviation Across Phases")

# Add lines for the aligned phase (trials 1-88)
lines(trial_numbers_aligned, c_data$reachdeviation_deg, col="red")

# Add lines for the rotated phase (trials 89-232)
lines(trial_numbers_rotated, combined_data$reachdeviation_deg, col="blue")

#lets take the average and do a line graph
#aim60_path <- 'data/Instructed_summary/aiming60'
#all_60data <- list()
#aim60files <- list.files(aim60_path, pattern = "*.csv", full.names = TRUE)

#all_reach_deviations <- list()


#for (i in 1:length(aim60files)) {
 # df <- read.csv(aim60files[i], stringsAsFactors = FALSE)
 # all_reach_deviations[[i]] <- df$reachdeviation_deg
#}

#combinedreachdev <- do.call(cbind,all_reach_deviations)

#average_reach_deviation <- apply(combinedreachdev, 1, mean, na.rm = TRUE)

#plot(average_reach_deviation, type = "l", col = "hotpink2", lwd = 2,   xlim = c(0, 256), ylim = c(-30,60),
   #  main = "Average Reach Deviation with a 60 Degree Perturbation", xlab = "Trial", ylab = "Reach Deviation (degrees)")
#smooth_line2 <- smooth.spline(average_reach_deviation)
#lines(smooth_line2, col = "black", lwd = 3, lty=3)

#abline(h=0, col="black", lwd = 1, lty=3)


