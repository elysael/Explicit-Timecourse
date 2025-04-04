plotAllAimOverlay <- function() {
  plot(-1000, 1000, type = "n", xlim = c(1, 256), ylim = c(-5, 80),
       xlab = "Trial", ylab = "Mean Aim Deviation (°)",
       main = "Aiming Deviation Across Rotation Sizes")
  
  lines(smoothall40, col='magenta', lwd=2) 
  lines(smoothall50, col='orange', lwd=2)  
  lines(smoothall60, col='cyan', lwd=2) 
  abline(v=233, col="black", lty=3)
  text(x = 233, y = 0, labels = "Left hand Trials for Group 1", col = "black", pos = 2, cex = 1)
  
  # Add a legend
  legend("topright", legend = c("40°", "50°", "60°"),
         col = c("magenta", "orange", "cyan"), lty = 1, lwd = 2)
  

}


plotAllReachOverlay <- function() {
  
  plot(-1000, 1000, type = "n", xlim = c(1, 256), ylim = c(-5, 80),
       xlab = "Trial", ylab = "Mean Aim Deviation (°)",
       main = "Reach Deviation Across Rotation Sizes")

  lines(smoothreach40, col='magenta', lwd=2) 
  lines(smoothreach50, col='orange', lwd=2)  
  lines(smoothreach60, col='cyan', lwd=2) 
  

  abline(v=233, col="black", lty=3)
  text(x = 233, y = 0, labels = "Left hand Trials for Group 1", col = "black", pos = 2, cex = 1)

  legend("topright", legend = c("40°", "50°", "60°"),
         col = c("magenta", "orange", "cyan"), lty = 1, lwd = 2)
}

