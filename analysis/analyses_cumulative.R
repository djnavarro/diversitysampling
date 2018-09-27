# read from the merged file
df <- read.csv("./output/data_merged.csv")

# compute the cumulative distribution functions
cumprob <- matrix(0,12,8)
n <- vector()
i <- 0
for(cond in c("Helpful", "Random")) {
  for(type in c("Diverse", "NonDiverse")) {
    for(target in 1:3) {
      i <- i + 1
      n[i] <- paste(cond,type,target)
      for(v in 1:7) {
        cumprob[i,v+1] <- mean(df[df$Sampling==cond, paste0(type,"_Target",target)] <= v)
      }
    }
  }
}
rownames(cumprob) <- n


# information for styling the plots
lty <- c("Diverse"=5,"NonDiverse"=1)
pch <- c("Diverse"=5,"NonDiverse"=21)
cex <- c("Diverse"=1.2,"NonDiverse"=1.6)
col <- c("Diverse"="grey70","NonDiverse"="black")
main <- c("Random"="Weak Sampling", "Helpful"="Strong Sampling")

layout(matrix(1:2,1,2))
for(cond in  c("Random","Helpful")) { # loop over sampling conditions
  
  # initialise the plot
  plot.new()
  plot.window(xlim=c(.5,7.5),ylim=c(0,1))
  axis(1,at=1:7)
  axis(2)
  
  for(type in c("Diverse","NonDiverse")) { # loop over evidence types
    for(target in 1:3) { # loopt over the three target arguments

      # plot the cumulative distribution function for responses
      ind <- grep(paste(cond,type,target), rownames(cumprob))
      lines(
        x = 1:7, y = cumprob[ind,-1],
        lty = lty[type], lwd = 2, pch = pch[type], type = "b",
        bg = col[type], cex = cex[type], col = col[type]
      )
      
    }
  }
  title(xlab="Argument Strength",
        ylab="Cumulative Probability",
        main=main[cond])
  box()
}

legend(x = "bottomright",
       legend = c("Diverse","NonDiverse"),
       pch = pch,
       lty = lty,
       pt.bg = c("white","black"),
       col = c("grey70","black"),
       lwd = 2,
       bty = "n")

layout(1)
dev.print(pdf,file="./output/cdf.pdf", width=9, height=5)
write.csv(cumprob,file="./output/cdf.csv")



