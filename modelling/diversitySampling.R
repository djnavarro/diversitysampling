# The relationship between premise diversity effects and sampling
# assumptions. In this simulation we consider a "generic" setting
# in which there is no particular structure to the stimulus domain:
# the learner has a set of hypotheses, each of which defines a
# subset of entities that possess the novel feature. To illustrate
# the fact that the sampling effect does not depend on any specific
# domain structure, we generate random "hypothesis spaces". 
#
# All arguments considered in the paper have the form:
#
#   X has P
#   Y has P
#   Z has P
#  --------------
#   all S have P
#
# where S is a superordinate category encompassing X, Y and Z (rather
# than a narrower subcategory that might do so). As a general rule we
# should expect that the diversity of X, Y and Z to have an impact 
# on the strength of the argument. The diversity effect states that 
# the more diverse X, Y and Z are (i.e., the less similar they are
# to one another), the more likely people are to endorse the 
# superordinate conclusion. A Bayesian approach to inductive reasoning
# makes the prediction that the strength of this effect should be 
# sensitive to the manner in which the premise items were chosen. We
# consider two possibilities:
#
#  - weak sampling: X, Y and Z were chosen at random
#  - strong sampling: X,Y and Z were chosen because they have P

# Main loop
doSim <- doSave <- FALSE
doPlot <- TRUE

if(doSim) {
  
  # Define the settings for the simulation
  settings <- list(
    nHypotheses = 100,  # how many possible hypotheses are there?
    nStimuli = 20,      # how many stimuli are we considering?
    sparsity = .05,      # what proportion of items are covered by a hypothesis?
    nPremises = 3,      # how many premise items appear in the argument?
    nIterations = 10000  # how many random feature matrices to generate?
  )
  
  # Function to generate n samples from a Bernoulli distribution with 
  # success probability p. Trivial, but it tidies the code below!
  rbernoulli <- function(n,p) {
    u <- runif(n)
    x <- as.numeric(u < p)
    return(x)
  }
  
  # initialise data frame to store the results
  allcases <- data.frame(
    similarity = numeric(0),
    diversity = numeric(0),
    weak = numeric(0),
    strong = numeric(0)
  )
  
  for(i in 1:settings$nIterations) {
    
    if(i%%10==0) cat(".") # on screen display
    
    # construct a random hypothesis space
    hypotheses <- matrix(
      data = rbernoulli(  # binary membership, bernoulli sampled
        n = settings$nStimuli * settings$nHypotheses,
        p = settings$sparsity
      ),
      nrow = settings$nStimuli,    # each row is an entity
      ncol = settings$nHypotheses  # each column is a hypothesis
    )
    
    # ensure the final hypothesis is a superordinate category
    # that contains every stimulus in the domain
    hypotheses[,settings$nHypotheses] <- 1
    
    # calculate the size of each hypothesis (i.e., number of
    # entities presumed to possess the property according to
    # that hypothesis)
    sizes <- colSums(hypotheses)
    
    # construct the set of all possible combinations of premises
    # that might be used to make an argument about these items,
    # and the number of such combinations
    premiseSets <- t( combn( settings$nStimuli, settings$nPremises ))
    nPremiseSets <- dim(premiseSets)[1]
    
    # initialise a data frame that will store the relevant information 
    # about a single hypothesis space
    onecase <- data.frame(
      similarity = numeric(nPremiseSets),
      diversity = numeric(nPremiseSets),
      weak = numeric(nPremiseSets),
      strong = numeric(nPremiseSets)
    )
    
    # consider every possible premise set...
    for(i in 1:nPremiseSets) {
      
      # calculate pairwise similarities among premise items
      h <- hypotheses[premiseSets[i,],] # hypothesis space for the premise items only
      s <- h %*% t(h)                   # pairwise similarities among all premises
      
      # store information about the premise similarity & diversity
      onecase$similarity[i] <- mean(s[upper.tri(s)])  # average similarity
      onecase$diversity[i] <- (settings$nHypotheses - onecase$similarity[i]) / 
        settings$nHypotheses  # diversity
      
      # find the list of hypotheses that are consistent with all premises 
      valid <- apply(h, 2, prod) # valid hypotheses
      
      # under weak sampling, the only role that the premises can play is to
      # falsify invalid hypotheses, so the "superordinate category hypothesis"
      # like every other valid hypothesis, has posterior probability of 1/nv
      # where nv is the number of valid hypotheses (assuming a uniform prior
      # of course)
      onecase$weak[i] <- 1/sum(valid)
      
      # under strong sampling, it's a little more complicated. The person 
      # selecting premises is assumed to know the true hypothesis and is 
      # deliberately choosing positive evidence - for simplicity, assumed to
      # be randomly selected. That measn the size of different categories is
      # relevant:
      likelihood <- (1 / sizes[valid == 1]) ^ settings$nPremises # sampling probability
      posterior <- likelihood / sum(likelihood) # posterior (under uniform priors)
      onecase$strong[i] <- posterior[length(posterior)]  # superordinate category is last
      
    }
    
    # because we want to weight each random hypothesis space in the simulation
    # equally we'll compute the relationship between premise diversity and posterior 
    # probability of the superordinate separately for each matrix...
    tmp <- aggregate(weak ~ diversity, onecase, mean)
    tmp$strong <- aggregate(strong ~ diversity, onecase, mean)[[2]]
    
    # and append to our list
    allcases <- rbind(allcases, tmp)
  }
  
  # of course, we don't care about any specific hypothesis space, so at the end of 
  # the simulation, we'll aggregate across the random samples:
  results <- aggregate(weak ~ diversity, allcases, mean)
  results$strong <- aggregate(strong ~ diversity, allcases, mean)[[2]]
  
  # finally, the literal "diversity" value doesn't mean anything, as it's highly 
  # dependent on the sparsity of the domain etc (e.g., you tend to get numbers 
  # ranging between .9 and 1 because most entities belong to very few categories)
  # What matters is the rank ordering:
  results$diversity <- order(results$diversity)
  
  # save file
  if(doSave) {
    save(settings, results, file="diversityData.Rdata")
  }
  
  # tidy the workspace
  rm(i, likelihood, nPremiseSets, posterior, sizes, valid, 
     s, premiseSets, h, hypotheses, allcases, onecase, tmp)
  
} else {
  load("./diversityData.Rdata")
}


# there's a few different ways we might want to visualise the data, so 
# to keep things tidy I'll group all the plotting functions in to a list
plots <- list()

# the original one
plots$original <- function(df) {
  
  plot.new()
  xr <- range(df$diversity)
  plot.window(xlim = xr, ylim = c(0,1))
  title(xlab = "Premise Diversity", ylab = "Evidence for Superordinate")
  lines(df$diversity, df$weak, type = "b", pch = 19, col = "black")
  lines(df$diversity, df$strong, type="b", pch = 1, col = "black")
  axis(1, at = c(xr[1], mean(xr), xr[2]), labels = c("Low","Medium","High"))
  axis(2)
  box()
  legend(x = "bottomright", pch=c(19, 1), col = c("black"),
         legend = c("Weak Sampling","Strong Sampling"), bty = "n")
  
}

# mirror the style of the data plot, 
plots$empiricalstyle <- function(df) {
  
  # colours for the plot
  highfill <- "grey50"
  lowfill <- "grey70"
  
  # use loess regression to generate slightly smoother functions
  # for the purposes of optimising parameter values - doesn't actually
  # matter much though.
  mods <- list()
  mods$strong <- loess(strong ~ diversity, df) 
  mods$weak <- loess(weak ~ diversity, df) 
  
  # function used when optimising parameter values
  sse <- function(d, ver) {
    
    # what are the empirical values to approximate?
    if(ver == "diverse") {
      obs_strong <- (4.773 - 1)/6 # a value between 0 and 1
      obs_weak <- (5.393 - 1)/6 # a value between 0 and 1
    } else {
      obs_strong <- (3.891 - 1)/6 
      obs_weak <- (5.069 - 1)/6
    }
    
    # compute the sse for the current choice of threshold
    pred_strong <- predict(mods$strong, newdata = data.frame(diversity = d))
    pred_weak <- predict(mods$weak, newdata = data.frame(diversity = d))    
    out <- (pred_weak - obs_weak)^2 + (pred_strong - obs_strong)^2
    return(out)
    
  }
  
  # where should we set the thresholds d and n?
  thresholds <- c(
    "diverse" = optim(par = 6, fn = sse, ver = "diverse", 
             method = "Brent", lower = 1, upper = 10)$par,
    "nondiverse" = optim(par = 6, fn = sse, ver = "nondiverse", 
             method = "Brent", lower = 1, upper = 10)$par
  )
  
  # what does the model predict at these values?
  pred <- rbind(
    "strong" = c( predict(mods$strong, newdata = data.frame(diversity = thresholds["diverse"])),
                  predict(mods$strong, newdata = data.frame(diversity = thresholds["nondiverse"]))),
    "weak" = c( predict(mods$weak, newdata = data.frame(diversity = thresholds["diverse"])),
                predict(mods$weak, newdata = data.frame(diversity = thresholds["nondiverse"])))
    )
  print(pred)
  
  # set up plot
  layout(matrix(1:3,1,3), widths = c(4,2,2))
  
  # omit the ends of the plots where there are so few data
  xr <- c(.1,.85) * (max(df$diversity) - 1) + 1

  # panel (a)  
  plot.new()
  plot.window(
    xlim = c(xr + c(-1, 1) * .05),
    ylim = c(0, 1)
    )

  # plot thresholds as the vertical lines
  abline(
    v = thresholds, 
    lty = 3, lwd = 3, 
    col=c(highfill, lowfill)
    )
  
  # which indices should we plot?
  ind <- df$diversity >= xr[1] & df$diversity <= xr[2]
  
  # strong sampling
  lines(df$diversity[ind], df$strong[ind], 
        type="o", pch = 21, col = "black", 
        bg = "black", lwd=1.5, cex=1.2)
  
  # weak sampling
  lines(df$diversity[ind], df$weak[ind], 
        type = "o", pch = 21, col = "black", 
        bg = "white", lwd=1.5, cex=1.2)
  
  # add prettiness
  box()
  axis( side = 1, 
        at = c(xr[1], 
               thresholds["nondiverse"], 
               thresholds["diverse"], 
               xr[2]), 
        labels = c("Least Diverse",
                   "n",
                   "d", 
                   "Most Diverse")
      )
  axis(side = 2)
  legend( x = "topleft", 
          pch = c(21, 21), 
          col = c("black"), 
          pt.bg = c("black", "white"),
          legend = c("Strong Sampling", "Weak Sampling"), 
          bty = "n"
          )
  title(
    xlab = "Diversity of Premises", 
    ylab = "Evidence for Conclusion",
    main = "(a) Evidence, Diversity and Sampling"
    )

  # panel (b)
  plot.new()
  plot.window(xlim = c(-1,6), ylim = c(0,1))
  title(xlab = "Sampling Condition", 
        ylab = "Evidence for Conclusion",
        main = "(b) Model Behaviour")
  
  # convenince function
  rectangle <- function(left, right, top, bottom, ...) {
    polygon(
      x = c(left, left, right, right), 
      y = c(bottom, top, top, bottom),
      ...)
  }
  
  # draw the four bars
  rectangle(0, 1, 0, pred["strong", "diverse"], col = highfill) 
  rectangle(1, 2, 0, pred["strong", "nondiverse"], col = lowfill)
  rectangle(3, 4, 0, pred["weak", "diverse"], col = highfill) 
  rectangle(4, 5, 0, pred["weak", "nondiverse"], col = lowfill)  
  
  # add prettiness
  axis(side = 1, 
       at = c(1, 4), 
       labels = c("Strong", "Weak")
       )
  axis(side = 2)
  box()
  legend(x = "topright", 
         legend = c("Diverse (d)", "Non-Diverse (n)"),
         fill = c(highfill, lowfill), 
         bty = "n"
         )
  
  # panel (c)
  x <- read.csv("./empiricalData.csv")
  
  # begin plotting
  plot.new()
  plot.window(xlim = c(-1,6), ylim = c(1,7))
  title(xlab = "Sampling Condition", 
        ylab = "Rated Argument Strength",
        main = "(c) Empirical Data")

  # empirical means
  d <-  aggregate(Diverse ~ Condition, x, mean)
  nd <- aggregate(Nondiverse ~ Condition, x, mean)
  
  # draw the four bars
  rectangle(0, 1, 1, d[1,2], col = highfill) 
  rectangle(1, 2, 1, nd[1,2], col = lowfill) 
  rectangle(3, 4, 1, d[2,2], col = highfill) 
  rectangle(4, 5, 1, nd[2,2], col = lowfill) 

  # compute the confidence intervals
  require(lsr)
  dc <- aggregate(Diverse ~ Condition, x, ciMean)[[2]]
  ndc <- aggregate(Nondiverse ~ Condition, x, ciMean)[[2]]
  
  # convenience function
  addCI <- function(ci, xval, w = .25, ...) {
    lines(c(xval, xval), ci, ...)
    lines(c(xval, xval) + c(-w, w) / 2, ci[1] * c(1,1), ...)  
    lines(c(xval, xval) + c(-w, w) / 2, ci[2] * c(1,1), ...)
  }
  
  # add error bars to the plot
  addCI(ci = dc[1,], xval = .5, lwd=2)
  addCI(ci = ndc[1,], xval = 1.5, lwd=2)  
  addCI(ci = dc[2,], xval = 3.5, lwd=2)
  addCI(ci = ndc[2,], xval = 4.5, lwd=2)
  
  # add prettiness
  axis(side = 1, at = c(1, 4), labels = c("Strong","Weak"))
  axis(side = 2)
  box()
  legend(x = "topright", 
         legend = c("Diverse", "Non-Diverse"),
         fill = c(highfill, lowfill), 
         bty = "n"
         )
  
  # reset the graphics layout
  layout(1)
  
}

if(doPlot) {
  # plots$original(results)
  plots$empiricalstyle(results)
}


