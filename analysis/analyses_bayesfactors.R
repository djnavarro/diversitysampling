library(BayesFactor)
library(reshape2)

# read from the merged file
df <- read.csv("./output/data_merged.csv")

# convert to a form we can do BF analyses on... apparently this 
# predates me learning how to use "gather" and "spread" so the
# reshaping is beeing done with "cast" and "melt". 
df$Id <- as.factor(paste0("s",1:dim(df)[1]))
df_small <- df[, c("Id","Sampling",grep("Target",names(df),value=TRUE))]
df_molten <- melt(
  data = df_small,
  id.vars = c("Id","Sampling"),
  measure.vars = grep("Target",names(df_small),value=TRUE)
)
names(df_molten)[3:4] <- c("Target","Rating")
df_molten$Type <- as.factor(gsub("_.*$","",as.character(df_molten$Target)))
df_molten$Sampling <- as.factor(df_molten$Sampling)

# aggregate the three arguments within subject
df_averaged <- aggregate(Rating ~ Sampling + Type + Id, df_molten, mean)

# Bayesian random effects ANOVA
bf1 <- anovaBF(
  formula = Rating ~ Sampling + Type + Id, 
  data = df_averaged,
  whichRandom = c("Id")
)

# Bayesian paired samples t-test 
bf2 <- ttestBF(
  x = df_averaged$Rating[df_averaged$Sampling == "Random" & df_averaged$Type == "Diverse"],
  y = df_averaged$Rating[df_averaged$Sampling == "Random" & df_averaged$Type == "NonDiverse"],
  paired = TRUE
)

# Bayesian paired samples t-test 
bf3 <- ttestBF(
  x = df_averaged$Rating[df_averaged$Sampling == "Helpful" & df_averaged$Type == "Diverse"],
  y = df_averaged$Rating[df_averaged$Sampling == "Helpful" & df_averaged$Type == "NonDiverse"],
  paired = TRUE
)

# save it
save(df, df_molten, df_averaged, bf1, bf2, bf3, file = "./output/bayesfactors.Rdata")
