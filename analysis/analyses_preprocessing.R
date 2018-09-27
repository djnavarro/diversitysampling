# load the raw data from the four individual files 
# merge into a single CSV and save the results

# import the data
a <- read.csv(file = './data/helpful_version1.csv')
b <- read.csv(file = './data/helpful_version2.csv')
c <- read.csv(file = './data/random_version1.csv')
d <- read.csv(file = './data/random_version2.csv')

# fix inconsistent naming within the raw data
a <- a[,1:17]
names(a)[1:5] <- names(c)[1:5] <- names(d)[1:5] <- names(b)[1:5]

# add condition information
a$Sampling <- "Helpful"
b$Sampling <- "Helpful"
c$Sampling <- "Random"
d$Sampling <- "Random"

# merge the data & tidy the workspace
df <- merge(a,b,all=TRUE)
df <- merge(df,c,all=TRUE)
df <- merge(df,d,all=TRUE)
rm(a,b,c,d)

write.csv(df,"./output/data_merged.csv",row.names = FALSE)


