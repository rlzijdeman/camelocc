# Copyright: Richard L. Zijdeman, 2015, MIT-License
# Author: richard.zijdeman at iisg.nl
# File: 
# - purpose:Evalute data from coding experiments "october 12 meeting"
# - input: output files from experiment run by GK.
# - output: - outputConfidence.png

# clean workspace
rm(list = ls())

# Sources and Libs
library(ggplot2)
library(data.table)


# set directories
setwd("~/SURFdrive/II/projects/disc/data/original/meeting_12_oct_experiments/")
gitPath = "~/git/camelocc/"

# define experiment specific files
efile3 <- "cambridge_hisco-1.0-classify-classified-census_uk_anu.csv"
efile4 <- "zijdeman-1.0-classify-classified-census_uk_anu.csv"
efile5 <- "cambridge_hisco-zijdeman-1.0-classify-classified-census_uk_anu.csv"

# read experiment specific files as data.frames
e3 <- read.csv(paste0("./results_summary/experiment_3/", efile3), stringsAsFactors = FALSE)
e4 <- read.csv(paste0("./results_summary/experiment_4/", efile4), stringsAsFactors = FALSE)
e5 <- read.csv(paste0("./results_summary/experiment_5/", efile5), stringsAsFactors = FALSE)

# create new data.frame from output_code experiments
df <- cbind(e3[, c(2, 3, 5) ], e4[ ,c(2, 3, 5)], e5[ ,c(2, 3, 5)])
all.equal(df$e3RAW_DATA, df$e4RAW_DATA, df$e5RAW_DATA) # yes, keeping only 1 'raw_data"
df <- df[, c(1:3,5,6,8,9)]

# cleaning up variable names
names(df)[2:3] <- paste0(rep("e3",2), names(df)[2:3])
names(df)[4:5] <- paste0(rep("e4",2), names(df)[4:5])
names(df)[6:7] <- paste0(rep("e5",2), names(df)[6:7])
names(df) <- gsub(".1", "", names(df))
names(df) <- gsub(".2", "", names(df))


# What's the overlap in confidence, between e3 (Cambridge HISCO) and e4 (UK/Zijdeman)?
g1 <- ggplot(df, aes(x = df$e3OUTPUT_CODE, y = df$e3CONFIDENCE))
g2 <- g1 + geom_jitter(colour = "blue", alpha = .4)
g3 <- g2 + geom_jitter(aes(df$e4OUTPUT_CODE, y = df$e4CONFIDENCE), 
                       colour = "red", alpha = .4) +
        theme_bw(base_size = 16) +
        labs(list(x = "output code", 
                      y = "confidence", 
                      title = "Output by confidence 
                  \nblue = Cam-HISCO, red = UK/Zijdeman"))
g3
ggsave(paste0(gitPath,"graphs/outputConfidence.png"), g3)


# What's the overlap in occupational titles?
df$e3e4 <- df$e3OUTPUT_CODE == df$e4OUTPUT_CODE
df$e3e5 <- df$e3OUTPUT_CODE == df$e5OUTPUT_CODE
df$e4e5 <- df$e4OUTPUT_CODE == df$e5OUTPUT_CODE
df$e3e4e5 <- df$e4OUTPUT_CODE == df$e5OUTPUT_CODE


ggplot(df, aes(x = e3e4, label = RAW_DATA)) + 
        geom_bar(stat = "bin")
ggplot(df, aes(x = e3e5, label = RAW_DATA)) + 
        geom_bar(stat = "bin")
ggplot(df, aes(x = e4e5, label = RAW_DATA)) + 
        geom_bar(stat = "bin")
ggplot(df, aes(x= e3e4e5, label = "RAW_DATA")) +
        geom_bar(stat = "bin")

# df[df$e3e4e5 == TRUE, df$RAW_DATA] # strange: undefinied columns selected
# switching to data.table
dt <- data.table(df)
length(dt[e3e4e5 == TRUE, RAW_DATA])
dim(dt)
dt.allsame <- dt[e3e4e5 == TRUE, ]
ggplot(dt.allsame, aes(x = e3CONFIDENCE, y = e4CONFIDENCE)) +
        geom_point(aes(colour = e5CONFIDENCE)) + 
        scale_colour_gradient(low = "red", high = "green")
        


