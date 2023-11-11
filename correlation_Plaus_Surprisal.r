
library(data.table)
library(dplyr)
library(Kendall)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_SPR_Plaus_Ratings/")

dt_p <- fread("GP6SPR_processed.csv")

# add columns, condition, Item and TarDist to dt_s
dt_p$Condition <- rep(c("A", "A", "B", "B", "C", "C"), 60)
dt_p$TarDist <- rep(c("Target", "Distractor"), 180)
dt_s$Item <- rep(1:60, each = 6)


# Extract rows where "TarDist" is "Target"
subset_target <- dt_s[dt_s$TarDist == "Target", c("Condition", "Item", "GPT2_s_sep")]

# Extract rows where "TarDist" is "Distractor"
subset_distractor <- dt_s[dt_s$TarDist == "Distractor", c("Condition", "Item", "GPT2_s_sep")]

# Merge the two subsets based on "Item" and "Condition"
new_dt_s <- merge(subset_target, subset_distractor, by = c("Item", "Condition"), all = TRUE)

# Rename the columns to differentiate between "Target" and "Distractor" values
colnames(new_dt_s) <- c("Item", "Condition", "Surprisal_Target", "Surprisal_Distractor")

# Merge new_dt_s fir surprisal and dt_p for plausibility
dt_final <- merge(dt_p, new_dt_s, by = c("Item", "Condition"), all = TRUE)

# rename more columns
colnames(dt_final)[colnames(dt_final) == "Rating"] <- "Rating_Target"
colnames(dt_final)[colnames(dt_final) == "Ratingdist"] <- "Rating_Distractor"

# Correlation Matrix
correlation_matrix <- cor(dt_final[, c("Rating_Target", "Rating_Distractor", "Surprisal_Target", "Surprisal_Distractor")])
correlation_matrix





