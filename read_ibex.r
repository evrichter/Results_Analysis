### SPR PARSING AND DATA VIZ 
## CHRISTOPH AURNHAMMER, 2021
## Adapted from comp_ibex.r

### PACKAGES
library(ggplot2)
library(data.table)
library(gridExtra)
library(dplyr)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_Analysis/")
source("ibex_fns.r")

excluded_participants <- c("ffdd30f484d223ac5999ce51deb40e33",
                           "6b1c3b3f4a456ceb42f68dedb8f5b931")

#### DATA FORMATTING
# Get DEMOG CONSENT SURVEY data
cn <- get_consent("consent.txt")
dm <- get_demog("demog.txt")
sv <- get_survey("survey.txt")

cn <- cn[!(IPhash %in% excluded_participants),]
dm <- dm[!(IPhash %in% excluded_participants),] 
sv <- sv[!(IPhash %in% excluded_participants),] 

survey <- merge(dm, sv, by=c("IPhash"), all=TRUE)
survey$Age <- as.numeric(survey$Age)


round(mean(survey$Age),2)
round(sd(survey$Age),2)
range(survey$Age)
table(survey$gender)
nrow(survey[!(Native_language %in% c("Deutsch", "deutsch")),])
table(survey$Task_difficulty)/length(survey$Task_difficulty)
table(survey$Experiment_length)/length(survey$Experiment_length)
table(survey$handedness)

# Get REACTION TIMES & READING TIMES
rc <- get_reacts("task.txt")
pr <- get_plausibility_rating("task.txt")
rd <- get_reads("reading.txt")

rc <- rc[!(IPhash %in% excluded_participants),]
pr <- pr[!(IPhash %in% excluded_participants),] 
rd <- rd[!(IPhash %in% excluded_participants),] 

df <- merge(rd, rc[,c("ReactionTime", "Accuracy", "IPhash", "Item")], by=c("IPhash", "Item"), all=TRUE)
df <- merge(df, pr[,c("IPhash", "Item", "Condition", "SPR_Plaus_Rating", "SPR_Plaus_avg")], by=c("IPhash", "Item", "Condition"), all=TRUE)

# Change IPhashes to subject numbers
colnames(df)[1] <- "Subject"
df[, Subject := .GRP, by = .(Subject)]
df$Subject <- as.character(df$Subject)

# Check accuracies / reaction times
agg_df <- df[, lapply(.SD, mean, na.rm = TRUE), by=Subject, .SDcols=c("ReactionTime", "Accuracy", "ReadingTime")]
agg_df$Subject <- as.factor(agg_df$Subject)
agg_df[order(agg_df$ReactionTime),]
agg_df[order(agg_df$Accuracy),]


# merge with assoc and cloze pretest values
pretests <- fread("GradedP6_FollowUpStudy_Pretests.csv")
df <- merge(df, pretests[,c("Item", "Condition", "Verb", "Target", "Distractor", "Last_Mentioned", "Cloze", "Cloze_distractor", "Cloze_C_alternative", "Plaus_target_avg", "Plaus_dist_avg", "Surprisal_target", "Surprisal_distractor")], by=c("Item", "Condition"))

# add precritRT as predictor
df$precritRT <- rep(df[Region=="Pre-critical",]$ReadingTime, each=5)
#change column order
df <- df %>% relocate(precritRT, .before= ReadingTime)

fwrite(df, "GP6SPR.csv")


#############################################################################################################################################
# Plaus Data Viz for avg Plausratings from SPR Study
library(ggplot2)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_Analysis/")
dt <- fread("GP6SPR.csv")

means <- aggregate(SPR_Plaus_avg ~ Condition, dt, FUN=mean)
means$Plaus_SE <- aggregate(SPR_Plaus_avg ~ Condition, dt, FUN=se)$SPR_Plaus_avg
dt_items_abc <- dt[, lapply(.SD, mean), by=list(Item, Condition), .SDcols=c("SPR_Plaus_avg")]

# density plot
p <- ggplot(dt_items_abc, aes(x=SPR_Plaus_avg, color=Condition, fill=Condition)) + geom_density(alpha=0.4) + theme_minimal() + xlim(1,7) + ylim(0, 1.5)
p <- p + geom_vline(data=means, aes(xintercept=SPR_Plaus_avg, color=Condition), linetype="dashed") + scale_x_continuous(breaks=seq(1,7))
p <- p + scale_color_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + scale_fill_manual(labels=c("A", "B", "C"), values=c("black", "red", "blue"))
p <- p + labs(title = "Plausibility (SPR)", y="Density", x="Plausibility" )
ggsave("DensityPlot_Plausibility_SPR.pdf", p, device=cairo_pdf, width=4, height=4)
p

# barplot
q <- ggplot(means, aes(x=Condition, y=SPR_Plaus_avg)) + geom_bar(stat="identity") + labs(title = "Average Plausibility Ratings per Condition (SPR)", y = "Plausibility",  x = "Condition") + geom_errorbar(aes(ymin=SPR_Plaus_avg-Plaus_SE, ymax=SPR_Plaus_avg+Plaus_SE), width=.4, position=position_dodge(.9)) + theme_minimal() + coord_cartesian(ylim = c(1, 7)) + scale_y_continuous(breaks = c(1:7))
ggsave("BarPlot_Plausibility_SPR.pdf", q, device=cairo_pdf, width=4, height=4)
q
#############################################################################################################################################
#fwrite(df, "lmerSPR/data/GP6SPR.csv")
# Verb length per cond
# df$ncharverb <- nchar(df$Verb)
# aggregate(ncharverb ~ Condition, df, mean)

# ########## READ PROCESSED DATA
# setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results_Analysis/")
# df_bal <- fread("Exp4_SPR.csv")
# df_bal$Subject <- as.factor(df_bal$Subject)

# # data exclusion per region
# cutoff_in_sd = 4
# # df_first <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "First",], sd_cutoff=cutoff_in_sd)
# # df_second <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Second",], sd_cutoff=cutoff_in_sd)
# # df_third <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Third",], sd_cutoff=cutoff_in_sd)
# # df_fourth <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Fourth",], sd_cutoff=cutoff_in_sd)
# # df_fifth <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Fifth",], sd_cutoff=cutoff_in_sd)
# # df_sixth <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Sixth",], sd_cutoff=cutoff_in_sd)
# # df_precrit_2 <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Pre-critical_2",], sd_cutoff=cutoff_in_sd)
# # df_precrit <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Pre-critical",], sd_cutoff=cutoff_in_sd)
# # df_crit <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Critical",], sd_cutoff=cutoff_in_sd)
# # df_spill <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Spillover",], sd_cutoff=cutoff_in_sd)
# # df_postspill <- exclude(df_bal[ReadingTime > 100 & ReadingTime < 2500 & Region == "Post-spillover",], sd_cutoff=cutoff_in_sd)

# #### DATA VISUALISATION

# ## Per Condition RTs per region
# #df_bal_excl <- rbind(df_first, df_second, df_third, df_fourth, df_fifth, df_sixth)
# #df_bal_excl <- rbind(df_precrit_2, df_precrit, df_crit, df_spill, df_postspill)
# df_bal_excl <- rbind(df_precrit, df_crit, df_spill, df_postspill)


# # PLOT WORDSTEPS
# df_bal_excl$ReadingTime <- log(df_bal_excl$ReadingTime)
# plusminus <- aggregate(ReadingTime ~ Region + Condition, df_bal_excl, FUN=mean)
# #plusminus$Region <- factor(plusminus$Region, levels=c("First", "Second", "Third", "Fourth", "Fifth", "Sixth"))
# plusminus$Region <- factor(plusminus$Region, levels=c("Pre-critical", "Critical", "Spillover", "Post-spillover"))
# plusminus$SE <- aggregate(ReadingTime ~ Region + Condition, df_bal_excl, FUN=se)$ReadingTime

# p <- ggplot(plusminus, aes(x=Region, y=ReadingTime, color=Condition, group=Condition)) + geom_point(size=2.2) + geom_line(size=0.5)
# p <- p + theme_minimal() + geom_errorbar(aes(ymin=ReadingTime-SE, ymax=ReadingTime+SE), width=.1, size=0.3)
# p <- p + scale_color_manual(name="Condition", labels=c("A: Expected Plausible", "B: Unexpected Less Plausible", "C: Unexpected Implausible"), values=c("#000000", "#BB5566", "#004488", "#DDAA33"))
# p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7))
# p <- p + labs(x="Region", y="Reading Time")
# p
# ggsave("/Users/chr/Desktop/35Subjects_SPR.pdf", p, width=4, height=4)

# ## Reaction Time
# # Histogram Reaction Time
# df_react <- df_crit[!is.na(df_crit$ReactionTime),]

# p <- ggplot(df_react, aes(ReactionTime)) + geom_histogram() + theme_minimal() 
# p <- p + geom_vline(xintercept=mean(df_react$ReactionTime)) + geom_vline(xintercept=median(df_react$ReactionTime))
# p

# p <- ggplot(df_react, aes(log(ReactionTime))) + geom_histogram() + theme_minimal()
# p

# # Barplot Reaction Time
# reac_agg <- df_react[, lapply(.SD, mean, na.rm=TRUE), by=Condition, .SDcols=c("ReactionTime")]
# reac_agg$SE <- df_react[, lapply(.SD, se, na.rm=TRUE), by=Condition, .SDcols=c("ReactionTime")]$ReactionTime
# p <- ggplot(reac_agg, aes(x=Condition, y=ReactionTime)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=ReactionTime-SE, ymax=ReactionTime+SE), width=.2)
# p <- p + theme_minimal() + labs(x="Condition", y="Reaction Time", title="CAP SPR Reaction Times", subtitle="A: Expected Plausible B: Unexpected Less Plausible C: Unexpected Implausible")
# #p <- p + ylim(0, 3500)
# p

# ## Accuracy
# # Barplot Accuracy
# acc_agg <- df_react[, lapply(.SD, mean, na.rm=TRUE), by=Condition, .SDcols=c("Accuracy")]
# acc_agg$SE <- df_react[, lapply(.SD, se, na.rm=TRUE), by=Condition, .SDcols=c("Accuracy")]$Accuracy
# p <- ggplot(acc_agg, aes(x=Condition, y=Accuracy)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=Accuracy-SE, ymax=Accuracy+SE), width=.2)
# p <- p + theme_minimal() + labs(x="Condition", y="Accuracy", title="CAP SPR Accuracies", subtitle="A: Expected Plausible B: Unexpected Less Plausible C: Unexpected Implausible")
# p <- p + ylim(0, 1)
# p
