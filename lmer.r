### LMER ###
# load lme4 package
library(lme4)
library(dplyr)

setwd("~/Downloads/Master_Thesis/3_SPR_Study/Results/")
GP6 <- read.csv("GP6SPR_processed.csv")


residuals <- data.frame(
  Region = character(0),
  Condition = character(0),
  Residual = numeric(0),
  SE = numeric(0)
)

logRT_estimated <- data.frame(
  Region = character(0),
  Condition = character(0),
  Estimated_logRT <- numeric(0)
)





##### PRE-CRITICAL #####
# Precritical region
Precritical <- subset(GP6, Region == "Pre-critical")
# standardise predictors
Precritical$scaled_Plaus_Precritical <- scale(Precritical$SPR_Plaus_Rating)
Precritical$scaled_Surprisaldist_Precritical <- scale(Precritical$Surprisal_distractor)
# invert predictor plausibility
Precritical$inverted_scaled_Plaus_Precritical <- (Precritical$scaled_Plaus_Precritical) * (-1)
#log transform reading times
Precritical$logRT_Precritical <- log(Precritical$ReadingTime)

# define and run the linear mixed-effects regression model for the precritical region 
model_Precritical <- lmer(logRT_Precritical ~ inverted_scaled_Plaus_Precritical + scaled_Surprisaldist_Precritical + 
                            (1 + inverted_scaled_Plaus_Precritical + scaled_Surprisaldist_Precritical | Subject) + 
                            (1 + inverted_scaled_Plaus_Precritical + scaled_Surprisaldist_Precritical | Item), data = Precritical)

# print the summary of the model
summary_Precritical <- summary(model_Precritical)
summary_Precritical



#####predict condition A, precritical#####
Precritical_A <- subset(Precritical, Condition == "A")
Precritical_A$Precritical_A_Predicted <- predict(model_Precritical, newdata = Precritical_A,  type = "response")

# calculate residuals
Residual_Precritical_A <- mean(Precritical_A$logRT_Precritical) - mean(Precritical_A$Precritical_A_Predicted)
Residual_Precritical_A
# observed RT for condition A precritical
Precrit_A_logRT_observed <- mean(Precritical_A$logRT_Precritical)
Precrit_A_logRT_observed
# estimated RT for condition A precritical
Precrit_A_logRT_estimated <- mean(Precritical_A$Precritical_A_Predicted)
Precrit_A_logRT_estimated

# calculate standard error for residuals
SE_Res_Precrit_A <- sqrt(sd(Precritical_A$logRT, na.rm = TRUE)^2/length(Precritical_A$logRT) + sd(Precritical_A$Precritical_A_Predicted, na.rm = TRUE)^2/length(Precritical_A$Precritical_A_Predicted))
SE_Res_Precrit_A
new_row_residuals <- data.frame(Region = 'Pre-critical', Condition = "A", Residual = Residual_Precritical_A, SE_Residual = SE_Res_Precrit_A)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
SE_est_Precrit_A <- sd(Precritical_A$Precritical_A_Predicted, na.rm = TRUE) / sqrt(length(Precritical_A$Precritical_A_Predicted)) 
SE_est_Precrit_A
new_row_logRT_estimated <- data.frame(Region = 'Pre-critical', Condition = "A", Estimated_logRT = Precrit_A_logRT_estimated, SE_Estimated = SE_est_Precrit_A)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



####predict condition B, precritical#####
Precritical_B <- subset(Precritical, Condition == "B")
Precritical_B$Precritical_B_Predicted <- predict(model_Precritical, newdata = Precritical_B,  type = "response")

# calculate residuals
Residual_Precritical_B <- mean(Precritical_B$logRT_Precritical) - mean(Precritical_B$Precritical_B_Predicted)
Residual_Precritical_B
Precrit_B_logRT_observed <- mean(Precritical_B$logRT_Precritical)
Precrit_B_logRT_observed
Precrit_B_logRT_estimated <- mean(Precritical_B$Precritical_B_Predicted)
Precrit_B_logRT_estimated

# calculate standard error for plotting error bars
Precritical_B$SE <- sd(Precritical_B$logRT, na.rm = TRUE) / sqrt(nrow(Precritical_B))
new_row_residuals <- data.frame(Region = 'Pre-critical', Condition = "B", Residual = Residual_Precritical_B, SE = Precritical_B$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Pre-critical', Condition = "B", Estimated_logRT = Precrit_B_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



#####predict condition C, precritical#####
Precritical_C <- subset(Precritical, Condition == "C")
Precritical_C$Precritical_C_Predicted <- predict(model_Precritical, newdata = Precritical_C,  type = "response")

# calculate residuals
Residual_Precritical_C <- mean(Precritical_C$logRT_Precritical) - mean(Precritical_C$Precritical_C_Predicted)
Residual_Precritical_C
Precrit_C_logRT_observed <- mean(Precritical_C$logRT_Precritical) 
Precrit_C_logRT_observed
Precrit_C_logRT_estimated <- mean(Precritical_C$Precritical_C_Predicted)
Precrit_C_logRT_estimated

# calculate standard error for plotting error bars
Precritical_C$SE <- sd(Precritical_C$logRT, na.rm = TRUE) / sqrt(nrow(Precritical_C))
new_row_residuals <- data.frame(Region = 'Pre-critical', Condition = "C", Residual = Residual_Precritical_C, SE = Precritical_C$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Pre-critical', Condition = "C", Estimated_logRT = Precrit_C_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)





##### CRITICAL #####
# critical region
Critical <- subset(GP6, Region == "Critical")
# standardise predictors
Critical$scaled_Plaus_Critical <- scale(Critical$SPR_Plaus_Rating)
Critical$scaled_Surprisaldist_Critical <- scale(Critical$Surprisal_distractor)
# invert predictor plausibility
Critical$inverted_scaled_Plaus_Critical <- (Critical$scaled_Plaus_Critical) * (-1)
#log transform reading times
Critical$logRT_Critical <- log(Critical$ReadingTime)

# define and run the linear mixed-effects regression model for the Critical region 
model_Critical <- lmer(logRT_Critical ~ inverted_scaled_Plaus_Critical + scaled_Surprisaldist_Critical + 
                            (1 + inverted_scaled_Plaus_Critical + scaled_Surprisaldist_Critical | Subject) + 
                            (1 + inverted_scaled_Plaus_Critical + scaled_Surprisaldist_Critical | Item), data = Critical)

# print the summary of the model
summary_Critical <- summary(model_Critical)
summary_Critical


#####predict condition A, Critical#####
Critical_A <- subset(Critical, Condition == "A")
Critical_A$Critical_A_Predicted <- predict(model_Critical, newdata = Critical_A,  type = "response")

# calculate residuals
Residual_Critical_A <- mean(Critical_A$logRT_Critical) - mean(Critical_A$Critical_A_Predicted)
Residual_Critical_A
Critical_A_logRT_observed <- mean(Critical_A$logRT_Critical) 
Critical_A_logRT_observed
Critical_A_logRT_estimated <- mean(Critical_A$Critical_A_Predicted)
Critical_A_logRT_estimated

# calculate standard error for plotting error bars
Critical_A$SE <- sd(Critical_A$logRT, na.rm = TRUE) / sqrt(nrow(Critical_A))
new_row_residuals <- data.frame(Region = 'Critical', Condition = "A", Residual = Residual_Critical_A, SE = Critical_A$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Critical', Condition = "A", Estimated_logRT = Critical_A_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



#####predict condition B, Critical#####
Critical_B <- subset(Critical, Condition == "B")
Critical_B$Critical_B_Predicted <- predict(model_Critical, newdata = Critical_B,  type = "response")

# calculate residualds
Residual_Critical_B <- mean(Critical_B$logRT_Critical) - mean(Critical_B$Critical_B_Predicted)
Residual_Critical_B
Critical_B_logRT_observed <- mean(Critical_B$logRT_Critical)
Critical_B_logRT_observed
Critical_B_logRT_estimated <- mean(Critical_B$Critical_B_Predicted)
Critical_B_logRT_estimated

# calculate standard error for plotting error bars
Critical_B$SE <- sd(Critical_B$logRT, na.rm = TRUE) / sqrt(nrow(Critical_B))

new_row_residuals <- data.frame(Region = 'Critical', Condition = "B", Residual = Residual_Critical_B, SE = Critical_B$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Critical', Condition = "B", Estimated_logRT = Critical_B_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



#####predict condition C, Critical#####
Critical_C <- subset(Critical, Condition == "C")
Critical_C$Critical_C_Predicted <- predict(model_Critical, newdata = Critical_C,  type = "response")

# calculate residuals
Residual_Critical_C <- mean(Critical_C$logRT_Critical) - mean(Critical_C$Critical_C_Predicted)
Residual_Critical_C
Critical_C_logRT_observed <- mean(Critical_C$logRT_Critical)
Critical_C_logRT_observed
Critical_C_logRT_estimated <- mean(Critical_C$Critical_C_Predicted)
Critical_C_logRT_estimated

# calculate standard error for plotting error bars
Critical_C$SE <- sd(Critical_C$logRT, na.rm = TRUE) / sqrt(nrow(Critical_C))
new_row_residuals <- data.frame(Region = 'Critical', Condition = "C", Residual = Residual_Critical_C, SE = Critical_C$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Critical', Condition = "C", Estimated_logRT = Critical_C_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)





##### SPILLOVER #####
# Spillover region 
Spillover <- subset(GP6, Region == "Spillover")
# standardise predictors
Spillover$scaled_Plaus_Spillover <- scale(Spillover$SPR_Plaus_Rating)
Spillover$scaled_Surprisaldist_Spillover <- scale(Spillover$Surprisal_distractor)
# invert predictor plausibility
Spillover$inverted_scaled_Plaus_Spillover <- (Spillover$scaled_Plaus_Spillover) * (-1)
#log transform reading times
Spillover$logRT_Spillover <- log(Spillover$ReadingTime)

# define and run the linear mixed-effects regression model for the Spillover region 
model_Spillover <- lmer(logRT_Spillover ~ inverted_scaled_Plaus_Spillover + scaled_Surprisaldist_Spillover + 
                         (1 + inverted_scaled_Plaus_Spillover + scaled_Surprisaldist_Spillover | Subject) + 
                         (1 + inverted_scaled_Plaus_Spillover + scaled_Surprisaldist_Spillover | Item), data = Spillover)

# print the summary of the model
summary_Spillover <- summary(model_Spillover)
summary_Spillover



#####predict condition A, Spillover#####
Spillover_A <- subset(Spillover, Condition == "A")
Spillover_A$Spillover_A_Predicted <- predict(model_Spillover, newdata = Spillover_A,  type = "response")

# calculate residuals
Residual_Spillover_A <- mean(Spillover_A$logRT_Spillover) - mean(Spillover_A$Spillover_A_Predicted)
Residual_Spillover_A
Spillover_A_logRT_observed <- mean(Spillover_A$logRT_Spillover)
Spillover_A_logRT_observed 
Spillover_A_logRT_estimated <- mean(Spillover_A$Spillover_A_Predicted)
Spillover_A_logRT_estimated

# calculate standard error for plotting error bars
Spillover_A$SE <- sd(Spillover_A$logRT, na.rm = TRUE) / sqrt(nrow(Spillover_A))
new_row_residuals <- data.frame(Region = 'Spillover', Condition = "A", Residual = Residual_Spillover_A, SE = Spillover_A$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Spillover', Condition = "A", Estimated_logRT = Spillover_A_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



#####predict condition B, Spillover#####
Spillover_B <- subset(Spillover, Condition == "B")
Spillover_B$Spillover_B_Predicted <- predict(model_Spillover, newdata = Spillover_B,  type = "response")

# calculate residuals
Residual_Spillover_B <- mean(Spillover_B$logRT_Spillover) - mean(Spillover_B$Spillover_B_Predicted)
Residual_Spillover_B
Spillover_B_logRT_observed <- mean(Spillover_B$logRT_Spillover)
Spillover_B_logRT_observed
Spillover_B_logRT_estimated <- mean(Spillover_B$Spillover_B_Predicted)
Spillover_B_logRT_estimated

# calculate standard error for plotting error bars
Spillover_B$SE <- sd(Spillover_B$logRT, na.rm = TRUE) / sqrt(nrow(Spillover_B))
new_row_residuals <- data.frame(Region = 'Spillover', Condition = "B", Residual = Residual_Spillover_B, SE = Spillover_B$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Spillover', Condition = "B", Estimated_logRT = Spillover_B_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



#####predict condition C, Spillover#####
Spillover_C <- subset(Spillover, Condition == "C")
Spillover_C$Spillover_C_Predicted <- predict(model_Spillover, newdata = Spillover_C,  type = "response")

# calculate residuals
Residual_Spillover_C <- mean(Spillover_C$logRT_Spillover) - mean(Spillover_C$Spillover_C_Predicted)
Residual_Spillover_C
Spillover_C_logRT_observed <- mean(Spillover_C$logRT_Spillover)
Spillover_C_logRT_observed
Spillover_C_logRT_estimated <- mean(Spillover_C$Spillover_C_Predicted)
Spillover_C_logRT_estimated

# calculate standard error for plotting error bars
Spillover_C$SE <- sd(Spillover_C$logRT, na.rm = TRUE) / sqrt(nrow(Spillover_C))
new_row_residuals <- data.frame(Region = 'Spillover', Condition = "C", Residual = Residual_Spillover_C, SE = Spillover_C$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Spillover', Condition = "C", Estimated_logRT = Spillover_C_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)





##### POST-SPILLOVER #####
# Post-spillover region 
Postspillover <- subset(GP6, Region == "Post-spillover")
# standardise predictors
Postspillover$scaled_Plaus_Postspillover <- scale(Postspillover$SPR_Plaus_Rating)
Postspillover$scaled_Surprisaldist_Postspillover <- scale(Postspillover$Surprisal_distractor)
# invert predictor plausibility
Postspillover$inverted_scaled_Plaus_Postspillover <- (Postspillover$scaled_Plaus_Postspillover) * (-1)
#log transform reading times
Postspillover$logRT_Postspillover <- log(Postspillover$ReadingTime)

# define and run the linear mixed-effects regression model for the Postspillover region 
model_Postspillover <- lmer(logRT_Postspillover ~ inverted_scaled_Plaus_Postspillover + scaled_Surprisaldist_Postspillover + 
                          (1 + inverted_scaled_Plaus_Postspillover + scaled_Surprisaldist_Postspillover | Subject) + 
                          (1 + inverted_scaled_Plaus_Postspillover + scaled_Surprisaldist_Postspillover | Item), data = Postspillover)

# print the summary of the model
summary_Postspillover <- summary(model_Postspillover)
summary_Postspillover

#####predict condition A, Postspillover#####
Postspillover_A <- subset(Postspillover, Condition == "A")
Postspillover_A$Postspillover_A_Predicted <- predict(model_Postspillover, newdata = Postspillover_A,  type = "response")

# calculate residuals
Residual_Postspillover_A <- mean(Postspillover_A$logRT_Postspillover) - mean(Postspillover_A$Postspillover_A_Predicted)
Residual_Postspillover_A
Postspillover_A_logRT_observed <- mean(Postspillover_A$logRT_Postspillover)
Postspillover_A_logRT_observed
Postspillover_A_logRT_estimated <- mean(Postspillover_A$Postspillover_A_Predicted)
Postspillover_A_logRT_estimated

# calculate standard error for plotting error bars
Postspillover_A$SE <- sd(Postspillover_A$logRT, na.rm = TRUE) / sqrt(nrow(Postspillover_A))
new_row_residuals <- data.frame(Region = 'Post-spillover', Condition = "A", Residual = Residual_Postspillover_A, SE = Postspillover_A$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Postspillover', Condition = "A", Estimated_logRT = Postspillover_A_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



#####predict condition B, Postspillover#####
Postspillover_B <- subset(Postspillover, Condition == "B")
Postspillover_B$Postspillover_B_Predicted <- predict(model_Postspillover, newdata = Postspillover_B,  type = "response")

# calculate residuals
Residual_Postspillover_B <- mean(Postspillover_B$logRT_Postspillover) - mean(Postspillover_B$Postspillover_B_Predicted)
Residual_Postspillover_B
Postspillover_B_logRT_observed <- mean(Postspillover_B$logRT_Postspillover) 
Postspillover_B_logRT_observed
Postspillover_B_logRT_estimated <- mean(Postspillover_B$Postspillover_B_Predicted)
Postspillover_B_logRT_estimated

# calculate standard error for plotting error bars
Postspillover_B$SE <- sd(Postspillover_B$logRT, na.rm = TRUE) / sqrt(nrow(Postspillover_B))
new_row_residuals <- data.frame(Region = 'Post-spillover', Condition = "B", Residual = Residual_Postspillover_B, SE = Postspillover_B$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Postspillover', Condition = "B", Estimated_logRT = Postspillover_B_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)



#####predict condition C, Postspillover#####
Postspillover_C <- subset(Postspillover, Condition == "C")
Postspillover_C$Postspillover_C_Predicted <- predict(model_Postspillover, newdata = Postspillover_C,  type = "response")

# calculate residuals
Residual_Postspillover_C <- mean(Postspillover_C$logRT_Postspillover) - mean(Postspillover_C$Postspillover_C_Predicted)
Residual_Postspillover_C
Postspillover_C_logRT_observed <- mean(Postspillover_C$logRT_Postspillover)
Postspillover_C_logRT_observed
Postspillover_C_logRT_estimated <- mean(Postspillover_C$Postspillover_C_Predicted)
Postspillover_C_logRT_estimated

# calculate standard error for plotting error bars
Postspillover_C$SE <- sd(Postspillover_C$logRT, na.rm = TRUE) / sqrt(nrow(Postspillover_C))
new_row_residuals <- data.frame(Region = 'Post-spillover', Condition = "C", Residual = Residual_Postspillover_C, SE = Postspillover_C$SE)
residuals <- rbind(residuals, new_row_residuals)

# calculate standard error for logRT estimated
# calculate here
new_row_logRT_estimated <- data.frame(Region = 'Postspillover', Condition = "C", Estimated_logRT = Postspillover_C_logRT_estimated)
logRT_estimated <- rbind(logRT_estimated, new_row_logRT_estimated)


# plot residuals
# Create a line plot 
p <- ggplot(residuals, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                          y = Residual, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (0.10, -0.10)
p <- p + theme_minimal() + geom_errorbar(aes(ymin=Residual-SE, ymax=Residual+SE), width=.1, size=0.3) 
p <- p + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p <- p + labs(x="Region", y="logRT", title = "Residuals: Plausibility Target + Surprisal Distractor") 
p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p 
ggsave("Residuals_Plot.pdf", p, width=4, height=4)


# plot estimated logRTs
# Create a line plot 
p <- ggplot(logRT_estimated, aes(x = factor(Region, levels = c("Pre-critical", "Critical", "Spillover", "Post-spillover")), 
                           y = Estimated_logRT, color = Condition, group = Condition)) + geom_point(shape = 4, size = 3.5, stroke = 0.4) + geom_line(linewidth=0.5) + ylim (5.5, 5.7)
#p <- p + theme_minimal() + geom_errorbar(aes(ymin=Estimated_logRT-SE, ymax=Estimated_logRT+SE), width=.1, size=0.3) 
p <- p + scale_color_manual(name="Condition", labels=c("A: Plausible", "B: Medium Plausible", "C: Implausible"), values=c("#000000", "#FF0000", "#0000FF"))
p <- p + labs(x="Region", y="logRT", title = "Estimated RTs") 
p <- p + theme(legend.position="bottom", legend.text=element_text(size=7), legend.title=element_text(size=7), axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14)) 
p 
ggsave("Estimated_RTs_Plot.pdf", p, width=4, height=4)


# manual predict
# intercept
#intercept_critical_A <- summary_Critical$coefficients[("(Intercept)"), "Estimate"]
#intercept_critical_A

# mean plausibility scaled inverted critical a
#critical_a <- subset(Critical, Condition == "A")
#plaus_crit_scaled_A <- scale(critical_a$SPR_Plaus_Rating)
#plaus_crit_scaled_inv_A <- (plaus_crit_scaled_A) * (-1)
#plaus_crit_scaled_inv_A_mean <- mean(plaus_crit_scaled_inv_A)
#plaus_crit_scaled_inv_A_mean

# coefficient plausibility
#coeff_plaus_critical_A <-summary_Critical$coefficients[("inverted_scaled_Plaus_Critical"), "Estimate"]
#coeff_plaus_critical_A

# mean surprisal dist critical a
#surprisaldist_crit_scaled_A <- scale(critical_a$Surprisal_distractor)
#surprisaldist_crit_scaled_A_mean <- mean(surprisaldist_crit_scaled_A)
#surprisaldist_crit_scaled_A_mean

# coefficient surprisaldist
#coeff_plaus_critical_A <-summary_Critical$coefficients[("scaled_Surprisaldist_Critical"), "Estimate"]
#coeff_plaus_critical_A

#predict mean of condition A in region critical
#predict_critical_A <- intercept_critical_A + (plaus_crit_scaled_inv_A_mean * coeff_plaus_critical_A) + (surprisaldist_crit_scaled_A_mean * coeff_plaus_critical_A)
#predict_critical_A