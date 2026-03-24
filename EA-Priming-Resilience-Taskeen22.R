# Title: Wheat Stress Resilience & Priming Analysis
# Author: Taskeen22
# Purpose: Comprehensive 2-way ANOVA, Post-hoc Analysis, and Visualizations
# -------------------------------------------------------------------------

# 1. SETUP & LIBRARIES
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

# Load the data - Ensure your file is named 'wheat_data.csv'
df <- read.csv("wheat_data.csv")
df$Stress <- as.factor(df$Stress)
df$Priming <- as.factor(df$Priming)

# 2. DESCRIPTIVE STATISTICS (Mean, SD, SEM)
# Calculating SAM (Standard Error of Mean) for all metrics
stats_summary <- df %>%
  group_by(Stress, Priming) %>%
  summarise(across(where(is.numeric), 
                   list(Mean = ~mean(.x, na.rm = TRUE), 
                        SD = ~sd(.x, na.rm = TRUE),
                        SEM = ~sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"),
            .groups = "drop")

write.csv(stats_summary, "Wheat_Summary_Statistics.csv", row.names = FALSE)

# 3. STATISTICAL ANALYSIS (2-Way ANOVA)
# Loop through all metrics to screen for significance
metrics <- c("Height", "RootL", "SpikeL", "AwnL", "Spikelets", "Biomass", "Yield", "Grains")

sink("Full_Statistical_Report.txt")
cat("2-WAY ANOVA AND TUKEY HSD REPORT\n\n")

for (m in metrics) {
  cat("\n--- ANALYSIS FOR:", m, "---\n")
  formula_str <- as.formula(paste(m, "~ Stress * Priming"))
  fit <- aov(formula_str, data = df)
  print(summary(fit))
  
  # Only run Tukey for significant interactions (The "Gold" Variables)
  if (summary(fit)[[1]][["Pr(>F)"]][3] < 0.05) {
    cat("\nSIGNIFICANT INTERACTION FOUND. RUNNING TUKEY HSD:\n")
    print(TukeyHSD(fit))
  }
}
sink()

# 4. VISUALIZATIONS (The Figures)

# A. Grouped Bar Chart with SEM (for RootL)
plot_data <- stats_summary %>% select(Stress, Priming, RootL_Mean, RootL_SEM)
p1 <- ggplot(plot_data, aes(x=Stress, y=RootL_Mean, fill=Priming)) +
  geom_bar(stat="identity", position=position_dodge(0.9), color="black") +
  geom_errorbar(aes(ymin=RootL_Mean-RootL_SEM, ymax=RootL_Mean+RootL_SEM), 
                position=position_dodge(0.9), width=0.2) +
  theme_minimal() + labs(title="Root Length Response", y="Mean Root Length ± SEM")

ggsave("Figure_Bar_RootL.png", plot=p1, width=7, height=5)

# B. Interaction Plot (Visualizing the 'Why')
png("Figure_Interaction_RootL.png", width=800, height=600)
interaction.plot(df$Priming, df$Stress, df$RootL, col=1:4, lty=1, lwd=2,
                 main="Interaction: Stress vs Priming on RootL")
dev.off()

# C. Boxplot (Integrity Check)
p2 <- ggplot(df, aes(x=Stress, y=RootL, fill=Priming)) +
  geom_boxplot() + geom_jitter(alpha=0.3) + theme_bw()
ggsave("Figure_Boxplot_RootL.png", plot=p2, width=7, height=5)

# D. Correlation Heatmap
num_data <- df %>% select(where(is.numeric)) %>% select(-matches("Rep"))
png("Figure_Correlation_Heatmap.png", width=800, height=800)
corrplot(cor(num_data, use="complete.obs"), method="color", addCoef.col="black")
dev.off()

cat("Analysis complete. All reports and figures saved.")

