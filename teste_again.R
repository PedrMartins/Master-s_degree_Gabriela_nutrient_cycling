boxplot(biomass ~ site * tempo, data = df,
        col = c("lightblue", "lightgreen"),
        main = "Biomass by Site and Time (ANOVA)",
        xlab = "Group (Site x Time)",
        ylab = "Biomass (g)")

# Add mean points
means <- tapply(df$biomass, interaction(df$site, df$tempo), mean)
points(1:length(means), means, pch = 19, col = "red")



library(dplyr)
summary_df <- df %>%
  group_by(site, tempo) %>%
  summarise(mean = mean(biomass), se = sd(biomass) / sqrt(n()), .groups = "drop")

# Make grouped barplot
bar_centers <- barplot(
  height = tapply(summary_df$mean, list(summary_df$site, summary_df$tempo), mean),
  beside = TRUE, col = c("skyblue", "lightgreen"),
  ylim = c(0, max(summary_df$mean + summary_df$se) + 5),
  ylab = "Mean ± SE", main = "ANOVA Group Means"
)

# Add error bars
arrows(bar_centers, summary_df$mean - summary_df$se,
       bar_centers, summary_df$mean + summary_df$se,
       angle = 90, code = 3, length = 0.05)





library(ggplot2)
library(dplyr)

summary_df <- df %>%
  group_by(site, tempo) %>%
  summarise(mean = mean(biomass), se = sd(biomass) / sqrt(n()), .groups = "drop")

ggplot(summary_df, aes(x = tempo, y = mean, fill = site)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = 0.2) +
  geom_text(aes(label = round(mean, 1)),
            position = position_dodge(0.9), vjust = -0.5, size = 3.5) +
  labs(title = "ANOVA: Biomass by Site and Time",
       x = "Time (tempo)",
       y = "Mean Biomass ± SE") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")



TukeyHSD(anova_model)
plot(TukeyHSD(anova_model))
