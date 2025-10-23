############rbase plot##########

# Colors for groups
cols <- c("darkolivegreen3", "skyblue3")

# Create grouped barplot
bp <- barplot(
  height = tapply(df$mean, list(df$nivel, df$tempo), mean),
  beside = TRUE,
  col = cols,
  ylim = c(0, max(df$mean + df$SD) + 10),
  ylab = "Mean ± SD",
  main = "Barplot with Error Bars by Site and Time",
  legend.text = rownames(tapply(df$mean, list(df$nivel, df$tempo), mean)),
  args.legend = list(x = "topright", bty = "n")
)

# Add error bars
arrows(
  x0 = bp,
  y0 = df$mean,
  x1 = bp,
  y1 = df$mean + df$SD,
  angle = 90,
  code = 2,
  length = 0.05
)
arrows(
  x0 = bp,
  y0 = df$mean,
  x1 = bp,
  y1 = df$mean - df$SD,
  angle = 90,
  code = 2,
  length = 0.05
)



######ggplot #######

library(ggplot2)

ggplot(df, aes(x = tempo, y = mean, fill = nivel)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Time (tempo)",
    y = "Mean ± SD",
    title = "Mean Biomass by Site and Time"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


