############rbase plot##########

# Colors for groups
cols <- colorRampPalette(c("darkolivegreen3", "skyblue3"))

# Create grouped barplot
bp <- barplot(
  height = tapply(MastersGaby$mean,
                  list(MastersGaby$nivel,
                       MastersGaby$tempo),
                  mean),
  beside = TRUE,
  col = cols (6),
  ylim = c(0, max(MastersGaby$mean + MastersGaby$SD) + 10),
  ylab = "Mean ± SD",
  main = "Barplot with Error Bars by Site and Time",
  legend.text = rownames(tapply(MastersGaby$mean, list(MastersGaby$nivel, MastersGaby$tempo), mean)),
  args.legend = list(x = "topright", bty = "n")
)

# Add error bars
arrows(
  x0 = bp,
  y0 = MastersGaby$mean,
  x1 = bp,
  y1 = MastersGaby$mean + MastersGaby$SD,
  angle = 90,
  code = 2,
  length = 0.05
)
arrows(
  x0 = bp,
  y0 = MastersGaby$mean,
  x1 = bp,
  y1 = MastersGaby$mean - MastersGaby$SD,
  angle = 90,
  code = 2,
  length = 0.05
)



######ggplot #######

library(ggplot2)

ggplot(MastersGaby_folha, aes(x = tempo, y = mean, fill = nivel)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Tempo",
    y = "Perda (%)",
    title = "Perda por Loca e Tempo (folha)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")+
  ggsave(filename = "plot_folhas.jpg")



library(ggplot2)

ggplot(MastersGaby_galho, aes(x = tempo, y = mean, fill = nivel)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean - SD, ymax = mean + SD),
                width = 0.2, position = position_dodge(0.9)) +
  labs(
    x = "Tempo",
    y = "Perda %",
    title = "Perda por Loca e Tempo (galho)"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  ggsave(filename = "plot_gabys_galho.jpg")


