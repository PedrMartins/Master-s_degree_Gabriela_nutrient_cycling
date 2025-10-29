
######ggplot #######

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

######boxplot ########

boxplot(perda_folha_percentage ~ local * tempo, data = Mestrado_Gabriela_Litter_Bags_folha_s_outlier,
        main = "Biomass by Site and Time (ANOVA)",
        xlab = "Group (Site x Time)",
        ylab = "Perda folha (%)")

# Add mean points
means <- tapply(Mestrado_Gabriela_Litter_Bags_folha_s_outlier$perda_folha_percentage,
                interaction(Mestrado_Gabriela_Litter_Bags_folha_s_outlier$local,
                            Mestrado_Gabriela_Litter_Bags_folha_s_outlier$tempo),
                mean)
points(1:length(means), means, pch = 19, col = "red")

######Anova plot########

##folha

summary_df <- Mestrado_Gabriela_Litter_Bags_folha_s_outlier %>%
  group_by(local, tempo) %>%
  summarise(mean = mean(perda_folha_percentage),
            se = sd(perda_folha_percentage) / sqrt(n()), .groups = "drop")


ggplot(summary_df, aes(x = tempo, y = mean, fill = local)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "ANOVA: Perda de massa (%) \n por local e tempo \n Folha",
       x = "Tempo",
       y = "Média da porcentagem \n de perda de massa ± SE") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  ggsave(filename = "plot_gabys_folha_anova.jpg")

##galho

summary_df <- Mestrado_Gabriela_Litter_Bags_galho_s_outlier %>%
  group_by(local, tempo) %>%
  summarise(mean = mean(perda_galho_percentage),
              se = sd(perda_galho_percentage) / sqrt(n()), .groups = "drop")


ggplot(summary_df, aes(x = tempo, y = mean, fill = local)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.9), width = 0.2) +
  labs(title = "ANOVA: Perda de massa (%)\n por local e tempo \n Galho",
       x = "Tempo",
       y = "Média da porcentagem \n de perda de massa ± SE") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  ggsave(filename = "plot_gabys_galho_anova.jpg")


