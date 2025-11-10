source ("load.R")

Mestrado_Gabriela_Litter_Bags <- read_excel("Mestrado Gabriela G. de Matos - Litter Bags.xlsx")
Mestrado_Gabriela_Litter_Bags <- Mestrado_Gabriela_Litter_Bags [,-c(9:11)]
Mestrado_Gabriela_Litter_Bags <- na.omit(Mestrado_Gabriela_Litter_Bags)

names (Mestrado_Gabriela_Litter_Bags) [c(4:7)] <- c ("galho_g", "folha_g",
                                                     "perda_galho_percentage",
                                                     "perda_folha_percentage")

############retirar outlier##########

out_folha=boxplot.stats(Mestrado_Gabriela_Litter_Bags$folha_g)
out_folha <- c (out_folha$out)
Mestrado_Gabriela_Litter_Bags_folha_s_outlier <- Mestrado_Gabriela_Litter_Bags[
  !Mestrado_Gabriela_Litter_Bags$folha_g %in% out_folha,
]


out_galho=boxplot.stats(Mestrado_Gabriela_Litter_Bags$galho_g)
out_galho <- c (out_galho$out)
Mestrado_Gabriela_Litter_Bags_galho_s_outlier <- Mestrado_Gabriela_Litter_Bags[
  !Mestrado_Gabriela_Litter_Bags$galho_g %in% out_galho,
]



############Tidy tabela##########


####galho####
taxa_degrad_galho <- pivot_wider(Mestrado_Gabriela_Litter_Bags_galho_s_outlier_clean,
                                 names_from = c(tempo),
                                 values_from =  galho_g)
taxa_degrad_galho$T0 <- rep(10, length(taxa_degrad_galho$local))

taxa_degrad_galho <- taxa_degrad_galho|>
  relocate( T0, .before = T1)

taxa_degrad_galho$taxt0_t1 <- taxa_degrad_galho$T0 - taxa_degrad_galho$T1
taxa_degrad_galho$taxt1_t2 <- taxa_degrad_galho$T1 - taxa_degrad_galho$T2
taxa_degrad_galho$taxt2_t3 <- taxa_degrad_galho$T2 - taxa_degrad_galho$T3


summarised_tax_t0_t1<- taxa_degrad_galho %>%
  group_by(local, cor) %>%
  summarise(mean = mean(taxt0_t1, na.rm = TRUE),
            sd = sd(taxt0_t1,  na.rm = TRUE),
            se = sd(taxt1_t2,  na.rm = TRUE)/sqrt( n()),
            .groups = "drop")

summarised_tax_t1_t2<- taxa_degrad_galho %>%
  group_by(local, cor) %>%
  summarise(mean = mean(taxt1_t2, na.rm = TRUE),
            sd = sd(taxt1_t2,  na.rm = TRUE),
            se = sd(taxt1_t2,  na.rm = TRUE)/sqrt( n()),
            .groups = "drop")

summarised_tax_t2_t3<- taxa_degrad_galho %>%
  group_by(local, cor) %>%
  summarise(mean = mean(taxt2_t3, na.rm = TRUE),
            sd = sd(taxt2_t3,  na.rm = TRUE),
            se = sd(taxt1_t2,  na.rm = TRUE)/sqrt( n()),
            .groups = "drop")

all_tax <- full_join(summarised_tax_t0_t1, summarised_tax_t1_t2,
                     join_by(local, cor))

all_tax <- full_join(all_tax, summarised_tax_t2_t3,
                     join_by(local, cor))

all_tax <- all_tax|>
  relocate( c(sd.x,sd.y,sd), .after = mean) |>
  relocate (c(se.x,se.y,se), .after=sd)

names (all_tax) [3:11] <- c("t0_t1"
                            ,"t1_t2",
                            "t2_t3",
                            "t0_t1",
                            "t1_t2",
                            "t2_t3",
                            "t0_t1"
                            ,"t1_t2",
                            "t2_t3")


colnames (taxa_degrad_galho) [8:10] <-  c("t0_t1"
                                          ,"t1_t2",
                                          "t2_t3")

taxa_degrad_galho <- taxa_degrad_galho [,c(1:3,8:10)] |>
  pivot_longer(c (t0_t1
                  ,t1_t2,
                  t2_t3),names_to = "tempo",
               values_to = "perda")



##


##### folha#####
taxa_degrad_folha <- pivot_wider(Mestrado_Gabriela_Litter_Bags_folha_s_outlier_clean,
                                 names_from = c ( tempo),
                                 values_from =  folha_g)

taxa_degrad_folha$T0 <- rep(5, length(taxa_degrad_folha$local))

taxa_degrad_folha <- taxa_degrad_folha|>
  relocate( T0, .before = T1)

taxa_degrad_folha$taxt0_t1 <- taxa_degrad_folha$T0 - taxa_degrad_folha$T1
taxa_degrad_folha$taxt1_t2 <- taxa_degrad_folha$T1 - taxa_degrad_folha$T2
taxa_degrad_folha$taxt2_t3 <- taxa_degrad_folha$T2 - taxa_degrad_folha$T3


summarised_tax_t0_t1<- taxa_degrad_folha %>%
  group_by(local, cor) %>%
  summarise(mean = mean(taxt0_t1, na.rm = TRUE),
            sd = sd(taxt0_t1,  na.rm = TRUE),
            se = sd(taxt1_t2,  na.rm = TRUE)/sqrt( n()),
            .groups = "drop")

summarised_tax_t1_t2<- taxa_degrad_folha %>%
  group_by(local, cor) %>%
  summarise(mean = mean(taxt1_t2, na.rm = TRUE),
            sd = sd(taxt1_t2,  na.rm = TRUE),
            se = sd(taxt1_t2,  na.rm = TRUE)/sqrt( n()),
            .groups = "drop")

summarised_tax_t2_t3<- taxa_degrad_folha %>%
  group_by(local, cor) %>%
  summarise(mean = mean(taxt2_t3, na.rm = TRUE),
            sd = sd(taxt2_t3,  na.rm = TRUE),
            se = sd(taxt1_t2,  na.rm = TRUE)/sqrt( n()),
            .groups = "drop")

all_tax_folha <- full_join(summarised_tax_t0_t1, summarised_tax_t1_t2,
  join_by(local, cor))

all_tax_folha <- full_join(all_tax_folha, summarised_tax_t2_t3,
                     join_by(local, cor))

all_tax_folha <- all_tax_folha|>
  relocate( c(sd.x,sd.y,sd), .after = mean) |>
  relocate (c(se.x,se.y,se), .after=sd)

names (all_tax_folha) [3:11] <- c("t0_t1"
                         ,"t1_t2",
                         "t2_t3",
                         "t0_t1",
                         "t1_t2",
                         "t2_t3",
                         "t0_t1"
                         ,"t1_t2",
                         "t2_t3")

colnames (taxa_degrad_folha) [8:10] <-  c("t0_t1"
                                          ,"t1_t2",
                                          "t2_t3")

taxa_degrad_folha <- taxa_degrad_folha [,c(1:3,8:10)] |>
  pivot_longer(c (t0_t1
                  ,t1_t2,
                  t2_t3),names_to = "tempo",
               values_to = "perda")

# Mestrado_Gabriela_Litter_Bags_galho_s_outlier_T1 <- Mestrado_Gabriela_Litter_Bags_galho_s_outlier[
#   Mestrado_Gabriela_Litter_Bags_galho_s_outlier$tempo == "T1",
# ]
#
# Mestrado_Gabriela_Litter_Bags_galho_s_outlier_T2 <- Mestrado_Gabriela_Litter_Bags_galho_s_outlier[
#   Mestrado_Gabriela_Litter_Bags_galho_s_outlier$tempo == "T2",
# ]
#
# Mestrado_Gabriela_Litter_Bags_galho_s_outlier_T3 <- Mestrado_Gabriela_Litter_Bags_galho_s_outlier[
#   Mestrado_Gabriela_Litter_Bags_galho_s_outlier$tempo == "T3",
# ]


MastersGaby_folha <- stats_bag(Mestrado_Gabriela_Litter_Bags_folha_s_outlier
                         )
MastersGaby_galho <- stats_bag(Mestrado_Gabriela_Litter_Bags_galho_s_outlier,
                              part = "galho")





######anova two way local cor####

####galho####
#####by_taxa t0_t1 t1_t2 ...#####
 anova_galho_cor_tempo <- aov (perda ~ local*cor,
             data = taxa_degrad_galho)


 tukey_galho_cor_tempo <- TukeyHSD(aov (perda ~ local*cor,
                                       data = taxa_degrad_galho))

 letters_galho <- multcompLetters4(anova_galho_cor_tempo,
                             tukey_galho_cor_tempo)

 letters_galho <- as.data.frame.list(letters_galho$`local:cor`)

 letters_galho$local_cor <- rownames(letters_galho)

 letters_galho <-  letters_galho [,-c(3:6)]

 colnames(letters_galho)  <- c("group", "tempo", "local_cor")

 #####by_taxa t0_tx ...#####

 anova_galho_cor_tempo <- aov (perda_galho_percentage  ~ (local+cor)*tempo,
                               data = Mestrado_Gabriela_Litter_Bags_galho_s_outlier)



 tukey_galho_cor_tempo <- TukeyHSD(aov (perda_galho_percentage ~ (local+cor)*tempo,
                                        data = Mestrado_Gabriela_Litter_Bags_galho_s_outlier))

 letters_galho <- multcompLetters4(anova_galho_cor_tempo,
                                   tukey_galho_cor_tempo)

 letters_galho <- as.data.frame.list(letters_galho$`local:cor`)

 letters_galho$local_cor <- rownames(letters_galho)

 letters_galho <-  letters_galho [,-c(3:6)]

 colnames(letters_galho)  <- c("group", "tempo", "local_cor")

#####folha #####
 #####by_taxa t0_t1 t1_t2 ...#####

  anova_folha_cor_tempo <- aov (perda ~  cor*tempo,
             data = taxa_degrad_folha)

  tukey_folha_cor_tempo <- TukeyHSD(aov (perda ~ cor*tempo,
                                        data = taxa_degrad_folha))

  letters <- multcompLetters4(anova_folha_cor_tempo,
                              tukey_folha_cor_tempo)

  letters <- as.data.frame.list(letters$`cor:tempo`)

  letters$cor_tempo <- rownames(letters)

  colnames(letters)  <- c("group", "tempo", "cor_temp")

  #####by_taxa t0_tx ...#####

  anova_folha_cor_tempo <- aov (perda_galho_percentage  ~ (local+cor)*tempo,
                                data = Mestrado_Gabriela_Litter_Bags_folha_s_outlier)



  tukey_folha_cor_tempo <- TukeyHSD(aov (perda_galho_percentage ~ (local+cor)*tempo,
                                         data = Mestrado_Gabriela_Litter_Bags_folha_s_outlier))

  letters_folha <- multcompLetters4(anova_folha_cor_tempo,
                                    tukey_folha_cor_tempo)

  letters_folha <- as.data.frame.list(letters_folha$`local:cor`)

  letters_folha$local_cor <- rownames(letters_folha)

  letters_folha <-  letters_folha [,-c(3:6)]

  colnames(letters_folha)  <- c("group", "tempo", "local_cor")




#########################

 cores <-  c(rep ("blue",3),
             rep ("pink",3),
             rep ("green",3))

 par (mfrow = c(1,2), bty = "l")
 boxplot (galho_g ~ tempo + cor,
          data =Mestrado_Gabriela_Litter_Bags,
          col = cores, pch = "*", xaxt = "n")

 boxplot (folha_g ~ tempo + cor,
          data =Mestrado_Gabriela_Litter_Bags,
          col = cores, pch = "*", xaxt = "n")

dev.off ()


par (mfrow = c(1,2), bty = "l")

boxplot (galho_g ~ local + tempo,
         data =Mestrado_Gabriela_Litter_Bags
         , pch = "*")

boxplot (folha_g ~ local + tempo,
         data =Mestrado_Gabriela_Litter_Bags_folha_s_outlier,
          pch = "*")
range()

dev.off ()




