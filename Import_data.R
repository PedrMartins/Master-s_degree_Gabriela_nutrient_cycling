library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library (multcompView)
source("function.r")

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
Mestrado_Gabriela_Litter_Bags_folha_s_outlier_clean <- Mestrado_Gabriela_Litter_Bags_folha_s_outlier [, -c(4,6,7)]

out_galho=boxplot.stats(Mestrado_Gabriela_Litter_Bags$galho_g)
out_galho <- c (out_galho$out)
Mestrado_Gabriela_Litter_Bags_galho_s_outlier <- Mestrado_Gabriela_Litter_Bags[
  !Mestrado_Gabriela_Litter_Bags$galho_g %in% out_galho,
]
Mestrado_Gabriela_Litter_Bags_galho_s_outlier_clean <- Mestrado_Gabriela_Litter_Bags_galho_s_outlier [, -c(5:7)]


############Tidy tabela##########


##galho
taxa_degrad_galho <- pivot_wider(Mestrado_Gabriela_Litter_Bags_galho_s_outlier_clean,
                                 names_from = c(tempo),
                                 values_from =  galho_g)
taxa_degrad_galho$T0 <- rep(10, length(taxa_degrad_galho$local))

taxa_degrad_galho <- taxa_degrad_galho|>
  relocate( T0, .before = T1)

taxa_degrad_galho$taxt0_t1 <- taxa_degrad_galho$T0 - taxa_degrad_galho$T1
taxa_degrad_galho$taxt1_t2 <- taxa_degrad_galho$T1 - taxa_degrad_galho$T2
taxa_degrad_galho$taxt2_t3 <- taxa_degrad_galho$T2 - taxa_degrad_galho$T3


## folha
taxa_degrad_folha <- pivot_wider(Mestrado_Gabriela_Litter_Bags_folha_s_outlier_clean,
                                 names_from = c ( tempo),
                                 values_from =  folha_g)

taxa_degrad_folha$T0 <- rep(5, length(taxa_degrad_folha$local))

taxa_degrad_folha <- taxa_degrad_folha|>
  relocate( T0, .before = T1)

taxa_degrad_folha$taxt0_t1 <- taxa_degrad_folha$T0 - taxa_degrad_folha$T1
taxa_degrad_folha$taxt1_t2 <- taxa_degrad_folha$T1 - taxa_degrad_folha$T2
taxa_degrad_folha$taxt2_t3 <- taxa_degrad_folha$T2 - taxa_degrad_folha$T3

colnames (taxa_degrad_folha) [8:10] <-  c("t0_t1"
                                    ,"t1_t2",
                                    "t2_t3")

taxa_degrad_folha <- taxa_degrad_folha [,c(1:3,8:10)] |>
  pivot_longer(c (t0_t1
                  ,t1_t2,
                  t2_t3),names_to = "tempo",
               values_to = "perda")


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




######anova one way######

 anova (aov (galho_g ~ cor,
             data = Mestrado_Gabriela_Litter_Bags)
        )
 anova (aov (folha_g ~ cor,
             data = Mestrado_Gabriela_Litter_Bags)
        )


 anova (aov (galho_g ~ local,
             data = Mestrado_Gabriela_Litter_Bags)
        )

 anova (aov (folha_g ~ local,data = Mestrado_Gabriela_Litter_Bags)
        )

 anova (aov (galho_g ~ tempo,
             data = Mestrado_Gabriela_Litter_Bags)
 )

 anova (aov (folha_g ~ tempo,data = Mestrado_Gabriela_Litter_Bags)
 )

######anova two way local cor####


 anova (aov (galho_g ~ tempo*cor,
             data = Mestrado_Gabriela_Litter_Bags_galho_s_outlier_clean)
 )



  anova_fola_cor_tempo <- aov (perda ~  cor*tempo*local,
             data = taxa_degrad_folha)

  tukey_fola_cor_tempo <- TukeyHSD(aov (perda ~ cor*tempo*local,
                                        data = taxa_degrad_folha))

  letters <- multcompLetters4(anova_fola_cor_tempo, tukey_fola_cor_tempo)

  letters <- as.data.frame.list(letters$`cor:local`)

  letters$cor_tempo <- rownames(letters)

  colnames(letters) [c(1:2,5)] <- c("group", "tempo", "loc_temp")


 anova (aov (folha_g ~ cor*local,
             data = Mestrado_Gabriela_Litter_Bags_galho_s_outlier_T2)
 )

 anova (aov (folha_g ~ cor*local,
             data = Mestrado_Gabriela_Litter_Bags_galho_s_outlier_T3)
 )

#arrumar!
############anova two way local tempo#############


 anova (aov (galho_g ~ local*tempo,
             data = Mestrado_Gabriela_Litter_Bags_galho_s_outlier)
 )


 anova (aov (folha_g ~ local*tempo,
             data = Mestrado_Gabriela_Litter_Bags_folha_s_outlier)
 )


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




