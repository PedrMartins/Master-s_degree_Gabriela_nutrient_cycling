library(readxl)
library(ggplot2)
library(dplyr)
source("function.r")
Mestrado_Gabriela_Litter_Bags <- read_excel("Mestrado Gabriela G. de Matos - Litter Bags.xlsx")
Mestrado_Gabriela_Litter_Bags <- Mestrado_Gabriela_Litter_Bags [,-c(9:11)]
Mestrado_Gabriela_Litter_Bags <- na.omit(Mestrado_Gabriela_Litter_Bags)

names (Mestrado_Gabriela_Litter_Bags) [c(4:7)] <- c ("galho_g", "folha_g",
                                                     "perda_galho_percentage",
                                                     "perda_folha_percentage")

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


 anova (aov (galho_g ~ local*cor,
             data = Mestrado_Gabriela_Litter_Bags_galho_s_outlier)
 )

 anova (aov (folha_g ~ cor*local,
             data = Mestrado_Gabriela_Litter_Bags_folha_s_outlier)
 )

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




