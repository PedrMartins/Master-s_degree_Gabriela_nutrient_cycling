library(readxl)
library(ggplot2)
Mestrado_Gabriela_Litter_Bags <- read_excel("Mestrado Gabriela G. de Matos - Litter Bags.xlsx")
Mestrado_Gabriela_Litter_Bags <- Mestrado_Gabriela_Litter_Bags [,-c(9:11)]
Mestrado_Gabriela_Litter_Bags <- na.omit(Mestrado_Gabriela_Litter_Bags)

names (Mestrado_Gabriela_Litter_Bags) [c(4:7)] <- c ("galho_g", "folha_g",
                                                     "perda_galho_percentage",
                                                     "perda_folha_percentage")

plot (tempo ~galho_g, data =Mestrado_Gabriela_Litter_Bags)

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

 anova (aov (galho_g ~ cor*local,
             data = Mestrado_Gabriela_Litter_Bags)
 )
 anova (aov (folha_g ~ cor*local,
             data = Mestrado_Gabriela_Litter_Bags)
 )


is.na (Mestrado_Gabriela_Litter_Bags$galho_g )



 par  (mfrow = c(1,2))
 boxplot (folha_g ~tempo
          , data = Mestrado_Gabriela_Litter_Bags)
 boxplot (galho_g ~tempo
          , data = Mestrado_Gabriela_Litter_Bags)

