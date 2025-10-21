color <- c (rep("darkblue",3),
            rep("blue",3),
            rep("lightblue",3),
            rep("darkred",3),
            rep("red",3),
            rep("pink",3))

boxplot(galho_g ~  tempo +local,
        data =Mestrado_Gabriela_Litter_Bags_galho_s_outlier,
        col = color, pch ="*")

boxplot(folha_g ~  tempo +local,
        data =Mestrado_Gabriela_Litter_Bags_folha_s_outlier,
        col = color, pch ="*")

cores <-  c(rep ("blue",3),
            rep ("pink",3),
            rep ("green",3))

boxplot(galho_g ~  tempo +cor,
        data =Mestrado_Gabriela_Litter_Bags_galho_s_outlier,
        col = cores, pch ="*")

boxplot(folha_g ~  tempo +cor,
        data =Mestrado_Gabriela_Litter_Bags_folha_s_outlier,
        col = cores, pch = "*")
