# chargement des librairies utiles

library(FactoMineR)
library(factoextra)
library(corrplot)
library(ggplot2)
library(psych)
library(skimr)
library(PCDimension)

# importation du jeu de données 

morts <- read.csv("cleaned_data.csv", header = TRUE, sep = ",")


# visualisation des données

View(morts)



# Déclaration de la colonne Country comme identificateur des individus

rownames(morts) <- morts[,1]

# Suppression de la variable country

morts <- morts[,-1]

# Vérification du jeu de données

head(morts)

# Représentation par un diagramme en baton des nombres de décès par cause dans quelques pays (Arménie, France, Mali)


par(mar = c(6,10,4,2))
barplot(
  as.numeric(morts["ARM", ]),
  names.arg = colnames(morts),
  col = "purple3",
  border = "white",
  main = "Arménie",
  horiz = TRUE,
  las = 1,
  xlab = "Nombre de morts"
)


par(mar = c(6,10,4,2))
barplot(
  as.numeric(morts["MLI", ]),
  names.arg = colnames(morts),
  col = "green",
  border = "white",
  main = "Mali",
  horiz = TRUE,
  las = 1,
  xlab = "Nombre de morts"
)

par(mar = c(6,10,4,2))
barplot(
  as.numeric(morts["FRA", ]),
  names.arg = colnames(morts),
  col = "blue",
  border = "white",
  main = "France",
  horiz = TRUE,
  las = 1,
  xlab = "Nombre de morts"
)

# Tableau profil ligne



morts1 <- data.matrix(morts)

profils_lignes <- morts1 / rowSums(morts1)

profils_lignes <- as.data.frame(profils_lignes)

View(profils_lignes)

rowSums(profils_lignes)



# Tableau profil colonne

# Passage en matrice numérique
morts1 <- data.matrix(morts)

# Somme des colonnes
somme_colonnes <- colSums(morts1)

# Profils colonnes
profils_colonnes <- sweep(morts1, 2, somme_colonnes, "/")

# Conversion en data.frame (optionnel)
profils_colonnes <- as.data.frame(profils_colonnes)

# Vérification : chaque colonne doit sommer à 1
colSums(profils_colonnes)
View(profils_colonnes)


# Représentation graphique des profils



library(ggplot2)



# Extraction du profil ligne de la France
profil_france <- profils_lignes["FRA", , drop = FALSE]

# Transformation en data.frame “long” simple
profil_france_long <- data.frame(
  Variable = colnames(profil_france),
  Profil = as.numeric(profil_france[1, ])
)



# Diagramme (barplot)
ggplot(profil_france_long, aes(x = Variable, y = Profil)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Profil ligne de la France (FRA)",
    x = "Colonnes",
    y = "Proportion"
  ) +
  theme_minimal()

#comparaison de deux profils ligne

library(ggplot2)

# Sélection des deux profils ligne
profils_deux <- profils_lignes[c("FRA", "MLI"), , drop = FALSE]

# Transformation en data.frame “long” simple
profils_deux_long <- data.frame(
  Pays = rep(rownames(profils_deux), each = ncol(profils_deux)),
  Variable = rep(colnames(profils_deux), times = nrow(profils_deux)),
  Profil = as.numeric(t(profils_deux))
)

# Diagramme comparatif (barplot côte à côte)
ggplot(profils_deux_long, aes(x = Variable, y = Profil, fill = Pays)) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparaison des profils ligne : France vs Mali",
    x = "Colonnes",
    y = "Proportion"
  ) +
  theme_minimal()

# Réalisation du test du khi-deux

# H0 : les pays et les causes sont indépendants VS H1 : il y a dépendance

resu.chi2 <- chisq.test(morts[1:204,])

resu.chi2 <- suppressWarnings(chisq.test(morts[1:204, ]))

names(resu.chi2)
summary(resu.chi2)

resu.chi2$statistic

resu.chi2$p.value

# P-value 


# la structure du tableau ne permet pas vraiment de faire un test du ki deux, 
# on peut directement faire une AFC pour explorer les dépendances


# V Cramer
tot <- sum(morts[1:204,])
sqrt(resu.chi2$statistic/(30*tot))

# On obtient 0.1493, laison faible

# effectifs attendus sous hyp d'indépendance : effectif théorique
resu.chi2$expected

# Residus standardisés
resu.chi2$stdres["FRA",]

resu.chi2$stdres

# Sous l'hyp H0 d'indépendance, les profils attendus pour différents départements sont égaux
resu.chi2$expected["FRA",] / sum(resu.chi2$expected["FRA",])
resu.chi2$expected["MLI",] / sum(resu.chi2$expected["MLI",])


#Lancement de l'AFC avec R


# AFC avec lignes actives = les 204 pays  
pres.afc <- CA(morts[1:204,])

# Pour éviter la superposition des libellés 
fviz_ca_biplot(pres.afc, repel = T)


# Eboulis des valeures propres

pres.afc$eig

#diagramme en baton pour étudier la décroissance de l'inertie des axes


fviz_eig(pres.afc, addlabels = TRUE)


# Inertie totale

# Total du tableau (nb total de votes à l'issue du 1er tour)
tot <- sum(morts[1:204,])

# Inertie totale = D2/n
resu.chi2$statistic/tot # La statistique D2/n est également appelée Phi2

# Inertie totale = somme des valeurs propres
sum(pres.afc$eig[,1])

# On détermine le nombre d'axes intéressants de 2 façons différentes :

# Critère de Kaiser 


inertie_totale <- sum(pres.afc$eig[, 1])
nb_axes <- nrow(pres.afc$eig)
inertie_moyenne <- inertie_totale / nb_axes
inertie_moyenne

pres.afc$eig
inertie_axes <- pres.afc$eig[, 1]
barplot(
  inertie_axes,
  names.arg = paste("Axe", 1:length(inertie_axes)),
  main = "Décroissance de l’inertie des axes",
  xlab = "Axes factoriels",
  ylab = "Inertie",
  col = "lightblue",
  las = 2
)
# Ajout de la ligne de l’inertie moyenne
abline(
  h = inertie_moyenne,
  col = "red",
  lwd = 2,
  lty = 2
)

inertie_pct <- pres.afc$eig[, 2]

barplot(
  inertie_pct,
  names.arg = paste("Axe", 1:length(inertie_pct)),
  main = "Décroissance de l’inertie (en %)",
  xlab = "Axes factoriels",
  ylab = "Pourcentage d’inertie",
  col = "lightgreen",
  las = 2
)



#calcul de la valeur propremoyenne 

mean(pres.afc$eig[,1])

# on note six axes dont l'inertie est supérieur





# critère du baton brisé


#  Nombre d’axes factoriels (axes non nuls)
nb_axes <- nrow(pres.afc$eig)

#  Inertie observée issue de l’AFC (en %)
# Colonne 2 de pres.afc$eig = pourcentage d’inertie
vp <- pres.afc$eig[, 2]

#  Calcul du critère du bâton brisé (en %)
# brokenStick donne des proportions → on multiplie par 100
bs <- 100 * brokenStick(1:nb_axes, nb_axes)


#  Comparaison numérique : axes pertinents
# TRUE = axe retenu
vp > bs

# Axes retenus selon le critère du bâton brisé
which(vp > bs)


barplot(
  rbind(vp, bs),
  beside = TRUE,
  names.arg = paste("Axe", 1:nb_axes),
  col = c("tomato1", "turquoise3"),
  border = "white",
  legend.text = c("Inertie observée", "Bâton brisé"),
  main = "Sélection des axes par le critère du bâton brisé",
  ylab = "Pourcentage d’inertie"
)

# avec broken stick on observe 4 axes interessants



# Plan des modalités lignes

fviz_ca_row(pres.afc)
fviz_ca_row(pres.afc, repel=T)
?fviz_ca_row


# Plan des modalités colonnes

fviz_ca_col(pres.afc)
fviz_ca_col(pres.afc, repel=T)

# Premier plan factoriel

fviz_ca_biplot(pres.afc, axes = c(1,2), repel = T) # meilleur plan
fviz_ca_biplot(pres.afc, axes = c(1,3))
fviz_ca_biplot(pres.afc, axes = c(1,3), repel = T)

fviz_ca_biplot(pres.afc, axes = c(1,4))
fviz_ca_biplot(pres.afc, axes = c(3,2))
fviz_ca_biplot(pres.afc, axes = c(4,2))
fviz_ca_biplot(pres.afc, axes = c(4,3))

#contributiob des lignes 


sort(pres.afc$row$contrib[,1], T)
sort(pres.afc$row$contrib[,2], T)

# Avec factoextra : plus facile à interpréter 
fviz_contrib(pres.afc, choice = "row", axes=1)
fviz_contrib(pres.afc, choice = "row", axes=2)
fviz_contrib(pres.afc, choice = "row", axes=3)
fviz_contrib(pres.afc, choice = "row", axes=4)



#Diagramme illisible à cause du nombre de pays(204)
#Triage par ordre décroissant des contributions, puis séléction des 10 premiers

# Axe 1 : NGA
contrib_dim1 <- pres.afc$row$contrib[, 1]
contrib_dim1_sorted <- sort(contrib_dim1, decreasing = TRUE)
head(contrib_dim1_sorted, 10)

# Axe 2 : ZAF
contrib_dim1 <- pres.afc$row$contrib[, 2]
contrib_dim1_sorted <- sort(contrib_dim1, decreasing = TRUE)
head(contrib_dim1_sorted, 10)

# Axe 3 : AFG
contrib_dim1 <- pres.afc$row$contrib[, 3]
contrib_dim1_sorted <- sort(contrib_dim1, decreasing = TRUE)
head(contrib_dim1_sorted, 10)

# Axe 4 : IND
contrib_dim1 <- pres.afc$row$contrib[, 4]
contrib_dim1_sorted <- sort(contrib_dim1, decreasing = TRUE)
head(contrib_dim1_sorted, 10)


# Contributions des causes

sort(pres.afc$col$contrib[,1], T)
sort(pres.afc$col$contrib[,2], T)

# Avec factoextra : plus facile à interpréter
fviz_contrib(pres.afc, choice = "col", axes=1)
fviz_contrib(pres.afc, choice = "col", axes=2)
fviz_contrib(pres.afc, choice = "col", axes=3) 
fviz_contrib(pres.afc, choice = "col", axes=4) 


# Qualité de représentation


# Plan des modalités lignes (row) ou colonnes (col) selon les Qltés de représentation

fviz_ca_row(pres.afc, col.row ="cos2", repel = T) 
fviz_ca_col(pres.afc, col.col="cos2", repel = T) 
fviz_ca_biplot(pres.afc, col.col="cos2", repel = T)

# En filtrant sur un cos2 > 0.7
fviz_ca_row(pres.afc, col.row = "cos2", select.row = list(cos2 = 0.5), repel = T)
# En filtrant sur les 20 meilleurs cos2
fviz_ca_row(pres.afc, col.row = "cos2", select.row = list(cos2 = 50), repel = T)





# Calcul et interprétation des distances au CG pour les pays...


# Les inerties sont données par 

pres.afc$row$inertia

# Sachant que les masses des individus sont données par
pres.afc$call$marge.row

# En déduire les distances aux CG : inertia/m_i

pres.afc$row$inertia/pres.afc$call$marge.row
barplot(sort(pres.afc$row$inertia/pres.afc$call$marge.row), las=2, cex.names=0.6)

# Quel pays a le profil le plus proche du profil moyen ?
which.min(pres.afc$row$inertia/pres.afc$call$marge.row)
# Quel pays a le profil le plus éloigné du profil moyen ?
which.max(pres.afc$row$inertia/pres.afc$call$marge.row)

# Interprétation de ces distances 
# La distance évalue le caractère +/- atypique d'un profil en regard du profil moyen

# Pour les colonnes
pres.afc$col$inertia/pres.afc$call$marge.col
barplot(sort(pres.afc$col$inertia/pres.afc$call$marge.col), las=2, cex.names=0.6)


# Distances représentées avec ggplot

dafr <- data.frame(distance = pres.afc$row$inertia/pres.afc$call$marge.row)
summary(dafr)

# Barres verticales
ggplot(dafr, aes(x=reorder(row.names(dafr), -distance), y=distance)) +
  geom_col(fill = "steelblue")  +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(title="Distance au barycentre", x ="Pays", y = "Distance")

# Barres horizontales
ggplot(dafr, aes(y=reorder(row.names(dafr), -distance), x=distance)) +
  geom_col(fill = "steelblue") +
  theme_grey(base_size = 7) +
  labs(title="Distance au barycentre", y ="Pays", x = "Distance")


