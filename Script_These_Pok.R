---
title: "Analyse des données de qualité de l'eau de la rivière Pokemouche 2021"
authors: "Jean-Luc Boudreau, Alain Patoine"
date: "2022-02-24"
---

# Historique des versions de fichiers Excel 

#resultat.
# 1) Traiter le tableau d'entrée

getwd()
setwd("Shippagan_mai_aout_2024")
setwd("Analyse_R_sum")
list.files()

```{r}
Pok2001.2022=read.csv2("Qualite_eau_CGERP_2001-2022.csv")
dim(Pok2001.2022) # 198 rangées x 80 colonnes
str(Pok2001.2022)

```

En théorie, le tableau devrait comprendre 12 stations x 4 années x 
4 sorties/année = 192 objets "stations-dates". Comprendre pourquoi
le fichier contient 198 rangées plutôt que 192.

```{r, echo=T}
# Lire les premières lignes de la variables "AnalysisSurfaceWater":
head(Pok2001.2022$AnalysisSurfaceWater)

# Uniformiser la variable "AnalysisSurfaceWater" et l'attribuer à la
# nouvelle variable "Station":

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-1"] <- "Pok-01"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-1"] <- "Pok-01"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-2"] <- "Pok-02"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-2"] <- "Pok-02"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-3"] <- "Pok-03"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-3"] <- "Pok-03"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-4"] <- "Pok-04"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-4"] <- "Pok-04"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-5"] <- "Pok-05"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-5"] <- "Pok-05"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-6"] <- "Pok-06"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-6"] <- "Pok-06"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-7"] <- "Pok-07"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-7"] <- "Pok-07"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-8"] <- "Pok-08"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-8"] <- "Pok-08"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-9"] <- "Pok-09"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-9"] <- "Pok-09"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-10"] <- "Pok-10"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-10"] <- "Pok-10"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-11"] <- "Pok-11"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-11"] <- "Pok-11"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-12"] <- "Pok-12"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-12"] <- "Pok-12"

head(Pok2001.2022$Station)
# Convertir le type de la variable "Station" en type "facteur" 
Pok2001.2022$Station=as.factor(Pok2001.2022$Station)
summary(Pok2001.2022$Station)






# Créer la nouvelle variable "Station2" avec des variables plus parlantes:
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-01"] <- "br. sud amont 01"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-02"] <- "Suggary 02"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-03"] <- "Branche sud aval 03"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-04"] <- "Pok amont 04"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-05"] <- "McConnell Six Rd 05"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-06"] <- "Morrison 06"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-07"] <- "Waugh sanct. 07"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-08"] <- "Sewell 08"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-09"] <- "Trout Val-Doucet 09"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-10"] <- "Pollard 10"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-11"] <- "Malt 11"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-12"] <- "Dempsey 12"

# Ordonner les valeurs de la variables "Station2" de l'amont vers l'aval:
Pok2001.2022$Station2=ordered(Pok2001.2022$Station2,
levels=c("Pok amont 04",      "Suggary 02",          "Morrison 06", 
         "br. sud amont 01",  "Branche sud aval 03", "Trout Val-Doucet 09", 
         "Sewell 08",         "Dempsey 12",          "Pollard 10",
         "Malt 11",           "Waugh sanct. 07",     "McConnell Six Rd 05"))

summary(Pok2001.2022$Station2)



# Lister les noms de variables (en-têtes des colonnes):
names(Pok2001.2022)
# Rechercher les variables comportant le mot "date":
names(Pok2001.2022[grep("date",names(Pok2001.2022), ignore.case=T)])
# Trois (3) variables de dates:
head(Pok2001.2022$DateSampled)
head(Pok2001.2022$DateSampled2)
head(Pok2001.2022$Datesampled3)
class(Pok2001.2022$DateSampled)

# Créer la nouvelle variable "Date" au format "date".
Pok2001.2022$Date=format(as.Date(Pok2001.2022$DateSampled,format="%Y-%m-%d"))
Pok2001.2022$Date=as.Date(Pok2001.2022$Date)
summary(Pok2001.2022$Date)

# Extraire les éléments d'année et en faire une variable "Year"
# https://stackoverflow.com/questions/36568070/extract-year-from-date
Pok2001.2022$Year=as.factor(format(Pok2001.2022$Date, "%Y"))
summary(Pok2001.2022$Year)
table(Pok2001.2022$Year,Pok2001.2022$Station)





# Créer une variable "Period" qui désigne la période 2001-2002 ou 2021-2022.
Pok2001.2022$Period[Pok2001.2022$Year==2001] <- "2001-2002"
Pok2001.2022$Period[Pok2001.2022$Year==2002] <- "2001-2002"
Pok2001.2022$Period[Pok2001.2022$Year==2021] <- "2021-2022"
Pok2001.2022$Period[Pok2001.2022$Year==2022] <- "2021-2022"
Pok2001.2022$Period=as.factor(Pok2001.2022$Period)

summary(Pok2001.2022$Period)
# Nous avons 102 enregistrements (rangées) pour la période 2001-2002
# et 96 pour la période 2021-2022

```

## 1.1) Comprendre la structure du tableau

```{r}
# La fonction "table" produit un tableau de contingence 
# indiquant le nombre de rangées de données pour chaque combinaison:
table(Pok2001.2022$Date, Pok2001.2022$Year)

table(Pok2001.2022$Station, Pok2001.2022$Year)
# À chaque année, chaque station a été visitée 4 ou 5 fois.

```


## 1.2) Créer des sous-groupes par tributaire

```{r tributaires, echo=T, eval=F}

Malt=subset(Pok2001.2022, Station2=="Malt 11")
dim(Malt) # 16 x 85
summary(Malt$Date)

# ... faire de même pour les onze autres stations (pendant l'été 2024).

```


# 2) Analyser les données


## Dureté

### Examiner les données

```{r}

hist(Pok2001.2022$Hardness_as_CaCO3_mg_L)

# Comparer 2001-2002 à 2021-2022
boxplot(Pok2001.2022$Hardness_as_CaCO3_mg_L ~ Pok2001.2022$Period)

```

Gaël: assure-toi de savoir comment interpréter un tel box-plot: que signifie
la ligne médiane, la boîte et les "moustaches".

Les valeurs de dureté en 2021-2022 sont légèrement inférieures à celles de
2001-2002.

Obtenir des statistiques par période avec la fonction "tapply"

```{r}

tapply(Pok2001.2022$Hardness_as_CaCO3_mg_L, Pok2001.2022$Period, summary)

```

En moyenne, la dureté en 2021-2022 (44 mg CaCO3/L) est plus faible qu'en
2001-2002 (48 mg/L). Cette différence est-elle imputable à l'erreur 
d'échantillonnage (hypothèse nulle H0) ou bien à des forces hydrologiques,
biologiques, physiques, chimiques, sociologiques, psychologiques ou autres 
(hypothèse alterne H1)?

### Vérifier les conditions d'application d'un test de t

```{r}
shapiro.test(Pok2001.2022$Hardness_as_CaCO3_mg_L)

qqnorm(Pok2001.2022$Hardness_as_CaCO3_mg_L)
qqline(Pok2001.2022$Hardness_as_CaCO3_mg_L)

```

Les valeurs extrêmes sont plus fréquentes (ou plus extrêmes) qu'attendues 
si les données devaient suivre une distribution normale.

Même si les données ne sont pas normalement distriuées, tentons un test de t
pour voir si la différence de dureté entre époques est attribuable au hasard
de l’échantillonnage.

Comme les mêmes "individus" (stations) sont mesurés avant et après, 
il nous faut un test de t "apparié" (diapo 79).

```{r}
# Créer deux groupes de même taille
# car le test de t suppose des groupes de même taille.

Hard2001.2002=subset(Pok2001.2022, Period=="2001-2002", 
                     select="Hardness_as_CaCO3_mg_L")
class(Hard2001.2002)
dim(Hard2001.2002) # 102 rangées x 1 colonne
# Ne prendre que les 96 premières rangées pour que la série soit
# d'égale longueur au groupe 2021-2022
groupe1Hard=Hard2001.2002[1:96,]
groupe1Hard
length(groupe1Hard)

Hard2021.2022=subset(Pok2001.2022, Period=="2021-2022", 
                     select="Hardness_as_CaCO3_mg_L")
dim(Hard2021.2022) # 96 rangées x 1 colonne
groupe2Hard=Hard2021.2022[1:96,]
length(groupe2Hard)

t.test(groupe1Hard, groupe2Hard, paired=T)

```

La valeur p=0.015 représente la probabilité que l’hypothèse nulle soit vraie. 
Comme elle est inférieure au seuil de signification généralement utilisé de 5 pourcen, 
on décide de... accepter ou rejeter Ho??? On s’en reparle demain (2024-04-05 
vendredi).


En fait, les tests appariés supposent que l’individu qui fait l’objet
d’une mesure AVANT est le même individu après. Dans notre cas, il
serait difficile de dire que l’objet "Station 1 du 1er août 2001" est
le même objet que "Station 1 du 28 juillet 2022".

Répétons alors le test t sans l’option "paired", et donc sans tronquer nos données.

```{r}

t.test(Pok2001.2022$Hardness_as_CaCO3_mg_L~Pok2001.2022$Period)

# p=0.038

```

### Test de rang

En transformant les données en rangs, on perd de l’information, ce qui
est désavantageux. Par contre, des tests statistiques peuvent être
utilisés sur des rangs avec très peu de conditions à respecter. Le
désavantage est que ces tests sont moins puissants (moins aptes à
détecter une différence réel.

```{r}

# Obtenir de l'aide sur une fonction

# help(wilcox.test)

wilcox.test(Pok2001.2022$Hardness_as_CaCO3_mg_L~Pok2001.2022$Period)

# p=0.019

```


### Tester les différences entre stations avec l'ANOVA

```{r}

boxplot(Pok2001.2022$Hardness_as_CaCO3_mg_L ~ Pok2001.2022$Station2)

```

Les différences observées entre les moyennes de dureté par stations sont-elles dues
au hasard de l’échantillonnage (hypothèse nulle) ou bien à un
processus (hypothèse alterne)? Essayons l’analyse de variance avec la fonction aov. 
                                                              

```{r}

# D’abord vérifier les conditions
# ... à faire....

# Exécuter le test si les conditions sont respectées:

modelaov1=aov(Pok2001.2022$Hardness_as_CaCO3_mg_L ~ Pok2001.2022$Station)

modelaov1
summary(modelaov1)
# p<0.0001

```

### La dureté et l'alcalinité sont-elles corrélées?

```{r}

# Lister les noms de variables
# names(Pok2001.2022)

plot(Pok2001.2022$Hardness_as_CaCO3_mg_L~Pok2001.2022$AlkalinityCaCO3_mg_L)

cor.test(Pok2001.2022$Hardness_as_CaCO3_mg_L, Pok2001.2022$AlkalinityCaCO3_mg_L,
method=c('pearson'))

# Corrélation de rang (plus robuste, mais moins puissant)

cor.test(Pok2001.2022$Hardness_as_CaCO3_mg_L, Pok2001.2022$AlkalinityCaCO3_mg_L,
method=c('spearman'))

# Avertissement sur les cas ex-aequo.

# help(cor.test)

```

## Fluore


```{r Fluoride, eval=F}

# Lister les valeurs de la variable:
Pok2001.2022$Fluoride_mg_L

# Convertir les cas sous la limite de détection à la moitié de la limite:
Pok2001.2022$Fluoride2_mg_L=ifelse(Pok2001.2022$Fluoride_mg_L==0.1, 0.05, Pok2001.2022$Fluoride_mg_L)
class(Pok2001.2022$Fluoride2_mg_L)
subset(Pok2001.2022, select=c(Station, Date, Fluoride_mg_L ,Fluoride2_mg_L))

summary(Pok2001.2022$Fluoride_mg_L);summary(Pok2001.2022$Fluoride2_mg_L)
# Noter la moyenne plus faible pour la variable 2.



```

### Fluore: box-plot par période

```{r, fig=T}

ggplot(aes(y=Fluoride2_mg_L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
```

### Fluore: box-plot par période et station

```{r, fig=T}

ggplot(aes(y=Fluoride2_mg_L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

```




### Fluore à Maltempec

```{r}

Fluore2001.2002=ggplot(aes(y=Fluoride2_mg_L, x=Date), data=Malt) + 
  geom_line(color="blue") + geom_point(color="blue", size=3) +
  ggtitle("Concentration de fluore, station POK-11 Maltempec 2001 à 2022") +
   scale_x_date(date_labels="%b %Y", breaks = scales::breaks_pretty(11),
                limits = as.Date(c('2001-07-01','2002-12-01'))) +
     theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
Fluore2001.2002

Fluore2021.2022=ggplot(aes(y=Fluoride2_mg_L, x=Date), data=Malt) + 
  geom_line(color="blue") + geom_point(color="blue", size=3) +
  ggtitle("Concentration de fluore, station POK-11 Maltempec 2001 à 2022") +
   scale_x_date(date_labels="%b %Y", breaks = scales::breaks_pretty(11),
                limits = as.Date(c('2021-07-01','2022-12-01'))) +
     theme(axis.text=element_text(size=14), axis.title=element_text(size=14))
Fluore2021.2022

# Placer côte à côte les graphiques avec la fonction "ggarrange"
ggarrange(Fluore2001.2002, Fluore2021.2022, nrow=1, ncol=2)

```

## Oxygène dissous à la station Maltempec

```{r, echo=T, eval=F}
Pok2001.2022$Oxyg_dissous_YSI_mg_L

Malt$Oxyg_dissous_YSI_mg_L

subset(Malt, select=c(Station, Station2, Date, Oxyg_dissous_YSI_mg_L))

ggplot(aes(y=Oxyg_dissous_YSI_mg_L, x=Date, group=1), data=Malt) + 
  geom_point() +
  geom_line() +
   ggtitle("Concentration d'oxygène dissous, station POK-11 Maltempec 2001 à 2022") +
  scale_x_date(date_labels="%Y", breaks = scales::breaks_pretty(10)) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14))


```


## Sulfate

```{r Sulfate}

Pok2001.2022$Sulfate2_mg.L=ifelse(Pok2001.2022$Sulfate2_mg.L==1, 0.5, Pok2001.2022$Sulfate2_mg.L)
summary(Pok2001.2022$Sulfate2_mg.L)
head(subset(Pok2001.2022,select=c(Sulfate_mg.L,Sulfate2_mg.L)))
summary(Pok2001.2022$Sulfate2_mg.L)

ggplot(aes(y=Sulfate2_mg.L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Sulfate2_mg.L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


```

## Ammonia (seulement en 2001 et 2002)

```{r Ammonia}


Pok2001.2022$Ammonia2.N._mg.L=ifelse(Pok2001.2022$Ammonia2.N._mg.L==0.05, 0.025, Pok2001.2022$Ammonia2.N._mg.L)
Pok2001.2022$Ammonia2.N._mg.L=ifelse(Pok2001.2022$Ammonia2.N._mg.L==0.01, 0.005, Pok2001.2022$Ammonia2.N._mg.L)
subset(Pok2001.2022,select=c(Ammonia..as.N._mg.L,Ammonia2.N._mg.L))
# sapply(Moncton, function(x)(sum(is.na(x)))) # NA counts
# List cases where Ammonia is not lower than detection limit
subset(Pok2001.2022,!grepl("<", Pok2001.2022$Ammonia..as.N._mg.L) & !is.na(Pok2001.2022$Ammonia2.N._mg.L),select=c(Year, Ammonia..as.N._mg.L,Ammonia2.N._mg.L))
tamm=subset(Pok2001.2022,!grepl("<", Pok2001.2022$Ammonia..as.N._mg.L) & !is.na(Pok2001.2022$Ammonia2.N._mg.L))
dim(tamm)

summary(Pok2001.2022$Ammonia2.N._mg.L)

sum(is.na(Pok2001.2022$Ammonia2.N._mg.L  ))
dim(Pok2001.2022)
# sapply(Pok2001.2022, function(x)(sum(is.na(x)))) # NA counts

ggplot(aes(y=Ammonia2.N._mg.L, x=Station2, fill=Period), data=tamm) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
    ggtitle("Seulement 21 données (2001-2002) sont supérieures à la limite de détection")

# Un.ionized...20ºC_mg.L
# Nitrate...Nitrite..as.N._mg.L
# Nitrite..as.N._mg.L
# Nitrate..as.N._mg.L
```

## Nitrite et nitrate (2022 seulement)

```{r NOx}

Pok2001.2022$NOx_mg.L=ifelse(Pok2001.2022$NOx_mg.L==0.05, 0.025, Pok2001.2022$NOx_mg.L)
subset(Pok2001.2022,select=c(Year, Nitrate...Nitrite..as.N._mg.L, NOx_mg.L))
subset(Pok2001.2022,select=c(Nitrate...Nitrite..as.N._mg.L, NOx_mg.L))[sample(x=seq(1,length(Pok2001.2022),1), size=5),]
summary(Pok2001.2022$NOx_mg.L)
dim(subset(Pok2001.2022,NOx_mg.L>0,03))

ggplot(aes(y=NOx_mg.L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
    ggtitle("Neuf valeurs (sur 48) sont sous la limite de détection de 0,05; elles sont replacées par 0,025")

```

## Phosphorus


```{r Phosphorus2}


Pok2001.2022$Phosphorus2_mg.L=ifelse(Pok2001.2022$Phosphorus2_mg.L==0.005, 0.0025, Pok2001.2022$Phosphorus2_mg.L)
subset(Pok2001.2022,select=c(Phosphorus...Total_mg.L,Phosphorus2_mg.L))[sample(x=seq(1,length(Pok2001.2022),1), size=5),]
summary(Pok2001.2022$Phosphorus2_mg.L)

ggplot(aes(y=Phosphorus2_mg.L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Phosphorus2_mg.L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) + ylab("Phosphore total (mg/L)")



```
 
## E.coli 2001-2021

```{r E.coli}


Pok2001.2022$E.Coli2.MPN.100.mL=ifelse(Pok2001.2022$E.Coli2.MPN.100.mL==10, 5, Pok2001.2022$E.Coli2.MPN.100.mL)
subset(Pok2001.2022,select=c(E..Coli..MPN.100.mL.,E.Coli2.MPN.100.mL))[sample(x=seq(1,length(Pok2001.2022),1), size=5),]
summary(Pok2001.2022$E.Coli2.MPN.100.mL)

# Noter que la valeur de 2000 est en fait > 2000
# par(mar = c(15, 4, 4, 2) + 0.1) 
# summary(Pok2001.2022$Station2)
# levels(Pok2001.2022$Station2)
# boxplot(Pok2001.2022$E.Coli2.MPN.100.mL~Pok2001.2022$Date)
# as.character(Pok2001.2022$Station2)

# par(mar = c(7, 4, 2, 2) + 0.1) 
# boxplot(Pok2001.2022$E.Coli2.MPN.100.mL~Pok2001.2022$Station2, las=2)
# library("ggplot2") # to use ggplot

ggplot(aes(y=E.Coli2.MPN.100.mL, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=E.Coli2.MPN.100.mL, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

```

## Carbon...Total.Organic_mg.L

```{r Carboneorg}


Pok2001.2022$Carbone2.org_mg.L=ifelse(Pok2001.2022$Carbone2.org_mg.L==1, 0.5, Pok2001.2022$Carbone2.org_mg.L)
subset(Pok2001.2022,select=c(Carbon...Total.Organic_mg.L,Carbone2.org_mg.L))[sample(x=seq(1,length(Pok2001.2022),1), size=5),]
summary(Pok2001.2022$Carbone2.org_mg.L)
subset(Pok2001.2022,is.na(Carbone2.org_mg.L), select=c(Station,Date, Carbon...Total.Organic_mg.L, Carbone2.org_mg.L))


ggplot(aes(y=Carbone2.org_mg.L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Carbone2.org_mg.L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

```



## Colour_TCU

```{r Couleur}


subset(Pok2001.2022,select=c(Colour_TCU,Couleur2_TCU))[sample(x=seq(1,length(Pok2001.2022),1), size=5),]
summary(Pok2001.2022$Couleur2_TCU)


ggplot(aes(y=Couleur2_TCU, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
   ggtitle("Recommandations pour la qualité de l'eau potable au Canada < 15 TCU")

ggplot(aes(y=Couleur2_TCU, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
   ggtitle("Recommandations pour la qualité de l'eau potable au Canada < 15 TCU")


```

## Turbidity_NTU

```{r Turbidity}

summary(Pok2001.2022$Turbidity_NTU)


ggplot(aes(y=Turbidity_NTU, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(limits=c(0,10)) +
  ggtitle("Une valeur à 45 en 2001-2002")

ggplot(aes(y=Turbidity_NTU, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  scale_y_continuous(limits=c(0,10)) +
  ggtitle("Une valeur à 45 en 2001-2002") 


```

## pH

```{r pH}

summary(Pok2001.2022$pH_units)


ggplot(aes(y=pH_units, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) 

ggplot(aes(y=pH_units, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

```

## Total dissolved solids

"Overall average concentration for total dissolved solids in the Miramichi River Basin is 22 mg/L" Chadwick 1995: 29.

```{r TDS}
summary(Pok2001.2022$TDS..calc._mg.L)


ggplot(aes(y=TDS..calc._mg.L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
        ggtitle("Solides totaux dissous (mg/L)")

ggplot(aes(y=TDS..calc._mg.L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


```

## Aluminium

```{r Aluminium}

summary(Pok2001.2022$Aluminum_mg.L)


ggplot(aes(y=Aluminum_mg.L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggtitle("La concentration maximale acceptable (CMA) d'aluminium total dans l'eau potable est de 2,9 mg/L")

ggplot(aes(y=Aluminum_mg.L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggtitle("La concentration maximale acceptable (CMA) d'aluminium total dans l'eau potable est de 2,9 mg/L")

```

## Fer

```{r Fe2}

summary(Pok2001.2022$Iron_mg.L)


ggplot(aes(y=Iron_mg.L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggtitle("«Au Canada, la concentration de fer dans les eaux de surface est généralement inférieure à 10 mg/L» ")

ggplot(aes(y=Iron_mg.L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  ggtitle("«Au Canada, la concentration de fer dans les eaux de surface est généralement inférieure à 10 mg/L» ")


```

## Oxygène dissous

```{r O2}

Pok2001.2022$Oxyg_dissous_YSI_mg_L


summary(Pok2001.2022$Oxyg_dissous_YSI_mg_L)

ggplot(aes(y=Oxyg_dissous_YSI_mg_L, x=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) 

ggplot(aes(y=Oxyg_dissous_YSI_mg_L, x=Station2, fill=Period), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

```

boxplot(Pok2001.2022$Hardness_as_CaCO3_mg_L ~ Pok2001.2022$Year)


ggplot(aes(y=Fluoride2_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Sulfate_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=AlkalinityCaCO3_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Calcium_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Magnesium_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Calcium_mgL, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Sulfate_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Carbon_Total_Organic_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Conductivity_microS_cm, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=pH_units, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Turbidity_NTU, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Hardness_as_CaCO3_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=TDS_calc_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Turbidity_NTU, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Oxyg_dissous_YSI_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Temp_eau_YSI_degC, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))


Les métaux

ggplot(aes(y=Cadmium_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

Les bactéries 

ggplot(aes(y=E.coli_MPN_100 mL, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=E.coli_MPN_100 mL, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
ggplot(aes(y=Potassium_mg_L, x=Year, fill=Station), data=Pok2001.2022) + geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

class(Pok2001.2022$DateSampled)

ggplot(aes(y=Hardness_as_CaCO3_mg_L, x=DateSampled, fill=Year), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Hardness_as_CaCO3_mg_L, x=DateSampled, fill=Year), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
Pok2001.2022$Station

Pok4=subset(Pok2001.2022, Station=="Pok-04")
dim(Pok4)
ggplot(aes(y=Hardness_as_CaCO3_mg_L, x=DateSampled, fill="Pok amont 04"), data=Pok4) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

plot(Pok4$Hardness_as_CaCO3_mg_L ~ Pok4$Date, type="b")

Period01=subset(Pok2001.2022, Period=="2001-2002")
dim(Period01)

plot(Period01$Hardness_as_CaCO3_mg_L ~ Period01$Date, type="b")

names(Pok4)

class(Pok4$Date)

# Créer une variable "Period" qui désigne la période 2001-2002 ou 2021-2022.
Pok2001.2022$Period[Pok2001.2022$Year==2001] <- "2001-2002"
Pok2001.2022$Period[Pok2001.2022$Year==2002] <- "2001-2002"
Pok2001.2022$Period[Pok2001.2022$Year==2021] <- "2021-2022"
Pok2001.2022$Period[Pok2001.2022$Year==2022] <- "2021-2022"
Pok2001.2022$Period=as.factor(Pok2001.2022$Period)

summary(Pok2001.2022$Period)

ggplot(aes(y=Hardness_as_CaCO3_mg_L, x=Year, fill="Pok amont 04"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

"Pok amont 04"

ggplot(aes(y=Hardness_as_CaCO3_mg_L, x=Year, fill="Branche sud aval 03"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Pok amont 04"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="McConnell Six Rd 05"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Morrison 06"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Waugh sanct. 07"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Sewell 08"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Trout Val-Doucet 09"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Pollard 10"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Malt 11"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(aes(y=Fluoride2_mg_L, x=Pok2001.2022$Year, fill="Dempsey 12"), data=Pok2001.2022) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust=1),axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))




#Ce 13 mai 2024

getwd()
setwd("C:/Users/mrdos/Githubb")
getwd()
list.files()

setwd("C:/Users/mrdos/OneDrive/Documents/Shippagan_mai_aout_2024/Analyse_R_sum/Github")
getwd()
list.files()

Pok2001.2022=read.csv2("Qualite_eau_CGERP_2001-2022.csv")
dim(Pok2001.2022)
str(Pok2001.2022)

Normalement le tableau devrait comprendre 12 stations x 4 années x 
4 sorties/année = 192 objets "stations-dates".
Le fait que le fichier contient 198 rangées plutôt que 192 s'explique par la suite:

# Lire les premières lignes de la variables "AnalysisSurfaceWater":
head(Pok2001.2022$AnalysisSurfaceWater)

# Uniformiser la variable "AnalysisSurfaceWater" et l'attribuer à la
# nouvelle variable "Station":

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-1"] <- "Pok-01"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-1"] <- "Pok-01"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-2"] <- "Pok-02"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-2"] <- "Pok-02"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-3"] <- "Pok-03"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-3"] <- "Pok-03"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-4"] <- "Pok-04"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-4"] <- "Pok-04"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-5"] <- "Pok-05"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-5"] <- "Pok-05"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-6"] <- "Pok-06"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-6"] <- "Pok-06"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-7"] <- "Pok-07"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-7"] <- "Pok-07"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-8"] <- "Pok-08"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-8"] <- "Pok-08"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-9"] <- "Pok-09"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-9"] <- "Pok-09"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-10"] <- "Pok-10"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-10"] <- "Pok-10"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-11"] <- "Pok-11"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-11"] <- "Pok-11"

Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="Pok-12"] <- "Pok-12"
Pok2001.2022$Station[Pok2001.2022$AnalysisSurfaceWater=="POK-12"] <- "Pok-12"

head(Pok2001.2022$Station)

# Convertissons le type de la variable "Station" en type "facteur"

Pok2001.2022$Station=as.factor(Pok2001.2022$Station)
summary(Pok2001.2022$Station)

# Créer la nouvelle variable "Station2" avec des variables plus parlantes:

Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-01"] <- "br. sud amont 01"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-02"] <- "Suggary 02"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-03"] <- "Branche sud aval 03"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-04"] <- "Pok amont 04"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-05"] <- "McConnell Six Rd 05"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-06"] <- "Morrison 06"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-07"] <- "Waugh sanct. 07"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-08"] <- "Sewell 08"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-09"] <- "Trout Val-Doucet 09"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-10"] <- "Pollard 10"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-11"] <- "Malt 11"
Pok2001.2022$Station2[Pok2001.2022$Station=="Pok-12"] <- "Dempsey 12"

# Ordonner les valeurs de la variables "Station2" de l'amont vers l'aval:
Pok2001.2022$Station2=ordered(Pok2001.2022$Station2,
levels=c("Pok amont 04",      "Suggary 02",          "Morrison 06", 
         "br. sud amont 01",  "Branche sud aval 03", "Trout Val-Doucet 09", 
         "Sewell 08",         "Dempsey 12",          "Pollard 10",
         "Malt 11",           "Waugh sanct. 07",     "McConnell Six Rd 05"))

summary(Pok2001.2022$Station2)



# Lister les noms de variables (en-têtes des colonnes):
names(Pok2001.2022)

# Rechercher les variables comportant le mot "date":
names(Pok2001.2022[grep("date",names(Pok2001.2022), ignore.case=T)])

# Trois (3) variables de dates:
head(Pok2001.2022$DateSampled)
head(Pok2001.2022$DateSampled2)
head(Pok2001.2022$Datesampled3)
class(Pok2001.2022$DateSampled)

# Créer la nouvelle variable "Date" au format "date".
Pok2001.2022$Date=format(as.Date(Pok2001.2022$DateSampled,format="%Y-%m-%d"))
Pok2001.2022$Date=as.Date(Pok2001.2022$Date)
summary(Pok2001.2022$Date)

# Extraire les éléments d'année et en faire une variable "Year"
# https://stackoverflow.com/questions/36568070/extract-year-from-date
Pok2001.2022$Year=as.factor(format(Pok2001.2022$Date, "%Y"))
summary(Pok2001.2022$Year)
table(Pok2001.2022$Year,Pok2001.2022$Station)

# Créer une variable "Period" qui désigne la période 2001-2002 ou 2021-2022.
Pok2001.2022$Period[Pok2001.2022$Year==2001] <- "2001-2002"
Pok2001.2022$Period[Pok2001.2022$Year==2002] <- "2001-2002"
Pok2001.2022$Period[Pok2001.2022$Year==2021] <- "2021-2022"
Pok2001.2022$Period[Pok2001.2022$Year==2022] <- "2021-2022"
Pok2001.2022$Period=as.factor(Pok2001.2022$Period)

summary(Pok2001.2022$Period)

# Nous avons 102 enregistrements (rangées) pour la période 2001-2002
# et 96 pour la période 2021-2022


## 1.1) Comprendre la structure du tableau

# La fonction "table" produit un tableau de contingence 
# indiquant le nombre de rangées de données pour chaque combinaison:

table(Pok2001.2022$Date, Pok2001.2022$Year)

table(Pok2001.2022$Station, Pok2001.2022$Year)

# À chaque année, chaque station a été visitée 4 ou 5 fois.


levels(Pok2001.2022$Station2)
levels(Pok2001.2022$Period)

# Créer un tableau avec les données de la période 1 seulement:
Period1=subset(Pok2001.2022, Period=="2001-2002")
dim(Period1)

# Créer un tableau avec les données de 2001 seulement
Year2001=subset(Pok2001.2022, Year=="2001")
dim(Year2001)

levels(Pok2001.2022$Station2)

# Les graphiques de haut niveau "ggplot" permettent de produire facilement
# un graphiques avec plusieurs courbes (niveaux):

library("ggplot2")

O2g1=ggplot(aes(y=Oxyg_dissous_YSI_mg_L, x=Date), data=Year2001) + 
  geom_line(aes(color=Station2, linetype=Station2)) + 
  geom_point(aes(color=Station2, shape=Station2)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B")

#  date_minor_breaks = "1 week",
O2g1
 
# Modifier, adapter l'axe des X (dates)

O2g2=ggplot(aes(y=Oxyg_dissous_YSI_mg_L, x=Date), data=Year2001) + 
  geom_line(aes(color=Station2, linetype=Station2)) + 
  geom_point(aes(color=Station2, shape=Station2)) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%B") +
  theme_bw()
O2g2
 
# Date breaks:
# an interval specification, one of "sec", "min", "hour", "day", "week", "month", "year"
# https://www.rdocumentation.org/packages/scales/versions/1.3.0/topics/date_breaks
 
 
# Modifier la syntaxe pour permettre plus de 6 formes 
# grâce à "scale_shape_manual" selon
# https://stackoverflow.com/questions/26223857/more-than-six-shapes-in-ggplot
 
# Détermine le nombre de formes nécessaires:
nlevels(Year2001$Station2)
# 12
 
O2g3=ggplot(aes(y=Oxyg_dissous_YSI_mg_L, x=Date, 
                group=Station2, shape=Station2, color=Station2), data=Year2001) + 
  scale_shape_manual(values=1:nlevels(Year2001$Station2)) +
  geom_line() + 
  geom_point() +
  scale_x_date(date_labels = "%d %b", breaks=c(as.Date("2001-08-01"), as.Date("2001-09-01"),
                                               as.Date("2001-10-01"), as.Date("2001-11-01"),
                                               as.Date("2001-12-01"))) +
  theme_bw()
O2g3
	 
 
# Graphique de bas niveau: syntax simple, mais possibilités plus limitées
# Plutôt que de créer 48 sous-tableaus (12 stations x 4 années), 
# contrôler la fenêtre temporelle avec "xlim" pour chacune des 12 stations...
 
plot( Malt$Oxyg_dissous_YSI_mg_L~Malt$Date, type="b",
     xlim=c(as.Date("2001-07-01"),as.Date("2001-12-01")))
# Ajouter une autre série de points avec "points"
# et contrôler l'allure des courbes avec "col" (couleur),
# "pch" (forme du symbol) et "lty" (line type).
# "las" permet de faire pivoter les étiquettes de l'axe des x.
points(Waugh$Oxyg_dissous_YSI_mg_L~Waugh$Date, type="b", col="blue", pch=2, lty=2)
 
# Mieux contrôler les dates sur l'axe des X avec axis.Date
 
plot(Malt$Oxyg_dissous_YSI_mg_L~Malt$Date, type="b",
     xaxt = "n",
     xlim=c(as.Date("2001-07-01"),as.Date("2001-12-01")))
axis.Date(1,
     at=seq(as.Date("2001-07-01"), as.Date("2001-12-01"), by="months"), format="%d %b", las=3)
# Ajouter une autre série de points avec "points"
points(Waugh$Oxyg_dissous_YSI_mg_L~Waugh$Date, type="b", col="blue", pch=2, lty=2)
# Ajouter les autres stations avec d'autres lignes "points(...)"
# Il faut ensuite construire la légende...
 
# La fonction "scatterplot" permet facilement de représenter une association 
# pour différents sous-groupes (ici, les stations), mais ne permet pas de
# connecter les points...
library("car")
scatterplot(Period1$Oxyg_dissous_YSI_mg_L~Period1$Date | Period1$Station2,
            smooth=F)
 
```
