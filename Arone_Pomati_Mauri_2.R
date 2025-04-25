rm(list = ls())
#############
#HOMEWORK_II#
#############
#Lavoro di:
#Tommaso Arone (mat. 896282)
#Davide Mauri (mat. 895695)
#Giovanni Pomati (mat. 903516)

# This file was produced by the NASA Exoplanet Archive  http://exoplanetarchive.ipac.caltech.edu
#
# COLUMN toi:            TESS Object of Interest
# COLUMN tid:            TESS Input Catalog ID
# COLUMN tfopwg_disp:    TFOPWG Dispostion (CP | FP | KP | PC)
# COLUMN rastr:          RA [sexagesimal]
# COLUMN ra:             RA [deg]
# COLUMN decstr:         Dec [sexagesimal]
# COLUMN dec:            Dec [deg]
# COLUMN st_pmra:        PMRA [mas/yr]
# COLUMN st_pmdec:       PMDec [mas/yr]
# COLUMN pl_tranmid:     Planet Transit Midpoint Value [BJD]
# COLUMN pl_orbper:      Planet Orbital Period Value [days]
# COLUMN pl_trandurh:    Planet Transit Duration Value [hours]
# COLUMN pl_trandep:     Planet Transit Depth Value [ppm]
# COLUMN pl_rade:        Planet Radius Value [R_Earth]
# COLUMN pl_insol:       Planet Insolation Value [Earth flux]
# COLUMN pl_eqt:         Planet Equilibrium Temperature Value [K]
# COLUMN st_tmag:        TESS Magnitude
# COLUMN st_dist:        Stellar Distance [pc]
# COLUMN st_teff:        Stellar Effective Temperature Value [K]
# COLUMN st_logg:        Stellar log(g) Value [cm/s**2]
# COLUMN st_rad:         Stellar Radius Value [R_Sun]
# COLUMN toi_created:    TOI Created Date
# COLUMN rowupdate:      Date Modified
#

#librerie utilizzate:
library(tidyverse) #gestione dei dati e grafici
library(heatmaply) #heatmap correlazione
library(Rmixmod) #modello di classification
library(caret) #confusion matrix
library(flexmix) #MeM
library(gridExtra) #grid.arrange()
library(GGally) #ggpairs

#####################
#PULIZIA DEL DATASET:
#####################
data <- read.csv("C:\\Users\\tomma\\OneDrive\\Desktop\\TOI_2025.01.16_00.57.30.csv")
str(data)

#TOGLIAMO VALORI NULLI:
data <- data |> filter(tfopwg_disp != "")
data

#gli na non sono  molti nelle varie variabili, si passa da
#7356 osservazioni a 6292 eliminandoli, è un risultato
#accettabile:
prova <- na.omit(data)

#togliamo le variabili qualitative:
noqual <- prova[,-c(2,4,6,22,23)]

#salviamo le labels:
labels <- as.factor(noqual$tfopwg_disp)

#modifico il nome delle righe, con il codice univoco di ogni pianeta
row.names(noqual) <- noqual$toi

#dataset tidy:
def <- noqual[,-c(1)]



#####################
#ANALISI ESPLORATIVE:
#Correlazione:#######
#####################


corr <- cor(def[,-1])

gray_colors <- colorRampPalette(c("black", "white"))(256)
heatmaply_cor(corr,
              cellnote = corr,
              dendrogram = F,
              col = colorRampPalette(c("#415A77","white","#C06C84")))
?heatmaply_cor
        

colors <- c("#1B263B", "#415A77","#6C5B7B", "#A8B2C1", "#F9A825")



#comando per alleggerire il "viewer"
dev.off()

#vari grafici:

#Accelerazione Gravitazionale vs Raggio della Stella:
ggplot(data = def, aes(x = st_rad, y = st_logg)) + geom_point(alpha = .7, col = "slateblue", size = 2)+
  labs(x = "raggio della stella", y = "accelerazione gravitazionale della stella") + theme_light()

#Boxplot Temperatura di Equilibrio stratificato per la Labels:
ggplot(data = def) + geom_boxplot(aes(x = labels, y = pl_eqt ), alpha = .5) +
  labs(x = "Labels", y = "Temperatura di Equilibrio del Pianeta") + 
  scale_fill_manual(values = colors) +
  labs(x = "Labels", y = "Temperatura di Equilibrio del Pianeta") +
  theme_minimal()


# Creazione di un vettore di colori alternati
colors <- rep(c("darkorange1", "darkblue"), length.out = length(unique(labels)))

# Creazione del grafico
ggplot(data = def, aes(x = labels, y = pl_eqt, fill = labels)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = colors) +
  labs(x = "Labels", y = "Temperatura di Equilibrio del Pianeta") +
  theme_minimal()



#Boxplot Raggio del pianeta stratificato per la Labels:
ggplot(data = def) + geom_boxplot(aes(x = labels, y = pl_rade ), col = "firebrick", fill = "firebrick3", alpha = .5) +
  labs(x = "Labels", y = "Raggio del Pianeta") + theme_light()

#Boxplot Raggio della stella stratificato per la Labels:
ggplot(data = def) + geom_boxplot(aes(x = labels, y = st_rad ), col = "firebrick", fill = "firebrick3", alpha = .5) +
  labs(x = "Labels", y = "Raggio della Stella") + theme_light()

#Boxplot Temperatura effettiva della stella stratificato per la Labels:
ggplot(data = def) + geom_boxplot(aes(x = labels, y = st_teff ), col = "firebrick", fill = "firebrick3", alpha = .5) +
  labs(x = "Labels", y = "Temperatura effettiva della Stella") + theme_light()


#densità di accelerazione gravitazionale per le labels
ggplot(data = def) + geom_density(aes(x = st_logg, fill = labels), alpha = 0.4) +facet_wrap(labels) +
  labs(title = "Densità dell'accelerazione gravitazionale della stella",
       x = "accelerazione gravitazionale") + theme_light()

#densità temperatura di equilibrio:
eqt <- ggplot(data = def) + geom_histogram(aes(x = pl_eqt, y = after_stat(density)), col = "white", fill = "#A8B2C1") +
  geom_density(aes(x = pl_eqt), alpha = 0.4, fill ="#A8B2C1" ) +
  labs(x = "Temperatura di Equilibrio del Pianeta", title = "Temperatura di Equilibrio del pianeta:") + theme_gray()

#densità raggio del pianeta:
rag <- ggplot(data = def) + geom_histogram(aes(x = pl_rade, y = after_stat(density)), col = "white", fill = "#A8B2C1") +
  geom_density(aes(x = pl_rade), alpha = 0.4, fill = "#A8B2C1") +
  labs(x = "Raggio del Pianeta", title = "Raggio del pianeta:") + theme_gray()

#densità raggio della stella:
rad <- ggplot(data = def) + geom_histogram(aes(x = st_rad, y = after_stat(density)), col = "white", fill = "#A8B2C1") +
  geom_density(aes(x = st_rad), alpha = 0.4, fill = "#A8B2C1") +
  labs(x = "Raggio della Stella", title = "Raggio della stella:") + theme_gray()

#densità dell'insolazione del pianeta:
ins <- ggplot(data = def) + geom_histogram(aes(x = pl_insol, y = after_stat(density)), col = "white", fill = "#A8B2C1") +
  geom_density(aes(x = pl_insol), alpha = 0.4, fill = "#A8B2C1") +
  labs(x = "Insolazione del Pianeta", title = "Densità di pl_insol:") + theme_gray()

#densità temperatura effettiva della stella:
tef <- ggplot(data = def) + geom_histogram(aes(x = st_teff, y = after_stat(density)), col = "white", fill = "#A8B2C1") +
  geom_density(aes(x = st_teff), alpha = 0.4, fill = "#A8B2C1") +
  labs(x = "Temperatura Effettiva della stella", title = "Temperatura effettiva della stellla:") + theme_gray()

grid.arrange(eqt,rag,rad,tef)

#scatter-plot:
#raggio stella vs eqtemp.
ggplot(data = def, aes(x = st_rad, y = pl_eqt)) +
  geom_point(col = "firebrick", size = 2, alpha = .6) +
  labs(x = "Raggio della Stella", y = "Temperatura di Equilibrio del Pianeta", title = "Temp. di Equilibrio del pianeta vs Raggio della stella:") +
  theme_minimal()

#pl insol vs eqtemp.
ggplot(data = def, aes(x = pl_insol, y = pl_eqt)) +
  geom_point(col = "firebrick", size = 2, alpha = .6)+
  labs(x = "Insolazione del pianeta", y = "Temperatura di Equilibrio del Pianeta", title = "Temp. di Equilibrio del pianeta vs Insolazione del Pianeta:") +
  theme_minimal()

#tmp eff. stella vs eqtemp.
ggplot(data = def, aes(x = st_teff, y = pl_eqt)) +
  geom_point(col = "firebrick", size = 2, alpha = .6)+
  labs(x = "Temperatura effettiva della stella", y = "Temperatura di Equilibrio del Pianeta", title = "Temp. di Equilibrio del pianeta vs Temp. Effettiva della stella:") +
  theme_minimal()

#stlogg vs eqtemp.
ggplot(data = def, aes(x = pl_eqt, y = st_logg)) +
  geom_point(col = "firebrick", size = 2, alpha = .6)+
  labs(x = "Temperatura di Equilibrio del Pianeta", y = "Accelerazione Gravitazionale della stella", title = "Temp. di Equilibrio del pianeta vs Acc. gravitazionale della Stella:") +
  theme_minimal()



#####################################
#ANALISI DELLE COMPONENTI PRINCIPALI:
#####################################

pca <- princomp(def[,-1])

summary(pca) #notiamo che le prime due componenti principali spiegano gran parte della varianza
screeplot(pca)

round(pca$loadings[,1:2],3)

#variabili considerate:
#pl_trandep
#pl_insol
#pl_eqt
#st_teff

variabili <- c('tfopwg_disp',  'pl_trandep', 'pl_insol', 'pl_eqt', 'st_teff')
def_pca <- def[variabili]
def_pca$tfopwg_disp <- as.factor(def_pca$tfopwg_disp)
str(def_pca)

###########################################
#PRIMO TENTATIVO DI CLASSIFICATION:########
#Proviamo ad usare come label: tfopwg_disp#
#e usando le variabili scelte tramite pca##
#oss: PROVA ANDATA MALE ###################
###########################################

#prova di classification (andata male):
set.seed(123)
smp <- sample(1:nrow(def_pca),5033)
train <- def_pca[c(smp),]
test <- def_pca[-c(smp),]
str(train)

#training:

model <- mixmodLearn(train[,-1],train$tfopwg_disp,
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                     criterion=c('CV','BIC'))
model

#viene stimato un:Gaussian_pk_L_C

#verifichiamo che sia una stima adeguata:
#ripetiamo 50 prove per verificare che sia il modello secondo la CrossValidation:
prove <- 50
nomi <- vector()
cv <- vector()
bic <- vector()
#scelta modello:
for(i in 1:prove){
  mod <- mixmodLearn(train[,-1],train$tfopwg_disp,
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                     criterion=c('CV','BIC'))
  cv[i] <- mod@results[[1]]@criterionValue[1]
  bic[i] <- mod@results[[1]]@criterionValue[2]
  nomi[i] <- mod@results[[1]]@model
}

risultati <- data.frame(modello = nomi, cv = cv, bic = bic)

risultati |> arrange(cv)

#effettivamente il modello "pk_L_C" è il migliore

#test:

mod_pred <- mixmodPredict(test[,-1], classificationRule = model["bestResult"])

pred <- as.factor(mod_pred@partition)
lab <- as.integer(test[,1])

#Confution Matrix:
confusionMatrix(pred, as.factor(lab))
#i risultati della confution matrix portano ad un'accuracy molto bassa: 0.6529 
#notiamo che il modello non alloca alcuna unità statistica nelle prime tre classi


#Effettivamente ci sono classi con poche unità statistiche:
train |> group_by(tfopwg_disp) |> summarize(n())
test  |> group_by(tfopwg_disp) |> summarize(n())

#problema: imbalanced classes, alcune classi sono poco rappresentate
#Proviamo a trovare una soluzione unendo FA e FP e usando
#il metodo dell'UnderSampling su PC (la classe sovrarappresentata)

#(oss: PROVA ANDATA MALE)

#uniamo FP e FA:
under <- def_pca |> filter(tfopwg_disp != "FA" & tfopwg_disp != "PC")

#undersampling su PC
PC <- def_pca |> filter(tfopwg_disp == "PC")
tolgo <- sample(1:4041,1000)
PC_tolte <- PC[-tolgo,]
undersamp <- rbind(under,PC_tolte)
str(undersamp)
undersamp$tfopwg_disp <- as.character(undersamp$tfopwg_disp)
undersamp$tfopwg_disp <- as.factor(undersamp$tfopwg_disp)
levels(undersamp$tfopwg_disp)

#riproviamo la classificazione:
set.seed(123)
smp <- sample(1:nrow(undersamp),4183, replace = T)
trainu <- undersamp[c(smp),]
testu <- undersamp[-c(smp),]
str(trainu)
#training:
modelu <- mixmodLearn(trainu[,-1],trainu$tfopwg_disp,
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                     criterion=c('CV','BIC'))
modelu
#scelta modello:
for(i in 1:prove){
  mod <- mixmodLearn(trainu[,-1],trainu$tfopwg_disp,
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                     criterion=c('CV','BIC'))
  cv[i] <- mod@results[[1]]@criterionValue[1]
  bic[i] <- mod@results[[1]]@criterionValue[2]
  nomi[i] <- mod@results[[1]]@model
}

risultati <- data.frame(modello = nomi, cv = cv, bic = bic)
risultati |> arrange(cv)

#test:
mod_predu <- mixmodPredict(testu[,-1], classificationRule = modelu["bestResult"])
predu <- as.factor(mod_predu@partition)

lab <- as.integer(testu[,1])
confusionMatrix(predu, as.factor(lab))
?confusionMatrix
train |> group_by(tfopwg_disp) |> summarize(n())
test  |> group_by(tfopwg_disp) |> summarize(n())

#abbiamo provato a usare un modello di classification usando come labels:
labels
#ci siamo accorti che c'è un problema di imbalanced classes, abbiamo provato a risolverlo
#utilizzando metodi di undersampling, ma con scarsi risultati, otteniamo un accuracy molto bassa,
#probabilmente le labels non sono sufficientemente spiegate dalle variabili a nostra disposizione

###############################################################
#CAMBIO DI APPROCCIO:##########################################
#Creiamo nuove labels e usiamo le variabili più correlate:#####
###############################################################

#prendiamo in considerazione solo i pianeti candidati:
def_cor <- def |> filter(tfopwg_disp == "PC") 
#scelta delle variabili:
#Scegliamo ora le variabili più correlate con pl_eqt:
#il motivo di questa scelta è che creeremo le labels utilizzando la variabile pl_eqt
def_cor <- def_cor[c("pl_eqt","st_teff","st_rad","pl_rade","pl_insol")]

########################
#labels con temperatura:

temp <- def_cor |> select(pl_eqt)

temp_lab <- as.factor(ifelse(temp<273,"Pianeti Freddi",
                             ifelse(temp<373,"Pianeti Abitabili",
                                    ifelse(temp<500,"Pianeti tiepidi",
                                           ifelse(temp < 1000, "Pianeti Caldi",
                                                  ifelse(temp <1500, "Super Terre","Estremi"))))))
def_temp <- cbind(def_cor |> select(-pl_eqt),temp_lab)

def_temp |> count(temp_lab)


colors <- c("#1B263B", "#415A77", "#6C5B7B", "#A8B2C1", "#C06C84", "#F9A825")

ggplot(data = def_temp) + geom_bar(aes(x = temp_lab, fill = temp_lab), show.legend = F) +
  scale_fill_manual(values = colors) +
  labs(x = "", y = "Frequenze assolute") + theme_minimal()

#ANALISI ESPLORATIVE: 

pairs(def_cor, pch = 20)

pairs(def_temp |> select(-temp_lab), col = def_temp$temp_lab, pch = 20)
#boxplot:

g1 <- ggplot(data = def_temp,aes(x = temp_lab, y = pl_rade)) + geom_boxplot(aes(x = temp_lab, y = pl_rade, fill = temp_lab), alpha = .7,outlier.shape = "O", outlier.size = 2) +
  scale_fill_manual(values = colors) +
  labs(x = "Labels", y = "Raggio del pianeta") +
  theme_minimal()+
  theme(legend.position = "none")

levels(temp_lab)

g2 <- ggplot(data = def_temp,aes(x = temp_lab, y = st_teff)) + 
  geom_boxplot(aes(x = temp_lab, y = st_teff,fill = temp_lab), alpha = .7,outlier.shape = "O", outlier.size = 2) +
  scale_fill_manual(values = colors) +
  labs(x = "Labels", y = "Temperatura della stella") +
  theme_minimal()+
  theme(legend.position = "none")

g3 <- ggplot(data = def_temp,aes(x = temp_lab, y = st_rad)) + 
  geom_boxplot(aes(x = temp_lab, y = st_rad, fill = temp_lab), alpha = .7,outlier.shape = "O", outlier.size = 2) +
  scale_fill_manual(values = colors) +
  labs(x = "Labels", y = "Raggio della stella") +
  theme_minimal()+
  theme(legend.position = "none")
  
  
g4 <- ggplot(data = def_temp,aes(x = temp_lab, y = pl_insol)) + 
  geom_boxplot(aes(x = temp_lab, y = pl_insol, fill=temp_lab), alpha = .7,outlier.shape = "O", outlier.size = 2) +
  scale_fill_manual(values = colors) +
  labs(x = "Labels", y = "Insolazione del pianeta") +
  theme_minimal()+
  theme(legend.position = "none")


grid.arrange(g1,g3)

grid.arrange(g1,g2,g3,g4)

#scatterplot:

ggplot(data = def_temp, aes(x = st_rad, y = st_teff, color = temp_lab)) + 
  geom_point(size = 2, alpha = 0.6) +
  scale_color_manual(values = colors) +
  facet_wrap(~ temp_lab) +
  labs(x = "Raggio della stella", y = "Temperatura della stella")+
  theme_gray()+
  theme(legend.position = "none")
  
ggplot(data = def_temp, aes(x = st_rad, y = pl_rade, color = temp_lab)) + 
  geom_point(size = 2, alpha = 0.6) +
  scale_color_manual(values = colors) +
  facet_wrap(~ temp_lab) +
  labs(x = "Raggio della stella", y = "Raggio dell pianeta")+
  theme_minimal()+
  theme(legend.position = "none")

ggplot(data = def_temp, aes(x = pl_rade, y = pl_insol)) + geom_point(size = 2,alpha = .6) +
  scale_color_manual(values = colors) +
  facet_wrap(~ temp_lab) +
  labs(x = "Raggio del pianeta", y = "Insolazione del pianeta")+
  theme_gray()+
  theme(legend.position = "none")

ggplot(data = def_temp, aes(x = st_rad, y = pl_rade)) + geom_point(size = 2, col = "#6C5B7B",alpha = .6) +
  facet_wrap(temp_lab)

ggplot(data = def_temp, aes(x = st_rad, y = pl_insol)) + geom_point(size = 2, col = "#A8B2C1",alpha = .6) +
  facet_wrap(temp_lab)


#histogram:
colors <- c("#1B263B", "#415A77", "#6C5B7B", "#A8B2C1", "#C06C84", "#F9A825")  # Colori chiari
border_colors <- c("#0F1A2A", "#2E4A5D", "#4A4564", "#7B8895", "#9E4A67", "#B38115")  # Colori scuri



ggplot(data = def_temp) + geom_histogram(aes(x = pl_rade, y = after_stat(density), col=temp_lab, fill=temp_lab,), bins = 40) +
  geom_density(aes(x = pl_rade, y = after_stat(density)), col="#4A4A4A" ,alpha = .5) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = border_colors) +
  facet_wrap(temp_lab)+
  theme_grey() +
  theme(legend.position = "none")

ggplot(data = def_temp) + geom_histogram(aes(x = st_teff, y = after_stat(density)), col = "#1B263B",fill = "#A8B2C1", bins = 40) +
  geom_density(aes(x = st_teff, y = after_stat(density)), col = "#F9A825", fill = "#F9A825", alpha = .5) + facet_wrap(temp_lab)

ggplot(data = def_temp) + geom_histogram(aes(x = st_rad, y = after_stat(density)), col = "#1B263B",fill = "#A8B2C1") +
  geom_density(aes(x = st_rad, y = after_stat(density)), col = "#F9A825", fill = "#F9A825", alpha = .5) + facet_wrap(temp_lab)


#################
#CLASSIFICATION:#
#################
#oss: NON ANCORA DEFINITIVA!!

#train:
set.seed(123)
tsm <- sample(1:4041,3233,replace = T)

train <- def_temp[tsm,]
test <- def_temp[-tsm,]

#stima modello:

set.seed(123)
stima1 <- mixmodLearn(train[,-5],train$temp_lab,
                      models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                      criterion=c('CV','BIC'))

#scelta modello:
cv <- vector()
bic <- vector()
nomi <- vector()
prove <- 50
for(i in 1:prove){
  mod <- mixmodLearn(train[,-5],train$temp_lab,
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                     criterion=c('CV','BIC'))
  cv[i] <- mod@results[[1]]@criterionValue[1]
  bic[i] <- mod@results[[1]]@criterionValue[2]
  nomi[i] <- mod@results[[1]]@model
}

risultati <- data.frame(modello = nomi, cv = cv, bic = bic)

risultati |> arrange(cv)

#utiliziamo il modello: Gaussian_pk_Lk_D_Ak_D

#prediction:

pred <- mixmodPredict(test[,-5],classificationRule = stima1["bestResult"])

predcl <- as.factor(pred@partition)

confusionMatrix(predcl,as.factor(as.integer(test$temp_lab)))
#0.8703 acc.

#otteniamo un accuracy migliore,
#la sensibility di ogni classe è buona, tranne per la classe "Pianeti Freddi"
#che probabilmente è poco rappresentata

############################
#Proviamo a risolvere 
#togliendo la labels: "Estrema"
#perchè raappresentano pianeti con temperature fuori dal normale


def_temp2 <- def_temp |> filter(temp_lab != "Estremi")

def_temp2$temp_lab <- as.integer(def_temp2$temp_lab)
def_temp2$temp_lab <- as.factor(def_temp2$temp_lab)
str(def_temp2)

def_temp2 |> count(temp_lab)

pairs(def_temp2 |> select(-temp_lab), col = def_temp2$temp_lab, pch = 20)



colors2 <- c("#1B263B", "#415A77", "#6C5B7B", "#A8B2C1", "#C06C84")


ggpairs(def_temp2 |> select(-temp_lab),aes(col = def_temp2$temp_lab),upper = list(continuous = "points"), diag = list(continuos = "barDiag")) +
  scale_color_manual(values = colors2) + scale_fill_manual(values = colors2) + theme_light()

#classification:

#train:
set.seed(123)
tsm <- sample(1:2997,2397,replace = T)
def_temp2 |> count(temp_lab)


train <- def_temp2[tsm,]
test <- def_temp2[-tsm,]

#stima modello:
set.seed(123)
stima2 <- mixmodLearn(train[,-5],train$temp_lab,
                      models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                      criterion=c('CV','BIC'))

#scelta modello:
cv <- vector()
bic <- vector()
nomi <- vector()
prove <- 50
for(i in 1:prove){
  mod <- mixmodLearn(train[,-5],train$temp_lab,
                     models=mixmodGaussianModel(family="all",equal.proportions=FALSE),
                     criterion=c('CV','BIC'))
  cv[i] <- mod@results[[1]]@criterionValue[1]
  bic[i] <- mod@results[[1]]@criterionValue[2]
  nomi[i] <- mod@results[[1]]@model
}

risultati <- data.frame(modello = nomi, cv = cv, bic = bic)

risultati |> arrange(cv)

#modello: Gaussian_pk_Lk_D_Ak_D

stima2

#prediction:

pred <- mixmodPredict(test[,-5],classificationRule = stima2["bestResult"])

predcl <- as.factor(pred@partition)

dat <- cbind(test,predcl)

conf_matrix <- confusionMatrix(predcl,as.factor(as.integer(test$temp_lab)))
#0.9316 accuracy

round(mean(predcl == as.integer(test$temp_lab)),3)


#il risultato è migliorato molto, in particolare:
#l'accuracy è migliorata e anche la sensibilità per ogni classe.

#grafico della confusion matrix:
conf_table <- as.data.frame(as.table(conf_matrix$table))




colors2 <- c("#1B263B", "#415A77", "#6C5B7B", "#A8B2C1", "#C06C84")

#classi predette
ggpairs(dat |> select(-temp_lab, - predcl),aes(col = dat$predcl),upper = list(continuous = "points")) +
  scale_color_manual(values = colors2) + scale_fill_manual(values = colors2) + theme_light()

#classi reali:
ggpairs(dat |> select(-temp_lab, - predcl),aes(col = dat$temp_lab),upper = list(continuous = "points")) +
  scale_color_manual(values = colors2) + scale_fill_manual(values = colors2) + theme_light()


# Grafico con ggplot
ggplot(conf_table, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "white") + scale_fill_gradient(low = "white", high = "firebrick") +
  geom_text(aes(label = Freq), color = "black", size = 5) + 
  labs(title = "Confusion Matrix", x = "Predicted", y = "True") +
  theme(legend.position = "none")



#######################################
#REGRESSIONE:##########################
#utilizziamo la variabile temperatura del pianete come risposta
#e come covariate le stesse della classification


#molto pesante
(regmodeqt <-stepFlexmix(pl_eqt~st_rad+pl_rade+pl_insol+st_teff,data=def_cor, k=1:10, concomitant = FLXPmultinom(~st_rad+pl_rade+pl_insol+st_teff),
                         nrep = 10, control = list(iter = 300)))


set.seed(123)
fit <- flexmix(pl_eqt~st_rad+pl_rade+pl_insol+st_teff,data=def_cor,k=2,
               concomitant = FLXPmultinom(~st_rad+pl_rade+pl_insol+st_teff), control = list(iter = 500))

summary(fit)

#parametri del modello binomiale
parameters(fit, which = "concomitant")

#parametri dei glm gaussiani
sign <- refit(fit)
summary(sign)


#$Comp.1
#Estimate  Std. Error z value  Pr(>|z|)    
#(Intercept)  1.3062e+03  5.4466e+01 23.9813 < 2.2e-16 ***
#  st_rad       8.8753e+01  1.6698e+01  5.3154 1.064e-07 ***
#  pl_rade     -5.3909e+00  1.8082e+00 -2.9813   0.00287 ** 
#  st_teff      5.7186e-02  7.8108e-03  7.3214 2.454e-13 ***
#  pl_insol     3.0586e-02  8.5848e-04 35.6285 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#$Comp.2
#Estimate Std. Error z value  Pr(>|z|)    
#(Intercept) 5.0507e+02 1.1526e+01 43.8205 < 2.2e-16 ***
#  st_rad      2.5684e+01 7.4389e+00  3.4527  0.000555 ***
#  pl_rade     3.7872e+00 5.5135e-01  6.8690 6.464e-12 ***
#  st_teff     1.3814e-02 2.4127e-03  5.7253 1.033e-08 ***
#  pl_insol    1.2909e+00 1.4310e-02 90.2097 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



cluster <- as.factor(fit@cluster)

labdef <- cbind(def_cor,cluster)

ggplot(data = labdef) + geom_boxplot(aes(x = labdef$cluster,y = pl_eqt))

ggplot(data = labdef) + geom_point(aes(x = pl_rade, y = pl_eqt, col = cluster))+
  geom_smooth(aes(x = pl_rade, y = pl_eqt, col = cluster), method = "lm")

#visualizzazione delle rette per pl_rade:
ggplot(data = labdef) +
  geom_point(aes(x = pl_rade, y = pl_eqt, col = cluster), alpha = 0.5, size = 1.5) +
  geom_smooth(aes(x = pl_rade, y = pl_eqt, col = cluster), method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#415A77", "#C06C84")) + 
  theme_minimal() +
  labs(
    x = "Raggio del pianeta",
    y = "Temperatura di equilibrio del pianeta",
    color = "Cluster"
  )

ggplot(data = labdef) +
  geom_point(aes(x = st_rad, y = pl_eqt, col = cluster), alpha = 0.5, size = 1.5) +
  geom_smooth(aes(x = st_rad, y = pl_eqt, col = cluster), method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#415A77", "#C06C84")) + 
  theme_minimal() +
  labs(
    x = "Raggio della stella",
    y = "Temperatura di equilibrio del pianeta",
    color = "Cluster"
  )

ggplot(data = labdef |> filter(st_teff < 20000)) +
  geom_point(aes(x = st_teff, y = pl_eqt, col = cluster), alpha = 0.5, size = 1.5) +
  geom_smooth(aes(x = st_teff, y = pl_eqt, col = cluster), method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#415A77", "#C06C84")) + 
  theme_minimal() +
  labs(
    x = "Temperatura di equilibrio della stella",
    y = "Temperatura di equilibrio del pianeta",
    color = "Cluster"
  )

ggplot(data = labdef ) +
  geom_point(aes(x = pl_insol, y = pl_eqt, col = cluster), alpha = 0.5, size = 1.5) +
  geom_smooth(aes(x = pl_insol, y = pl_eqt, col = cluster), method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#415A77", "#C06C84")) + 
  theme_minimal() +
  labs(
    x = "Temperatura di equilibrio della stella",
    y = "Temperatura di equilibrio del pianeta",
    color = "Cluster"
  )


