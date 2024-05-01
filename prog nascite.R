#Caricamento dataset
neonati <- read.csv("neonati.csv")
library(dplyr)
library(summarytools)
library(ggplot2)
library(tidyr)
library(car)
#Controllo primi valori e indici.
head(neonati)
summary(neonati)
#Controllo discrepanze e valori nulli.

var_cresc <- sort(neonati$Anni.madre)
head(var_cresc, 5)

#SOSTITUZIONE E AGGIORNAMENTO DATASET

indici_da_sost = order(neonati$Anni.madre)[1:2]
media_sostituzione <- mean(neonati$Anni.madre[-indici_da_sost])
neonati$Anni.madre[indici_da_sost] <-media_sostituzione

var_cresc2 <-sort(neonati$Anni.madre)
head(var_cresc2)

desc_table_neonati <- descr(neonati, stats.exclude = c("Tipo.parto", "Ospedale", "Sesso", "Fumatrici"))
min(neonati$Anni.madre)
summary(neonati$Anni.madre)
#Controllo variabili qualitative categoriali e calcolo percentuali.

attach(neonati)

table(Fumatrici)
freq_fum <- prop.table(table(Fumatrici))*100
freq_fum
table(Tipo.parto)
freq_parto <- prop.table(table(Tipo.parto))*100
freq_parto
table(Ospedale)
freq_osp <- prop.table(table(Ospedale))*100
freq_osp
table(Sesso)
freq_sesso <- prop.table(table(Sesso))*100
freq_sesso

## PESO NEONATI ALLA NASCITA - densità
min_peso <- min(Peso)
max_peso <- max(Peso)

ggplot(neonati, aes(x = Peso, fill=Sesso)) +
  geom_density( alpha = 0.5)+
  labs(title = "Distribuzione del peso dei neonati alla nascita", 
       x = "Peso in grammi", 
       y = "Densità") +
  scale_x_continuous(breaks = seq(0, max_peso, by=250))+
  scale_y_continuous(labels = scales::comma)

# ISTOGRAMMA ETA' MADRE ~ NUMERO OSSERVAZIONI ~ SESSO
count_data <- neonati %>%
  group_by(Anni.madre, Sesso) %>%
  summarize(count = n())

ggplot(count_data, aes(x = Anni.madre, y = count, fill = Sesso)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.6) +
  labs(x = "Età della madre",
       y = "Numero di osservazioni",
       title = "Numero di osservazioni per ogni anno della madre e sesso") +
  scale_fill_manual(values = c("blue", "pink")) +  # Imposta colori diversi per i due sessi
  scale_x_continuous(breaks = seq(min(count_data$Anni.madre),
                                  max(count_data$Anni.madre),
                                  by = 1))

# GRAFICO ETA' MADRE PESO NEONATO OK

mean_weight <- neonati %>%
  group_by(Anni.madre, Sesso) %>%
  summarize(mean_peso = mean(Peso, na.rm =TRUE))

ggplot(mean_weight,
       aes(x= Anni.madre, y= mean_peso, color= Sesso))+
       geom_line()+
       geom_point(alpha= 0.7, size= 3)+
       labs(X= "Età della madre",
            y= "Media peso del neonato",
            title = "Media peso alla nascita in relazione all'età della madre")+
  scale_x_continuous(breaks = seq(min(mean_weight$Anni.madre),
                                  max(mean_weight$Anni.madre),
                                  by = 1))

# Boxplot del peso del neonato per tipo di parto ok
ggplot(neonati, aes(x = Tipo.parto, y = Peso, fill= Sesso)) +
  geom_boxplot() +
  labs(title = "Peso del neonato per tipo di parto", 
       x = "Tipo di parto", 
       y = "Peso del neonato")

# ISTOGRAMMA PESO MEDIO PER NUM GRAVIDANZE, CON RILEVANZA PERCENTUALE OK
frequencies <- table(neonati$N.gravidanze)
total <- sum(frequencies)
percentages <- frequencies / total*100

peso_medio_gravidanza <- neonati %>% 
  group_by(N.gravidanze)%>%
  summarise(peso_medio = mean(Peso),
  count = n())

ggplot(peso_medio_gravidanza,
       aes(x = factor(N.gravidanze), y = peso_medio)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(round(percentages, 1), "%")), 
            vjust = -0.5, size = 3) +
  labs(x = "Numero di gravidanze",
       y = "Peso medio del neonato", 
       title = "Peso medio del neonato per numero di gravidanze, con rilevanza percentuale.")

# Relazione tra gestazione e peso del neonato per sesso: ok 
ggplot(neonati, aes(x=Gestazione, y= Peso, col= Sesso))+
  geom_point(position = "jitter")+
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relazione tra settimane di gestazione e peso del neonato")
  
# Relazione tra Lunghezza cranica e peso del neonati, per sesso
ggplot(neonati, aes(x = Cranio, y = Peso, color = Sesso)) +
  geom_point(position = "jitter")+
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relazione tra Larghezza Cranica e Peso dei Neonati", 
       x = "Larghezza Cranica", 
       y = "Peso")

# Boxplot del peso del neonato in base alla madre fumatrice

neonati$Fumatrici <- factor(neonati$Fumatrici, levels = c(0, 1), labels = c("No", "Si"))

ggplot(neonati, aes(x = Fumatrici, y = Peso, fill=Sesso)) +
  geom_boxplot() +
  labs(x = "Madre fumatrice", 
       y = "Peso del neonato", 
       title = "Distribuzione del peso del neonato per madre fumatrice")

#boxplot fumatrici ~ cranio OK
ggplot(neonati, aes(x = Fumatrici, y = Cranio, fill = Fumatrici)) +
  geom_boxplot() +
  labs(title = "Relazione tra fumatrice e larghezza cranica", 
       x = "Madre fumatrice", 
       y = "Lunghezza cranica")

#boxplot fumatrici ~ lunghezza
ggplot(neonati, aes(x = Fumatrici, y = Lunghezza, fill = Fumatrici)) +
  geom_boxplot() +
  labs(title = "Relazione tra fumatrice e lunghezza", 
       x = "Madre fumatrice", 
       y = "Lunghezza")

# SCATTERPLOT Rapporto tra gestazione e larghezza cranio del neonato OK
ggplot(neonati, aes(x = Gestazione, y = Cranio, col= Sesso)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Gestazione", y = "Larghezza del cranio del neonato", 
       title = "Rapporto tra gestazione e larghezza cranio del neonato")

# Relazione tra gestazione e lunghezza del neonato ok
ggplot(neonati, aes(x = Gestazione, y = Lunghezza, col = Sesso)) +
  geom_point(position = "jitter") +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(x = "Gestazione", 
       y = "Lunghezza del neonato", 
       title = "Relazione tra gestazione e lunghezza del neonato")
#################################################
#################################################
#################################################

# Medie della popolazione
media_peso <- 3400
media_peso_femmine <- 3300
media_peso_maschi <- 3450
media_lunghezza <- 500
#Test di Shapiro per controllare la normalità dei dati di Peso.
shapiro.test(Peso)
shapiro.test((Peso[Sesso=="F"]))
shapiro.test((Peso[Sesso=="M"]))
# Wilcoxon per peso
Wilcox_peso <- wilcox.test(Peso, mu=media_peso)
Wilcox_peso_femmine <- wilcox.test(Peso[Sesso=="F"], mu = media_peso_femmine)
wilcox_peso_maschi <- wilcox.test(Peso[Sesso=="M"], mu = media_peso_maschi)
#Test di Shapiro per controllare la normalità dei dati di Peso
shapiro.test(Lunghezza)
shapiro.test(Lunghezza[Sesso=="F"])
shapiro.test(Lunghezza[Sesso=="M"])
# wilcox PER LUNGHEZZA

wilcox_lunghezza <- wilcox.test(Lunghezza, mu = media_lunghezza)  
wilcox_lunghezza_femmine <-wilcox.test(Lunghezza[Sesso=="F"], mu = media_lunghezza)
wilcox_lunghezza_maschi <- wilcox.test(Lunghezza[Sesso == "M"], mu = media_lunghezza)  

#Shapiro test per Cranio
shapiro.test(Cranio)
shapiro.test(Cranio[Sesso=="F"])
shapiro.test(Cranio[Sesso=="M"])
#Wilcoxon per cranio
media_cranio <- 350

wilcox_cranio <- wilcox.test(Cranio, mu = media_cranio)  
wilcox_cranio_femmine <-wilcox.test(Cranio[Sesso=="F"], mu = media_cranio)
wilcox_cranio_maschi <- wilcox.test(Cranio[Sesso == "M"], mu = media_cranio)  

# IPOTESI PARTI CESAREI TRAMITE CHI-QUADRO

cont_table2 <- neonati %>%
  count(Tipo.parto, Ospedale) %>%
  spread(key = Ospedale, value = n)

chi_square <- chisq.test(cont_table2[-1])
#############################################
#############################################
#############################################

options(scipen = 0)
n <-length(neonati)
# Selezione delle colonne numeriche
dati_numerici <- neonati[,sapply(neonati, is.numeric)]

# Calcolo della matrice di correlazione
matrice_correlazione <- cor(dati_numerici, use = "complete.obs")  # 'use = "complete.obs"' gestisce i valori mancanti

library(corrplot)
corrplot(matrice_correlazione, method = "color", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

?pairs

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
x11()
neonati_numerici <- neonati[, sapply(neonati, is.numeric)]
pairs(neonati_numerici, upper.panel = panel.smooth)

mod1 <- lm(Peso ~ . , data=neonati)
summary(mod1)

mod2 <- update(mod1,~.-Anni.madre)
summary(mod2)
anova(mod2,mod1)
BIC(mod2,mod1)
car::vif(mod2)

mod3 <- update(mod2,~.-Ospedale)
summary(mod3)
anova(mod3,mod2)
BIC(mod3,mod2,mod1)
car::vif(mod3)

mod4 <- update(mod3,~.-Tipo.parto)
summary(mod4)
anova(mod4,mod3)
BIC(mod4,mod3,mod2,mod1)
car::vif(mod4)

mod4nofum <- update(mod4,~.-Fumatrici)
summary(mod4nofum)
anova(mod4nofum, mod4)
BIC(mod4nofum,mod4,mod3,mod2,mod1)
car::vif(mod4nofum)

mod5 <- lm(Peso~ N.gravidanze+Fumatrici+Gestazione + Lunghezza + Gestazione*Lunghezza+Cranio+Sesso, data=neonati)
summary(mod5)

mod7 <- lm(Peso~N.gravidanze+Fumatrici+Lunghezza*Cranio+Lunghezza+ Cranio+Gestazione+Sesso, data=neonati)
summary(mod7)

mod8 <- lm(Peso~ N.gravidanze+Fumatrici+Gestazione+Cranio+Sesso*Lunghezza+Sesso+Lunghezza, data=neonati)
summary(mod8)

mod9 <- lm(Peso~ N.gravidanze+Fumatrici+Gestazione+Lunghezza+Sesso*Cranio+Sesso+Cranio, data=neonati)
summary(mod9)

mod_non_lin1 <- lm(Peso~ N.gravidanze + Fumatrici + Gestazione + Cranio + I(Cranio^2) + Lunghezza + 
                     Sesso, data = neonati)
summary(mod_non_lin1)

mod_non_lin2 <- lm(Peso~ N.gravidanze + Fumatrici + Gestazione + Cranio + Lunghezza + I(Lunghezza^2) + 
                     Sesso, data = neonati)
summary(mod_non_lin2)
anova(mod_non_lin2, mod4)
BIC(mod_non_lin2,mod4)
car::vif(mod_non_lin2)

#analisi dei residui, ricorda di ricontrollare i modelli

par(mfrow=c(2,2))
plot(mod4)

shapiro.test(residuals(mod4)) 
lmtest::bptest(mod4)
lmtest::dwtest(mod4) 

plot(density(residuals(mod4)))

#leverage
lev <- hatvalues(mod4)
plot(lev)
p <- sum(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

#outliers
plot(rstudent(mod4))
abline(h=c(-2,2), col=2)
car::outlierTest(mod4)

righe_interessate <- neonati[c(1551, 155, 1306, 310), ]
print(righe_interessate)
#distanza di cook
cook <-cooks.distance(mod4)
plot(cook)

max(cook)

nuovi_neonatifumSI <- data.frame(
  N.gravidanze = 3,
  Gestazione = 39,
  Fumatrici = 1,
  Sesso = "F",
  Lunghezza = mean(Lunghezza),
  Cranio = mean(Cranio)
  
  )

previsionifumSI <- predict(mod4, newdata=nuovi_neonatifumSI)

nuovi_neonatifumNO <- data.frame(
  N.gravidanze = 3,
  Gestazione = 39,
  Fumatrici = 0,
  Sesso = "F",
  Lunghezza = mean(Lunghezza),
  Cranio = mean(Cranio)
  
)

previsionifumNO <- predict(mod4, newdata=nuovi_neonatifumNO)

crPlots(mod4)

######
#MODELLO SEMPLIFICATO USANDO LE VARIABILI CON p-values più bassi

mod_simpl <- lm(Peso ~ Lunghezza + Cranio, data = neonati)
summary(mod_simpl)
library(plotly)
df_plot <- neonati
df_plot$Peso_predetto <- predict(mod_simpl, newdata = neonati)

# Creare il grafico
plot_ly(data = df_plot, x = ~Lunghezza, y = ~Cranio, z = ~Peso_predetto,
               type = 'scatter3d', mode = 'markers',
               marker = list(size = 2, color = ~Gestazione, colorscale='Viridis', opacity = 0.8))


##
##Creazione di un nuovo modello
##
neonati2 <- neonati[-c(310, 1551), ]

mod4plus <- lm(formula = Peso ~ N.gravidanze + Fumatrici + Gestazione + Lunghezza + 
                 Cranio + Sesso, data = neonati2)
summary(mod4plus)

#analisi dei residui, ricorda di ricontrollare i modelli

par(mfrow=c(2,2))
plot(mod4plus)

shapiro.test(residuals(mod4plus)) 
lmtest::bptest(mod4plus)
lmtest::dwtest(mod4plus) 

plot(density(residuals(mod4plus)))

#leverage
n <- length(neonati2)
lev <- hatvalues(mod4plus)
plot(lev)
p <- sum(lev)
soglia=2*p/n
abline(h=soglia,col=2)
lev[lev>soglia]

#outliers
plot(rstudent(mod4plus))
abline(h=c(-2,2), col=2)
car::outlierTest(mod4plus)

#distanza di cook
cookplus <-cooks.distance(mod4plus)
plot(cookplus)
max(cookplus)

indice_max_cook <- which(cookplus == max(cookplus))

# Stampa l'indice
print(indice_max_cook)

plus_neonatifumSI <- data.frame(
  N.gravidanze = 3,
  Gestazione = 39,
  Fumatrici = 1,
  Sesso = "F",
  Lunghezza = mean(Lunghezza),
  Cranio = mean(Cranio)
  
)

plus_previsionifumSI <- predict(mod4plus, newdata=plus_neonatifumSI)

plus_neonatifumNO <- data.frame(
  N.gravidanze = 3,
  Gestazione = 39,
  Fumatrici = 0,
  Sesso = "F",
  Lunghezza = mean(Lunghezza),
  Cranio = mean(Cranio)
  
)

plus_previsionifumNO <- predict(mod4plus, newdata=plus_neonatifumNO)

crPlots(mod4plus)

#MODELLO SEMPLIFICATO USANDO LE VARIABILI CON p-values più bassi
# creato dal modello pulito

mod_simpl_plus <- lm(Peso ~ Lunghezza + Cranio, data = neonati2)
summary(mod_simpl_plus)
library(plotly)
df_plot2 <- neonati2
df_plot2$Peso_predetto <- predict(mod_simpl_plus, newdata = neonati2)

# Creare il grafico
plot_ly(data = df_plot2, x = ~Lunghezza, y = ~Cranio, z = ~Peso_predetto,
        type = 'scatter3d', mode = 'markers',
        marker = list(size = 2, color = ~Gestazione, colorscale='Viridis', opacity = 0.8))
