### Gruppenarbeit - wissenschaftliches Arbeiten ###
## Funktionen-R-Skript-1 ##
## daten <- read.csv("Datensatz.csv")
#_______________________________________________________________________________
#(a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
#fuer metrische Variablen berechnet und ausgibt

# Funktion fuer Lage- und Streuungsmasse von metrischen Variablen:
Lage_und_Streuung <- function(variable){
  # Lagemasse:
  Mittelwert <- mean(variable)
  Median <- median(variable)
  Minimum <- min(variable)
  Maximum <- max(variable)
  Quantile <- quantile(variable)
  
  # Streuungsmasse:
  Varianz <- var(variable)
  Standardabweichung <- sd(variable)
  Spannweite <- Maximum - Minimum
  Interquartilsabstand <- IQR(variable)
  
  # zu Liste zusammenfuegen:
  Zusammenfassung <- list("Mittelwert" = Mittelwert, 
                          "Median" = Median, 
                          "Minimum" = Minimum, "Maximum" = Maximum, 
                          "Quantile" = Quantile, "Varianz" = Varianz, 
                          "Standardabweichung" = Standardabweichung,
                          "Spannweite" = Spannweite, 
                          "Interquartilsabstand" = Interquartilsabstand)
  return(Zusammenfassung)
}

#_______________________________________________________________________________
#(b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
#fuer kategoriale Variablen berechnet und ausgibt

desk_stat_k <- function(variable){
  #Lagemass:
  Modalwert <- sort(table(variable))[length(table(variable))]
  
  #Streuungsmass:
  Range <- length(unique(variable))
  StichprobenUmfrang<- length(variable)
  
  # zu Liste Zusammenfassen
  Zusammenfassung <- list("Modalwert" = Modalwert, "Range/Spannweite" = Range, 
                          "Stichprobenumfang" = StichprobenUmfrang,
                          "Haeufigkeitstabelle" = table(variable))
  
  return(Zusammenfassung)
}


#auch fuer nominale variablen:
desk_stat_nom <- function(variable){
  entropie_vec <- table(variable)
  n <- length(entropie_vec)
  rel_hfgk <- entropie_vec / length(variable)
  Entropie <- sum(rel_hfgk * log(1/rel_hfgk))
  norm_Entropie <- Entropie / log(n)
  return(c("normierte Entropie" = norm_Entropie))
}

#_______________________________________________________________________________
#(c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt

# Idee: Kreuztabelle, weil Kovarianz oder Korrelation bei kategorialen Variablen 
# nicht geht:

zsmhang <- function(GruppeEins, GruppeZwei, names = c("GruppeEins", "GruppeZwei")){
  
  Zusammenhang <- table(GruppeEins, GruppeZwei, dnn = names)
  
  return(Zusammenhang)
}

#_______________________________________________________________________________
#(d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
#berechnet und ausgibt


MetrischDichotom<- function(VarMetrisch, VarDichotom) #eingabe mit 
                                                        #Tabelle$Spaltenname 
{
  #anzeigen:
  boxplot(VarMetrisch~VarDichotom, daten, 
          main= paste("Boxplot: Zusammenhang zwischen", 
                      deparse1(substitute(VarMetrisch)), 
                      "und", deparse1(substitute(VarDichotom))),
          xlab = deparse1(substitute(VarDichotom)),
          ylab = deparse1(substitute(VarMetrisch)))
  
  #abspeichern:
  pdf("MetrischDichotom.pdf")
  boxplot(VarMetrisch~VarDichotom, daten, 
          main = paste("Boxplot: Zusammenhang zwischen", 
                       deparse1(substitute(VarMetrisch)), 
                       "und", deparse1(substitute(VarDichotom))),
          xlab = deparse1(substitute(VarDichotom)),
          ylab = deparse1(substitute(VarMetrisch)))
  dev.off()

}

metrischDichotom_werte <- function(VarMetrisch, VarDichotom){
  Korrelation <- cor(VarMetrisch, VarDichotom)
  return(c("Korrelation" = Korrelation))
}


#_______________________________________________________________________________
#(e) Eine Funktion, die eine mindestens ordinal skalierte Variable 
#quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")


# Versuch quantilbasiert:
# man koennte dann bspsw. eine neue Spalte an den Datensatz anhaengen:
# daten$Mathe_kod <- kod_quantile(daten$Mathematik_Interesse)

kod_quantile <- function(variable){
  kodiert <- leer(variable)  # leeren Vektor erstellen
  for(i in 1:length(kodiert)){
    if(variable[i] <= quantile(variable, 0.25)){kodiert[i] <- "niedrig"} else
      if(variable[i] >= quantile(variable, 0.75)){kodiert[i] <- "hoch"} else
        kodiert[i] <- "mittel"
      }
  return(kodiert)
}

#_______________________________________________________________________________
#(f)Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
#kategorialen Variablen erstellt

# Idee: Mosaikplot fuer 3 kategoriale Variablen:

mosaic <- function(var1, var2, var3){
  x <- table(var1, var2, var3)
  mosaicplot(x, col = TRUE,
             main = paste("Zusammenhang zwischen", deparse1(substitute(var1)), 
                          ",", deparse1(substitute(var2)), "und", 
                          deparse1(substitute(var3))), 
             xlab = paste(deparse1(substitute(var1)), "unterteilt nach", 
                          deparse1(substitute(var3))) , 
             ylab = deparse1(substitute(var2)))
}


#_______________________________________________________________________________
# Eine Funktion, die eine kategorielle und eine dichotome Variable vergleicht


function(daten){
  # #welche Studiengaenge vorhanden:
  # unique(daten$Studienfach)
  # #[1] "Data Science" "Statistik"    "Informatik"   "Mathe" 
  #Caro: ist doch eig. schon vorgegeben, dass es nur diese Faecher gibt, oder?
  
  pdf("VergleichStudienfachMatheLK.pdf")
  studienfach<-matrix(c(table(MatheLK$Studienfach), 
                        table(NichtMatheLK$Studienfach)), nrow= 2, byrow= T)
  barplot((studienfach), beside=TRUE, ylim= c(0,25), col = c("red", "blue"), 
          names.arg= c("Data Science","Informatik", "Mathe","Statistik"), 
          main= "Aufteilung nach Studienfach und Belegung des Mathe-LK", 
          xlab= "Studienfach", ylab= "Absolute Haeufigkeit")
  legend("top", fill = c("red", "blue"), box.lty=0, c("ja", "nein"))
  dev.off()
}
#_______________________________________________________________________________
#Freiwillig: weitere zur Deskription und Visualisierung geeignete 
#Funktionen


