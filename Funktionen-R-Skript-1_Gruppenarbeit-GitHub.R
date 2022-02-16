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
  Modalwert <- sort(table(variable))[length(table(variable))]
  Range <- length(unique(variable))
  
  # zu Liste Zusammenfassen
  Zusammenfassung <- list("Modalwert" = Modalwert, "Range/Spannweite" = Range)
  
  return(Zusammenfassung)
}

#_______________________________________________________________________________
#(c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt

# Idee: Kreuztabelle, weil Kovarianz oder Korrelation bei kategorialen Variablen nicht geht:

#zsmhang <- function(){
#  Zusammenhang <- data.frame(c(table(MatheLK$Studienfach)), c(table(NichtMatheLK$Studienfach)))
#  rownames(Zusammenhang) <- c("Data Science", "Informatik", "Mathe", "Statistik")
#  colnames(Zusammenhang) <- c("Mathe-LK", "Kein-Mathe-LK")
#  
#  return(Zusammenhang)
#}

#hier wären die beiden Gruppen schon frei wählbar die untersuchende Var jedoch nicht
zsmhang <- function(gruppeEins, gruppeZwei){
  Zusammenhang <- data.frame(c(table(gruppeEins$Studienfach)), c(table(gruppeZwei$Studienfach)))
  rownames(Zusammenhang) <- c("Data Science", "Informatik", "Mathe", "Statistik")
  colnames(Zusammenhang) <- c("Mathe-LK", "Kein-Mathe-LK")
  
  return(Zusammenhang)
}

#_______________________________________________________________________________
#(d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
#berechnet und ausgibt

#verhältniss Alter(matrisch) und Mathe-LK(dichotrom)
#AlterMatheLK<- function(){
    #anzeigen:
#    boxplot(Alter~Studienfach, daten, 
#            main= "Altersstruktur innerhalb der Studiengaenge")
#
#    #abspeichern:
#    pdf("VergleichAlterMatheLK.pdf")
#    boxplot(Alter~Studienfach, daten, 
#            main= "Altersstruktur innerhalb der Studiengaenge")
#    dev.off()
#}

MetrischDichtotrom<- function(VarEins, VarZwei) #eingabe mit Tabelle$Spaletenname entspricht VarEins bzw. VarZwei
  {
  #anzeigen:
  boxplot(VarEins~VarZwei, daten, 
          main= "Altersstruktur innerhalb der Studiengaenge")
  
  #abspeichern:
  pdf("MetrischDichotrom.pdf")
  boxplot(VarEins~VarZwei, daten, 
          main= "Altersstruktur innerhalb der Studiengaenge")
  dev.off()
}


#_______________________________________________________________________________
#(e) Eine Funktion, die eine mindestens ordinal skalierte Variable 
#quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")

#Interesse an Mathe
umcodieren<- function(daten){
  daten$Mathematik_Interesse[daten$Mathematik_Interesse== 7]<- "sehr hoch"
  daten$Mathematik_Interesse[daten$Mathematik_Interesse== 6]<- "hoch"
  daten$Mathematik_Interesse[daten$Mathematik_Interesse== 5]<- "hoch"
  daten$Mathematik_Interesse[daten$Mathematik_Interesse== 4]<- "mittel"
  daten$Mathematik_Interesse[daten$Mathematik_Interesse== 3]<- "mittel"
  daten$Mathematik_Interesse[daten$Mathematik_Interesse== 2]<- "niedrig"
  daten$Mathematik_Interesse[daten$Mathematik_Interesse== 1]<- "niedrig"


  daten$Programmier_Interesse[daten$Programmier_Interesse==7]<- "sehr hoch"
  daten$Programmier_Interesse[daten$Programmier_Interesse==6]<- "hoch"
  daten$Programmier_Interesse[daten$Programmier_Interesse==5]<- "hoch"
  daten$Programmier_Interesse[daten$Programmier_Interesse==4]<- "mittel"
  daten$Programmier_Interesse[daten$Programmier_Interesse==3]<- "mittel"
  daten$Programmier_Interesse[daten$Programmier_Interesse==2]<- "niedrig"
  daten$Programmier_Interesse[daten$Programmier_Interesse==1]<- "niedrig"
}

#_______________________________________________________________________________
#(f)Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
#kategorialen Variablen erstellt

# Idee: Mosaikplot fuer 3 kategoriale Variablen:
# anbieten wuerde sich z.B. 
# var1: Studienfach, var2: Mathe-LK (ja/nein), var3: Interesse an Mathe/Programmieren

mosaic <- function(var1, var2, var3){
  x <- table(var1, var2, var3)
  mosaicplot(x, col = TRUE,
             main = paste("Zusammenhang zwischen", deparse1(substitute(var1)), 
                          ",", deparse1(substitute(var2)), "und", deparse1(substitute(var3))), 
             xlab = paste(deparse1(substitute(var1)), "unterteilt nach", 
                          deparse1(substitute(var3))) , 
             ylab = deparse1(substitute(var2)))
}


#_______________________________________________________________________________
# Eine Funktion, die eine kategorielle und eine dichotome Variable vergleichen


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
