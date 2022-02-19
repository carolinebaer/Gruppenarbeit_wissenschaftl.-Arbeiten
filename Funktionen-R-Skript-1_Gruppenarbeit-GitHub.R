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
  
  # zu Liste Zusammenfassen
  Zusammenfassung <- list("Modalwert" = Modalwert, "Range/Spannweite" = Range)
  
  return(Zusammenfassung)
}


#auch fuer nominale variablen:
desk_stat_nom <- function(variable){
  entropie_vec <- table(variable)
  n <- length(entropie_vec)
  rel_hfgk <- entropie_vec / length(variable)
  Entropie <- sum(rel_hfgk * log(1/rel_hfgk))
  norm_Entropie <- Entropie / log(n)
  return("normierte Entropie" = norm_Entropie)
}

#_______________________________________________________________________________
#(c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt

# Idee: Kreuztabelle, weil Kovarianz oder Korrelation bei kategorialen Variablen 
# nicht geht:

#zsmhang <- function(){
#  Zusammenhang <- data.frame(c(table(MatheLK$Studienfach)), 
#                      c(table(NichtMatheLK$Studienfach)))
#  rownames(Zusammenhang) <- c("Data Science", "Informatik", "Mathe", "Statistik")
#  colnames(Zusammenhang) <- c("Mathe-LK", "Kein-Mathe-LK")
#  
#  return(Zusammenhang)
#}

zsmhang <- function(GruppeEins, GruppeZwei){
  
  Zusammenhang <- table(GruppeEins, GruppeZwei)
  
  return(Zusammenhang)
}

#_______________________________________________________________________________
#(d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
#berechnet und ausgibt

#verhaeltniss Alter(metrisch) und Mathe-LK(dichotom)
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

MetrischDichotom<- function(VarEins, VarZwei, main) #eingabe mit Tabelle$Spaltenname 
                                       # entspricht VarEins bzw. VarZwei
                                       # Titel muss auch Variabel sein
  {
  #anzeigen:
  boxplot(VarEins~VarZwei, daten, 
          main= main)
  
  #abspeichern:
  pdf("MetrischDichotom.pdf")
  boxplot(VarEins~VarZwei, daten, 
          main= main)
  dev.off()

}


#_______________________________________________________________________________
#(e) Eine Funktion, die eine mindestens ordinal skalierte Variable 
#quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")

umkodieren <- function(Variable){
  if(max(range(Variable)) %% 3 == 0){  # wenn die Spannweite der Auspraegungen  
                                          #ohne Rest durch drei teilbar ist
    Varibale[Variable == c(min(Variable): max(range(Variable)%/%3))] <- "niedrig"
    Varibale[Variable == c(max(range(Variable))%/%3 +1 : 2*(max(range(Variable)%/%3)))] <- "mittel"
    Varibale[Variable == c(2*(max(range(Variable)%/%3)) +1 : max(Variable))] <- "hoch"
  } else 
    if(max(range(Variable)) %% 3 == 1){  #wenn der Rest der Division durch drei 1 ist     
      Varibale[Variable == c(min(Variable): max(range(Variable)%/%3))] <- "niedrig"
      Varibale[Variable == c(max(range(Variable))%/%3 +1 : 2*(max(range(Variable))%/%3))] <- "mittel"
      Varibale[Variable == c(2*(max(range(Variable))%/%3) +1 : max(Variable)-1)] <- "hoch"
      Varibale[Variable == max(Variable)] <- "sehr hoch"
    } else {                   # wenn der Rest der Division durch drei 2 ist  
      Varibale[Variable == c(min(Variable): range(Variable)%/%3)] <- "niedrig"
      Varibale[Variable == c(max(range(Variable))%/%3 +1 : 2*(max(range(Variable))%/%3))] <- "mittel"
      Varibale[Variable == c(2*(max(range(Variable))%/%3) +1 : max(Variable)-2)] <- "hoch"
      Varibale[Variable == c(max(Variable)-1 , max(Variable))] <- "sehr hoch"   
    }
}

#man muss die ganzzahlige Division verwenden. Das war der Fehler

#_______________________________________________________________________________
#(f)Eine Funktion, die eine geeignete Visualisierung von drei oder vier 
#kategorialen Variablen erstellt

# Idee: Mosaikplot fuer 3 kategoriale Variablen:
# anbieten wuerde sich z.B. 
# var1: Studienfach, var2: Mathe-LK (ja/nein), var3: Interesse an 
#  Mathe/Programmieren

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
