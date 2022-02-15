### Gruppenarbeit - wissenschaftliches Arbeiten ###
## Funktionen-R-Skript-2 ##

#(Aufgabenstellung) Funkionen-R-Skript 2 soll Helfer-Funktionen enthalten, die 
#nicht selbst zur Deskription und Visualisierung der Daten verwendet werden, 
#sondern die nur in Funktionen-Skript 1 Anwendung finden (interne Funktionen).
#Funktionen-R-Skript 2 muss mindestens eine Funktion enthalten.

#(EMail) Manche Berechnungen benoetigt ihr moeglicherweise fuer mehrere Funktionen.  
#Da ihr diese Berechnung nicht in jeder Funktion neu implementieren moechtet, 
#koennt ihr dafuer eine so genannte Hilfsfunktion schreiben, die ihr dann 
#innerhalb der eigentlichen Funktionen immer wieder verwenden könnt. Diese 
#Hilfsfunktionen sollen in ein extra R-Script geschrieben werden.
#_______________________________________________________________________________
# erstellen von Unterdatensaetzen:
MatheLK <- subset(daten, LK_in_Mathe == "ja")
NichtMatheLK <- subset(daten, LK_in_Mathe == "nein")

Statistiker <- subset(daten, Studienfach == "Statistik")
Data_Scientists <- subset(daten, Studienfach =="Data Science")
Mathematiker <- subset(daten, Studienfach == "Mathe")
Informatiker <- subset(daten, Studienfach == "Informatik")


# Hilfsfunktion fuer deskriptive Statistiken in a):
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
