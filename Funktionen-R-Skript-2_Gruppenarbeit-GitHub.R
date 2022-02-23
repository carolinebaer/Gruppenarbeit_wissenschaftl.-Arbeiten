### Gruppenarbeit - wissenschaftliches Arbeiten ###
## Funktionen-R-Skript-2 ##

#(Aufgabenstellung) Funkionen-R-Skript 2 soll Helfer-Funktionen enthalten, die 
#nicht selbst zur Deskription und Visualisierung der Daten verwendet werden, 
#sondern die nur in Funktionen-Skript 1 Anwendung finden (interne Funktionen).
#Funktionen-R-Skript 2 muss mindestens eine Funktion enthalten.

#(EMail) Manche Berechnungen benoetigt ihr moeglicherweise fuer mehrere 
#Funktionen.  
#Da ihr diese Berechnung nicht in jeder Funktion neu implementieren moechtet, 
#koennt ihr dafuer eine so genannte Hilfsfunktion schreiben, die ihr dann 
#innerhalb der eigentlichen Funktionen immer wieder verwenden koennt. Diese 
#Hilfsfunktionen sollen in ein extra R-Script geschrieben werden.
#_______________________________________________________________________________
# erstellen von Unterdatensaetzen:
MatheLK <- subset(daten, LK_in_Mathe == "ja")
NichtMatheLK <- subset(daten, LK_in_Mathe == "nein")

Statistiker <- subset(daten, Studienfach == "Statistik")
Data_Scientists <- subset(daten, Studienfach =="Data Science")
Mathematiker <- subset(daten, Studienfach == "Mathe")
Informatiker <- subset(daten, Studienfach == "Informatik")

#"ja" und "nein" in 1 und 0 umkodieren (geht bestimmt auch kuerzer und einfacher;) )
daten_dichtom_kodiert <- daten
daten_dichtom_kodiert$LK_in_Mathe[daten_dichtom_kodiert$LK_in_Mathe == "ja"] <- 1
daten_dichtom_kodiert$LK_in_Mathe[daten_dichtom_kodiert$LK_in_Mathe == "nein"]<-0
daten_dichtom_kodiert$LK_in_Mathe <-as.numeric(daten_dichtom_kodiert$LK_in_Mathe)


### Hilfsfunktion

# erstellt einen leeren Vektor mit Laenge einer Variable
# anwendung in kod_quantile

leer <- function(variable){
   leer<- rep(0, length(variable)) 
}













