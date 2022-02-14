### Gruppenarbeit - wissenschaftliches Arbeiten ###
## Funktionen-R-Skript-1 ##
## daten <- read.csv("Datensatz.csv")
#_______________________________________________________________________________
#(a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
#fuer metrische Variablen berechnet und ausgibt

interese_mathe <- function(){
  Mittelwert <- mean(daten$Mathematik_Interesse)
  Median <- median(daten$Mathematik_Interesse)
  Varianz <- var(daten$Mathematik_Interesse)
  Standardabweichung <- sd(daten$Mathematik_Interesse)
  Quantil <- quantile(daten$Mathematik_Interesse)
  Minimum <- min(daten$Mathematik_Interesse)
  Maximum <- max(daten$Mathematik_Interesse)
  Spannweite <- Maximum - Minimum
  Interquartilsabstand <- IQR(daten$Mathematik_Interesse)
  
  uebersicht_interesse_mathe <- list("Mittelwert" = Mittelwert, "Median" = Median,
          "Varianz" = Varianz, "Standardabweichung" = Standardabweichung,
          "Quantil" = Quantil, "Minimum" = Minimum, "Maximum" = Maximum,
          "Spannweite" = Spannweite, "Interquartilsabstand" = Interquartilsabstand)
  return(uebersicht_interesse_mathe)
}

interese_progr <- function(){
  Mittelwert <- mean(daten$Programmier_Interesse)
  Median <- median(daten$Programmier_Interesse)
  Varianz <- var(daten$Programmier_Interesse)
  Standardabweichung <- sd(daten$Programmier_Interesse)
  Quantil <- quantile(daten$Programmier_Interesse)
  Minimum <- min(daten$Programmier_Interesse)
  Maximum <- max(daten$Programmier_Interesse)
  Spannweite <- Maximum - Minimum
  Interquartilsabstand <- IQR(daten$Programmier_Interesse)
  
  uebersicht_interesse_progr <- list("Mittelwert" = Mittelwert, "Median" = Median,
                                     "Varianz" = Varianz, "Standardabweichung" = Standardabweichung,
                                     "Quantil" = Quantil, "Minimum" = Minimum, "Maximum" = Maximum,
                                     "Spannweite" = Spannweite, "Interquartilsabstand" = Interquartilsabstand)
  return(uebersicht_interesse_progr)
}


#_______________________________________________________________________________
#(b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
#fuer kategoriale Variablen berechnet und ausgibt


studienfach <- function(){
  Modalwert <- sort(table(daten$Studienfach))[4]
  Range <- length(unique(daten$Studienfach)) # Anzahl der verschiedenen Auspraegungen der Variable
  
  uebersicht_studienfach <- list("Modalwert" = Modalwert, "Range" = Range)
  return(uebersicht_studienfach)
}

lk <- function(){
  Modalwert <- sort(table(daten$LK_in_Mathe))[2]
  Range <- length(unique(daten$LK_in_Mathe)) # Anzahl der verschiedenen Auspraegungen der Variable
  
  uebersicht_lk <- list("Modalwert" = Modalwert, "Range" = Range)
  return(uebersicht_lk)
}


#_______________________________________________________________________________
#(c) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammenhang zwischen zwei kategorialen Variablen berechnet ausgibt



#_______________________________________________________________________________
#(d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
#berechnet und ausgibt

#verhältniss Alter(matrisch) und Mathe-LK(dichotrom)
AlterMatheLK<- function(){
    #anzeigen:
    boxplot(Alter~Studienfach, daten, 
            main= "Altersstruktur innerhalb der Studiengaenge")
  
    #abspeichern:
    pdf("VergleichAlterMatheLK.pdf")
    boxplot(Alter~Studienfach, daten, 
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
