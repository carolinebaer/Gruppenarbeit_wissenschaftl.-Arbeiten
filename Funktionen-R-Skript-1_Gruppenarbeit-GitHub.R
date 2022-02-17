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

# Idee: Kreuztabelle, weil Kovarianz oder Korrelation bei kategorialen Variablen nicht geht:

zsmhang <- function(){
  Zusammenhang <- data.frame(c(table(MatheLK$Studienfach)), c(table(NichtMatheLK$Studienfach)))
  rownames(Zusammenhang) <- c("Data Science", "Informatik", "Mathe", "Statistik")
  colnames(Zusammenhang) <- c("Mathe-LK", "Kein-Mathe-LK")
  
  return(Zusammenhang)
}

#_______________________________________________________________________________
#(d) Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer 
#den Zusammengang zwischen einer metrischen und einer dichotomen Variablen 
#berechnet und ausgibt

#verhältniss Alter(matrisch) und Mathe-LK(dichotrom)
AlterMatheLK<- function(){
    #anzeigen:
<<<<<<< Updated upstream
    boxplot(Alter~Studienfach, daten, 
            main= "Altersstruktur innerhalb der Studiengaenge")
  
    #abspeichern:
    pdf("VergleichAlterMatheLK.pdf")
    boxplot(Alter~Studienfach, daten, 
            main= "Altersstruktur innerhalb der Studiengaenge")
    dev.off()
=======
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
                                       # der main muss ja auch Variabel sein
  {
  #anzeigen:
  boxplot(VarEins~VarZwei, daten, 
          main= main)
  
  #abspeichern:
  pdf("MetrischDichotom.pdf")
  boxplot(VarEins~VarZwei, daten, 
          main= main)
  dev.off()
>>>>>>> Stashed changes
}

#_______________________________________________________________________________
#(e) Eine Funktion, die eine mindestens ordinal skalierte Variable 
#quantilbasiert kategorisiert (z.B. in "niedrig", "mittel", "hoch")

#Interesse an Mathe
<<<<<<< Updated upstream
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
=======
# umcodieren<- function(daten){
#   daten$Mathematik_Interesse[daten$Mathematik_Interesse== 7]<- "sehr hoch"
#   daten$Mathematik_Interesse[daten$Mathematik_Interesse== 6]<- "hoch"
#   daten$Mathematik_Interesse[daten$Mathematik_Interesse== 5]<- "hoch"
#   daten$Mathematik_Interesse[daten$Mathematik_Interesse== 4]<- "mittel"
#   daten$Mathematik_Interesse[daten$Mathematik_Interesse== 3]<- "mittel"
#   daten$Mathematik_Interesse[daten$Mathematik_Interesse== 2]<- "niedrig"
#   daten$Mathematik_Interesse[daten$Mathematik_Interesse== 1]<- "niedrig"
# 
#   daten$Programmier_Interesse[daten$Programmier_Interesse==7]<- "sehr hoch"
#   daten$Programmier_Interesse[daten$Programmier_Interesse==6]<- "hoch"
#   daten$Programmier_Interesse[daten$Programmier_Interesse==5]<- "hoch"
#   daten$Programmier_Interesse[daten$Programmier_Interesse==4]<- "mittel"
#   daten$Programmier_Interesse[daten$Programmier_Interesse==3]<- "mittel"
#   daten$Programmier_Interesse[daten$Programmier_Interesse==2]<- "niedrig"
#   daten$Programmier_Interesse[daten$Programmier_Interesse==1]<- "niedrig"
# }

umkodieren <- function(Variable){
  if(range(Variable) %% 3 == 0){  # wenn die Spannweite der Auspägungen  ohne Rest durch drei teilbar ist
    Varibale[Variable == c(min(Variable): range(Variable)/3)] <- "niedrig"
    Varibale[Variable == c(range(Variable)/3 +1 : 2*(range(Variable)/3))] <- "mittel"
    Varibale[Variable == c(2*(range(Variable)/3) +1 : max(Variable))] <- "hoch"
      } else 
  if(range(Variable) %% 3 == 1){ # wenn der Rest der Division durch drei 1 ist     
    Varibale[Variable == c(min(Variable): range(Variable)/3)] <- "niedrig"
    Varibale[Variable == c(range(Variable)/3 +1 : 2*(range(Variable)/3))] <- "mittel"
    Varibale[Variable == c(2*(range(Variable)/3) +1 : max(Variable)-1)] <- "hoch"
    Varibale[Variable == max(Variable)] <- "sehr hoch"
     } else {                   # wenn der Rest der Division durch drei 2 ist  
    Varibale[Variable == c(min(Variable): range(Variable)/3)] <- "niedrig"
    Varibale[Variable == c(range(Variable)/3 +1 : 2*(range(Variable)/3))] <- "mittel"
    Varibale[Variable == c(2*(range(Variable)/3) +1 : max(Variable)-2)] <- "hoch"
    Varibale[Variable == c(max(Variable)-1 , max(Variable)] <- "sehr hoch"   
        }
>>>>>>> Stashed changes
}
# ich bin mir nicht sicher ob da so richtig ist

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
