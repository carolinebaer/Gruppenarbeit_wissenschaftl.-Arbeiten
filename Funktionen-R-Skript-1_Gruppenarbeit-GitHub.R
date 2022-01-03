### Gruppenarbeit - wissenschaftliches Arbeiten ###
## Funktionen-R-Skript-1 ##

#_______________________________________________________________________________
#(a) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
#fuer metrische Variablen berechnet und ausgibt




#_______________________________________________________________________________
#(b) Eine Funktion, die verschiedene geeignete deskriptive Statistiken 
#fuer kategoriale Variablen berechnet und ausgibt





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



