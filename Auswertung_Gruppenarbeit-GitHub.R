### Gruppenarbeit - wissenschaftliches Arbeiten ###
## Auswertung der Daten ##

# Datensatz laden
library(openxlsx)
daten <- read.csv("Datensatz.csv")


# Auswetung der Daten mit Hilfe der Funktionen aus Skript 1

#Allgemeine Altersstruktur des Datensatzes
boxplot(daten$Alter, main= "Altersstruktur des Datensatzes", ylab= "Alter in Jahren")
barplot(table(daten$Alter),  main= "Altersstruktur des Datensatzes", xlab= "Alter in Jahren", 
        ylab= "Absolute Anzahl", ylim= c(0, 30))
barplot(table(daten$Alter)/nrow(daten),  main= "Altersstruktur des Datensatzes", 
        xlab= "Alter in Jahren", ylab= "Relative Anzahl", ylim= c(0, 0.25))

## Die groeßte Altersgruppe bilden die 25-jaehrigen (24%).
## Die juengste Person ist 21 Jahre alt, 25 % der Personen ins maximal 24 Jahre alt.
## Die aelteste Person ist 31 Jahre alt. Dies stellt einen Ausreißer da, 
## da 75% der Stichprobe maximal 29 Jahre alt sind.

# Unterdatensaetze erstellen
Statistiker <- subset(daten, Studienfach == "Statistik")
Data_Scientists <- subset(daten, Studienfach =="Data Science")
Mathematiker <- subset(daten, Studienfach == "Mathe")
Informatiker <- subset(daten, Studienfach == "Informatik")

# Funktion a
Lage_und_Streuung(daten$Alter)
#$Mittelwert
#[1] 24.76
#
#$Median
#[1] 25
#
#$Minimum
#[1] 21
#
#$Maximum
#[1] 31
#
#$Quantile
#0%   25%   50%   75%  100% 
#21.00 23.75 25.00 26.00 31.00 
#
#$Varianz
#[1] 4.022626
#
#$Standardabweichung
#[1] 2.005649
#
#$Spannweite
#[1] 10
#
#$Interquartilsabstand
#[1] 2.25




Lage_und_Streuung(Statistiker$Alter)
#$Mittelwert
#[1] 25
#
#$Median
#[1] 25
#
#$Minimum
#[1] 21
#
#$Maximum
#[1] 31
#
#$Quantile
#0%  25%  50%  75% 100% 
#21   23   25   26   31 
#
#$Varianz
#[1] 5.333333
#
#$Standardabweichung
#[1] 2.309401
#
#$Spannweite
#[1] 10
#
#$Interquartilsabstand
#[1] 3

Lage_und_Streuung(Data_Scientists$Alter)
#$Mittelwert
#[1] 24.60606
#
#$Median
#[1] 25
#
#$Minimum
#[1] 21
#
#$Maximum
#[1] 29
#
#$Quantile
#0%  25%  50%  75% 100% 
#21   23   25   26   29 
#
#$Varianz
#[1] 4.621212
#
#$Standardabweichung
#[1] 2.1497
#
#$Spannweite
#[1] 8
#
#$Interquartilsabstand
#[1] 3


Lage_und_Streuung(Mathematiker$Alter)
#$Mittelwert
#[1] 25
#
#$Median
#[1] 25
#
#$Minimum
#[1] 22
#
#$Maximum
#[1] 27
#
#$Quantile
#0%   25%   50%   75%  100% 
#22.00 24.75 25.00 26.00 27.00 
#
#$Varianz
#[1] 2.181818
#
#$Standardabweichung
#[1] 1.477098
#
#$Spannweite
#[1] 5
#
#$Interquartilsabstand
#[1] 1.25

Lage_und_Streuung(Informatiker$Alter)
#$Mittelwert
#[1] 24.47619
#
#$Median
#[1] 24
#
#$Minimum
#[1] 21
#
#$Maximum
#[1] 27
#
#$Quantile
#0%  25%  50%  75% 100% 
#21   24   24   25   27 
#
#$Varianz
#[1] 2.261905
#
#$Standardabweichung
#[1] 1.503963
#
#$Spannweite
#[1] 6
#
#$Interquartilsabstand
#[1] 1

#Alterstruktur des Datensatzes seperiert nach Studienfaechern
boxplot(Alter~Studienfach, daten, main= "Alterstruktur des Datensatzes seperiert nach Studienfach")

##die Daten von Data Science und Statistik sind sich ähnlich
# Median beide male bei 25
# minimum bei 21
# max bei 29, 31 -> unetrschiedlicher Interquartilsabstand
# var 4.8 und 5.33 -> unterscheidung lediglich 11% mehr von Stat zu DS
#  standartabweichung von 2.149 zu 2.31 -> 7.5% mehr bei Stat zu DS
# interquartilsabstand bei 3

## Mathemathik und Informatik ebenso
# min 22 bzw. 21
# max 27 -> jünger als bei Statistik bzw. SC
# var bei 2.26 zu 2.16 -> 4.6% unterschied
# var: 1.504 zu 1.477 -> 1.828%
# interquartilsabstand: 1 zu 1.25 -> 25%

#alle 
#median immer bei 25 außer Informatik(24)

##Gesamtauswetung:
# Die Altersstruktur innerhalb der Studiengänge Mathematik und Informatik, sowie Statistik und Data Science sind ähnlich
# hier bei liegt der Median bei allen Studiengängen, ausgenommen Informatik(24) bei 25.
# Ebenso liegt das Minimum bei 21 außer bei Mathematik (22)
# die Mathematiker sind jedoch durchschnittlich die jüngsten wenn man die Werte der Quantile betrachetet

# Angaben in Jahren

#Funktion b
desk_stat_k(daten$LK_in_Mathe)
#$Modalwert
#nein 
#53 

#$`Range/Spannweite`
#[1] 2

#mindestens 50% der SP hatten keinen MatheLK (55 von 100)


desk_stat_k(Statistiker$LK_in_Mathe)
#$Modalwert
#ja 
#22 

#$`Range/Spannweite`
#[1] 2
#$Stichprobenumfang
#[1] 34

#die Mehrheit der Statistiker hatte Mathe LK-> 22 von 34 -> 0.6471

desk_stat_k(Data_Scientists$LK_in_Mathe)
#$Modalwert
#nein 
#22 
#
#$`Range/Spannweite`
#[1] 2
#
#$Stichprobenumfang
#[1] 33

# die meisten der Data Scientisten hatten keinen MatheLK -> lediglich 11 von 33 -> 1/3


desk_stat_k(Mathematiker$LK_in_Mathe)
#$Modalwert
#ja 
#8 
#
#$`Range/Spannweite`
#[1] 2
#
#$Stichprobenumfang
#[1] 12

##die Mehrheit der Mathematik studenten hatte Mathe LK (8 von 12) -> 2/3

desk_stat_k(Informatiker$LK_in_Mathe)
#$Modalwert
#nein 
#15 
#
#$`Range/Spannweite`
#[1] 2
#
#$Stichprobenumfang
#[1] 21
## die meisten der Informatiker hatten keinen Mathe-Lk lediglich (15/21) ->  0.2857143

## es ist bekannt, das Mathematik und Statistik einen höheren Matheanteil besitzen als Informatik und Data Science
##-> je höher der Matheanteil in einem Studium ist, desto größer ist der Anteil der Studierenden die vorher den MatheLK belegt haben



#c)
# Interesse an Mathematik und Programmieren kategorisieren und an den Datensatz anhängen
daten$Mathe_kod <- kod_quantile(daten$Mathematik_Interesse)
daten$Info_kod <- kod_quantile(daten$Programmier_Interesse)


##Zusammenhang zwischen dem Studienfach und Interesse an Mathematik

zsmhang(daten$Studienfach, daten$Mathe_kod, names= c("Studienfach", "Interesse an Mathmatik"))
# Interesse an Mathmatik
# Studienfach    hoch mittel niedrig
# Data Science   10      4      19
# Informatik      5      1      15
# Mathe          11      1       0
# Statistik      21     10       3

pdf("Zsmhang.Studienfach.Mathe_Int.pdf")
mosaicplot(zsmhang(daten$Studienfach, daten$Mathe_kod, 
                   names= c("Studienfach", "Interesse an Mathmatik")), 
           main = "Zusammenhang Studienfach und Interesse an Mathmatik")
dev.off()

# Mathematiker weisen das hoechstes Interesse an Mathematik auf. Es sind fast alle 
# Mathestudierende sehr an Mathematik Interessiert.
# Das zweithoechste Interesse an Mathematik weisen die Statistikstudierenden auf.
# Die Data Science-Studierende haben ein geringeres Interesse an Mathematik als die
# Statik-Studierenden, aner Ihr Interesse an Mathematik ist etwas hoeher als das der 
# Informatik-Studierenden.


##Zusammenhang zwischen dem Studienfach und Interesse an Programmieren

zsmhang(daten$Studienfach, daten$Info_kod, names= c("Studienfach", "Interesse an Programmieren"))
# GruppeZwei
# GruppeEins     hoch mittel niedrig
# Data Science    4     26       3
# Informatik     15      6       0
# Mathe           1      2       9
# Statistik       8     12      14

pdf("Zsmhang.Studienfach.Prog_Int.pdf")
mosaicplot(zsmhang(daten$Studienfach, daten$Info_kod, 
                   names= c("Studienfach", "Interesse an Programmieren")),
           main = "Zusammenhang Studienfach und Interesse an Mathmatik")
dev.off()

# Die Informatiker weisen das hoechste Interesse an Programmieren auf. Keiner der
# Informatikstudierenden ist weist ein niederiges Interesse an Programmieren auf.
# Der Großteil der Data-Science-Studierenden ist mittelhoch an Programmieren interessiert.
# Die Statistikstudierenden weisen ein deutlich geringeres Interesse an Informatik auf.
# Die Mathematik-Studierende haben ein sehr niedriges Interesse an Programmieren.

## d)
## Zusammenhang zwischen Mathematik/Programmier Interesse und ob der Studierende Mathe-Lk hatte

MetrischDichotom(daten$Mathematik_Interesse, daten$LK_in_Mathe)
# # Die Interesse an Mathematik ist bei den Mathe Lker höher als der den Nicht-Mathe Lker. Der Median der Mathe LKer liegt bei 5
# # und das obere bzw das untere Quartil liegt bei 6 bzw. bei 4.5. Die Werte bei den Nicht Mathe LKer sind
# # viel niedriger, nämlich liegt der Median bei 3 und das obere bzw das untere Quartil liegt bei 4 bzw. bei 3. Außerdem hat keiner der Mathe LK-Schueler*innen
# # die niedrigste Bewertung ausgewählt.
# 
MetrischDichotom(daten$Programmier_Interesse, daten$LK_in_Mathe)
# # Die Personen, die Mathe LK haben zeigen weniger Interesse an prorammieren als die Personen die keinen Mathe LK haben,
# # Der Median bei den nicht Mathe LKer liegt bei 6 und das obere bzw. das untere Quartil liegt bei 7 bzw. bei 5. Die Werte 
# # sind bei den Mathe LKer niedriger, nämlich ist der Median bei 5 und das obere bzw. das untere Quartil liegt bei 6.5 und 3.


# Math-Lk umkodieren
daten_dichtom_kodiert <- daten
daten_dichtom_kodiert$LK_in_Mathe[daten_dichtom_kodiert$LK_in_Mathe == "ja"] <- 1
daten_dichtom_kodiert$LK_in_Mathe[daten_dichtom_kodiert$LK_in_Mathe == "nein"]<-0
daten_dichtom_kodiert$LK_in_Mathe <-as.numeric(daten_dichtom_kodiert$LK_in_Mathe)

metrischDichotom_werte(daten_dichtom_kodiert$Mathematik_Interesse, daten_dichtom_kodiert$LK_in_Mathe)
# Korrelation 
# 0.5736662 

## Es gibt eine positive Korrelation zwischen dem Mathematik-Interesse und einer Mathe-LK-Wahl

metrischDichotom_werte(daten_dichtom_kodiert$Programmier_Interesse, daten_dichtom_kodiert$LK_in_Mathe)
# Korrelation 
# -0.1714027 

## Es gibt eine schwache negative Korrelation zwischen dem Programmier-Interesse und einer Math-LK-Wahl



## f)
## Zusammenhang zwischen Studienfach, Mathematikinteresse/Programmierinteresse und Mathe LK Wahl
Studienfach <- daten_dichtom_kodiert$Studienfach
Mathematik_Interesse <- kod_quantile(daten_dichtom_kodiert$Mathematik_Interesse)
Programmier_Interesse <- kod_quantile(daten_dichtom_kodiert$Programmier_Interesse)
LK_in_Mathe <- daten$LK_in_Mathe

pdf("Zusammenhang zwischen Studienfach, Programmierinteresse und Mathe LK Wahl")
mosaic(Studienfach, Mathematik_Interesse, LK_in_Mathe)
dev.off()

## Das Interesse an Mathematik ist bei den Mathestudierenden am höchsten. Bei den Statistikstudierenden ist es am zweithöchsten.
## Bei den Informatikstudierenden ist das Interesse an Mathematik am niedrigsten.
## Bei allen vier Studiengängen lässt sich sagen, dass je höher das Interesse an Mathematik besteht, umso höher 
## ist der Anteil der Leute, die Mathe-LK hatten.

pdf("Zusammenhang zwischen Studienfach, Mathematikinteresse und Mathe LK Wahl")
mosaic(Studienfach,Programmier_Interesse, LK_in_Mathe)
dev.off()

## Das Interesse am Programmieren ist bei den Informatikstudierenden am höchsten und bei den Mathestudierenden am niedrigsten.
## Bei den Data Scientist-Studierenden hat die Mehrheit ein mittleres Interesse und bei den Statistik-Studierenden sind die drei Kategorien ausgeglichen verteilt.
## Je höher das Interesse am Programmieren bei den Mathestudierenden ist, umso kleiner ist der Anteil der Leute die Mathe-LK hatten. Bei den Informatikstudierenden
## ist das Gegenteil der Fall. Bei den Statistikstudierenden ist der Anteil der Leute die Mathe-LK hatten bei allen drei Kategorien groesser, als der Anteil der Nicht-Mathe-LKer.




## Sonstige Visualisierungen des Datensatzes

#Haeufigkeit der Studiengaenge
barplot(table(daten$Studienfach), xlab= "Studienfach", ylab = "absolute Haeufigkeit", main= "Verteilung der Studienfächer", ylim= c(0, 35))
barplot(table(daten$Studienfach)/nrow(daten),ylim = c( 0, 0.35), xlab= "Studienfach", ylab = "relative Haeufigkeit", main= "Verteilung der Studienfächer")
## Der Studiengang Mathe ist am seltenstens vorhanden (12%).
## Informatik ist mit etwas mehr als 20% vertreten.
## Data Science und Statistik sind jeweils ca. 1/3 der SP, und damit am haeufistens vorhanden.


boxplot(Alter~LK_in_Mathe, daten, main = "Altersstruktur seperiert nach MatheLK")
## Der Median liegt im Durchschnitt bei 25 jahren in beiden Teilgruppen.
## Das 25 %Quantil liegt bei den ehemaligen Mathe-LKlern liegt bei 24 bei den nicht Mathe-LKlern hingegen bei 23. Das 75% Quantil liegt beide male bei 26.
## Dies deutete darauf hin, dass das Alter und das belegen eines Mathe-LKs keinen Einfluss hat.




