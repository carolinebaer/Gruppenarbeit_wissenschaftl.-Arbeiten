### Gruppenarbeit - wissenschaftliches Arbeiten ###
## Auswertung der Daten ##

# Datensatz laden




# Auswetung der Daten mit Hilfe der Funktionen aus Skript 1


#funktion b
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

##die Mehrheit der Mathematik studenten hatte Mathe LK (8 von 12) -> 75%

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








# Visualisierung der Daten