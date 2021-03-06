### Gruppenarbeit - wissenschaftliches Arbeiten - Teil 1###

# zur Reproduzierbarkeit
set.seed(2802)

# ID-Spalte:
ID <- 1:100

# Alter normalverteilt und anschliessend abgerundet:
Alter <- floor(rnorm(100, mean=25, sd=2))

# Studienfach mit verschiedenen Wahrscheinlichkeiten:
Studienfach <- sample(c("Statistik", "Data Science", "Informatik", "Mathe"), 100, 
                      replace = TRUE, prob = c(0.35, 0.35, 0.2, 0.1))

# Interesse an Mathematik: bei Studienfach Mathe am hoechsten, bei Statistik niedriger,
# Informatik und Data Science relativ unabhaengig vom Studienfach
Mathe_Interesse <- function(){
  Mathe_interesse <- rep(0, 100)
  for(i in 1:100){
    if(Studienfach[i] == "Statistik"){
      Mathe_interesse[i] <- sample(1:7, 1, 
                                   prob=c( 0, 0.02, 0.09, 0.2, 0.4, 0.2, 0.09))
    }
    if(Studienfach[i] == "Mathe"){
      Mathe_interesse[i] <- sample(1:7, 1, 
                                   prob=c( 0, 0, 0, 0.03, 0.07, 0.3, 0.6))
    }
    if(Studienfach[i] %in% c("Data Science", "Informatik")){
      Mathe_interesse[i] <- sample(1:7, 1, 
                                   prob=c( 0.1, 0.15, 0.25, 0.15, 0.15, 0.1, 0.1))
    }
  }
  return(Mathe_interesse)
}

mathe_interesse <- Mathe_Interesse()

# Interesse am Programmieren: bei Studienfach Informatik am hoechsten, bei Data Science
# etwas geringer und bei Statistik und Mathe (relativ) unabhaengig vom Studienfach
Programmier_Interesse <- function(){
  Programmier_interesse <- rep(0, 100)
  for(i in 1:100){
    if(Studienfach[i] == "Statistik"){
      Programmier_interesse[i] <- sample(1:7, 1)
    }
    if(Studienfach[i] == "Informatik"){
      Programmier_interesse[i] <- sample(1:7, 1, 
                                         prob = c( 0, 0, 0, 0, 0.05, 0.15, 0.8))
    }
    if(Studienfach[i] == "Data Science"){
      Programmier_interesse[i] <- sample(1:7, 1, 
                                         prob = c( 0, 0, 0.05, 0.1, 0.15, 0.6, 0.1))
    }
    if(Studienfach[i] == "Mathe"){
      Programmier_interesse[i] <- sample(1:7, 1, 
                                    prob = c( 0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.1))
    }
  }
  return(Programmier_interesse)
}

programmier_interesse <- Programmier_Interesse()

# Mathe-LK: abhaengig vom Interesse an Mathe
LK_in_Mathe <- function(){
  LK_in_mathe <- rep(0, 100)
  for(i in 1:100){
    if(mathe_interesse[i] > 5){
      LK_in_mathe[i] <- sample(c("ja", "nein"), 1, prob = c(0.9, 0.1))
    }
    if(mathe_interesse[i] < 4){
      LK_in_mathe[i] <- sample(c("ja", "nein"), 1, prob = c(0.1, 0.9))
    }
    if(mathe_interesse[i] %in% c(4,5)){
      LK_in_mathe[i] <- sample(c("ja", "nein"), 1, prob = c(0.5, 0.5))
    }
  }
  return(LK_in_mathe)
}

LK_in_mathe <- LK_in_Mathe()

# Datensatz zusammenfuegen:
datensatz <- data.frame("ID" = ID,        
                        "Alter" = Alter, 
                        "Studienfach" = Studienfach, 
                        "Mathematik_Interesse" = mathe_interesse, 
                        "Programmier_Interesse" = programmier_interesse,
                        "LK_in_Mathe" = LK_in_mathe)

# csv-Datei erstellen:
write.csv(datensatz, "Datensatz.csv")
