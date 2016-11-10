library(PogromcyDanych)
library(microbenchmark)
auta <- auta2012
statystyki_ceny = function(Marka = NULL, Model = NULL, Lata = NULL){
  dane <- c("Marka", "Model", "Rok.produkcji", "Cena.w.PLN")
  auta.wybrane <- auta[,dane]
  if(!is.null(Marka)){
    stopifnot(typeof(Marka) == "character")
    auta.wybrane <- auta.wybrane[(auta.wybrane$Marka %in% Marka), ]
  }
  if(!is.null(Model)){
    stopifnot(typeof(Model) == "character")
    auta.wybrane <- auta.wybrane[(auta.wybrane$Model %in% Model), ]
  }
  if(!is.null(Lata)){
    stopifnot(typeof(Lata) == "double" || typeof(Lata) == "integer")
    auta.wybrane <- auta.wybrane[(auta.wybrane$Rok.produkcji %in% Lata), ]
  }
  wynik <- tapply(auta.wybrane$Cena.w.PLN, auta.wybrane$Rok.produkcji, FUN=mean)
  wynik <- data.frame(rownames(wynik), wynik)
  colnames(wynik) <- c("Rok", "Cena")
  return(wynik)
}
utrata_ceny = function(Marka, Model){
  starsze <- statystyki_ceny(Marka, Lata=c(2011))
  starsze.srednia <- mean(starsze$Cena)
  nowsze <- statystyki_ceny(Marka, Lata=c(2012))
  starsze.nowsze <- mean(nowsze$Cena)
  wynik <- starsze.srednia/nowsze.srednia
  return(wynik)
}
