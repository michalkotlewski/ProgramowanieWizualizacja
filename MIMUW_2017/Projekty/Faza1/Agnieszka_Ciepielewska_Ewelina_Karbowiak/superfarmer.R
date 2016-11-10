library(randomForest)


#stan stada gracza (w kolejności cenowej malejąco)
stock.status = c(0, 0, 0, 0, 0, 0, 0)
names(stock.status) = c("horse", "cow", "bdog", "pig", "sheep", "sdog", "rabbit")
#liczba zwierząt w pudełku
max.stock = c(6, 12, 2, 20, 24, 4, 60)
names(max.stock) = c("horse", "cow", "bdog", "pig", "sheep", "sdog", "rabbit")
#macierz ruchów, tworzona przez "split"
move = numeric(length = 7)
#obie kostki
die1 = c("rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "sheep", "sheep", "sheep", "pig", "cow", "wolf")
die2 = c("rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "sheep", "sheep", "pig", "pig", "horse", "fox")
#lista możliwych zamian małych zwierząt na jedno większe
to.big.animal = list()
#lista możliwych zamian dużego zwierzęcia na mniejsze
to.small.animals = list()
#dane do uczenia maszynowego
data = data.frame(matrix(NA, nrow = 1e6, ncol = 15))
#wyniki random foresta
forest.result = list()
#wyniki grania losowego
random.result = c()
#ceny zwierzat
prices.sorted = c(72, 36, 36, 12, 6, 6, 1)
names(prices.sorted) = c("horse", "cow", "bdog", "pig", "sheep", "sdog", "rabbit")


#Funkcja rekurencyjna tworząca macierze wymian. Działa tak: 
#1. odpala się najpierw dla najwyższej ceny, patrzy ile najdroższych zwierząt jest w stanie kupić
#2. odpala się dla każdej kupionej liczby najdroższych zwierząt, ale już bez najwyższej ceny
#3. w końcu dochodzi do samych królików, gdzie zwraca pre_moves, które narastało przy wcześniejszych odpaleniach
split = function(n.rabbits, prices.sorted, pre_moves=c(), moves=c()) {
  ratio = floor(n.rabbits/prices.sorted[1])
  
  #Warunek końcowy
  if(length(prices.sorted) == 1){
    return(c(pre_moves, ratio))
  }
  
  for(i in 0:ratio){
    #Dopisywany jest tutaj kolejny wiersz macierzy ruchów
    moves = rbind(moves, split(n.rabbits - i*prices.sorted[1],
                               prices.sorted[-1],
                               c(pre_moves, i),
                               moves))
  }
  return(unique(moves))
}

#Tworzenie listy zawierającejnazwy zwierząt i przypisane im macierze wszystkich wymian:
#Dla każdego zwierzęcia odpalana jest funkcja split, a wszystko to zapisuje się w dwóch listach, 
#dzięki czemu tworzy się to tylko raz dla wszytkich gier.
for(i in names(stock.status)){
  to.big.animal[[i]] = -split(prices.sorted[[i]], prices.sorted)
  to.big.animal[[i]][, which(names(prices.sorted) == i)] = 1
  #Wyrzucanie przypadków, że wymianiamy jedno zwierzę na nic 
  #(były takie gdy w funkcji split wychodziła np. wymiana konia na konia)
  to.big.animal[[i]] = to.big.animal[[i]][rowSums(to.big.animal[[i]]) != 1, , drop=F]
  to.small.animals[[i]] = -to.big.animal[[i]]
}
#to.big.animal wyglada teraz tak: zwierzęta, które oddajemy są na minusie, a te, które otrzymujemy na plusie



#Generowanie wszystkich mozliwych ruchów
moves.generation = function(stock.status){
  n.rabbits = sum(stock.status * prices.sorted)
  possible.moves = c()
  for(i in 1:length(prices.sorted)){
    if(n.rabbits >= prices.sorted[i]){
      possible.moves = rbind(possible.moves, to.big.animal[[names(prices.sorted[i])]])
    }
  }
  #argument ifa jest tylko po to, żeby R nie krzyczał, kiedy possible.moves jest pusty
  #lub ma zerowy pierwszy wymiar
  if(!is.null(dim(possible.moves)) && dim(possible.moves)[1] > 0){
    #Sprawdzanie czy mamy wystarczająco dużo zwierząt w stocku, żeby dokonać wumiany.
    #Nie znalazłam lepszego sposobu na dodanie stock.status do każdego wiersza 
    #zamiast do każdej kolumny niż transponowanie macierzy dwukrotnie
    enough.animals = rowMeans(t(t(possible.moves) + stock.status) >= 0)
    #Wszystkie wymiany, na które nie mamy zwierząt są odrzucane
    possible.moves = possible.moves[enough.animals == 1, , drop=F]
  }
  for(i in names(stock.status)){
    if(stock.status[[i]] != 0){
      possible.moves = rbind(possible.moves, to.small.animals[[i]])
    }
  }
  possible.moves = unique(possible.moves)
  if(!is.null(dim(possible.moves)) && dim(possible.moves)[1] > 0){
    #Sprawdzenie czy ruch jest możliwy ze względu na liczbę zwierząt w całej grze
    future.states = t(t(possible.moves) + stock.status)
    #Sprawdzamy czy średnia wyrażeń logicznych w każdym rzędzie jest równa 1, 
    #bo interesują nas tylko te ruchy, na które starczy wszystkich zwierząt
    possible.states = rowMeans(t(t(future.states) <= max.stock)) == 1
    #Bierzemy tylko te ruch, które są możliwe ze wzglęgu na maksymalną liczbę zwierząt
    possible.moves = as.matrix(possible.moves[possible.states, ])
    #Dorzucamy brak ruchu jako możliwość
    possible.moves = rbind(possible.moves, c(0, 0, 0, 0, 0, 0, 0))
  }
  return(possible.moves)
}


#sposób wybrania ruchu
choose.move = function(way, possible.moves, model = NULL, stock.status){
  if(way == "random"){
    #Losowy wybór ruchu
    number.moves = nrow(possible.moves)
    move = possible.moves[sample(1:number.moves, 1), ]
    return(move)
  } else if(way == "rf") {
    #Wybór w oparciu o model random forest
    df1 = matrix(stock.status, nrow = nrow(possible.moves), ncol = 7, byrow = T)
    df1 = cbind(df1, possible.moves)

    n.types.after = rowSums((df1[, 1:7] + df1[, 8:14])[, c(1, 2, 4, 5, 7)] > 0)
    n.types.before = rowSums((df1[, 1:7])[, c(1, 2, 4, 5, 7)] > 0)
    #(Dorzucamy te same kolumny co w funkcji "forest.training.and.results", 
    #żeby random forest miał tyle samo zmiennych)
    #Żeby modelowi łatwiej było się uczyć oraz przewidywać dorzucamy kolumny mówiące 
    #ile rodzajów zwierząt potrzebnych do wygranej mieliśmy, 
    #ile będziemy mieć po ruchu i jaka jest różnica
    df1 = cbind(df1, n.types.before)
    df1 = cbind(df1, n.types.after)
    df1 = cbind(df1, (n.types.after - n.types.before))
    colnames(df1) = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", 
                      "X10", "X11", "X12", "X13", "X14", "X16", "X17", "X18")
    pred = predict(rf, df1)
    #Bierzemy ruch, który otrzymał od naszego modelu najmniejszą przewidywaną liczbę ruchów do wygranej 
    which.move = sample(which(pred == min(pred)), 1)
    #Jako, że czasami występują dwa ruchy o tej samej predykcji, to bierzemy losowy z nich
    move = possible.moves[which.move, ]
    return(move)
  }
}


#powiekszanie stada po rzucie
reproduction = function(die1.result, die2.result, stock.status){
  if(die1.result == die2.result){
    stock.status[[die1.result]] = min(floor(stock.status[[die1.result]] + 
                                            (stock.status[[die1.result]]+2)/2), max.stock[[die1.result]])
  } else if(die1.result == "wolf" && die2.result == "fox"){
    if(stock.status[["bdog"]] == 0){
      stock.status[["rabbit"]] = 0
      stock.status[["sheep"]] = 0
      stock.status[["pig"]] = 0
      stock.status[["cow"]] = 0
    } else {
      stock.status[["bdog"]] = stock.status[["bdog"]] - 1
    }
    if(stock.status[["sdog"]] == 0){
      stock.status[["rabbit"]] = 0
    } else {
      stock.status[["sdog"]] = stock.status[["sdog"]] - 1
    }
  } else if(die1.result == "wolf"){
    if(stock.status[["bdog"]] == 0){
      stock.status[["rabbit"]] = 0
      stock.status[["sheep"]] = 0
      stock.status[["pig"]] = 0
      stock.status[["cow"]] = 0
    } else {
      stock.status[["bdog"]] = stock.status[["bdog"]] - 1
    } 
    stock.status[[die2.result]] = min(floor(stock.status[[die2.result]] + (stock.status[[die2.result]]+1)/2), max.stock[[die2.result]])
    
  } else if(die2.result == "fox"){
    if(stock.status[["sdog"]] == 0){
      stock.status[["rabbit"]] = 0
    } else {
      stock.status[["sdog"]] = stock.status[["sdog"]] - 1
    }
    stock.status[[die1.result]] = min(floor(stock.status[[die1.result]] + (stock.status[[die1.result]]+1)/2), max.stock[[die1.result]])
  } else {
    stock.status[[die1.result]] = min(floor(stock.status[[die1.result]] + (stock.status[[die1.result]]+1)/2), max.stock[[die1.result]])
    stock.status[[die2.result]] = min(floor(stock.status[[die2.result]] + (stock.status[[die2.result]]+1)/2), max.stock[[die2.result]])
  }
  return(stock.status)
}

#zmiana stanu stada
change = function(move, stock.status){
  stock.status + move
}

#czy wygrana
win = function(stock.status){
  min(stock.status[c(1, 2, 4, 5, 7)]) > 0
}

#pojedyncza gra przy generowaniu danych
play = function(nrows = 0, way, model = NULL, save.moves = T){
  turns = 0
  while(!win(stock.status)){
    turns = turns + 1
    nrows = nrows +1
    if(nrows >= 1e6){
      turns.to.end = turns:1
      data[(nrows-turns+1):nrows, 15] <<- turns.to.end
      return(nrows)
    }
    possible.moves = moves.generation(stock.status)
    if(!is.null(dim(possible.moves)) && dim(possible.moves)[1] > 0){
      move = choose.move(way, possible.moves, model, stock.status)
      if(save.moves){
        data[nrows, 1:14] <<- c(stock.status, move)
      }
      stock.status = change(move, stock.status)
    }
    if(win(stock.status)){
      turns.to.end = turns:1
      if(save.moves){
        turns.to.end = turns:1
        data[(nrows-turns +1):nrows, 15] <<- turns.to.end
        return(nrows)
      } else {
        return(turns)
      }
    }
    die1.result = sample(die1, 1)
    die2.result = sample(die2, 1)
    stock.status = reproduction(die1.result, die2.result, stock.status)
  }
  if(save.moves){
    turns.to.end = turns:1
    data[(nrows-turns +1):nrows, 15] <<- turns.to.end
    return(nrows)
  } else {
    return(turns)
  }
}


#generowanie danych do uczenia maszynowego
generate.data = function(way, model = NULL){
  nrows = 0
  for(i in 1:20000){
    nrows = play(nrows, way, model)
    cat(nrows, "\n")
  }
}

#Funkcja trenująca model oraz generująca ostateczne wyniki
forest.training.and.results = function(){
  
  random.resulst <<- sapply(1:10000, function(x) play(way="random", save.moves=F))
  
  #Można to napisać ładniej
  way = "random"
  generate.data(way)
  df = data[complete.cases(data), ]
  n.types.after = rowSums((df[, 1:7] + df[, 8:14])[, c(1, 2, 4, 5, 7)] > 0)
  n.types.before = rowSums((df[, 1:7])[, c(1, 2, 4, 5, 7)] > 0)
  df$X16 = n.types.before
  df$X17 = n.types.after
  df$X18 = n.types.after - n.types.before
  set.seed(23134)
  train = sample(1:nrow(df), 0.4*nrow(df))
  test = -train
  rf = randomForest(as.matrix(df[train, -15]), df[train, 15], ntree=25, do.trace=T, mtry=17)
  
  forest.result[[1]] <<- sapply(1:10000, function(x) play(way="rf", save.moves=F))
  
  for(i in 1:2){
    way = "rf"
    generate.data(way, rf)
    df = data[complete.cases(data), ]
    n.types.after = rowSums((df[, 1:7] + df[, 8:14])[, c(1, 2, 4, 5, 7)] > 0)
    n.types.before = rowSums((df[, 1:7])[, c(1, 2, 4, 5, 7)] > 0)
    df$X16 = n.types.before
    df$X17 = n.types.after
    df$X18 = n.types.after - n.types.before
    set.seed(123)
    #Tworzenie zbioru treningowego i testowego, żeby w czasie programowanie 
    #sprawdzić czy model w ogóle dobrze przewiduje.
    #Są to data frame'y zawierające true i false
    train = sample(1:nrow(df), 0.8*nrow(df))
    #Zbiór testowy to dopełnienie zbioru treningowego
    test = -train
    rf = randomForest(as.matrix(df[train, -15]), df[train, 15], ntree=25, do.trace=T, mtry=17)
    
    forest.result[[i+1]] <<- sapply(1:10000, function(x) play(way="rf", save.moves=F))
  }
}
