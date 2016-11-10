
#deklaracja zmiennych
stock.status = c(0, 0, 0, 0, 0, 0, 0)
names(stock.status) = c("horse", "cow", "bdog", "pig", "sheep", "sdog", "rabbit")
max.stock = c(6, 12, 2, 20, 24, 4, 60)
names(max.stock) = c("horse", "cow", "bdog", "pig", "sheep", "sdog", "rabbit")
move = numeric(length = 7)
die1 = c("rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "sheep", "sheep", "sheep", "pig", "cow", "wolf")
die2 = c("rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "rabbit", "sheep", "sheep", "pig", "pig", "horse", "fox")
to.big.animal = list()
to.small.animals = list()
data = data.frame(matrix(NA, nrow = 1e6, ncol = 15))

#możliwe wymiany na pojedyncze większe zwierze
prices.sorted = c(72, 36, 36, 12, 6, 6, 1)
names(prices.sorted) = c("horse", "cow", "bdog", "pig", "sheep", "sdog", "rabbit")

split = function(n.rabbits, prices.sorted, pre_moves=c(), moves=c()) {
  ratio = floor(n.rabbits/prices.sorted[1])
  
  if(length(prices.sorted) == 1){
    return(c(pre_moves, ratio))
  }
  
  for(i in 0:ratio){
    moves = rbind(moves, split(n.rabbits - i*prices.sorted[1],
                               prices.sorted[-1],
                               c(pre_moves, i),
                               moves))
  }
  return(unique(moves))
}
for(i in names(stock.status)){
  to.small.animals[[i]] = split(prices.sorted[[i]], prices.sorted)
  to.small.animals[[i]] = to.small.animals[[i]][rowSums(to.small.animals[[i]] > 0) == 1, , drop=F]
  to.small.animals[[i]][, which(names(prices.sorted) == i)] = -1
  to.small.animals[[i]] = to.small.animals[[i]][rowSums(to.small.animals[[i]] != 0) != 1, , drop=F]
  to.big.animal[[i]] = -split(prices.sorted[[i]], prices.sorted)
  to.big.animal[[i]][, which(names(prices.sorted) == i)] = 1
  to.big.animal[[i]] = to.big.animal[[i]][rowSums(to.big.animal[[i]]) != 1, , drop=F]
}



#wszystkie mozliwe ruchy
moves.generation = function(stock.status){
  n.rabbits = sum(stock.status * prices.sorted)
  possible.moves = c()
  for(i in 1:length(prices.sorted)){
    if(n.rabbits >= prices.sorted[i]){
      possible.moves = rbind(possible.moves, to.big.animal[[names(prices.sorted[i])]])
    }
  }
  if(!is.null(dim(possible.moves)) && dim(possible.moves)[1] > 0){
    enough.animals = rowMeans(t(t(possible.moves) + stock.status) >= 0)
    possible.moves = possible.moves[enough.animals == 1, , drop=F]
  }
  for(i in names(stock.status)){
    if(stock.status[[i]] != 0){
      possible.moves = rbind(possible.moves, to.small.animals[[i]])
    }
  }
  return(unique(possible.moves))
}


#sposób wybrania ruchu
choose.move = function(way, possible.moves){
  if(way == "random"){ 
    number.moves = nrow(possible.moves)
    move = possible.moves[sample(1:number.moves, 1), ]
    return(move)
  } else if(way == "randomforest") {
    return(0)
    #tutaj random forest!
  }
}


#powiekszanie stada po rzucie
reproduction = function(die1.result, die2.result, stock.status){
  if(die1.result == die2.result){
    stock.status[[die1.result]] = min(floor(stock.status[[die1.result]] + (stock.status[[die1.result]]+2)/2), max.stock[[die1.result]])
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

#pojedyncza gra
play = function(nrows){
  turns = 0
  way = "random"
  while(!win(stock.status)){
    turns = turns + 1
    nrows = nrows +1
    if(nrows == 1e6){
      turns.to.end = turns:1
      data[(nrows-turns+1):nrows, 15] <<- turns.to.end
      return(nrows)
    }
    possible.moves = moves.generation(stock.status)
    if(!is.null(dim(possible.moves)) && dim(possible.moves)[1] > 0){
      move = choose.move(way, possible.moves)
      data[nrows, 1:14] <<- c(stock.status, move)
      stock.status = change(move, stock.status)
    }
    if(win(stock.status)){
      turns.to.end = turns:1
      #cat(length((nrows-turns):nrows), length(turns.to.end), "\n")
      data[(nrows-turns +1):nrows, 15] <<- turns.to.end
      return(nrows)
    }
    die1.result = sample(die1, 1)
    die2.result = sample(die2, 1)
    stock.status = reproduction(die1.result, die2.result, stock.status)
    #cat(turns, stock.status, '\n')
  }
  turns.to.end = turns:1      
  #cat(length((nrows-turns):nrows), length(turns.to.end), "\n")
  data[(nrows-turns +1):nrows, 15] <<- turns.to.end
  return(nrows)
}


#generowanie danychdo uczenia
generate.data = function(){
  nrows = 0
  for(i in 1:20000){
    nrows = play(nrows)
    cat(nrows)
  }
}
  
