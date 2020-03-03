## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(matricks)

## ----init.gid.world------------------------------------------------------
# Defining possible actions
# FALSE = unavailable field
actions <- m(T, T, T, F, T|
             T, T, F, T, T|
             F, T, T, T, T|
             T, T, T, T, T|
             F, F, T, T, T|
             F, T, T, T, F)
plot(actions)
# Defining rewards matrix with two terminal states
rewards <- with_same_dims(actions, 0)
rewards <- sv(rewards,
              c(4, 4) ~ 1., # Win
              c(5, 3) ~ -1) # Lose
# We add small penalties, which encourages models
# to reduce number of steps it passes 
rewards[rewards == 0] <- -0.1
plot(rewards)
# List of possible state indices
states <- matrix_idx(actions, actions)

## ----fixed.policy--------------------------------------------------------
# Symbols for moves
U <- "U" # Up
D <- "D" # Down
L <- "L" # Left
R <- "R" # Right

fixed.policy <- m(R , D , L , NA, D |
                  R , D , NA, D , L |
                  NA, R , D , D , D |
                  R , R , D , NA, D |
                  NA, NA, NA, U , L |
                  NA, R , U , L , NA)

## ----as.idx--------------------------------------------------------------
as_idx <- function(x){
  n.row <- nrow(x)
  n.col <- ncol(x)

  result <- matrix(list(), nrow = n.row, ncol = n.col)

  for (i in 1:n.row){
    for (j in 1:n.col){
      coords <- c(i, j)
      
      if (is.na(at(x, coords)))
        next
      
      x.val <- x[i, j]

      switch (x.val,
        U = c(i - 1, j),
        D = c(i + 1, j),
        L = c(i, j - 1),
        R = c(i, j + 1),
      ) -> move.idx

      if (!is_idx_possible(x, move.idx))
        next

      result[i, j] <- list(move.idx)
    }
  }
  result
}

fixed.policy.idx <- as_idx(fixed.policy)
fixed.policy.idx

## ----policy.evaluation---------------------------------------------------
evaluate_policy <- function(actions, rewards, policy, 
                            epsilon = 1e-3, gamma = 0.9){
  
  V <- with_same_dims(policy, 0)
  policy.idx <- as_idx(policy)

  while (TRUE) {
    biggest.change <- 0
  
    for (move in seq_matrix(policy.idx)) {
      s <- move[[1]] # Action, value at index s
      a <- move[[2]]
   
      old.v <- at(V, s)
        
      if(!at(actions, s))
        next
      
      if(is.null(a))
        next
        
      r <- at(rewards, a)
      at(V, s) <- r + gamma * at(V, a)
      biggest.change <- max(biggest.change, abs(old.v - at(V, s)))
    }
    
    print(biggest.change)
    
    if (biggest.change < epsilon){
      break
    }
  }
  return(V)
}  

