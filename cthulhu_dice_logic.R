# This script defines functions to simulate rolling the dice pertaining to the table-top game Cthulhu: Death May Die.
library(tidyverse)

# Below is a summary of the outcomese on each of the dice
# Black Die
# 1 Blank
# 2 ! (success)
# 1 tentacle
# 1 star
# 1 tentacle + ! (sucess)

# Green Die
  # 2 Blank
  # 2 ! (success)
  # 1 star
  # 1 star + ! (success)

# This function contains the logic to roll the from the game with the correct probabilities.
# The inputs control the amount of black and green dice that should be used for the roll.
# The function returns a named vector containing the tally of each outcome for all the rolls.
roll_dice <- function(black, green){
    success <- 0
    star <- 0
    tentacle <- 0
  
  # Roll Black Dice
  if(black > 0){
    for(i in 1:black){
      roll <- sample(1:6, 1)
      # Blank
      #if(roll == 1) do nothing
      # Success
      if(roll %in% c(2,3)){
        success = success + 1
      }
      # Tentacle
      else if(roll == 4){
        tentacle = tentacle + 1
      }
      # Star
      else if(roll == 5){
        star = star + 1
      }
      # Success + Tentacle
      else if(roll == 6){
        success = success + 1
        tentacle = tentacle + 1
      }
    }
  }
  
  # Roll Green Dice
  if(green > 0 ){
    for(i in 1:green){
      roll <- sample(1:6, 1)
      # Blanks
      #if(roll %in% c(1,2)) do nothing
      # Success
      if(roll %in% c(3,4)){
        success = success + 1
      }
      # Star
      else if(roll == 5){
        star = star + 1
      }
      # Star + Success
      else if(roll == 6){
        success = success + 1
        star = star + 1
      }
    }
  }
  
  return(c(
    success = success,
    star = star,
    tentacle = tentacle)
  )
}

# This function calls the roll_dice() function n number of times and stores all the outcomes in a matrix
roll_n <- function(n, black, green){
  df <- matrix(nrow = n, ncol = 3, dimnames = list(c(), c("success", "star", "tentacle")))
  for(i in 1:n){
    temp <- roll_dice(black, green)
    df[i,] <- temp
  }
  return(df)
}

# This short function is for printing probability of each outcome dynamically based on input from the shiny app.
print_range <- function(low, high, string, max){
  if(low == high){
    if(low == 1) string <- str_replace(string,"(?<!l)es$|s$", "")
    return(paste("Exactly", low, string))
  }
  if(low < high && high == max && low == 0){
    return(paste("Any number of", string))
  }
  if (low < high && high == max && low != 0){
    if(low == 1) string <- str_replace(string,"(?<!l)es$|s$", "")
    return(paste("At least", low, string))
  }
  if(low < high){
    return(paste("Between", low, "and", high, string))
  }
}