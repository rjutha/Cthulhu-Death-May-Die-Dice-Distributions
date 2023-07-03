library(tidyverse)
# https://datascienceplus.com/strategies-to-speedup-r-code/

# Black Die
# 1 Blank
# 2 ! (success)
# 1 tentacle
# 1 star + tentacle
# 1 tentacle + ! (sucess)

# Green Die
  # 2 Blank
  # 2 ! (success)
  # 1 star
  # 1 star + ! (success)

roll_dice <- function(black, green, message = TRUE){
  success <- 0
  star <- 0
  tentacle <- 0
  probability <- 1
  
  if(black > 0){
    for(i in 1:black){
      roll <- sample(1:6, 1)
      # Blank
      if(roll == 1){
        if (message) print("Blank")
        probability = probability * (1/6)
      }
      # Success
      else if(roll %in% c(2,3)){
        success = success + 1
        probability = probability * (2/6)
        if (message) print("Success")
      }
      # Tentacle
      else if(roll == 4){
        tentacle = tentacle + 1
        probability = probability * (1/6)
        if (message) print("Tentacle")
      }
      # Star + Tentacle
      else if(roll == 5){
        star = star + 1
        tentacle = tentacle + 1
        probability = probability * (1/6)
        if (message) print("Star + Tentacle")
      }
      # Success + Tentacle
      else if(roll == 6){
        success = success + 1
        tentacle = tentacle + 1
        probability = probability * (1/6)
        if (message) print("Success + Tentacle")
      }
    }
  }
  
  if(green > 0 ){
    for(i in 1:green){
      roll <- sample(1:6, 1)
      # Blanks
      if(roll %in% c(1,2)){
        if (message) print("Blank")
        probability = probability * (2/6)
      }
      # Success
      else if(roll %in% c(3,4)){
        if (message) print("Success")
        success = success + 1
        probability = probability * (2/6)
      }
      # Star
      else if(roll == 5){
        if (message) print("Star")
        star = star + 1
        probability = probability * (1/6)
      }
      # Star + Success
      else if(roll == 6){
        if (message) print("Success + Star")
        success = success + 1
        star = star + 1
        probability = probability * (1/6)
      }
    }
  }
  
  return(
    c(
      success = success,
      star = star,
      tentacle = tentacle,
      probability = probability
    )
  )
}

#roll_dice(3,0)

roll_n <- function(n, black, green){
  df <- matrix(nrow = n, ncol = 4, dimnames = list(c(), c("success", "star", "tentacle", "probability")))
  for(i in 1:n){
    temp <- roll_dice(black, green, message = FALSE)
    df[i,] <- temp
  }
  return(df)
}

# n = 10000
# data <- roll_n(n,3,0) %>%
#   as_tibble() %>%
#   select(-probability) %>%
#   group_by_all() %>%
#   count(name = "count") %>%
#   ungroup() %>%
#   arrange(-count) %>%
#   mutate(dist = count / n)
# 
# hchart(data,
#        "column",
#        hcaes(x = 1:nrow(data), y = data$dist)) %>%
#   hc_plotOptions(
#     column = list(
#       pointPadding = 0,
#       borderWidth = 0,
#       groupPadding = 0,
#       shadow = FALSE
#          )
#        )
# 
#        

print_range <- function(low, high, string){
  if(low == high){
    if(low == 1) string <- str_replace(string,"es$|s$", "")
    return(paste("Exactly", low, string))
  }
  if (low < high && high == 9){
    if(low == 1) string <- str_replace(string,"(?<!l)es$|s$", "")
    return(paste("At least", low, string))
  }
  if(low < high){
    return(paste("Between", low, "and", high, string))
  }
}

# print_range(1,1,"Successes")
# print_range(2,2,"Successes")
# print_range(4,9, "Tentacles")
# print_range(1,9, "Tentacles")
# print_range(1,8,"Stars")
