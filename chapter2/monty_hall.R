### THE MONTY HALL PROBLEM ###

#create sampling function
sample.vec <- function(x, ...) x[sample(length(x), ...)]

# possible doors 1, 2, 3
doors = c(1:3)

n = 100000

no_change_score = 0
change_score = 0

for (i in 1:n){ 
  # one of the three doors has the car behind it
  car_door = sample.vec(doors,1)

  # the other two doors have goats behind them
  goat_doors = doors[doors != car_door]
  
  # the contestant chooses one door
  chosen_door = sample.vec(doors,1)

  # the show host shows a goat door that is not the chosen door
  show_door = sample.vec(goat_doors[goat_doors != chosen_door],1)

  # if you stick with your choice:
  no_change_score = no_change_score + (chosen_door == car_door)

  # if you change your choice:
  changed_door = sample.vec(doors[-c(chosen_door,show_door)],1)
  change_score = change_score + (changed_door == car_door)
}

no_change_score/n
change_score/n

barplot(c(no_change_score,change_score)/n,col=c('skyblue','brown'),ylim=c(0,1),names.arg=c('no change', 'change'))


#### variation on the monty hall problem to make it more intuitive ####

# suppose there are a hundred doors

# possible doors 1, 2, 3, ..., 100
doors = c(1:100)

no_change_score = 0
change_score = 0

for (i in 1:n){ 
  # one of the doors has the car behind it
  car_door = sample.vec(doors,1)
  
  # the other doors have goats behind them
  goat_doors = doors[doors != car_door]
  
  # the contestant chooses one door
  chosen_door = sample.vec(doors,1)
  
  # the show host shows all the goat doors except for the chosen door and one other door
  show_door = sample.vec(goat_doors[goat_doors != chosen_door],98)
  
  # if you stick with your choice:
  no_change_score = no_change_score + (chosen_door == car_door)
  
  # if you change your choice:
  changed_door = sample.vec(doors[! doors %in% c(show_door,chosen_door)],1)
  change_score = change_score + (changed_door == car_door)
}

no_change_score/n
change_score/n

barplot(c(no_change_score,change_score)/n,col=c('skyblue','brown'),ylim=c(0,1),names.arg=c('no change', 'change'))

# 
# Bayes' theorem
# p(A|B) = p(B|A) * p(A) / p(B)
#                     
# suppose we choose door 1 and the show host shows door 3, 
# if we don't change to door 2, our chance of getting the car = 1/3

# Bayesian proof that the chance of the car being behind our chosen door 1 is 1/3

# p(car_behind_1 | show_door_3) = p(show_door_3 | car_behind_1) * p(car_behind_1) / p(show_door_1)
# = 1/2 * 1/3 / 1/2 = 1/3

# Bayesian proof that switching to door 2 results in higher chance:
# p(car_behind_2 | show_door_3) = p(show_door_3 | car_behind_2) * p(car_behind_2) / p(show_door_1)
# = 1 * 1/3 / 1/2 = 2/3

# why is p(show_door_3 | car_behind_2) = 1 here?
# because you chose door 1, he can't show door 1, he also can't show door 2 because this 
# conditional assumes door 2 is correct, so only door 3 can be shown.

