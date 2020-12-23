#One single roll

IMroll2 <- function(IM1, IM2)
{
  roll <- sample(c('Silver Ball', 'Gold Ball', 'Black Ball', IM1, IM2), 1, prob = c(.50, .30, 13/15*.20, 1/15*.20, 1/15*.20))
}

IMroll3 <- function(IM1, IM2, IM3)
{
  roll <- sample(c('Silver Ball', 'Gold Ball', 'Black Ball', IM1, IM2, IM3), 1, prob = c(.50, .30, 12/15*.20, 1/15*.20, 1/15*.20, 1/15*.20))
}

#Try a fixed number of roll and show the result

TrialRoll2 = function(n_roll, IM1, IM2)
{
  result <- c()
  for (i in 1:n_roll) 
  {
    result[i] <- IMroll2(IM1, IM2)
    print(result[i])
  }
}

TrialRoll3 = function(n_roll, IM1, IM2, IM3)
{
  result <- c()
  for (i in 1:n_roll) 
  {
    result[i] <- IMroll3(IM1, IM2, IM3)
    print(result[i])
  }
}

#Roll until obtain the target(s)

CountRoll2 <- function(IM1, IM2)
{
  countIM1 = 0
  countIM2 = 0
  total_count = 0
  while (countIM1 < 1 || countIM2 < 1) 
  {
    roll <- IMroll2(IM1, IM2)
    if (roll == IM1)
    {
      countIM1 <- countIM1 + 1
    }
    if (roll == IM2)
    {
      countIM2 <- countIM2 + 1
    }
    total_count = total_count + 1
  }
  print(paste(IM1, ' = ', countIM1))
  print(paste(IM2, ' = ', countIM2))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll2target1 <- function(IM1, IM2, target1)
{
  countTarget1 = 0
  total_count = 0
  while (countTarget1 == 0)
  {
    roll <- IMroll2(IM1, IM2)
    if (roll == target1)
    {
      countTarget1 = countTarget1 + 1
    }
    total_count = total_count + 1
  }
  print(paste(target1, ' = ', countTarget1))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll2random1 <- function(IM1, IM2)
{
  countIM1 = 0
  countIM2 = 0
  total_count = 0
  while (countIM1 == 0 && countIM2 == 0) 
  {
    roll <- IMroll2(IM1, IM2)
    if (roll == IM1)
    {
      countIM1 <- countIM1 + 1
    }
    if (roll == IM2)
    {
      countIM2 <- countIM2 + 1
    }
    total_count = total_count + 1
  }
  print(paste(IM1, ' = ', countIM1))
  print(paste(IM2, ' = ', countIM2))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll3 <- function(IM1, IM2, IM3)
{
  countIM1 = 0
  countIM2 = 0
  countIM3 = 0
  total_count = 0
  while (countIM1 < 1 || countIM2 < 1 || countIM3 < 1) 
  {
    roll <- IMroll3(IM1, IM2, IM3)
    if (roll == IM1)
    {
      countIM1 <- countIM1 + 1
    }
    if (roll == IM2)
    {
      countIM2 <- countIM2 + 1
    }
    if (roll == IM3)
    {
      countIM3 <- countIM3 + 1
    }
    total_count = total_count + 1
  }
  print(paste(IM1, ' = ', countIM1))
  print(paste(IM2, ' = ', countIM2))
  print(paste(IM3, ' = ', countIM3))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll3target1 <- function(IM1, IM2, IM3, target1)
{
  countIM1 = 0
  countIM2 = 0
  countIM3 = 0
  countTarget1 = 0
  total_count = 0
  while (countTarget1 == 0) 
  {
    roll <- IMroll3(IM1, IM2, IM3)
    if (roll == IM1)
    {
      countIM1 <- countIM1 + 1
    }
    if (roll == IM2)
    {
      countIM2 <- countIM2 + 1
    }
    if (roll == IM3)
    {
      countIM3 <- countIM3 + 1
    }
    if (roll == target1)
    {
      countTarget1 <- countTarget1 + 1
    }
    total_count = total_count + 1
  }
  print(paste(IM1, ' = ', countIM1))
  print(paste(IM2, ' = ', countIM2))
  print(paste(IM3, ' = ', countIM3))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll3target2 <- function(IM1, IM2, IM3, target1, target2)
{
  countIM1 = 0
  countIM2 = 0
  countIM3 = 0
  countTarget1 = 0
  countTarget2 = 0
  total_count = 0
  while (countTarget1 == 0 || countTarget2 == 0) 
  {
    roll <- IMroll3(IM1, IM2, IM3)
    if (roll == IM1)
    {
      countIM1 <- countIM1 + 1
    }
    if (roll == IM2)
    {
      countIM2 <- countIM2 + 1
    }
    if (roll == IM3)
    {
      countIM3 <- countIM3 + 1
    }
    if (roll == target1)
    {
      countTarget1 <- countTarget1 + 1
    }
    if (roll == target2)
    {
      countTarget2 <- countTarget2 + 1
    }
    total_count = total_count + 1
  }
  print(paste(IM1, ' = ', countIM1))
  print(paste(IM2, ' = ', countIM2))
  print(paste(IM3, ' = ', countIM3))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll3random1 <- function(IM1, IM2, IM3)
{
  countIM1 = 0
  countIM2 = 0
  countIM3 = 0
  total_count = 0
  while (countIM1 == 0 && countIM2 == 0 && countIM3 == 0) 
  {
    roll <- IMroll3(IM1, IM2, IM3)
    if (roll == IM1)
    {
      countIM1 <- countIM1 + 1
    }
    if (roll == IM2)
    {
      countIM2 <- countIM2 + 1
    }
    if (roll == IM3)
    {
      countIM3 <- countIM3 + 1
    }
    total_count = total_count + 1
  }
  print(paste(IM1, ' = ', countIM1))
  print(paste(IM2, ' = ', countIM2))
  print(paste(IM3, ' = ', countIM3))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll3random2 <- function(IM1, IM2, IM3)
{
  countUniqueIM = 0
  alreadyhave = ''
  total_count = 0
  while (countUniqueIM < 2) 
  {
    roll <- IMroll3(IM1, IM2, IM3)
    if (roll == IM1 && roll != alreadyhave)
    {
      countUniqueIM <- countUniqueIM + 1
      alreadyhave <- roll
    }
    if (roll == IM2 && roll != alreadyhave)
    {
      countUniqueIM <- countUniqueIM + 1
      alreadyhave <- roll
    }
    if (roll == IM3 && roll != alreadyhave)
    {
      countUniqueIM <- countUniqueIM + 1
      alreadyhave <- roll
    }
    total_count = total_count + 1
  }
  print(paste('Number of unique IMs', ' = ', countUniqueIM))
  print(paste('Total number of rolls = ', total_count))
  return(total_count)
}

CountRoll3('CRUIJFF', 'KLUIVERT', 'DECO')

CountRoll3random1('CRUIJFF', 'KLUIVERT', 'DECO')

CountRoll3random2('CRUIJFF', 'KLUIVERT', 'DECO')

CountRoll3target1('CRUIJFF', 'KLUIVERT', 'DECO', 'CRUIJFF')

CountRoll3target2('CRUIJFF', 'KLUIVERT', 'DECO', 'CRUIJFF', 'KLUIVERT')

repeatTrial <- function(times)
{
  attempts = c()
  for (i in 1:times)
  {
    attempts[i] <- CountRoll3random2('CRUIJFF', 'KLUIVERT', 'DECO')
  }
  print(paste('Average number of pulls required: ', mean(attempts)))
  print(paste('Highest number of pulls required: ', max(attempts)))
  d <- density(attempts) # returns the density data
  plot(d, main = 'Distribution of required pulls for 2 random IM cards', xlab = 'Number of pulls', sub = 'N = 10000') # plots the results
  quantile(attempts, c(.01, .50, .75, .99))
}

repeatTrial(10000)
