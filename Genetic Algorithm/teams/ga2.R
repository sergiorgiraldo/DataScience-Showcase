library(GA)

teams <- read.csv("C:\\Users\\sgiraldo\\source\\r\\juan\\teams.csv",sep=";")
floorCapacity <- 150

#********************************************************#
#Fitness function
evalFunc <- function(chromosome) {
  
  #aux variables
  mondayTeams    <- teams[chromosome[(nrow(teams) * 0 + 1) : (1 * nrow(teams))] == 1, ]
  tuesdayTeams   <- teams[chromosome[(nrow(teams) * 1 + 1) : (2 * nrow(teams))] == 1, ]
  wednesdayTeams <- teams[chromosome[(nrow(teams) * 2 + 1) : (3 * nrow(teams))] == 1, ]
  thursdayTeams  <- teams[chromosome[(nrow(teams) * 3 + 1) : (4 * nrow(teams))] == 1, ]
  fridayTeams    <- teams[chromosome[(nrow(teams) * 4 + 1) : (5 * nrow(teams))] == 1, ]
  
  mondayValues    <- chromosome[(nrow(teams) * 0 + 1) : (1 * nrow(teams))]
  tuesdayValues   <- chromosome[(nrow(teams) * 1 + 1) : (2 * nrow(teams))]
  wednesdayValues <- chromosome[(nrow(teams) * 2 + 1) : (3 * nrow(teams))]
  thursdayValues  <- chromosome[(nrow(teams) * 3 + 1) : (4 * nrow(teams))]
  fridayValues    <- chromosome[(nrow(teams) * 4 + 1) : (5 * nrow(teams))]
  
  #********************************************************#
  #restriction 1: available seats
  currCapacityMonday	 <- (floorCapacity - sum(mondayTeams$total))
  ifelse(currCapacityMonday	 < 0, currCapacityMonday <- 999, #infeasibility 
         currCapacityMonday	 <- currCapacityMonday/floorCapacity)
  #divide by floorCapacity for normalization
  
  currCapacityTuesday <- (floorCapacity - sum(tuesdayTeams$total))
  ifelse(currCapacityTuesday < 0, currCapacityTuesday <- 999, #infeasibility  
         currCapacityTuesday <- currCapacityTuesday/floorCapacity)
  
  currCapacityWednesday <- (floorCapacity - sum(wednesdayTeams$total))
  ifelse(currCapacityWednesday < 0, currCapacityWednesday <- 999, #infeasibility 
         currCapacityWednesday <- currCapacityWednesday/floorCapacity)
  
  currCapacityThursday <- (floorCapacity - sum(thursdayTeams$total))
  ifelse(currCapacityThursday < 0, currCapacityThursday <- 999, #infeasibility 
         currCapacityThursday <- currCapacityThursday/floorCapacity)
  
  currCapacityFriday <- (floorCapacity - sum(fridayTeams$total))
  ifelse(currCapacityFriday < 0, currCapacityFriday <- 999, #infeasibility 
         currCapacityFriday <- currCapacityFriday/floorCapacity)
  
  #********************************************************#
  #restriction 2: no team must appear or not appear all days in a row. 
  currViolationsConsecutiveDaysAll <- 0 
  currViolationsConsecutiveDaysNone <- 0
  
  for (idx in 1: nrow(teams)){
    #teams at ps all days
    if (sum(mondayValues[idx], tuesdayValues[idx], wednesdayValues[idx], thursdayValues[idx], fridayValues[idx]) == 5){
      currViolationsConsecutiveDaysAll <- 999 #infeasibility
      break
    }
    
    #teams at home all days
    if (sum(mondayValues[idx], tuesdayValues[idx], wednesdayValues[idx], thursdayValues[idx], fridayValues[idx]) == 0){
      currViolationsConsecutiveDaysNone <- 999 #infeasibility
      break
    }
  }
  
  #if this restriction is not a blocker
  #currViolationsConsecutiveDaysAll <- currViolationsConsecutiveDaysAll / nrow(teams) #normalization
  #currViolationsConsecutiveDaysNone <- currViolationsConsecutiveDaysNone / nrow(teams) #normalization
  
  #********************************************************#
  #restriction 3: dependencies must be enforced
  
  checkDependencies <- function(dayOfWeekTeams){
    violations <-0
    for (dependencies in dayOfWeekTeams$dependsOn){
      if (dependencies != ""){
        for (dependency in unlist(strsplit(dependencies,","))){
          if (!dependency %in% dayOfWeekTeams$name){
            violations <- violations + 1
          }
        }
      }
    }  
    return(violations)
  }
  
  totalDependencies <- 0 
  for (dependencies in teams$dependsOn){
    if (dependencies != ""){
      totalDependencies <- totalDependencies + length(unlist(strsplit(dependencies,",")))
    }
  }  
  
  currViolationsDependenciesMonday	   <- checkDependencies(mondayTeams)/totalDependencies #normalization
  currViolationsDependenciesTuesday    <- checkDependencies(tuesdayTeams)/totalDependencies #normalization
  currViolationsDependenciesWednesday  <- checkDependencies(wednesdayTeams)/totalDependencies #normalization
  currViolationsDependenciesThursday   <- checkDependencies(thursdayTeams)/totalDependencies #normalization
  currViolationsDependenciesFriday     <- checkDependencies(fridayTeams)/totalDependencies #normalization
  
  #********************************************************#
  #restriction 4: no more than 15% of managers
  currViolationManagersMonday <- 0
  currViolationManagersTuesday <- 0
  currViolationManagersWednesday <- 0
  currViolationManagersThursday <- 0
  currViolationManagersFriday <- 0
  
  currManagersMonday <- sum(mondayTeams$mgr == "y")/ length(mondayTeams$name)
  ifelse(currManagersMonday > 0.15, currViolationManagersMonday <- 1, currViolationManagersMonday <- 0)
  
  currManagersTuesday <- sum(tuesdayTeams$mgr == "y")/ length(tuesdayTeams$name)
  ifelse(currManagersTuesday > 0.15, currViolationManagersTuesday <- 1, currViolationManagersTuesday <- 0)
  
  currManagersWednesday <- sum(wednesdayTeams$mgr == "y")/ length(wednesdayTeams$name)
  ifelse(currManagersWednesday > 0.15, currViolationManagersWednesday <- 1, currViolationManagersWednesday <- 0)
  
  currManagersThursday <- sum(thursdayTeams$mgr == "y")/ length(thursdayTeams$name)
  ifelse(currManagersThursday > 0.15, currViolationManagersThursday <- 1, currViolationManagersThursday <- 0)
  
  currManagersFriday <- sum(fridayTeams$mgr == "y")/ length(fridayTeams$name)
  ifelse(currManagersFriday > 0.15, currViolationManagersFriday <- 1, currViolationManagersFriday <- 0)
  
  #********************************************************#
  #restriction 5: no more than 10% of empty seats
  currVacancyMonday <- max(floorCapacity - sum(mondayTeams$total), 0)/floorCapacity
  ifelse(currVacancyMonday > 0.10, currViolationVacancyMonday <- 999, currViolationVacancyMonday <- 0)
  
  currVacancyTuesday <- max(floorCapacity - sum(tuesdayTeams$total), 0)/floorCapacity
  ifelse(currVacancyTuesday > 0.10, currViolationVacancyTuesday <- 999, currViolationVacancyTuesday <- 0)
  
  currVacancyWednesday <- max(floorCapacity - sum(wednesdayTeams$total), 0)/floorCapacity
  ifelse(currVacancyWednesday > 0.10, currViolationVacancyWednesday <- 999, currViolationVacancyWednesday <- 0)
  
  currVacancyThursday <- max(floorCapacity - sum(thursdayTeams$total), 0)/floorCapacity
  ifelse(currVacancyThursday > 0.10, currViolationVacancyThursday <- 999, currViolationVacancyThursday <- 0)
  
  currVacancyFriday <- max(floorCapacity - sum(fridayTeams$total), 0)/floorCapacity
  ifelse(currVacancyFriday > 0.10, currViolationVacancyFriday <- 999, currViolationVacancyFriday <- 0)
  
  return(
    sum(
         currCapacityMonday
         ,currCapacityTuesday
         ,currCapacityWednesday
         ,currCapacityThursday
         ,currCapacityFriday
         ,currViolationVacancyMonday
         ,currViolationVacancyTuesday
         ,currViolationVacancyWednesday
         ,currViolationVacancyThursday
         ,currViolationVacancyFriday
         ,currViolationsConsecutiveDaysAll
         ,currViolationsConsecutiveDaysNone
         ,currViolationsDependenciesMonday
         ,currViolationsDependenciesTuesday
         ,currViolationsDependenciesWednesday
         ,currViolationsDependenciesThursday
         ,currViolationsDependenciesFriday
         ,currViolationManagersMonday
         ,currViolationManagersTuesday
         ,currViolationManagersWednesday
         ,currViolationManagersThursday
         ,currViolationManagersFriday
    )
  )
}

GA2 <- ga2("binary", 
          fitness = evalFunc, 
          nBits = 5 * nrow(teams), popSize = 1000, 
          pcrossover = 0.8, pmutation = 0.5,
          elitism = 30, 
          maxiter = 1000, run = 200,
          numIslands = 4, 
          migrationRate = 0.1, 
          migrationInterval = 10
)

summary(GA2)
plot(GA2)

chromosome <- GA2@solution
chromosome
cat("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "\n",
    sum(teams[chromosome[(nrow(teams) * 0 + 1) :(1 *nrow(teams))] == 1, ]$total),
    sum(teams[chromosome[(nrow(teams) * 1 + 1) :(2 *nrow(teams))] == 1, ]$total),
    sum(teams[chromosome[(nrow(teams) * 2 + 1) :(3 *nrow(teams))] == 1, ]$total),
    sum(teams[chromosome[(nrow(teams) * 3 + 1) :(4 *nrow(teams))] == 1, ]$total),
    sum(teams[chromosome[(nrow(teams) * 4 + 1) :(5 *nrow(teams))] == 1, ]$total),"\n") 


