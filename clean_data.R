###
# EDA Things to subset the data
###

load(file = "/home/noah/git/datavis_project2/data/realitymining.Rda")

N <- 94

# Save my.group response for every subject
groups <- rep(NA, N)
for (subjectID in 1:N) {
  
  temp <- data$s["my.group", 1, subjectID][[1]]
  
  if (!is_empty(temp)) {
    temp <- temp[[1]]
    if (!is_empty(temp)) {
      temp <- temp[[1]]
      if (!is_empty(temp)) {
        groups[subjectID] <- temp[1,1]
      }
    }
  }
}

save(groups, file = "/home/noah/git/datavis_project2/data/groups.Rda")

# Save survey start date for every subject
start.dates <- rep(NA, N)
for (subjectID in 1:N) {
  
  temp <- data$s["survey.start.n", 1, subjectID][[1]]
  
  if (ncol(temp) > 0) {
    start.dates[subjectID] <- temp[1,1]
  }
}

save(start.dates, file = "/home/noah/git/datavis_project2/data/startDates.Rda")

# Save text message response for every subject

# Save friends matrix
friends <- data$network[[1]]

save(friends, file = "/home/noah/git/datavis_project2/data/friendsmatrix.Rda")

