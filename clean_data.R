###
# EDA Things to subset the data
###

load(file = "/home/noah/git/datavis_project2/data/realitymining.Rda")

# Save friends matrix and mapping of indices to subject IDs
friends <- data$network[[1]]

N <- nrow(friends)

sub_sort <- data$network[4][[1]][1,]

mapped_ids <- sapply(1:94, function(x) return (sub_sort[x]))

save(friends, file = "/home/noah/git/datavis_project2/data/friendsmatrix.Rda")

save(mapped_ids, file = "/home/noah/git/datavis_project2/data/networkIDmapping.Rda")


# Save my.group response for every subject in network
groups <- rep(NA, N)
for (subjectID in mapped_ids) {
  print(subjectID)
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


# Get a list of mac addresses in integer form for every subject
library(install.load)
install_load('Rmpfr')

macs <- rep(NA, N)
for (subjectID in 1:N) {
  
  temp <- data$s["my.mac", 1, subjectID][[1]]
  #print(subjectID)
  if (length(temp) != 0) {
    macs[subjectID] <- as.numeric(mpfr(temp[[1]][[1]][1,1], base=16))
  }
}

# Save proximity events
proximity.events <- rep(NA, N)

save(groups, file = "/home/noah/git/datavis_project2/data/groups.Rda")
