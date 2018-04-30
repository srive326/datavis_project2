###
# EDA things to subset the data
###

load(file = "/home/noah/git/datavis_project2/data/realitymining.Rda")

# Save friends matrix and mapping of indices to subject IDs
friends <- data$network[[1]]

N <- 93 # cut off row 94, because the sub_sort mapping maps that to subject ID 107, which doesn't exist :(

friends <- friends[1:N, 1:N]

sub_sort <- data$network[4][[1]][1,] # Maps network indices to subject ID

mapped_ids <- sapply(1:N, function(x) return (sub_sort[x])) # Get subject IDs for network indices

save(friends, file = "/home/noah/git/datavis_project2/data/friendsmatrix.Rda")

save(mapped_ids, file = "/home/noah/git/datavis_project2/data/networkIDmapping.Rda")


# Save my.group response for every subject in network
groups <- rep(NA, N)
for (index in 1:N) {
  #print(mapped_ids[index])
  temp <- data$s["my.group", 1, mapped_ids[index]][[1]]
  
  if (!is_empty(temp)) {
    temp <- temp[[1]]
    if (!is_empty(temp)) {
      temp <- temp[[1]]
      if (!is_empty(temp)) {
        groups[index] <- temp[1,1]
      }
    }
  }
}

save(groups, file = "/home/noah/git/datavis_project2/data/groups.Rda")

# Save survey start date for every subject (all 106)
start.dates <- rep(NA, 106)
for (subjectID in 1:106) {
  
  temp <- data$s["survey.start.n", 1, subjectID][[1]]
  
  if (ncol(temp) > 0) {
    start.dates[subjectID] <- temp[1,1]
  }
}
save(start.dates, file = "/home/noah/git/datavis_project2/data/startDates.Rda")

# Save text message response for every subject


# Get a list of mac addresses in integer form for every subject in network
library(install.load)
install_load('Rmpfr')

macs <- rep(NA, N)
for (index in 1:N) {
  
  temp <- data$s["my.mac", 1, mapped_ids[index]][[1]]
  #print(mapped_ids[index])
  if (length(temp) != 0) {
    macs[index] <- as.numeric(mpfr(temp[[1]][[1]][1,1], base=16))
  }
}

# Use mac addresses to save number of proximity events between network subjects
proximity.events <- matrix(0, nrow=N, ncol=N)
for (i in 1:N) {
  
  event_list <- data$s["device.macs", 1, mapped_ids[i]][[1]]
  #print(mapped_ids[i])
  event_list.length <- length(event_list)
  if (event_list.length != 0) {
    for (j in 1:N) {
      if (!is.na(macs[j])) {
        count <- 0
        
        for (k in 1:event_list.length) {
          temp <- event_list[k][[1]]
          if (!is.null(temp)) {
            temp <- temp[[1]][,1]
            count <- count + sum(temp == macs[j])
          }
        }
        
        proximity.events[i,j] <- count
      }
    }
  }
}

save(proximity.events, file = "/home/noah/git/datavis_project2/data/proximityEvents.Rda")
