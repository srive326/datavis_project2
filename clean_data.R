load(file = "data/realitymining.Rda")

n <- 94

groups <- rep(NA, n)
for (subjectID in 1:n) {
  
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
