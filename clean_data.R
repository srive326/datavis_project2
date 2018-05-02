###
# EDA things to subset the data
###

load(file = "/home/noah/git/datavis_project2/data/realitymining.Rda")

### Save friends matrix and mapping of indices to subject IDs

friends <- data$network[[1]] # 94 by 94 matrix, where each row maps to responses from some subject

N <- 106 # number of total subjects in data

network_N <- 93 # cut off row 94, because the sub_sort mapping maps that to subject ID 107, which doesn't exist :(

friends <- friends[1:network_N, 1:network_N]

sub_sort <- data$network[4][[1]][1,] # Maps network indices to subject ID

mapped_ids <- sapply(1:network_N, function(x) return (sub_sort[x])) # Get subject IDs for network indices

save(friends, file = "/home/noah/git/datavis_project2/data/friendsmatrix.Rda")

save(mapped_ids, file = "/home/noah/git/datavis_project2/data/networkIDmapping.Rda")


# Save my.group response for every subject in network
groups <- rep(NA, network_N)
for (index in 1:network_N) {
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

# Save my.affil response for every subject in network
affiliations <- rep(NA, network_N)
for (index in 1:network_N) {
  
  temp <- data$s["my.affil", 1, mapped_ids[index]][[1]]
  
  if (length(temp) != 0) {
    affiliations[index] <- temp[[1]][[1]][1,1]
  }
}

map_affiliations <- function(s) {
  return (switch(s, 
              grad =,
              mlgrad = "Media Lab Grad Student", 
              '1styeargrad ' = "Media Lab 1st Year Grad Student", 
              professor = "Media Lab Professor", 
              mlstaff = "Media Lab Staff",
              sloan =,
              sloan_2 = "Sloan Business School",
              mlurop = "Media Lab Undergraduate",
              mlfrosh = "Media Lab First Year Undergraduate"))
}


affiliations <- sapply(affiliations, map_affiliations, USE.NAMES = FALSE)

save(affiliations, file = "/home/noah/git/datavis_project2/Self-Report-Bias/data/affiliations.Rda")


# Save survey start date for every subject (all 106)
start.dates <- rep(NA, N)
for (subjectID in 1:N) {
  
  temp <- data$s["survey.start.n", 1, subjectID][[1]]
  
  if (ncol(temp) > 0) {
    start.dates[subjectID] <- temp[1,1]
  }
}
save(start.dates, file = "/home/noah/git/datavis_project2/data/startDates.Rda")


# Get a list of mac addresses in integer form for every subject in network
library(install.load)
install_load('Rmpfr')

macs <- rep(NA, network_N)
for (index in 1:network_N) {
  
  temp <- data$s["my.mac", 1, mapped_ids[index]][[1]]
  #print(mapped_ids[index])
  if (length(temp) != 0) {
    macs[index] <- as.numeric(mpfr(temp[[1]][[1]][1,1], base=16))
  }
}

# Use mac addresses to save number of proximity events between network subjects
proximity.events <- matrix(0, nrow=network_N, ncol=network_N)
for (i in 1:network_N) {
  
  event_list <- data$s["device.macs", 1, mapped_ids[i]][[1]]
  #print(mapped_ids[i])
  event_list.length <- length(event_list)
  if (event_list.length != 0) {
    for (j in 1:network_N) {
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

# Save predictability survey response for all subjects

predictability.responses <- rep(NA, N)
for (index in 1:N) {
  temp <- data$s["my.predictable", 1, index][[1]]
  
  if (length(temp) != 0) {
    temp <- temp[[1]][[1]]
    if (length(temp) != 0) {
      predictability.responses[index] <- temp[1,1]
    }
  }
}

save(predictability.responses, file = "/home/noah/git/datavis_project2/data/predictabilityResponses.Rda")


# Save text message data frame for all subjects

text.responses <- rep(NA, N)
for (index in 1:N) {
  temp <- data$s["surveydata", 1, index][[1]]
  if (length(temp) != 0) {
    text.responses[index] <- temp[1,][9]
  }
}

map_text_response <- function(x) {
  t <- switch(x, 
              "Several times / day", 
              "once / day", 
              "once / week", 
              "once / month", 
              "never")
  if (is.null(t)) {
    t <- "No Response"
  } 
  
  return(t)
}
  

text.responses <- sapply(text.responses, map_text_response)

text.responses <- factor(text.responses, 
                         levels = c(
                           "No Response",
                           "never",
                           "once / month",
                           "once / week",
                           "once / day",
                           "Several times / day"
                         ),
                         ordered = TRUE
                  )

end.dates <- rep(NA, N)
for (subjectID in 1:N) {
  
  temp <- data$s["my.enddate", 1, subjectID][[1]]
  
  if (ncol(temp) > 0) {
    start.dates[subjectID] <- temp[1,1]
  }
}

numTextsPerMonth <- rep(NA, N)
for (index in 1:N) {
  numTexts[index] <- data$s["comm.sms", 1, index][[1]][1,1]
}

texts <- data.frame(id = 1:N, response = text.responses, num = numTexts)

save(texts, file = "L:/Noah Johnson/data viz/datavis_project2/Self-Report-Bias/data/texts.Rda")


