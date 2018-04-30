#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Load data for vizzes
load(file = "data/friendsmatrix.Rda")
load(file = "data/networkIDmapping.Rda")
load(file = "data/groups.Rda")
load(file = "data/proximityEvents.Rda")


#####################################################
# Create nodes and edges for friendship network graph
n <- nrow(friends)

sources <- c()
targets <- c()
values <- c()

# Generate links
k <- 1
for (i in 1:N) {
  for (j in 1:N) {
    if (!is.na(friends[i,j])) {
      sources[k] <- i-1 # forceNetwork requires 0 indexing
      targets[k] <- j-1
      values[k] <- log(proximity.events[i,j] + proximity.events[j,i] + 1) + 1 # pad by 1 so there's no zero-width links
      k <- k + 1
    }
  }
}

# Count number of friends for every subject in the network
sizes <- sapply(1:N, function(s) return (exp(sum(targets + 1 == s))), USE.NAMES = FALSE)

# Create df for nodes (1 per subject)
friendNodes <- data.frame(subjectID = mapped_ids, size = sizes, group = groups)

# Create df for links
friendLinks <- data.frame(source = sources, target = targets, value = values)

#####################################################


# Define UI with tabs
ui <- navbarPage("Visualizing Survey Bias",
                 tabPanel("Predictability", plotOutput("distPlot")),
                 tabPanel("Friendship", forceNetworkOutput("friendNetwork"))
      )


# Define server logic required to draw vizzes
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     plot(iris)
   })
   
   output$friendNetwork <- renderForceNetwork({
     forceNetwork(Links = friendLinks, Nodes = friendNodes, Source = "source", 
                  Target = "target", Value = "value", NodeID = "subjectID", 
                  Nodesize = "size", Group = "group", 
                  linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
                  arrows = TRUE, legend = TRUE, zoom = T, 
                  opacity = 1, opacityNoHover = 1, charge = -75)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

