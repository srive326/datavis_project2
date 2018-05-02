library(shiny)
library(tidyverse)
library(networkD3)
library(ggplot2)
library(plotly)

# Load data for vizzes
load(file = "data/friendsmatrix.Rda")
load(file = "data/networkIDmapping.Rda")
load(file = "data/groups.Rda")
load(file = "data/affiliations.Rda")
load(file = "data/proximityEvents.Rda")
load(file = "data/list_ofplots.Rda")
load(file = "data/subjects_linegraph.Rda")
load(file = "data/predictabilityResponses.Rda")
load(file = "data/heatmap_list.Rda")
load(file = "data/texts.Rda")



#####################################################
###       Create friendship network graph         ###
#####################################################

N <- nrow(friends)

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
friendNodes <- data.frame(subjectID = mapped_ids, size = sizes, group = affiliations)

# Create df for links
friendLinks <- data.frame(source = sources, target = targets, value = values)

viz.forceNetwork <- forceNetwork(Links = friendLinks, Nodes = friendNodes, Source = "source", 
                                 Target = "target", Value = "value", NodeID = "subjectID", 
                                 Nodesize = "size", Group = "group", 
                                 linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
                                 arrows = TRUE, legend = TRUE, zoom = T, 
                                 opacity = 1, opacityNoHover = 1, charge = -30, 
                                 height = 10000, width = 10000)

#####################################################
###            Create box plot viz                ###
#####################################################

# Manually calculate IQR and quartiles so we can find outliers
vals <- texts %>% 
  group_by(response) %>% 
  summarise(iqr = IQR(num), upper.quartile = quantile(num, probs=0.75),
            lower.quartile = quantile(num, probs=0.25)) %>% 
  mutate(outlier.bound.lower = pmax(0, (lower.quartile - (1.5 * iqr))),
         outlier.bound.upper = upper.quartile + 1.5 * iqr)

# This is a kludge. looping through all factor levels to get data frame of outliers
outliers <- data.frame(id=c(), response=c(), num=c())
for (i in 1:nrow(vals)) {
  tmp <- texts %>% filter(response == vals$response[i])
  outliers <- bind_rows(outliers, 
                        tmp %>% filter(num < vals$outlier.bound.lower[i])
  )
  outliers <- bind_rows(outliers, 
                        tmp %>% filter(num > vals$outlier.bound.upper[i])
  )
}

# Box plot with scatter points of outliers added on top with proper tooltip
viz.boxplot <- plot_ly(texts, 
        y = ~num, x=~response, 
        color = ~response, 
        type = "box", 
        hoverinfo = "y") %>%
  add_markers(data = outliers, hoverinfo = "text", text = paste(outliers$num, "\nID: ", outliers[,'id'])) %>% 
  plotly::layout(title = '"How often do you send text messages?"', 
                 xaxis = list(title = "Response"), 
                 yaxis = list(title = "Avg. text msgs / month"),
                 hovermode = "closest"
  )
#####################################################


# Define UI as tabbed page
ui <- navbarPage(
  theme = "app.css",
  title = "Visualizing Survey Bias",
  tabPanel("Predictability",
    fluidRow(
      column(
        width = 6,
        selectizeInput(
          "SubjectID",
          "Subject ID", 
          subjects_linegraph[,1],
          selected = 3
        )
      ),
      column(
        width = 6,
        tags$div(tags$b('"How predictable are you?"'), class = "predictabilityResponse"),
        htmlOutput("predictabilityResponse")
      )
    ),

    fluidRow(
      column(width = 6, plotOutput("heatmap")),
      column(width = 6, plotOutput("lineGraph"))
    )

  ),
 
  tabPanel(
    "Friendship", 
    forceNetworkOutput("friendNetwork")
  ),

  tabPanel("Communication",
           plotlyOutput("boxPlot")
  )
)


# Define server logic required to draw vizzes
server <- function(input, output) {
   
   output$heatmap <- renderPlot({ 
     index <- match(input$SubjectID, subjects_linegraph[,1])
     latest_ting <- data.frame(heatmap_list[[index]])
     
     latest_ting[is.na(latest_ting)] <- 0
     latest_ting[latest_ting == 3] <- 0
     latest_ting[latest_ting == 2] <- 0
     
     happy <- aggregate(latest_ting[,2:25], by=list(day = latest_ting$day), FUN=sum)
     names(happy) <- c("day",1:24)
     happy<- happy[c(4,2,6,7,5,1,4),]
     heatmap(as.matrix(t(happy[,2:25])), Rowv=NA, Colv=NA, col = cm.colors(256),scale="none", margins=c(8,10), labCol = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
   })
   
   output$lineGraph <- renderPlot({
     index <- match(input$SubjectID, subjects_linegraph[,1])
     return(list_ofplots[[index]])
   })
   
   output$boxPlot <- renderPlotly({
     return(viz.boxplot)
   })
   
   output$friendNetwork <- renderForceNetwork({
     return(viz.forceNetwork)
   })
   
   output$predictabilityResponse <- renderText({
     t <- predictability.responses[as.integer(input$SubjectID)]
     if (is.na(t)) {
       t <- "No Response"
     }
     return(paste("<div id=predictabilityResponseResponse class=predictabilityResponse>", t, "</div>"))
   })
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

