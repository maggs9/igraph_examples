setwd("~/Documents/exptswithigraph")
library(igraph)
library(plyr)

graph_data<-read.csv("Workflow_DATA - Meghana.tsv", header=TRUE, sep="\t", stringsAsFactors = FALSE, strip.white=TRUE)

### assocaite Workflow name ###
for (i in 1:nrow(graph_data)){
  if (graph_data$Workflow.Name[i] != "") {
    wrkname <- graph_data$Workflow.Name[i]
    i <- i+1
    while (graph_data$Workflow.Name[i] == "" && i<= nrow(graph_data)){
      graph_data$Workflow.Name[i] <- wrkname
      i <- i+1
    }
  }
}

### Flatten ####

new_assoc <- graph_data[FALSE, c("Workflow.Name","Table.Name","Table.Dependence","Workflow.Step")]
k=1
for(i in 1:nrow(graph_data)){
  table_n <- graph_data$Table.Name[i]
  ass_tables <- graph_data$Table.Dependence[i]
  if(ass_tables != ""){
    table_list <- unlist(strsplit(ass_tables,","))
    for (j in 1:length(table_list)){
      newrow <- c(graph_data$Workflow.Name[i],trimws(table_n),trimws(table_list[j]),graph_data$Workflow.Step[i])
      print(paste(newrow))
      new_assoc[k,] <- newrow
      k <- k+1
    }
  }
}

## Graph according to workflow name ##
list_workflow <- unique(new_assoc$Workflow.Name)
workflowname <- readline(prompt= paste("Enter WORKFLOW Name from this list : ", paste(list_workflow, collapse=", ")," "))
req_df <- new_assoc[FALSE, c("Table.Dependence","Table.Name")]
vertex_list <- data.frame()
k <- 1
i <- 1
for (j in 1: nrow(new_assoc)){
  if (new_assoc$Workflow.Name[j] == workflowname){
    vertex_list[i,1] <- new_assoc$Table.Dependence[j]
    vertex_list[i+1,1] <- new_assoc$Table.Name[j]
    req_df[k,] <- c(trimws(new_assoc$Table.Dependence[j]),trimws(new_assoc$Table.Name[j]))
    k  <- k+1
    i <- i+2
  }
}
#vertex_list = unique(rbind.fill(data.frame(req_df$Table.Name), data.frame(req_df$Table.Dependence)))
net <- graph.data.frame(req_df,vertices = unique(vertex_list), directed=TRUE)
net <- simplify(net, remove.multiple = F, remove.loops = T)
plot(net, edge.arrow.size=.3,vertex.size= 5,vertex.label.degree = -pi/2)


