library(igraph)

graph_data<-read.csv("~/Downloads/StandingClusterOozieWorkflowDump/ScratchPad/for_arvind_2", header=FALSE, sep=" ", colClasses=c(rep("character",2)), stringsAsFactors = FALSE)
workflow_data<-read.csv("~/Downloads/StandingClusterOozieWorkflowDump/ScratchPad/20_workflow_names_and_ids", header=FALSE, sep=" ", colClasses=c(rep("character",3)), stringsAsFactors = FALSE)
required_hql_data<-read.csv("~/Downloads/StandingClusterOozieWorkflowDump/ScratchPad/all_hqls_by_workflow_dir_2", header=FALSE, sep=" ", colClasses=c(rep("character",2)), stringsAsFactors = FALSE)
all_hql_data<-read.csv("~/Downloads/StandingClusterOozieWorkflowDump/ScratchPad/all_hqls", header=FALSE, sep=" ", colClasses=c(rep("character",2)), stringsAsFactors = FALSE)

w_edges<-data.frame(from=graph_data[,1], to=graph_data[,2])
wh_edges<-data.frame(from=c(graph_data[,1],required_hql_data[,1]), to=c(graph_data[,2],required_hql_data[,2]))

w_g <- graph.data.frame(w_edges, directed=TRUE)
wh_g <- graph.data.frame(wh_edges, directed=TRUE)

top_workflows = workflow_data[,1]
# which(V(g)$name %in% top_workflows)
# V(g)[(V(g)$name %in% top_workflows)]
# sum(V(g)$name %in% top_workflows)

w_r<-graph.bfs(w_g, root=which(V(w_g)$name %in% top_workflows), neimode='out', unreachable=FALSE, order=TRUE)
is_w_20wf<-induced_subgraph(w_g, V(w_g) %in% w_r$order)
plot(is_w_20wf, vertex.size=4, vertex.label.cex=0.6)

wh_r<-graph.bfs(wh_g, root=which(V(wh_g)$name %in% top_workflows), neimode='out', unreachable=FALSE, order=TRUE)
is_wh_20wf<-induced_subgraph(wh_g, V(wh_g) %in% wh_r$order)
plot(is_wh_20wf, vertex.size=4, vertex.label.cex=0.6)

dcom_w_g = decompose.graph(w_g)
dcom_wh_g = decompose.graph(wh_g)
dcom_is_w_20wf = decompose.graph(is_w_20wf)
dcom_is_wh_20wf = decompose.graph(is_wh_20wf)

w_r1<-graph.bfs(w_g, root=which(V(w_g)$name %in% top_workflows[1]), neimode='out', unreachable=FALSE, order=TRUE, father=TRUE, succ=TRUE)
is_w1<-induced.subgraph(w_g, V(w_g) %in% w_r1$order)
plot(is_w1, vertex.size=4, vertex.label.cex=0.6)

wh_r1<-graph.bfs(wh_g, root=which(V(wh_g)$name %in% top_workflows[1]), neimode='out', unreachable=FALSE, order=TRUE, father=TRUE, succ=TRUE)
is_wh1<-induced.subgraph(wh_g, V(wh_g) %in% wh_r1$order)
plot(is_wh1, vertex.size=4, vertex.label.cex=0.6)

i=intersect(V(is_wh_20wf)$name,V(is_w_20wf)$name)
d=setdiff(V(is_wh_20wf)$name,V(is_w_20wf)$name)

w_of_interest=c(i, d[1:12])
h_of_interest=setdiff(d,w_of_interest)

# plot(dcom_w_g[[1]], vertex.size=4, vertex.label.cex=0.6, layout=layout_as_tree(gs[[1]], flip.y=FALSE))

