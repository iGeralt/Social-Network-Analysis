library(igraph)
G = make_empty_graph(101)
G = G + edges(c(1,5,1,3,1,4))
i = 3
while(i <51)
{
  G = G + edges(c(i,2*i,i,2*i+1))
  i = i+1
}
G <- G - 2
plot (G, vertex.color = "yellow", edge.color = "blue", edge.arrow.size = 0.1, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-3,5), vertex.size=50)


