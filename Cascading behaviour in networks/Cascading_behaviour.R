library(igraph)

G <- read_graph("Desktop/Clg/Sem 6/SNA/Cascading behvaiour/random_graph.gml","gml")
plot(G, vertex.label= V(G), edge.arrow.size=0.02,layout = layout.kamada.kawai)
G
number_of_vertices <- gorder(G)
default_behaviour <- function(G){
  #setting intial behaviour of all node to B
  for (i in 1:number_of_vertices){
    G <- set_vertex_attr(G,name="behaviour",index = i,value = "B")
  }
  return(G)
}

set_intial_adopters <- function(){
  num <- length(initial_adopters)
  for (i in 1:num){
    x <- initial_adopters[i]
    G <- set_vertex_attr(G,name="behaviour",index = x ,value = "A")
  }
  return(G)
}

set_colours <- function(){
  cl <- c()
  for(i in 1:number_of_vertices){
    if(V(G)[i]$behaviour == "B")
      cl <- c(cl,"green")
    else
      cl <- c(cl,"red")
  }
  return(cl)
}

find_neighbours <- function(behaviour,node){
  num = 0
  ng <- neighbors(G,node)
  for (i in 1:length(ng)){
    if(V(G)[ng[i]]$behaviour == behaviour)
      num <- num + 1
  }
  return(num)
}

re_calculate_options <- function(x,y){
  l <- c()
  payoff_a = x
  payoff_b = y
  for (i in 1:number_of_vertices){
    if(V(G)[i]$behaviour == "B"){
      num_neighbours_a <- find_neighbours("A",i)
      num_neighbours_b <- find_neighbours("B",i)
      
      #total payoff for adopting "A" will number of neighbours having behaviour A multiplied by payoff for behaviour
      #similary for b
      t_payoff_a = num_neighbours_a * payoff_a
      t_payoff_b = num_neighbours_b * payoff_b
      if (t_payoff_a > t_payoff_b)
        l <- c(l,"A")
      else
        l <- c(l,"B")
    }else{
      l <- c(l,"A")
    }
    
  }
  return(l)
}

  
reassign_behaviour <- function(nw){
  for(i in 1:number_of_vertices){
    G = set_vertex_attr(G,name = "behaviour",index = i , value = nw[i])
  }
  return(G)
}

  
a = 1
b = 1
  
for(x in 1:3){
  a = a + 1 #increase payoff by 1
  cat("Payoff for b :",b,"\n")
  cat("Payoff for a :",a,"\n")
  for(i in 1:number_of_vertices){
    for(j in 1:number_of_vertices){
      if(j>i){G <- default_behaviour(G)
        initial_adopters <- c(i,j)
        G <- set_intial_adopters()
        color_arr <- set_colours()
        plot(G, vertex.label= V(G), edge.arrow.size=0.02,layout = layout.kamada.kawai , vertex.color = color_arr)
        count = 0
        while(1){

          if(sum(V(G)$behaviour == "A") == number_of_vertices)
            break
          
          cal <- re_calculate_options(a,b)
          G_old <- V(G)$behaviour
          G <- reassign_behaviour(cal)
          G_new <- V(G)$behaviour
          color_arr <- set_colours()
          plot(G, vertex.label= V(G), edge.arrow.size=0.02,layout = layout.kamada.kawai , vertex.color = color_arr)
          
          if(sum(G_old == "A") == sum(G_new == "A"))
            break
        
        }
        
        cat("For nodes : ",i," ",j,"\n")
        if(sum(V(G)$behaviour == "A") == number_of_vertices){
          print("Complete Cascade")
          cat("Cascade size :",number_of_vertices,"\n\n\n\n")
        }else{
          print("incomplete Cascade")
          
          cat("Cascade size : ",sum(V(G)$behaviour == "A"),"\n\n\n")
        }
      }
    }
  }
}