library (igraph)

#G = read_graph ("dolphin.txt",format = "edgelist",directed = F)
G = read_graph("karate.gml",format = "gml")
number_of_vertex = gorder(G)

for (i in 1:gorder(G)){
  G <- set.vertex.attribute(G,"status",i,"S")
  G <- set.vertex.attribute(G,"time",i,0)
  G <- set.vertex.attribute(G,"colour",i,"cyan")
}

#intial values
i_inf = c(1,12)
p = 0.6
t_e = 6
t_i = 8
t_r = 5
day = 15
V(G)[i_inf]$status = "I"
V(G)[i_inf]$time = t_i

count = 1
while (count != day) {
  #Check if time of any node is greater than zero
  #if it's greater than zero than decrease it by 1
  for(i in 1:number_of_vertex){
    if(V(G)[i]$time > 0)
      V(G)[i]$time = V(G)[i]$time - 1     
  }
  #Check status of each node if it's in "I" state and it's time is equal to zero then
  #assign new state "R" and  change time to t_r
  #change colour to green
  for (i in 1:number_of_vertex){
    if (V (G)[i]$status == "I"){
      if (V (G)[i]$time  == 0){
        V(G)[i]$status = "R"
        V(G)[i]$time = t_r
        V(G)[i]$colour = "green"
      }
    }
  }
  #Make a list which contains only nodes whose state is "I"
  #Find neighbours of each node in the list 
  #check if the p is greater than random number(less than 1) and status is "S"
  #if both are true then change it's state to E and time to t_e and colour to orange
  inf_list = which(V(G)$status == "I")
  for(i in inf_list){
    ng = neighbors(G,i)
    for(j in ng){
      if(runif(1) < p && V(G)[j]$status == "S"){
        V(G)[j]$status = "E"
        V(G)[j]$time = t_e
        V(G)[j]$colour = "orange" 
      }
    }
  }
  #Check status of each node if it's in "R" state and it's time is equal to zero then
  #assign new state "S" 
  #change colour to cyan
  for(i in 1:number_of_vertex){
    if(V (G)[i]$status == "R"){
      if(V (G)[i]$time  == 0){
        V(G)[i]$status = "S"
        V(G)[i]$colour = "cyan"
      }
    }
  }
  #Check status of each node if it's in "E" state and it's time is equal to zero then
  #assign new state "I" and  change time to t_i
  #change colour to red
  for (i in 1:number_of_vertex){
    if (V (G)[i]$status == "E"){
      if (V (G)[i]$time  == 0){
        V(G)[i]$status = "I"
        V(G)[i]$time = t_i
        V(G)[i]$colour = "red"
      }
    }
  }
  count = count + 1
  
  cat("\n\n Day : ",count)
  cat("\n\n Susceptible :" ,which(V(G)$status == "S"))
  cat("\n Exposed :" ,which(V(G)$status == "E"))
  cat("\n Infected : ",which(V(G)$status == "I"))
  cat("\n Recovered : ",which(V(G)$status == "R"))
  
  cat("\n\nNumber of Susceptible nodes : ",sum(V(G)$status == "S"))
  cat("\nNumber of Exposed nodes : ",sum(V(G)$status == "E"))
  cat("\nNumber of Infected nodes : ",sum(V(G)$status == "I"))
  cat("\nNumber of Recovered nodes : ",sum(V(G)$status == "R"))
  
  plot (G,layout = layout.kamada.kawai,vertex.color = V(G)$colour, main = paste ("Day", count))
} 







