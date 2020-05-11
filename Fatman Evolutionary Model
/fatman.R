  library(igraph)
  
  
  G <- make_empty_graph(100) %>% set_vertex_attr("name",value = as.integer(runif(100,15,40))) %>% set_vertex_attr("Type",value = "Person")
  
  #adding foci nodes
  add_foci_nodes <- function(G)
  {
    i = gorder(G) + 1
    foci_nodes = c("gym","eatout","movie_club","karate_club","yoga_club")
    for(j in 1:5)
    {
      G = G + vertices(i)
      G = set_vertex_attr(G,"name",index = i,value = foci_nodes[j])
      G = set_vertex_attr(G,"Type",index = i,value = "foci")
      i = i + 1
    }
    return(G) 
  }
  
  get_size <- function(G)
  {
    num = gorder(G)
    nsl <- c()
    for (i in 1:num)
    {
      if (V(G)[i]$Type == "Person")
        nsl<- c(nsl,V(G)[i]$name)
      else
        nsl <- c(nsl,50)
    }
    return(nsl)
  }
  get_labels <- function(G)
  {
    num = gorder(G)
    ll <- c()
    for (i in 1:num)
    {
      if(V(G)[i]$Type == "Person")
        ll <- c(ll,(V(G)[i]$name))
      else
        ll <- c(ll,V(G)[i]$name)
    }
    return(ll)
  }
  
  get_color <- function(G)
  {
    num = gorder(G)
    cl <- c()
    for (i in 1:num)
    {
      if(V(G)[i]$Type == "Person")
        if(V(G)[i]$name == 15)
          cl <- c(cl,"green")
        else if(V(G)[i]$name == 39)
          cl <- c(cl,"yellow")
        else
          cl <- c(cl,"orange")
      else
        cl <- c(cl,"red")
    }
    return(cl)
  }
  
  get_person_nodes <- function()
  {
    num = gorder(G)
    for (i in 1:num)
    {
      p <- c()
      for (i in 1:num)
      {
        if(V(G)[i]$Type == "Person")
          p <- c(p,i)
      }
    }
    print(p)
    return (p)
  }
  
  get_foci_nodes <- function()
  {
    num = gorder(G)
    for (i in 1:num)
    {
      f <- c()
      for (i in 1:num)
      {
        if(V(G)[i]$Type == "foci")
          f <- c(f,i)
      }
    }
    return (f)
  }
  
  add_foci_edges <- function(G)
  {
    f = get_foci_nodes()
    p = get_person_nodes()
    for (j in p)
    {
      r = sample(f,1)
      G = G + edge(j,r)
    }
    return(G)
  }
  
  homophily <-function(G)
  { i =0
    pnodes = get_person_nodes()
    for (u in pnodes){
      for (v in pnodes)
      {
        if (u!=v)
        {
         
          diff = abs(as.integer(V(G)[u]$name) -as.integer( V(G)[v]$name))
          p = 1/(diff + 1000)
         
          r = runif(1)
   
          if (r < p)
          {
            i = i+1
            print(i)
            G = G + edges(u,v)
          }
        }
      }
    }
    return (G)
  }
  cmn <- function(u,v,G)
  {
    nv = neighbors(G,v)
    nu = neighbors(G,u)
    return (length(intersect(nv,nu)))
  }
  closures <- function(G)
  {
    arr <- c()
    num = gorder(G)
    for (u in 1:num)
      for (v in 1:num)
      {
        if(u!=v && (V(G)[u]$Type == "Person" || V(G)[v]$Type == "foci"))
        {
          k = cmn(u,v,G)
          p = 1 - (1-0.01)^k
          temp <- c()
          temp <- c(temp,u)
          temp <- c(temp,v)
          temp <- c(temp,p)
          arr <- rbind(arr,temp)
        }
      }
    for (each in 1:nrow(arr))
    {
      u = arr[each,1]
      v = arr[each,2]
      p = arr[each,3]
      r = runif(1)
      if (r < p)
      {
        G = G + edges(u,v)
      }
    }
    return (G)
  }
  
  G = add_foci_nodes(G)
  nodesize = get_size(G)
  labels = get_labels(G)
  color_arr = get_color(G)
  G = add_foci_edges(G)

  plot(G,vertex.label = labels,vertex.size=as.integer(nodesize),vertex.label.font = 0.01,
       rescale = FALSE, ylim=c(-4,12),xlim=c(-4,12), asp = 0,layout = layout.kamada.kawai,
       vertex.color = color_arr,edge.arrow.size = 0.1)
    G = homophily(G)

  plot(G,vertex.label = labels,vertex.size=as.integer(nodesize),vertex.label.font = 0.01,
       rescale = FALSE, ylim=c(-4,12),xlim=c(-4,12), asp = 0,layout = layout.kamada.kawai,
       vertex.color = color_arr,edge.arrow.size = 0.1)
  while(1){
  G = closures(G)
  plot(G,vertex.label = labels,vertex.size=as.integer(nodesize),vertex.label.font = 0.01,
       rescale = FALSE, ylim=c(-4,12),xlim=c(-4,12), asp = 0,layout = layout.kamada.kawai,
       vertex.color = color_arr,edge.arrow.size = 0.1)}
  
    