############ stringdistances ############

stringdistances <- function(seq, algo){
  if(algo == "BLOSUM80"){
    sim = stringDist(seq, method = "substitutionMatrix", substitutionMatrix = "BLOSUM80")
    sim = sim + abs(min(sim))
  } else if(algo == "LetterProb") {
    sim = letter.prob(seq)
  } else {
    sim = stringdistmatrix(seq, method = algo, useBytes = FALSE, q = 2)
  }
  
  if(algo %in% c("dl", "lv", "osa", "lcs", "hamming", "qgram", "BLOSUM80")){
    if(algo %in% c("lcs","qgram")){
      funn = sum
    } else {
      funn = max
    } 
    
    lengths = nchar(as.character(seq))
    normal = as.vector(combn(lengths, 2, FUN = funn))
    sim = sim / normal
    
    as.matrix(sim)
  }
  
  return(sim)
}

############ visualiseGenes ############ 

visualiseGenes <- function(data){
  
  udata = unique(data)
  udata = udata[order(udata)]
  label = match(data, udata)
  list("name" = udata,
       "label" = label)
  
}

############ uniteDatasets ############

uniteDatasets <- function(...){
  x = list(...)
  dataset = c()
  for(i in 1:length(x)){
    dataset = rbind(dataset, x[i])
  }
  
  dataset = as.data.frame(unique(dataset))
  return(dataset)
}

############ subtractDatasets ############

subtractDatasets <- function(minuend,subtracter){
  minuend = setdiff(minuend, subtracter)
  return(minuend)
}

############ include in graph ############

includeInGraph <- function(data, x){
  
  data = cbind(data, id = c(1:nrow(data)))
  
  for(l in x){
    tempdata = c()
    if(!is.numeric(data[,l[1]])){
      
      if(str_detect(l[2:length(l)], ";")){
        tempdata = find.motifs(data = data[,l[1]], motifs = l[2:length(l)])
        tempdata = data[tempdata, ]
      } else {
        tempdata = data[data[,l[1]] %in% l[2:length(l)],]
      }
      
    } else {
      tempdata = data[which(data[,l[1]] >= as.numeric(l[2]) & data[,l[1]] <= as.numeric(l[3])),]
    } 
    
    data = tempdata  
  }
  
  return(sort(data$id))   
}

############ excludeFromGraph ############

excludeFromGraph <- function(data, x){
  
  data = cbind(data, id = c(1:nrow(data)))
  
  for (l in x){
    if (!is.numeric(data[,l[1]])){
      data = data[-which(data[,l[1]] %in% l[2:length(l)]),]
    } else {
      data = data[-which(data[,l[1]]>l[2] & data[,l[1]]<l[3]),]
    }
  }
  
  return (data$id)
}

############ communities_general ############

communities_general <- function(ig, threshold = 1, weights = NULL, algorithm = "louvain"){
  
  ig = delete_edges(ig, which(E(ig)$weight > threshold))
  
  if (algorithm=="edge_betweenness"){
    if (is.null(weights)){
      model = cluster_edge_betweenness(ig)
    } else {
      model = cluster_edge_betweenness(ig, weights = weights)
    }
  } else if(algorithm == "hierarchical"){
    dist = distances(ig)
    hc = hclust(as.dist(dist), method = "ward.D")
    mod = c()
    for(i in 1:min(20, gorder(ig))){
      clusters = cutree(hc, k = i)
      mod = c(mod, modularity(ig, clusters))
    }
    clusters = cutree(hc, which.max(mod))
    model = list("membership" = clusters)
  } else {
    if(is.null(weights)){
      E(ig)$weight[E(ig)$weight != 0] = 1 - E(ig)$weight[E(ig)$weight !=0 ]
    }
    
    if(algorithm=="louvain"){
      model = cluster_louvain(ig, weights = weights)
    } else if(algorithm == "fast_greedy"){
      model = cluster_fast_greedy(ig, weights = weights)
    } else if(algorithm == "label_propagation"){ 
      model = cluster_label_prop(ig, weights = weights)
    } else if(algorithm == "walktrap"){
      model = cluster_walktrap(ig, steps = 10)
    } else if(algorithm == "leading_eigenvalue"){ 
      model = cluster_leading_eigen(ig, weights = weights)
    }
    
    if (is.null(weights)){
      E(ig)$weight=1-E(ig)$weight
    }
  }
  return(model)
}

############ confusion ############

confusion <- function(clustering1, clustering2, threshold = 0.5){
  
  mat = table(clustering1, clustering2)
  mat1 = prop.table(mat, 1)
  mat2 = t(prop.table(mat, 2))
  x1 = which(mat1 > threshold, arr.ind = TRUE)
  x2 = which(mat2 > threshold, arr.ind = TRUE)
  
  x3 = c()
  if(nrow(x1) > 0 && nrow(x2) > 0){
    for(i in 1:nrow(x1)){
      for(j in 1:nrow(x2)){
        temp = (x1[i,] == x2[j, c(2,1)])
        
        if(temp[1] && temp[2]){
          x3 = rbind(x3, x1[i,])
        }
      }
    }
  }
  
  
  list("list1" = x1,
       "list2" = x2,
       "list3" = x3,
       "mat1" = mat1,
       "mat2" = mat2)
}

############ conductance ############

conductance <- function(graph, membership){
  edges = as_data_frame(graph)
  edges$from = membership[edges$from]
  edges$to = membership[edges$to]
  con = c()
  total = sum(edges$weight)
  
  conduct <- function(i, edges, total){     
    inter = sum(edges$weight[xor(edges$to == i, edges$from == i)])
    clust = sum(edges$weight[edges$to == i | edges$from == i])
    con = inter / min(total - clust, clust)
    
    return(con)
  }
  
  con <- lapply(sort(unique(membership)), conduct, edges = edges, total = total)
  
  
  list(conductance = 1 - mean(unlist(con)), conductances = con)
}

############ coverage ############

coverage <- function(graph, membership){
  edges = as_data_frame(graph)
  edges$from = membership[edges$from]
  edges$to = membership[edges$to]
  
  return(sum(edges$weight[edges$from == edges$to]) / sum(edges$weight))
}

############ mstClustering ############

mstClustering <- function(ig, threshold = 1/15 * dim(ig[])[1]){
  
  mstig = mst(ig, algorithm = "prim")
  # dis=distances(mstig)
  degree = centr_degree(mstig)
  centroids = which(degree$res > threshold)

  # mins=apply(dis[,centroids],1,which.min)
  # clusters=match(centroids[mins],centroids)

  # df=as_data_frame(mstig,what = "edges")
  # linkedges=which(clusters[df[,"from"]]!=clusters[df[,"to"]])
  # keyvertices=df[linkedges,c("from","to")]

  # list("clusters"=clusters,"centroids"=centroids,"keyvertices"=keyvertices)
  return(centroids)
}

############ MST ############

MST <- function(x, algorithm = "prim"){
  
  forest = components(x, mode = "weak")
  no = forest$no
  member = forest$membership
  overall = c()
  for(i in 1:no){
    ig = induced_subgraph(x, which(member == i), impl = "auto")
    plot(ig)
    V(ig)$id2 = V(ig)$id
    V(ig)$id = 1:gorder(ig)
    vertices = as_data_frame(ig, what = "vertices")
    edges = as_data_frame(ig, what = "edges")
  
    edges_2 = getMinimumSpanningTree(vertices$id, as.matrix(edges[,c("from","to","weight")]), algorithm = algorithm, show.data = FALSE, show.graph = FALSE)$tree.arcs
    edges_2[,"ept1"] = V(ig)$id2[edges_2[,"ept1"]]
    edges_2[,"ept2"] = V(ig)$id2[edges_2[,"ept2"]]
  
    overall = rbind(overall, edges_2)
  }
  
  overall = as.data.frame(overall)
  colnames(overall) = c("from", "to", "weight")
  return(overall)
}

############ nucleotide position ############ 

letter.prob <- function(seq.vector){
  seq = as.character(seq.vector)
  seq = str_split(seq, "", simplify = TRUE)
  
  letters = unique(as.vector(seq))
  
  prob.matrix = matrix(data = 0, 
                       nrow = ncol(seq),
                       ncol = length(letters))
  
  prob.matrix = as.data.frame(prob.matrix)
  colnames(prob.matrix) = letters
  
  seq = apply(seq, 2, table)
  seq = lapply(seq, as.data.frame)
  
  for(i in 1:nrow(prob.matrix)){
    prob.matrix[i, as.character(seq[[i]]$Var1)] = seq[[i]]$Freq
  }
  
  prob.matrix = prob.matrix / length(seq.vector)
  
  seq = as.character(seq.vector)
  seq = str_split(seq, "", simplify = TRUE)
  
  one.run <- function(seq.row, prob.matrix){
    total = 1
    
    for(i in 1:length(seq.row)){
      total = total * prob.matrix[i, seq.row[i]]
    }
    
    return(data.table(Seq = paste(seq.row, collapse = ""),
                      Prob = total))
  }
  
  seq.prob = apply(seq, 1, one.run, prob.matrix)
  seq.prob = rbindlist(seq.prob)
  
  one.run <- function(index, prob.matrix){
    prob.matrix$Prob = abs(prob.matrix[index, ]$Prob - prob.matrix$Prob)
    prob.matrix = as.data.table(t(prob.matrix$Prob))
    
    return(prob.matrix)
  }
  
  seq.prob = lapply(1:nrow(seq.prob), one.run, seq.prob)
  seq.prob = rbindlist(seq.prob)
  
  colnames(seq.prob) = as.character(1:ncol(seq.prob))
  
  seq.prob = seq.prob / max(seq.prob)
  
  return(seq.prob)
}

############ find motifs ############ 

find.motifs <- function(data, motifs){
  
  library(data.table)
  library(stringr)
  
  ids = c()
  
  total = data.table(index = 1:length(data), sequence = data)
  
  motifs = unlist(str_split(motifs, ";"))
  motifs = motifs[which(motifs != "")]
  
  gap.loc = str_locate_all(motifs, '_')
  
  first.motifs = str_split(motifs, "_")
  
  for(i in 1:length(motifs)){
    first.motif = first.motifs[[i]][1]
    
    temp = total[which(str_detect(total$sequence, first.motif)), ]
    
    trans = str_locate(temp$sequence, first.motif)[,1]
    
    for(j in 1:nrow(gap.loc[[i]])){
      str_sub(temp$sequence, start = trans - 1 + gap.loc[[i]][j, 1], end = trans - 1 + gap.loc[[i]][j, 1]) = "_"
    }
    
    ids = c(ids, temp[which(str_detect(temp$sequence, motifs[i])), ]$index)
  }
  
  ids = unique(ids)
  ids = ids[order(ids)]
  
  return(ids)
  
}