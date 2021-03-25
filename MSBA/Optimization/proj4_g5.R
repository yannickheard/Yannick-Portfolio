
##Optimization Project 4

library(igraph)
library(imager)
library(dplyr)

setwd("C:\\Users\\cslun\\Google Drive\\Current School\\Stoch. Control\\Homework\\Project 4")

##read in the images and create dataframes
#1,1 represents top left corner. to read it in, goes across x axis then down to second row
#value represents the color
mat1=load.image("Pic1.jpg")
mat1=resize(mat1, 128,128)
df1=as.data.frame(mat1)

mat2=load.image("Pic2.jpg")
mat2=resize(mat2, 128,128)
df2=as.data.frame(mat2)

mat3=load.image("Pic3.jpg")
mat3=resize(mat3, 128,128)
df3=as.data.frame(mat3)

mat4=load.image("Pic4.jpg")
mat4=resize(mat4, 128,128)
df4=as.data.frame(mat4)

#plot the images
plot(mat1)
plot(mat2)
plot(mat3)
plot(mat4)

##Part 1- use histogram to create threshold

hist_threshold <- function(df, threshold=0){
  #takes a df and initial threshold guess. finds a decent threshold to divide image
  #assigns 1/0 to foreground/background
  threshold_new=0.5
  if(threshold_new!=threshold){
    mean_low=mean(df$value[which(df$value<=threshold_new)])
    mean_high=mean(df$value[which(df$value>threshold_new)])
    threshold_new=(mean_low+mean_high)/2
  }
  df$assignment<-NA
  df$assignment[which(df$value<=threshold_new)]=0
  df$assignment[which(df$value>threshold_new)]=1
  
  df_new = df[c("x","y","assignment")] #subset- need df in format to plot
  colnames(df_new) = c("x","y","value")
  return (df_new)
}

#test on image 1- plot original, then plot new
df1_new=hist_threshold(df1)
plot(mat1)
img1=mutate(df1_new) %>% as.cimg
plot(img1)

#test on image2
df2_new=hist_threshold(df2)
plot(mat2)
img2=mutate(df2_new) %>% as.cimg
plot(img2)

#test on image3
df3_new=hist_threshold(df3)
plot(mat3)
img3=mutate(df3_new) %>% as.cimg
plot(img3)

#test on image4
df4_new=hist_threshold(df4)
plot(mat4)
img4=mutate(df4_new) %>% as.cimg
plot(img4)

###Part 2

##calculate initial foreground and background of image

calc_fore <- function(pic) {
  ##takes picture and grabs subset for the foreground
  foreground = imsub(pic,(x %in% seq(50,90)), (y %in% seq(50,90)))
  return (mean(foreground))
}

calc_back <- function(pic) {
  background = imsub(pic,!(x %in% seq(50,90)), !(y %in% seq(50,90)))
  return (mean(background))
}

#create function to calculate ai and bi and cij
#then create function to calculate pf/pb and calls the above functions

calc_a <- function(df, pf, pb) {
  a=vector()
  pf=pf
  pb=pb
  for (i in 1:nrow(df)) {
    pi=df$value[i]
    a[i]=-log(abs(pi-pf)/(abs(pi-pf)+abs(pi-pb)))
  }
  return (a)
}

calc_b <- function(df, pf, pb) {
  b=vector()
  for (i in 1:nrow(df)) {
    pi=df$value[i]
    b[i]=-log(abs(pi-pb)/(abs(pi-pf)+abs(pi-pb)))
  }
  return (b)
}

calc_c <- function(pic) {
  #first resize picture smaller
  mat = matrix(pic, 128,128)
  
  #create vectors for from and to
  from = c()
  to = c()
  
  #next create adjancency matrix
  #cycle through pixels. if it's not on the last row, find the point below it
  #next, if it's not the point farthest on the right, find the point to the right
  for (i in 1:length(mat)) {
    if (i+128 < length(mat)) {
      from = c(from, i)
      to = c(to, i+128)
    }
    if (!(i %% 128 == 0)){
      from = c(from, i)
      to = c(to, i+1)
    }
    else{next} #if on the bottom row or right end, skip
  }
  
  #add the one forgotten point
  from = c(from, length(mat)-128)
  to = c(to, length(mat))
  
  #create df with from/to columns
  adj_df = data.frame(from, to)
  
  #create list out of original size pic matrix
  p_mat=as.matrix(pic)
  list_p = c(p_mat)
  
  #create empty capacity vector
  cap = c()
  
  #loop through to calculate capacity vector
  for (i in 1:nrow(adj_df)){
    
    K = .01
    #grab ith row from adj matrix and pull out from and to pixels
    row = adj_df[i,]
    from = row$from
    to = row$to
    
    pixel_from = list_p[from]
    pixel_to = list_p[to]
    
    #calculate Cij and add it to capacity vector
    Cij = K*(-exp((pixel_from-pixel_to)^2))
    cap = c(cap, Cij)
    
  }
  #adding capacity to from/to 
  param_df=adj_df
  param_df$capacity=cap
  return(param_df)
}

##make a function to make the edges bi-directional 
create_full_df = function(pic) {
  param_df = calc_c(pic)
  #switch from and to -- to and from
  opposite_df = data.frame(param_df$to,param_df$from,param_df$capacity)
  names(opposite_df) = c('from','to','capacity')
  full_df=rbind(param_df,opposite_df)
  return(full_df)
}

pic1_c = create_full_df(mat1)
pic2_c = create_full_df(mat2)
pic3_c = create_full_df(mat3)
pic4_c = create_full_df(mat4)

graph1=graph_from_data_frame(pic1_c)


plot_max_flow=function(pic){
  #find source and sink nodes
  nodes = seq(1,128^2)
  s = rep(1,128^2)
  t = rep(128^2,128^2)
  
  nodes_from = c(s,nodes)
  nodes_to = c(nodes,t)
  
  a = calc_a(as.data.frame(pic), calc_fore(pic), calc_back(pic))
  b = calc_b(as.data.frame(pic), calc_fore(pic), calc_back(pic))
  weights = c(a,b)
  
  node_df = data.frame(nodes_from,nodes_to,weights) #create df with nodes and weights
  names(node_df) <- c("from", "to","capacity")  #rename columns
  
  full_df = create_full_df(pic) 
  final_df <- rbind(node_df, full_df)
  
  #get directed graph
  graph = graph_from_data_frame(final_df, directed = TRUE, vertices = NULL)
  maxflow = max_flow(graph, 1, 128^2)
  
  #making blank image
  new_pic = rep(0,128^2)
  
  new_pic[maxflow$partition1]=0
  new_pic[maxflow$partition2]=1
  new_pic = matrix(new_pic,128,128) #scale new image
  new_pic = array(new_pic, c(128,128,1,1))
  new_pic = as.cimg(new_pic) #make into image to be able to plot
  cscale = scales::gradient_n_pal(colours=c('red','blue'),c(1,0))
  
  #plot old and new image
  par(mfrow=c(1,2))
  plot(pic)
  plot(new_pic, colourscale=cscale,rescale=FALSE)
}

plot_max_flow(mat1)
plot_max_flow(mat2)
plot_max_flow(mat3)
plot_max_flow(mat4)



