# Idea 1

# To do: get rid of redundancies, clean up functions
# and merge where needed.



# Load maze and convert to array
maze <- scan(file="maze1.txt",what="character", sep="\n")
maiz <- array(dim=c(21,41))
for (i in 1:21){
  maiz[i,]<-strsplit(maze[i], split="")[[1]]
}


# Find entry and exit spaces, save in array named "edge"
edge<-NULL
for(cl in c(1:41)){
  # check everything in the first and last column 
  if(cl==1||cl==41){
    for(rw in 1:21){
      if(maiz[rw,cl]==" "){
        edge<-rbind(edge,c(rw,cl))
      }
    }
  }
  else{
    #just check the first and last entries in the middle columns
    for(rw in c(1,21)){
      if(maiz[rw,cl]==" "){
        edge<-rbind(edge,c(rw,cl))
      }
    }
  }
}



# fill function

# Arguments:
# M = maze in array form
# char = character to fill it with
# pair.array = 2-column array, where each row represents an i,j pair
#              for the maze array. These coordinates indicate the
#              point(s) at which the filling starts

fill<- function(M, char, pair.array){
  LEN <- nrow(pair.array)
  # So long as we have pairs, fill those places in M with char
  while(!is.null(LEN)){
    for(i in 1:LEN){
      M[rbind(pair.array[i,])] <- char
    }
    
    # Create new pair array from open maze points adjacent to the current pairs
    newpairs<-NULL
    for(i in 1:LEN){
      # for a given current point, create pair list describing up, down, left, and right.
      choices<- sweep(rbind(c(-1,0), c(1,0), c(0,-1), c(0,1)), 2, -pair.array[i,])
      valid<-NULL
      for(j in 1:4){
        # And for each of those, if it's a space in the maze, accept it as
        # a "valid" move
        if(length(M[rbind(choices[j,])])>0){
          if(M[rbind(choices[j,])]==" "){
            valid<-rbind(valid,choices[j,])
          }
        }
      }      
      newpairs<-rbind(newpairs, valid)      
    }
    
    # valid moves from all current points become the new list, and the
    # loop reiterates until the filler has nowhere to go in the maze
    pair.array<-newpairs
    LEN <- nrow(rbind(pair.array))
  }
  M
}
# (the liberal/redundant use of rbind and overly-explicit logic is 
# there to prevent 1x2 arrays from geting coerced to vectors, which 
# produce a zero-length object for nrow and break logical statements) 







# Create solving function that uses "fill" for decision-making
mouse<-function(M,edges){
  current<-rbind(edges[1,])
  
  repeat{
    # same valid move finder
    choices<- sweep(rbind(c(-1,0), c(1,0), c(0,-1), c(0,1)), 2, -current)
    valid<-NULL
    for(j in 1:4){
      if(length(M[rbind(choices[j,])])>0){
        if(M[rbind(choices[j,])]==" "){
          valid<-rbind(valid,choices[j,])
        }
      }
    }
    
    # if there are no valid moves, end the loop
    if(is.null(valid)){
      M[current]<-"."
      break
    
    # if there is one move, fill current space and move focus to that space 
    }else if(nrow(rbind(valid))==1){
      M[current] <- "."
      current <- rbind(valid[1,])
      
    # if there are multiple valid moves, sequentially block all but one,
    # use filler, and check if end space is filled as a result. If so,
    # stop and make that selection.
    }else if(nrow(valid)>1){
      for(i in 1:nrow(valid)){
        M[valid] <- "X"
        M[rbind(valid[i,])] <- " "
        M2<-fill(M, "X", current)
        if(M2[rbind(edge[2,])]=="X"){
          M[current]<- "."
          current <- rbind(valid[i,])
          M[valid]<- " "
          break
        }  
      }
    }
  }
  M
}

# implement function and print 
solution<-mouse(maiz,edge)

pict<-NULL
for(i in 1:21){
  pict<-c(pict, paste(solution[i,], collapse=""))
}
cat(pict, sep="\n")

