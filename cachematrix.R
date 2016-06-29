## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  in_verse<- NULL
  set<-function(y){
    x<<-y
    in_verse<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) in_verse<<-inverse
  getInverse<-function() in_verse
  list(set = set,get = get, setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  in_verse<x$getInverse()
  if(!is.null(in_verse)){
    message("Getting cached")
    return(in_verse)
    
  }
  data<-x$get()
  in_verse<-solve(data,...)
  x$setInverse(in_verse)
  in_verse
  
}
