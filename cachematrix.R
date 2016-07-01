## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix function creates a special matrix object that can cache its inverse.
# * setMatrix      set the value of a matrix
# * getMatrix      get the value of a matrix
# * cacheInverse   get the cahced value (inverse of the matrix)
# * getInverse     get the cahced value (inverse of the matrix)

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



## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


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
