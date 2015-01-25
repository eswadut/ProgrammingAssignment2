## The function is about caching a function that has a long computational
## time. For example here Computing the inverse of a square matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set1<-function(y){
    x<<-y
    m<<-NULL
  }
  get1<-function() x
  set1matrix<-function(solve) m<<- solve
  get1matrix<-function() m
  list(set1=set1, get1=get1,
       set1matrix=set1matrix,
       get1matrix=get1matrix)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x=matrix(), ...) {
  m<-x$get1matrix()
  if(!is.null(m)){
    message("Getting the cached data")
    return(m)
  }
  matrix<-x$get1()
  m<-solve(matrix, ...)
  x$set1matrix(m)
  m
}