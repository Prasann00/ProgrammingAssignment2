## The functions here make a special matrix and 
## compute the inverse of the matrix in case it
## is not stored in the cache else it retrieves 
## the inverse from the cache


## This function makeCacheMatrix creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse)j<<-inverse
  getInverse<-function()j
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## This function cacheSolve computes the inverse 
## of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  j<-x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat<-x$get()
  j<-solve(mat,...)
  x$setInverse(j)
  j
}
