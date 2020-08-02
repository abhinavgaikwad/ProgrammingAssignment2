## Matrix inversion is a computationally an expensive process. This program shows a process 
## to cache the inverse of a matrix rather that computing it each time the inverse is required. 

## In the following function a special "matrix" object is created that can cache
## the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse <-function (inverse) m<<-inverse
  getinverse <-function ()m
  list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## In this function the inverse of the special "matrix" object returned by makeCacheMatrix 
## is calculated. If the matrix is previously calculated, then the inverse is returned 
## from the makeCacheMatrix. If not, the inverse is calculated with the solve () function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if (!is.null(m)){
    message("getting the cached data")
    return(m)
  }
  data <-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
