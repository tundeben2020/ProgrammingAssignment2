## The makeCacheMatrix function creates a special matrix while the cacheSolve function 
## calculates the inverse of the special matrix created by the makeCacheMatrix function.

## The makeCacheMatrix function is a list containing nested function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
get<-function() x
setinverse<-function(inverse) inv<<-inverse
getinverse<-function() inv
list(set=set,
     get=get,
     setinverse=setinverse,
     getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the special matrix created 
## by the makeCacheMatrix function above. However; it first checks to see if the inverse
## has already been calculated. If yes;it gets the inverse from the cache and skips the 
## computation, Else; it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
