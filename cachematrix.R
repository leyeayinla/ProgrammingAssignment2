## The makeCacheMatrix function creates a special 'matrix' object that can cache its inverse by 
## setting and getting the matrix, and settiing and getting the inverse of the matrix. 
## The cacheSolve function computes the inverse of the special 'matrix' created with the (makeCacheMatrix) function above.
## The cacheSolve function checks to find if the matrix inverse has been previously computed, in which case, it fetches
## the computed inverse from the cache and skips its calculation. Otherwise, it uses the solve(x, ...) function in R
## to compute the inverse of the supplied matrix data and sets the newly computed inverse in the cache via the 
## setmean() function. It is however, asssumed that the matrix, whose inverse is to be computed, is invertible.   

## Function (makeCacheMatrix) creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
  }
get<-function() x
setinverse<-function(inverse) inv<<-inverse
getinverse<-function() inv
list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## The (cacheSolve) function computes the inverse of the special 'matrix' returned by the (makeCacheMatrix) function above

cacheSolve <- function(x, ...) {
inv<-x$getinverse()        ## Return a matrix that is the inverse of 'x'
if(!is.null(inv)){
  message("getting cached data")
}
data<-x$get()
inv<-solve(data, ...)
x$setinverse(inv)
inv
}
