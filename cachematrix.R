## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to
## cache the inverse of a matrix.
## A special matrix is created by the makeCacheMatrix function.
## which is capable of storing its inverse within cache so it 
## does not have to be recomputed.

makeCacheMatrix<-function(x=matrix()){
inv<-NULL
set<-function(y){
x<<-y
inv<<-NULL
}
get<-function()x
setinv<-function(inver) inv <<- inver 
getinv<-function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following functions check to see if the inverse is already within memory and if not
## it will compute the inverse and then return the value.

cacheSolve <- function(x,...){
inv <- x$getinv()
if(!is.null(inv)){
message("getting cached data")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)        
inv
}