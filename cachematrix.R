## These functions take an ordinary matrix object, store it
## and calculate its inverse before also storing this.

library(matrixcalc)

## The makeCacheMatrix assigns the matrix x and the inverse
## in its local environment. The get() and getInverse() functions
## return the respective values whie the set and setInverse()
## functions assign the first arguments to the respective values 
## of the parent environment demonstrating lexical scoping.
## a list is returned containing the 4 functions.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function(){
        x
    }
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    setInverse <- function(inv){
        inverse <<- inv
    }
    getInverse <- function(){
        inverse
    }
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve first checks that the matrix x is square and
## provides a warning if it is not. It then gets the inverse
## and checks to see if it is null. If the inverse exists then
## it is returned. Otherwise the inverse is calculated, stored
## and returned.

cacheSolve <- function(x, ...) {
    if(!is.square.matrix(x$get())){
        warning("Matrix x is not square.")
        return()
    }
    i <- x$getInverse()
    if(!is.null(i)){
        return(i)
    }
    i <- solve(x$get())
    x$setInverse(i)
    i
}
