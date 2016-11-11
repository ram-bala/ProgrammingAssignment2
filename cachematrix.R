## Put comments here that give an overall description of what your
## functions do

## function saves a cached version of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    minverse<-NULL
    set <- function(y) {
        x<<- y
        minverse<<- NULL
    }
    get <- function() x
    setinv<- function(inverse) minverse<<- inverse
    getinv<- function() minverse
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    minverse<-x$getinv()
    if(!is.null(minverse)) {
        message("getting cached inverse")
        return(minverse)
    }
    data <- x$get()
    minverse<- solve(data)
    x$setinv(minverse)
    minverse
}
