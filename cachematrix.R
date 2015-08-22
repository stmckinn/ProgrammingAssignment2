## These functions are designed to cut out a lot of unnecessary calculations by caching 
## solutions to be called upon later when the same value needs to be calculated. This
## particular set of functions is useful when dealing with an nxn matrix where n is very 
## large.

## The makeCacheMatrix function is designed to create the cache matrix.
## This function caches the inverse of a particular matrix that can be called apon later.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function calls onto m the getInverse parameter of the pervious function. 
## If m is NULL, meaning a solution for this particular matrix was not cached,
  ## then this function will solve the matrix.
## If m is not NULL then the solution for this matrix has been cached, and cacheSolve will
  ## simply return the cached valued without having to make unnecessary calculations. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
