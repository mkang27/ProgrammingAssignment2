## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## function to set the matrix as y using lexical scoping
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## function to get the matrix
    get <- function() {
        x    
    }
    ## function to set the inverse of the matrix as the inverse object passed in using lexical scoping
    setInverse <- function(inverse) {
        inv <<- inverse    
    }
    ## function to get the inverse of the matrix
    getInverse <- function() {
        inv
    }
    
    ## return a list of functions (set, get, setInverse, getInverse)
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If inverse has already been calculated, retrieves the inverse from the cache
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    ## get the inverse of the matrix 'x'
    inv <- x$getInverse()
    
    if(is.null(inv)) {
        ## if inverse is null, compute the inverse using the solve() function by passing in the matrix 'data'
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        return(inv)
    }
    else {
        ## inverse has already been cached so return it
        message("getting cached data")
    }
    inv
}
