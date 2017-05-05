## Put comments here that give an overall description of what your
## functions do

## This function creats a matrix which will cashe its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) inverse <<- inv
        getInverse <- function() inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)){
                  message("getting cashed data")
                  return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}
