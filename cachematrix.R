##      According the the assignment the following functions will be used:
##      makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
##      cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##      If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## This function creates matrix which can cache its inverse:
makeCacheMatrix <- function(mtrx = matrix()) {
        inv <- NULL
        set <- function(matrix) {
                mtrx <<- matrix
                inv <<- NULL
        }
        get <- function() {
                mtrx
        }
        setInverse <- function(inverse) {
                inv <<- inverse
        }
        getInverse <- function() {
                inv
        }
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Next function computes inverse of the matrix returned by the makeCacheMatrix function. 
## cacheSolve function is also able to get an inverse from cache in some conditions.
cacheSolve <- function(mtrx, ...) {
        ## Return a matrix that is the inverse of 'mtrx'
        inv <- mtrx$getInverse()
        if(!is.null(inv)) {
                message("Attention: cached data will be obtained")
                return(inv)
        }
        data <- mtrx$get()
        inv <- solve(data, ...)
        mtrx$setInverse(inv)
        inv
}

## The task performed as a Coursera programming assignment. Thank you for your attention.
