## Two functions that identify the inverse matrix
## the makeCacheMatrix function makes a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        imatrix <- NULL
        set <- function(y) {
                x <<- y
                imatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) imatrix <<- inverse
        getInverse <- function() imatrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# cacheSolve: This function computes the inverse of the 
# special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the
#matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        imatrix <- x$getInverse()
        if (!is.null(imatrix)) {
        return(imatrix)
        }
        mat <- x$get()
        imatrix <- solve(mat, ...)
        x$setInverse(imatrix)
        imatrix
}
