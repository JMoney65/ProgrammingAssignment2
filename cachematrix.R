## makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated and the matrix has not 
## changed, cachesolve retrieves the inverse from the cache.

## makeCacheMaxtrix creates a a list containing a function to
## set: set the value of the matrix
## get: get the value of the matrix
## setinv: set the inverse of the matrix
## getinv: get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix.
## It first checks to see if the inverse of the matrix has already been calculated.
## If so, get the inverse from the cache. Otherwise, get the matrix (data) and
## calculate the inverse. Use the setinv function to set the value of the inverse in
## the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)                
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
