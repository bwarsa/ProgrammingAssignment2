## Assignment 2 Code (Brian Warsa) - Returns inverse of matrix.  If inverse has already previously
## been computed, returns cashed version to minimize calculation time.

## Test with the following:
## M <- matrix(c(1, 2, 3, 4), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)

##Creates a matrix object that can cashe its inverse

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
## Computes inverse of the matrix returned by makeCasheMatrix using solve(X) function. 
## If the inverse has already been calculated, for same matrix, retrieves the cashed version.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
