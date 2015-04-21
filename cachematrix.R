## The following two functions can 
## compute and cache the inverse of a matrix


## makeCacheMatrix function creates a special "matrix" object
## This object can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invmatrix <<- inv
        getinv <- function() invmatrix
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}


## cacheSolve function calculates the inverse of the special "matrix" created by makeCacheMatrix function.
## It first checks, if the inverse has already been calculated. 
## If yes: get the inverse from the cache
## Else, calculates the inverse of the data and set the value of the inverse in the cache using setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinv()

        ## If data fetched from cache is not null return invmatrix
        if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }

        ## Else compute invmatrix, store it in cache and return it
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinv(invmatrix)
        invmatrix
}
