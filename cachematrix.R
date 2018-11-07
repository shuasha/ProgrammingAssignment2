## The overall goal of the functions is to cache
## the inverse of a matrix rather than compute it repeatedly

## This function is to create a special "matrix" object 
## that can cache its inverse: set and get the value of the vector
## and set and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        ivse <- NULL
        set <- function(y) {
                x <<- y
                ivse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ivse <<- inverse
        getinverse <- function() ivse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function is to calculate the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
## If the inverse has already been computed (and the matrix has not changed), 
## the cachesolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ivse <- x$getinverse()
        if(!is.null(ivse)) {
                message("getting cached data")
                return(ivse)
        }
        data <- x$get()
        ivse <- solve(data, ...)
        x$setinverse(ivse)
        ivse
}