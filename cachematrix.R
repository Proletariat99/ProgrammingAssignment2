## Put comments here that give an overall description of what your
## functions do


## This takes an input matrix and sets 4 attributes/methods for the cached matrix, for quick / easy reference.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


# this function solves for the inverse of a matrix and returns the results.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inverse(data, ...)
        x$setinverse(m)
        m
        ## Returns a matrix that is the inverse of 'x'
}
