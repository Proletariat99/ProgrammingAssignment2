# Put comments here that give an overall description of what your
## functions do


## This takes an input matrix and sets 4 attributes/methods for the cached matrix, for quick / easy reference.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {                      # sets upper level x as input to set().
                x <<- y
                s <<- NULL                        # also resets the invert
        }
        get <- function() x                       #  just returns x (from one level up) when run
        setinv <- function(solve) s <<- solve     # function to set s using solve
        getinv <- function() s                    # returns s (solution), when called as x$getinv
        list(set = set, get = get,           # the returned value is a list of stuff from above.
             setinv = setinv,
             getinv = getinv)
}


# this function solves for the inverse of a matrix and returns the results.
cacheSolve <- function(x, ...) {                    # takes x from makeCacheMatrix
        s <- x$getinv()                             # runs the getinv function on x and assigns to s
        if(!is.null(s)) {                           # if s isn't empty
                message("getting cached data")      # print some stuff
                return(s)                           # return the not empty part.
        }
        data <- x$get()                             # elsewise, grab x and call it data.
        s <- solve(data, ...)                       # solve it for inverse
        x$setinv(s)                                 # tell x what its inverse is (saves in list)
        s                                           #  returns solution
        ## Returns a matrix that is the inverse of 'x'
}


M <- matrix(1:144, nrow=12, ncol=12)