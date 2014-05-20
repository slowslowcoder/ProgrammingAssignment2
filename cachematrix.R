## The matrix inversion is a time-consuming operation.
## This file provide a function that can be used when
## the inversion of the same matrix is repeatedly called.
## Instead of repeatedly calculating that matrix conversion,
## the result of the last call is cached, and the function
## will return the cached result if the same matrix inversion
## is called.

## Make Cache Matrix will return a list of functions, 
## set, get, setsolve, getsolve.
## "set" will set the input, and "get" is the function to get it.
## "setsolve" will calculate the inversion, 
## and "getsolve" will return the calculation result.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The argument of cacheSolve is the list of function
## (set, get, setsolve, getsolve), which has the same
## meaning as the output of makeCacheMatrix

## It will calculate matrix inversion if required, and
## return the cached result if available.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
