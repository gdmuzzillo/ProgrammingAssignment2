## Calculating the inverse of a matrix is computationally expensive
## to alleviate the calculation costs we will create a cache holding
## solve(x) calculations in order to retrieve previously computed results.



## this function will create the cache for matrix inverse results and provide 
## methods for setting and retrieving previously calculated values to / from it.

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


## this function returns the matrix x inversion by making a call to makeCacheMatrix function if present in the 
## matrix inverse cache, otherwise it will calculate the matrix inverse, store it into the cache for later use 
## and return the calculated value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setsolve(m)
    m
}
