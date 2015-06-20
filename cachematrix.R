## A set of functions that  cache a matrix called x and its inverse called s.

## There are four functions: set - moves a matrix into var x and sets the inverse matrix to null called s
## set - sets variable as matrix x and sets inverse to null
## get - returns cached matrix x
## setInverse - sets s as the inverse of matrix x
## getinverse - returns cached inverse s 

makeCacheMatrix <- function(x = matrix()) {  
    s <- NULL
    set <- function (y) {
          x <<- y
          s <<-  NULL
    }

    get <- function() x
    
    setInverse <- function(solve) s <<- solve
    
    getInverse  <- function()  s
    
    list (set = set, get= get,  setInverse = setInverse, getInverse = getInverse)
    
}


## This function solves for the inverse of a matrix if it has not yet been determined or returns the cache value
## if it has been previously been calculated

cacheSolve <- function(x, ...) {
    
    s <- x$getInverse
    
## Return a cached inverse if s is not null
    if (!is.null(s)) {
          message("Getting cached inverse")
          return (s)          
    }

## No cached inverse, so the inverse is calculated for matrix x

    data <- x$get()
    s <- solve(data)

##Sets inverse s for matrix x and returns s

    x$setInverse(s)
    s
}
