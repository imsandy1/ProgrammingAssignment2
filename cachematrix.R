## makeCacheMatrix is a list of functions to get and set matrix and inverse  
## matrix data into and from cache
## cacheSolve will compute the inverse matrix if it is not already available
## in cache and return it, else it will return it from the cache

## This function creates a list of functions that feed matrix and inverse 
## matrix into the cache and get matrix and inverse matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y = matrix()) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    ## update inverse into cache
    setinvmat <- function(invmat) m <<- invmat
    
    ## get inverse from cache
    getinvmat <- function() m
    
    ## returns list of 4 functions to get and set matrix and inverse
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
    
    
}


## This function checks if the inverse matrix is available in the cache 
## and returns it or computes the inverse matrix returns it and saves it 
## into the cache

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    
    ## check if inverse is available in cache, and return it from cache if true
    m <- x$getinvmat()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    
    ## if the above if clause fails, and inverse not available in cache
    ## compute inverse using solve function, and update it into cache
    message("getting new computed inverse matrix")
    data <- x$get()
    m <- solve(data, ...)
    x$setinvmat(m)
    m
    
    
}
