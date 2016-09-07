## The first function (makeCacheMatrix) creates a new object type called 
## cacheMatrix that will store a conventional matrix and its inverse whenever
## it is calculated through the second function (cacheSolve).

## The function makeCacheMatrix creates a cacheMatrix from a conventional matrix
## A cacheMatrix is a list of functions and two inner variables that store the
## original matrix and its inverse. The functions are:
## - set: changes the matrix and makes the inverse null (i.e. not calculated)
## - get: retrieves the original matrix
## - setinverse: changes the value of the inverse matrix
## - getinverse: retrieves the value of the inverse matrix, which will be null
##     if it has not been calculated

## Note that there is no guaratee that the stored inverse matrix is really the
## inverse of the original matrix...

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverse)
        inv <<- inverse
    getinverse <- function()
        inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## The function cacheSolve returns the inverse of a cacheMatrix x.
## If the cache is empty, the inverse is calculated and stored.
## If the cache is already filled in, its contents is returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## x has to be a cacheMatrix to work
    
    # Get the cached inverse matrix
    inv <- x$getinverse()
    # If there is a cached inverse matrix, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # If there is not a cached inverse matrix, calculate it and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
