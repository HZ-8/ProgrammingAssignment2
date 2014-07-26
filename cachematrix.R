## The two functions below are used to create a special object that stores
## a matrix and caches its inverse matrix

## The function MakeCacheMatrix creates a list of 4 functions to set/get matrix
## value and to set/get its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function calculates the inverse of the special "vector"
## created with the above function. It first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the data
## and sets the value of the inverse vmatrix in the cache via the setinv
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv    
}
