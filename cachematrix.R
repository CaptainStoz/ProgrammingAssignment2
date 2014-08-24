## The following functions cache the inverse of a matrix.

## The function makeCacheMatrix creates a  matrix object that can cache its inverse.
 
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## The function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix 
## If the inverse has been calculated cacheSolve retrieve the inverse from cache.
 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- x$get_inverse()
    if (!is.null(invx)) {
        message("getting cached inverse matrix")
        return(invx)
    } else {
        invx <- solve(x$get())
        x$set_inverse(invx)
        return(invx)
    }
}