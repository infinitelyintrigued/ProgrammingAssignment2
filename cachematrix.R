## Put comments here that give an overall description of what your
## functions do

#  This function creates a special "matrix" object that can cache its
#  inverse.
makeCacheMatrix <- function(x = matrix()) {

    # Reset the inverse
    inv <- NULL

    # Save the given matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

   # Retrieve the given matrix
    get <- function() x

   # Save the computed inverse
    setinverse <- function(inverse) inv <<- inverse
    
   # Compute the inverse unless the cache already exists
    getinverse <- function() inv

   # Return the special "vector"
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}

#  This function computes the inverse of the special "matrix" returned
#  by makeCacheMatrix above. If the inverse has already been
#  calculated (and the matrix has not changed), then the cachesolve
#  should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    if (is.null(x$getinverse())) {
        message("Calculating the inverse...")
        x$setinverse(solve(x$get()))
    }
    else {
        message("Getting from the cache...")
    }
    x$getinverse()
}


