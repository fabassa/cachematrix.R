##makeCacheMatrix creates a matrix and calculates its inverse 
##assigning these values in a list

##First defines value NULL to inverse, so if matrix is not invertible
##it returns NULL. Then determines value for matrix, calculates its inverse
##and stores the data.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##Check if there is the matrix inverse stored in cache
##otherwise, calculates it and the inverse is returned both cases
##except when not invertible (det=0)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}