## Functions for creating special Matrix that caches it inverse after the first calculation. 
## Every other inverse calculation returnes cached inverted matrix as long as the original doesn't change
## 
## Example Usage:
## mat1<-matrix(c(2,3,4,5),2)
## cachemat1<-makeCacheMatrix(mat1)
## cacheSolve(cachemat1)

## Creates special matrix inverse cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Calculate inverse of the given special Matrix using cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
