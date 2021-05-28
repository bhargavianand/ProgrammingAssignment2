## makeCacheMatrix function creates a special matrix object
## cacheSolve function computes the inverse of special matrix object returned from makeCacheMatrix function

## makeCacheMatrix function creates a special "matrix", which is actually a "list"

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   
        set <- function(y){
          x <<- y  
          m <<- NULL  
        }
        get <- function() {x}
        setInverse <- function(inverse){
          inv <<- inverse  
        }
        getInverse<-function() {inv}
        list(set = set, get = get,
             setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache. else, it calculates
## the inverse using solve().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        mat_data <- x$get()
        inv <- solve(mat_data, ...) 
        x$setInverse(inv)
        inv
}
