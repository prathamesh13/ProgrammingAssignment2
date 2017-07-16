## The following functions illustrate the use of R scoping rules to cache potentially time consuming computations

## function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    x_inverse <- NULL

    set <- function(y){       
        x <<- y
        x_inverse <<- NULL    
    }

    get <- function() x

    setInverse <- function(inverse) x_inverse <<- inverse

    getInverse <- function() x_inverse

    list(set = set,get = get,setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    x_inverse <- x$getInverse()
    
    if(!is.null(x_inverse)){
        print("Getting cached inverse.")
        return(x_inverse)
    }
    
    data <- x$get()
    
    print("calculating x inverse.")
    
    inverse <- solve(data)
    
    #cache the computed value for inverse
    x$setInverse(inverse)
    
    inverse

}
