## This function creates an object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x                             
    setInverse <- function(inverse) inv <<- inverse  
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    if(!is.null(inv)) {                      
        message("Getting Cached Invertible Matrix")   
        return(inv)                            
    }
    
    MatrixData <- x$get()                     
    inv <- solve(MatrixData, ...)            
    x$setInverse(inv)                        
    return(inv)                              
    ## Return a matrix that is the inverse of 'x'
}







