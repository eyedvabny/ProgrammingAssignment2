## Calculate an inverse of a provided matrix and cache the result
## Use the cached value for future calls unless the matrix changes

## A wrapper for a matrix to cache both the original matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # By default the wrapper has no cashed inverse
    inv <- NULL
    
    # Cache the copy of the provided matrix
    set <- function(mat){
        x <<- mat
        inv <<- NULL
    }
    
    # Return the cached matrix
    get <- function(){
        x
    }
    
    # Cache the inverse
    setInv <- function(i){
        inv <<- i
    }
    
    # Return the cached inverse
    getInv <- function(){
        inv
    }

    # By default return the list of inner functions
    list(set=set, get=get,setInv=setInv,getInv=getInv)
}


## Return an inverse of a provided matrix x
## x must have been created with the wrapper makeCacheMatrix
cacheSolve <- function(x, ...) {

    # Get the inverse from the cache
    inv <- x$getInv()

    # If the cache had the inverse, return that
    if (!is.null(inv)){
        return (inv)
    }
    
    # Otherwise compute and cache the new inverse
    inv <- solve(x$get())
    x$setInv(inv)

    # Return the new inverse
    return(inv)
}
