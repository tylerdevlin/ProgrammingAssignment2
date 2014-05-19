## The following functions can be used to compute the inverse of a square invertible matrix.
## After the inverse is computed once, the result is stored in the cache, thus avoiding
## any unnecessary computation

## This function is used to set the matrix, and store its inverse.

makeCacheMatrix = function(x = matrix()) 
{
        m = NULL
        
        set = function(y) 
        {
                x <<- y
                m <<- NULL
        }
        
        get = function() x
        
        setinverse = function(inverse) m <<- inverse
        
        getinverse = function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse, and then passes the result to be stored in the function ## above.  Once the inverse is computed, when the function is run again it will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        m = x$getinverse()
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)
        }
        
        data = x$get()
        
        m <- solve(data, ...)
        
        x$setinverse(m)
        
        m
}