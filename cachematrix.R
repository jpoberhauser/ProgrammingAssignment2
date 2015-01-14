## The first function, makeCacheMatrix creates a matrix. Basically a list containing a function 
## to set the value of the matrix, get that value, cache the value, inverse of the matrix, and 
## get the cached function,  inverse of the matrix

##The second function cacheSolve, uses the cached function, solve, to calculate the inverse of the matrix,
##and returns said value.
makeCacheMatrix <- function(x = matrix()) {
       
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() {
                x
        }

        cache_inverse <- function(solve) {
                m <<- solve
        }

        get_inverse <- function() {
                m
        }
        
        # return a list. Each named element of the list is a function
        list(set = set, get = get, cache_inverse = cache_inverse, get_inverse = get_inverse)
}

## This second function, cacheSolve, uses the previous list of functions to compute the inverse
## of the matrix. If the inverse has been calculated in the aboce function and stores in cache, then
## this fucntion should retrieve the inverse of the matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$cacheInverse(m)
        
        m
}
