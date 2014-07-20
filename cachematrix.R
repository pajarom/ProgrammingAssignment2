### cachematrix.R

#   Matrix inversion is usually a costly computation and their may be some benefit 
#   to caching the inverse of a matrix rather than compute it repeatedly
# 
#   Computing the inverse of a square matrix is done with the solve function in R.
#
#   The <<- operator which is used to assign a value to an object in an environment
#   that is different from the current environment.
#
#   Below are two functions that are used to create a special object that stores a 
#   matrix and cache's its inverse.

#   Usage:

#       > source("cachematrix.R")
#       > m <- matrix(rnorm(9),nrow=3,ncol=3)
#       > m
#           [,1]       [,2]       [,3]
#           [1,]  1.3329220 -0.3845237  2.0324827
#           [2,] -0.4206622 -0.9292330 -0.5858797
#           [3,]  0.1384127 -0.5281823 -0.4072528
#       > solve(m)
#           [,1]       [,2]        [,3]
#           [1,]  0.0764755 -1.3637582  2.34358942
#           [2,] -0.2798304 -0.9136930 -0.08210215
#           [3,]  0.3889148  0.7215051 -1.55248163
#       > mc <- makeCacheMatrix()
#       > mc$set(m)
#       > cacheSolve(mc)
#           [,1]       [,2]        [,3]
#           [1,]  0.0764755 -1.3637582  2.34358942
#           [2,] -0.2798304 -0.9136930 -0.08210215
#           [3,]  0.3889148  0.7215051 -1.55248163
#       > mc$get()
#           [,1]       [,2]       [,3]
#           [1,]  1.3329220 -0.3845237  2.0324827
#           [2,] -0.4206622 -0.9292330 -0.5858797
#           [3,]  0.1384127 -0.5281823 -0.4072528
#       > mc$getsolve()
#           [,1]       [,2]        [,3]
#           [1,]  0.0764755 -1.3637582  2.34358942
#           [2,] -0.2798304 -0.9136930 -0.08210215
#           [3,]  0.3889148  0.7215051 -1.55248163


##  makeCacheMatrix: 

#
#   This function creates a special empty "matrix" object that can cache its 
#   inverse. This special object is really a list containing a function to:
#
#       * set the value of the matrix:  set
#       * get the value of the matrix:  get
#       * set the value of the inverse: setsolve
#       * get the value of the inverse: getsolve
#

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x

    setsolve <- function(...) {
        inv <<- solve(x, ...)
    }
    getsolve <- function() inv

    # Return the list of function...
    list( set = set, 
          get = get,
          setsolve = setsolve,
          getsolve = getsolve )
    
}


## cacheSolve: 

#
#   This function computes the inverse of the special "matrix" returned by 
#   makeCacheMatrix above. If the inverse has already been calculated (and the 
#   matrix has not changed), then the cachesolve should retrieve the inverse 
#   from the cache.
#
#   Note: The logic has been slightly changed from cache mean example to avoid
#   setsolve invocation from outside cachesolve to ovewrite the cache with an
#   invalid value.
#

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
    }
    else {
        x$setsolve(...)
        inv <- x$getsolve()
    }
    inv    
}

### End cachematrix.R