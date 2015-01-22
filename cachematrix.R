# This file contains two functions makeCacheMatrix and cacheSolve.
#
# makeCacheMatrix works together with cacheSolve to wrap numeric matrices
# and avoid the repeated calculation of the inverse matrix.
#
# makeCacheMatrix creates an object that wraps a numeric matrix and can
# cache a reference to its inverse.  cacheSolve returns the value of the
# cached inverse matrix. If the cache is empty it first calculates 
# the inverse matrix and fills the cache.
#
# example use:
# 
# raw <- matrix(1:4, 2, 2)  # create invertible matrix for the example
# m <- makeCacheMatrix(raw) # make the cached matrix "object"
# inverse1 <- cacheSolve(m)  # invoke cacheSolve first time for m:
# # the inverse is calculated and cached
# inverse2 <- cacheSolve(m)  # invoke cacheSolve again for m:
# # this time the inverse is not calculated but returned from the cache
#
# This is not production code - access to the matrix is not properly
# restricted, there is no handling of exceptions, no checking that the
# matrix in consideration is invertible, etc.
# However adding all of this would obscure the use of lexical scoping
# which is the real goal of this assignment



## makeCacheMatrix
# makeCacheMatrix expects an invertible numerical matrix as parameter
# it returns a list (of functions) that wraps the matrix
# and has 4 attributes: "set", "get", "setinverse" and "getinverse"
# Each of these attributes refers to a function defined within
# makeCacheMatrix with roughly the following functionality
# 1) set : sets the object's internal matrix to a new value and clears
#    the cached inverse matrix
# 2) get : returns the object's internal matrix
# 3) setinverse: sets the value of the cache for the inverse matrix
# 4) getinverse: gets the value of the cache for the inverse matrix
#
makeCacheMatrix <- function(x = matrix()) {
        # initialize function environment var         
        cachedInverse <- NULL
        
        # set: reinitializes this "object" by reassigning the
        # top level function environment vars
        # notice the use of the <<- operator
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        
        # get: returns the raw matrix
        get <- function() x
        
        # setinverse: assigns its argument to the lexically scoped var
        # for the cached inverse matrix
        setinverse <- function(inverse) cachedInverse <<- inverse
        
        # getinverse: returns the value of the lexically scoped var
        # for the cached inverse matrix
        getinverse <- function() cachedInverse
        
        # return a list with the functions
        list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve
# cacheSolve is expecting an object returned by makeCacheMatrix
# as argument and returns a numerical matrix - the inverse of
# the matrix wrapped by the parameter passed in
# cacheSolve only calculates the inverse matrix once and writes
# it into the argument's cache
# If the argument's cache for the inverse matrix is already populated
# cacheSolve returns the matrix contained therein without
# recalculating.  In this case it outputs the message:
# "getting cached data" 
# before returning the cached inverse matrix
#
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        
        if(is.null(inv)) { 
          # we have no cached inverse, calculate it and assign it 
          rawMatrix <- x$get()  # get the raw matrix
          inv <- solve(rawMatrix) # invert it and put the result into inv
          x$setinverse(inv) # assign it into x's environment
        } else {
          # we have a cached inverse into inv
          message("getting cached data")
        }
        
        # return inv - at this point it contains the inverse
        # even if it was not in the cache at the beginning
        inv
}
