## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function return a list with 4 functions:
#       get: returns the value of the matrix
#       set: defines the value of the matrix to the argument of the function
#       get_inverse:    returns the inverse of the matrix, as it has been stored, or NULL if not set
#       set_inverse:    defines the value of the inverse to the argument of the function
makeCacheMatrix <- function(x = matrix()) {
        # Set the inverse to NULL, as it has not been set yet
        inverse <- NULL
        
        # Just returns x, which is the matrix
        get <- function() x
        # Set x to the given argument & resets the inverse (as the matrix has changed)
        # Using <<- in order to change the object in the parent env of the set function
        #       (so in the makeCacheMatrix function)
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # Just returns the inverse
        get_inverse <- function() inverse
        # Set the inverse to the given argument
        set_inverse <- function(y) inverse <<- y
        
        # List returned by the function: the 4 functions
        list(get = get, set = set, get_inverse = get_inverse, set_inverse = set_inverse)
}


## Write a short comment describing this function
# This function returns the inverse of the matrix
#       (not a matrix object, but a makeCacheMatrix function)
# Either the inverse is already set and then returns,
#       else, it is first computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        
        if (is.null(inverse)) {
        # Inverse is NULL, so it has to be computed and updated to x
                message("Computing and caching the inverse")
                inverse <- solve(x$get())
                x$set_inverse(inverse)
        }
        else {
        # Inverse is set, just have to return it (which is done at the end of the function)
                message("Using cached inverse")
        }
        
        # Returns the inverse matrix
        inverse
}



