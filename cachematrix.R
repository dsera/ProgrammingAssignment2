## Create and return a closure for the given matrix.  The closure will
## be used to save and retrieve the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL         # We don't yet have a value for the inverse.

    # This function takes a matrix and caches it if it is different
    # from the saved value.  If the new matrix is different, we'll
    # reset the inverse because the old inverse (if any) won't be
    # correct for this new matrix
    setmatrix <- function(newmat) {
        if(!(is.matrix(newmat) && dim(newmat) == dim(x) && all(newmat == x))) {
            # This new matrix is different from the original so cache
            # it and clear the cached inverse.
	    message("changing matrix and clearing inverse")
            x <<- newmat
	    inverse <<- NULL
	}
    }

    # This function returns the cached matrix
    getmatrix <- function() x

    # This function caches the given matrix as the inverse of the cached matrix.
    setinverse <- function(inv)
        inverse <<- inv

    # This function returns the cached inverse value.
    getinverse <- function() inverse

    ## Create and return a list containing the above functions.  These
    ## will serve as the API for manipulating the matrix and its inverse.
    list(setmatrix = setmatrix,
        getmatrix = getmatrix,
	setinverse = setinverse,
	getinverse = getinverse)
}


## Return a matrix that is the inverse of the matrix stored in the
## given list 'x' which serves as a closure for the desired matrix.

cacheSolve <- function(x, ...) {

    ## Try to get the cached value of the inverse using the
    ## getinverse function. If the inverse is available, we can
    ## just return it.

    m <- x$getinvers()
    if(!is.null(m)) {
        # We have the cached value so return it.
        message("getting cached inverse")
        return(m)
    }

    ## The cached matrix isn't available so we have to retrieve the
    ## matrix from the cache, calculate the inverse, store the
    ## inverse for future use, and return the inverse..

    matrix <- x$getmatrix()      # the matrix for which the inverse is required
    inverse <- solve(matrix)
    x$setinverse(inverse)        # Save it in case the inverse is needed again
    inverse
}
