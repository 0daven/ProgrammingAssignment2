# makeCacheMatrix is a function that returns a list of functions
# * setMatrix & getMatrix			set and get the value of a matrix
# * cacheInverse & get Inverse		set & get the cached value (inverse of the matrix)
# Creating function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
        # initially nothing is cached, setting it to NULL
        cache <- NULL
        # Storing matrix
        setMatrix <- function(mat) {
            x <<- mat
        # Flushing any cached values
			cache <<- NULL
        }
        # Getting the stored matrix
        getMatrix <- function() x
        # Sending the given argument to CACHE
        cacheInverse <- function(solve) {
                cache <<- solve
        }
        # Getting the cached value
        getInverse <- function() {
                cache
        }
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix,
			 getMatrix = getMatrix,
			 cacheInverse = cacheInverse,
			 getInverse = getInverse
			 )
}

# This function is used to Calculate the inverse of a matrix 
# created using the makeCacheMatrix function.
cacheSolve <- function(x, ...) {
        # Getting value from CACHE
        inverse <- x$getInverse()
        # If exist inverse value, return it, else calculate and store in CACHE
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$getMatrix()
        inverse <- solve(data)
        c$cacheInverse(inverse)
        # returning the inverse
        inverse
}