## Put comments here that give an overall description of what your
## functions do

## This function is meant to create a 'matrix' which is, in fact, a list. It stores a matrix and, after calculating it,
# its inverse. The list is made up by 4 items, each of which is a function. After creating an empty 'inverse' object they do the following:
# 1. sets the value of the matrix (set)
# 2. retrieves the value of the matrix previously created (get)
# 3. sets the value of the inverse matrix in the cache
# 4. gets the cached value

makeCacheMatrix <- function(x = matrix()) {
            inverse <- NULL
            set <- function(y) {
                    x <<- y
                    inverse <<- NULL
            }
            get <- function() x
            setinv <- function(solve) inverse <<- solve
            getinv <- function() inverse
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## The function cacheSolve first checks if a cached value of the inverse matrix is available. If so, it retrieves it
# and returns it with a message ('getting cached data').
# If the inverse is not in the cache, it retrieves the matrix and calculates its inverse. After that, it stores it in the cache 
# and prints it.

cacheSolve <- function(x, ...) {                        
        inverse <- x$getinv()
            if(!is.null(inverse)) {
                    message("getting cached data")
                    return(inverse)
            }
            data <- x$get()
            inverse <- solve(data, ...)
            x$setinv(inverse)
            inverse
}
