# makeCacheMatrix
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    # cm will store the cached inverse matrix
    cm <- NULL


#Defining functions

    # setring the matrix
    set <- function(y) {
        x <<- y
        cm <<- NULL
    }
    # getting the matrix
    get <- function() x

    # setting the inverse
    setinvcm <- function(inverse) cm <<- inverse
    
    # getting the inverse
    getinvcm <- function() cm

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinvcm = setinvcm, getinvcm = getinvcm)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.


cacheSolve <- function(x, ...) {
    cm <- x$getinvcm()

    # If the inverse is already calculated, return it
    if (!is.null(cm)) {
        message("getting cached data")
        return(cm)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    cm <- solve(data, ...)

    # Cache the inverse
    x$setinvcm(cm)

    # Return it
    cm
}