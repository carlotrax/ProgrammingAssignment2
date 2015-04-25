makeCacheMatrix <- function(x = matrix()) {
  
    #Initialize to NULL
    inv <- NULL

    #Give Value to the Matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL 
    }
    #Get Value for the Matrix
    get <- function() x

    #Set Inverse for Matrix
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    #Return a list
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

cacheSolve <- function(x, ...) {

    #Get Inverse
    inv <- x$getinverse()
    
    #Check if already cached and return it
    if(!is.null(inv)) {
        message("Caching Data")
        return(inv)
    }

    #Otherwise get Matrix
    data <- x$get()

    #Get inverse Matrix
    inv <- solve(data, ...)

    #Cache Inverse
    x$setinverse(inv)

    #Return Inverse
    inv
}