
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                       #Sets value of m to null
    set <- function(y) {                            #Sets the function for y
        x <<- y                                     #Assignes y to x
        m <<- NULL                                  #Sets value of m to null
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse   #Function to set the inverse
    getinverse <- function() m                      #Function to get the inverse
    list(set = set, get = get,                      #The list of elements
         setinverse = setinverse,                   #to be used in cacheSolve
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()                  #query the x vector's cache 
    if(!is.null(m)) {                     #to see whether there is a cache
        message("getting cached data")    
        return(m)                         #Returns the cache, no need for 
    }                                     #computations
    data <- x$get()                       #Calls the matrix
    m <- solve(data, ...)                 #Calculates the inverse of the matrix
    x$setinverse(m)                       #Saves the result back to the x cache
    m                                     #Returns the result
}
