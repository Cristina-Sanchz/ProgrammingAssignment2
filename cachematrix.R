#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        set.inverse <- function(inverse) i <<- inverse 
        get.inverse <- function() i
        list(set = set, get = get, 
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}

# This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
        i <- x$get.inverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$set.inverse(i)
        i
}