## Richard J. Urban
## 2016-07-03
## R Programming Assignment No. 2

## This function "creates a special 'matrix' object that can cache it its inverse."

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## This was confusing since I thought the example was calling the "mean" function,
        ## but really it is using "mean" as the variable in the function.  
        ## Here I choose to use "sovled" instead of "sovle" to make it clear this
        ## is the value being passed from cacheSovle
        setinverse <- function(solved) m <<- solved  
        getinverse <- function() m
        list(set =  set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
             )
}

## This function "computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## first test to see if the inverse has not been set by checking if m is null
        if(!is.null(m)) {
                ## when m is not null we next test if it is identical 
                ## based on discussion comment by Abhshek Dubey,
                ## however their identity matrix assumes a 2x2 matrix,
                ## not a square of nrow x ncol 
                ## this uses diag to create an identity matrix based on 
                ## the nrow of the current matrix
                identityMatrix <- diag(nrow(m))
                n <- x$get()
                result <- m %*% n
                if(all.equal(result, identityMatrix)) {
                        #when the inverse is the same, return the cached value
                        message("getting cached data")
                        return(m)   
                }
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
