## Creates special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


## Computes the inverse of makeCacheMatrix if unchanged,
## otherwise retires inverse from cache (stored in m)

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
       
        
## EXAMPLE OUTPUT
        
##        > source("cachematrix.R")
##        > data<-cbind(c(1,2),c(3,4))
##        > m<- makeCacheMatrix(data)
##        > cacheSolve(m)
##        [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
##        > cacheSolve(m)
##        getting cached data
##        [,1] [,2]
##        [1,]   -2  1.5
##        [2,]    1 -0.5
        
}
