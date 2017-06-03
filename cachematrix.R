## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
                matinv <- NULL
                set <- function(y) {
                        x <<- y
                        matinv <<- NULL
                }
                get <- function() x
                setinv <- function(inverse) matinv <<- inverse 
                getinv <- function() matinv
                list(set = set, get = get,
                     setinv=setinv,
                     getinv=getinv)
        }



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        matinv<-x$getinv()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv<-solve(data)
        x$setinv(matinv)
        matinv
        
        
        
        ## Return a matrix that is the inverse of 'x'
}




