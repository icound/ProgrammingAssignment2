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
}

##Test of Functions##
x=matrix(rnorm(9,5,1),3,3)  # matrix 3x3
solve(x)->solvedx # inverse matrix with function solve
cache<-makeCacheMatrix(x) # storing cache matrix into a variable
cacheSolve(cache) # execution of a function

cacheSolve(cache)-> # result from cache and storing to s
    #> cacheSolve(cache)#
    #getting cached data#
    #[,1]        [,2]      [,3]#
    #[1,]  7.175552 -1.12592503 -4.128271#
    #[2,] -3.387885  1.15454078  1.426205#
    #[3,] -4.293190 -0.01550097  3.245569#
    
    s-solved(x) # returns  3x3 = 0 if function works
