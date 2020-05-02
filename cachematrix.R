## saves the value of matrix x and its inverse in the global env
## returns the list of the 4 complementary functions created by su

makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y){
        x <<- y
        xi <<- NULL
    }
    get <- function() x
    setinv <- function(x_inv) xi <<- x_inv
    getinv <- function() xi
    list( set = set , get = get , setinv = setinv , getinv = getinv )
}


## the function below will return the inverser of matrix x 
## if x has been cached before then print GETTING CACHED DATA
## else will compute the inverse 
## and cache its value
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xi <- x$getinv()
    if(!is.null(xi)){
        print("getting cached data")
        return(xi)
    }
    data <- x$get()
    xi <- solve(data)
    x$setinv(xi)
    xi
}
