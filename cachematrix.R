## This pair of functions implement a special cached matrix
## object containing the matrix itself and its inverse obtained
## using solve()


## makeCacheMatrix creates a special cached matrix object,
## implemented as a list with four functions:
## * Set the value of the matrix
## * Get the value of the matrix
## * Set the value of the matrix inverse
## * Get the value of the matrix inverse
## The matrix and its inverse are cached in the parent
## environment using the <<- operator

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y){
        x <<- y
        xinv <<- NULL
    }
    get <- function(){
        x
    }
    setinv <- function(yinv){
        xinv <<- yinv
    }
    getinv <- function(){
        xinv
    }
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve retrieves the matrix inverse of x if it was
## previously cached in the parent enviroment. Otherwise,
## the matrix inverse is calculated using solve(). The
## calculated inverse is both cached and returned.

cacheSolve <- function(x, ...) {
    minv <- x$getinv()
    if(!is.null(minv)){
        minv
    }else{
        m <- x$get()
        minv <- solve(m, ...)
        x$setinv(minv)
        minv
    }
}
