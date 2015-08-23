
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()){
    inversion <- NULL;
    set <- function(value){
        x <<- value
        inversion <<- NULL;
    }

    get <- function() x

    setInversion <- function(value) inversion <<- value
    getInversion <- function() inversion

    list(set = set, get = get, 
           setInversion = setInversion,
           getInversion = getInversion)
}  


## This function computes the inverse of the special
##    "matrix" returned by `makeCacheMatrix` above. If the inverse has
##    already been calculated (and the matrix has not changed), then
##    `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInversion()
    if(!is.null(inv)){
        message('retrieve cached inversion.')
        return(inv)
    }
    data <- x$get()
    x$setInversion(solve(data))
    ## Return a matrix that is the inverse of 'x'
    x$getInversion()
}
