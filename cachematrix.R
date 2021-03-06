makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(new_x) {
        x <<- new_x
        inverse <<- NULL
    }
    get <- function() x
    set.inverse <- function(result) inverse <<- result
    get.inverse <- function() inverse
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}
#then
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get.inverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$set.inverse(inverse)
    inverse
}
