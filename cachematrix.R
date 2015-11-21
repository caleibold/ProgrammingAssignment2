## makeCacheMatrix creates and returns a list of functions
## to be used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    cache<-NULL
    set<-function(y) {
        x<<-y
        cache<<-NULL
    }
    get<-function() x
    setMatrix<-function(inverse) cache<<-inverse
    getInverse<-function() cache
    list(set = set, get = get,
         setMatrix = setMatrix,
         getInverse = getInverse)
}


## Returns the inverse matrix of matrix 'x'

cacheSolve <- function(x, ...) {
    cache<-x$getInverse()
    if(!is.null(cache)) {
        message("Getting cached data")
        cache
    }
    matrix<-x$get()
    cache<-solve(matrix,...)
    x$setMatrix(cache)
    cache
}
