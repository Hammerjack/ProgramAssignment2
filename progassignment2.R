
makeCacheMatrix <- function( m = matrix() ) {

    x <- NULL

    set <- function( matrix ) {
            m <<- matrix
            x <<- NULL
    }

    get <- function() {
    	m
    }
    setInverse <- function(inverse) {
        x <<- inverse
    }
    getInverse <- function() {
        x
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(a, ...) {

    m <- a$getInverse()
    if( !is.null(m) ) {
            return(m)
    }

     data <- a$get()

    m <- solve(data) %*% data

    a$setInverse(m)

    m
}