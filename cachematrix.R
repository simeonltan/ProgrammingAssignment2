## The function, makeCacheMatrix, creates a special "matrix" object that can 
## cache its inverse
#  It is a list containing a functions to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse matrix (setinverse)
## 4. get the value of the inverse matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}

## The function, cacheSolve, returns the inverse of the special "matrix"
## returned by makeCacheMatrix above
## It first checks to see if the inverse of the "matrix" has already been 
## calculated using getinverse
## If yes, it displays "getting cached data" and returns the 
## inverse of the "matrix"
## Otherwise it calculates the inverse of "matrix" using solve 
## sets the inverse of "matrix" in the cache using the setinverse function
## then returns the inverse of "matrix"

cacheSolve <- function(x = matrix() , ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
