## This function cached the inverse of a matrix, and will not compute again
## the inverse if already exists

## this function creates a special "vector", which contains a function to
## set and get the value of the matrix, and set and get the value of 
## the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y){
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


## This function cached the result of the inverse and compute if not exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
