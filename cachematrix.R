## The two functions are used to cache the inverse of a matrix.

## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the reverse of the special 
## "matrix" created with the above function. However, it 
## first checks to see if the reverse has already been calculated. 
## If so, it gets the reverse from the cache and skips the 
## computation. Otherwise, it calculates the reverse of the data 
## and sets the value of the reverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getreverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- reverse(data, ...)
    x$setreverse(i)
    i
}
