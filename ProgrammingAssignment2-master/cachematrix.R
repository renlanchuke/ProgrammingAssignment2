## makeCacheMatrix function create a object which store a matrix
## and cache its inverse

## The object the function created is actually a list containing 
## four function:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
               x <<- y
               inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)

}


## cacheSolve function calculates the inverse of the "matrix" object
## created with above function.It first checks to see if the inverse 
## has been solved.if so, it will return the inverse from the cache 
## Otherwise,it calculates the inverse of the data,and set the value 
## of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        if(!is.null(inv)) {
            #print("asd")
               message("getting cached data")
               return(inv)
        }
       
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
