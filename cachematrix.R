## MakeCacheMatrix is used to store the inverse of a matrix and 
## return the inverse directly from the cache if its already there or store it 

## Stores matrix as well as its inverse and returns the list of functions that are used to store matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set<- function(y){
        x <<- y
        inverse <<- NULL
    }
    setInverse <- function(inverse_matrix) inverse <<- inverse_matrix
    get <- function() x
    getInverse <- function() inverse
    
    list(set = set, setInverse = setInverse, getInverse = getInverse, get = get)
}


## Calculate the inverse of the matrix and return. But there are 2 conditions for return
## 1) If its already calculated then it is not calculated again and the cached value is returned
## 2) If its not already present in cache then it is calculated and the values are stored in cache

cacheSolve <- function(x, ...) {
       inverse <- x$getInverse()
       if(!is.null(inverse)){
               message("Getting cached values")
               return(inverse)
       }
       data <- x$get()
       x$set(data)
       inverse <- solve(data)
       x$setInverse(inverse)
       return(inverse)
}
