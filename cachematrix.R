## Below is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object,which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matix
## 4. get the cached inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        
        setinverse <- function(inverse) 
        {
                inv <<- inverse
        }
        
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## otherwise, it will calculate and return the inverse of the matrix

cacheSolve <- function(x, ...) {
        #x is special "matrix" returned by makeCacheMatrix
        
        i <- x$getinverse()
        
        
        if(!is.null(i) ) {                              #if the inverse was cached
                message("getting cached inverse")
                return(i)                               #exit the program with the cached inverse
        }
        
        data <- x$get()
        i <- solve(data, ...)                           #calculate the inverse
        x$setinverse(i)                                 #cache the inverse
        i                                               #return the inverse
}
