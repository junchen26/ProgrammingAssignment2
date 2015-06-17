## Below is a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object,which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the cached inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        #set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        #get the value of the matrix
        get <- function() x
        
        #set the inverse of the matix
        setinverse <- function(inverse) 
        {
                inv <<- inverse
        }
        
        #get the cached inverse of the matrix
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then function will retrieve the inverse from the cache.
## otherwise, it will calculate and return the inverse of the matrix

cacheSolve <- function(x, ...) {
        #x is special "matrix" returned by makeCacheMatrix
        
        i <- x$getinverse()
        
        #if the inverse was cached
        if(!is.null(i) ) {                              
                message("getting cached inverse")
                
                return(i)                               
        }
        
        data <- x$get()
        
        #calculate the inverse
        i <- solve(data, ...)  
        
        #cache the inverse
        x$setinverse(i)  
        
        #return the inverse
        i                                               
}
