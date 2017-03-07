## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
    invmatvalue <- NULL    
        set <- function(y)             
       {           
                x <<- y        
                invmatvalue <<- NULL    
        }     get <- function() x    
                setinverse <- function(inversemat) 
                invmatvalue <<- inversemat 
                getinverse <- function()  invmatvalue    
                list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function computes the inverse of "matrix" returned by makeCacheMatrix.
## If the inverse already calculated and the matrix has not changed, then it retrieves the inverse from the cache
cacheSolve <- function(x, ...) 
{
   ## Return a matrix that is the inverse of 'x'    
        invmatvalue <- x$getinverse()
        if (!is.null(invmatvalue))  
        { 
         message("getting cached data")  
         return(invmatvalue) 
        }    
        data <- x$get()    
        invmatvalue <- solve(data,...)    
        x$setinverse(invmatvalue)    invmatvalue
}
