## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
    invmatval <- NULL    
        set <- function(y)         
                {
                x <<- y 
                invmatval <<- NULL 
                }   
        get <- function() x    
             setinverse <- function(inversemat) 
                        invmatval <<- inversemat    
                        getinverse <- function() invmatval    
                        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
   ## Return a matrix that is the inverse of 'x'
         invmatval <- x$getinverse()    
        if (!is.null(invmatval))     
        {       message("getting cached data")         
                return(invmatval)    
        }       
                data <- x$get()    
                invmatval <- solve(data,...)   
        x$setinverse(invmatval)
}
