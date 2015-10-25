## Two functions that make a globally scoped vector of functions to handle the task of inverting a matrix and
## caching the inverse.  If the inverse has been generated, it is cached so that future calls won't recalculate the matrix 
## inverse.

makeCacheMatrix <- function(y = matrix()) ## assume y is an [n x n] invertable matrix
{
        
        ## make two globally scoped matrices of size [y]
        w <<- y
        z <<- matrix(nrow = nrow(y), ncol = ncol(y))
     
        hasinverse <- function() z[1, 1] ## use check to see if z contains numeric or "NA" values
   
        setinverse <- function(a) z <<- a ## cache the inverse of w in z 
        
        getinverse <- function() z ## return inverse of w
        
        ## function returns a globally scoped list of calls to data and inverse 
        
        x <<- list(getinverse = getinverse,
             hasinverse = hasinverse,
             setinverse = setinverse)
}

cacheSolve <- function(y = matrix()) ## assume y is an [n x n] invertable matrix 
{
       
        if(!is.na(x$hasinverse())) ## test if <hasinverse> is numeric, if so use cached data
        {
                message("Inverse previously calculated, using cached data")
                a <- x$getinverse()
        }
        
        else ## if <hasinverse> is "NA", calculate inverse of y, return and cache inverse
        {
                message("Inverse not previously calculated, using solve()")
        		a <- solve(y)
        		x$setinverse(a)
        }
        
        return(a)
}
