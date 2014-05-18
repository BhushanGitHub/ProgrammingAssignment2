## Function makeCacheMatrix
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function( x = matrix() ) {
        
        inversematrix = NULL
        
       set <- function( y ) {
                
          x <<- y
                
          inversematrix <<- NULL
        }
        
       get <- function() x
        
       setinverse <- function(solve) inversematrix <<- solve
        
       getinverse <- function() inversematrix

       list( set = set, get = get, setmean = setmean, getmean = getmean)
        
}


## cacheSolve calculates inverse of the matrix created abpove.
cacheSolve <- function( x, ... ) {
        
        inversematrix <- x$getinverse()
        ## Get inverse if available
        if( !is.null( inversematrix ) ) {
                message("getting cached data")
                
                return( inversematrix )
                
        }
        
        #otherwise calculates inverse of data
        invertiblematrix <- x$get()
        inversematrix <- solve(invertiblematrix, ...)
        
        #set mean to cached value
        x$setinverse(inversematrix)
        
        inversematrix
}
