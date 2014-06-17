## These functions take a matrix 'x' and calculate the inverse of the matrix.  
## If this matrix is one that we will be using frequently in our applications,
## it then makes sense to cache the inverse matrix rather than recomputing it
## each time we need it.  Thus, these functions, after calculating the inverse,
## also cache it.  To do this, the functions take advantage of the scoping rules
## of the R language and how these rules can be used to preerve state inside
## of an R object.


## The function 'makeCacheMatrix', creates a list containing a function 
## that will:
  #1.  set the matrix object
  #2.  get the matrix object
  #3.  set the inverse matrix object
  #4.  get the inverse matrix object
  
## IMPORTANT:  The matrix 'x' must be a square invertible matrix  

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        # sets the matrix x
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        # gets the matrix object
        get <- function() x
        # sets the inverse matrix of x
        setinverse <- function(solve) invm <<- solve(x)%*%x
        # gets the inverse matrix of x from cache
        getinverse <- function() invm
        # creates a list containing a function that will do the four tasks
        # described above; list is returned
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}


## The function cacheSolve returns a matrix that is the inverse of 'x'. 
## If available, the inverse matrix is returned from cache; it not, it is 
## calculated. 

cacheSolve <- function(x, ...) {
        # Check if inverse matrix is available in cache.  If it is, get it and
        # return it. 
        invm <- x$getinverse() 
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        # If the inverse matrix is not available in the cache, then calculate
        # the inverse matrix and set the cache
        data <- x$get()
        invm <- solve(data) %*%data 
        x$setinverse(invm)
        # Function returns the inverse matrix
        invm
}

