## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
##  sub functions: 
##      set() set matrix
##      get() get matrix
##      setsolved() set inversed matrix
##      getsolved() get inversed matrix

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

##############################################################################################

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # cache variable s is NULL at the beginning
    s <- NULL
    # set function, function sets a matrix 
    set <- function(new_mat) {
        s <<- NULL
        x <<- new_mat
    }
    # get function, function returns matrix stored in variable x  
    get <- function() x
    # getsolve function, function returns a matrix
    getsolved <- function() s
    # setsolve function, function sets a matrix
    setsolved <- function(solve) s <<- solve
    
    # return a list of functions
    list(set = set, get = get,
         setsolved = setsolved,
         getsolved = getsolved)
}




cachesolve <- function(x, ...) {
    #get matrix from the cache
    m <- x$getsolved()
    
    #if the cache with inversed matrix is not empty return the matrix from the cache and exit from the function
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    #the cache is empty so get the matrix
    data <- x$get()
    
    #inverse the matrix and store in the variable m
    m <- solve(data, ...)
    
    #store the variable m (inversed matrix) to the cache
    x$setsolved(m)
    
    # Return a matrix that is the inverse of 'x'
    m
}