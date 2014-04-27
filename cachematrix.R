## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## itens list : get, setsolve, getsolve
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        ## Make the list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        
        ## Check if the matrix exists
        if(!is.null(m)) { message("getting cached data")
                          ## Chek if the matrix has changed
                          y <- solve(x$get())
                          if   (identical(m,y)) {return(m)}
                          else message("matrix has changed")
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
