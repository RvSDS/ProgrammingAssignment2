## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 ## With argument matrix
	m <- NULL
	set <-function (y){
        ## To set the value of the matrix using a new function
		x <<-y
                ##double arrows to managing variables at different levels
		m <<- NULL
	}	
	get <- function()x
        ## To get the value of the matrix
	setInverse <- function(inverse) m <<- inverse
        ## To set the value of the inverse
      getInverse <- function() m
        ## To get the value of the inverse
      list(set = set, get = get, setInverse = setInverse,getInverse = getInverse)
        ## List created 
}
}

cacheSolve <- function(x, ...) {
       ## To compute the inversion of matrix above  
       ## Return a matrix that is the inverse of 'x'
                m <- x$getInverse()
                ## To return a matrix that is the inverse of X, assigned to m 
        if(!is.null(m)) {
        ## check if the inverse is already calculeted, if so return m
                message("getting cached data")
                ## if so, display "getting cached data"
                return(m)
        }
        data <- x$get()
        ## if not, compute the inverse of the matrix
        m <- solve(data, ...)
        ## Solve is standard R function to calculate the inverse
        x$setInverse(m)
        m
}
