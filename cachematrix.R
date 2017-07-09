## These functions compute the inverse of a square matrix. 
## The inverse matrix retrieved from the cache if it has already 
## been calculated and original matrix has not changed.

## Write a short comment describing this function
## This function cteates a list contaning a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix  

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(m) {
		x <<- m
		im <<- NULL		
	}
	get <- function() x
	set_inverse <- function(inv_matrix) im <<- inv_matrix 
	get_inverse <- function() im
	list (set = set, get = get, 
			   set_inverse = set_inverse,
			   get_inverse = get_inverse)
}


## This function return inverse of the matrix created 
## with "makeCacheMatrix" function. It gets inverse matrix 
## from the cache if it has already been calculated.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	im <- x$get_inverse()
	if (!is.null(im)) {
		message("getting cached data")
            return(im)
      }
	m <- x$get()
	im <- solve(m, ...)
	x$set_inverse(im)
	im		
}
