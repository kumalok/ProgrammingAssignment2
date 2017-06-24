## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## Below 2 functions will be used to cache the inverse of a squared matrix
## Assumption: Input Matrix is always invertible

# makeCacheMatrix function creates a matrix, which is really a list containing a function to
# 1. set matrix
# 2. get matrix
# 3. set inverse of the matrix
# 4. get inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
	inv_c <- NULL
    
	## set matrix
	set <- function(y) {
        mat <<- y
        inv_c <<- NULL
    }
	
	## get matrix
    get <- function() mat
	
	## set inverse of matrix
    setinverse <- function(inverse) inv_c <<- inverse
	
	## get inverse of matrix
    getinverse <- function() inv_c
	
	## initialize list and return
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# function: cacheSolve - returns inverse of matrix
# 1. checks if inverse has already been computed. 
# 		i)  yes - get the result and skips the computation. 
#		ii) no  - computes the inverse, sets the value in the cache via setinverse function.

# Assumption: Matrix 'mat' is always invertible
cacheSolve <- function(mat, ...) {
    inv_c <- mat$getinverse()
    if(!is.null(inv_c)) {
        message("fetching cached data.")
        return(inv_c)
    }
    data <- mat$get()
    inv_c <- solve(data)
    mat$setinverse(inv_c)
    inv_c
}
