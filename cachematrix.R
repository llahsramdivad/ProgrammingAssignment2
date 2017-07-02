## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. This pair of functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
	set = function(y) {
		x <<-y #assign a value to an object in an environment different
				#to current environment
		inv<<-NULL
	}
	get = function() x
	setinv = function(inverse) inv <<-inverse 
	getinv = function() inv

list(set=set, get=get, setinv=setinv, getinv=getinv)
#gives the name 'set' to the set() function defined above etc
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
inv = x$getinv()
        if (!is.null(inv)){ #if inv is NOT null i.e exists in cache
                return(inv)
        }
mat.data = x$get() # if not in cache
inv = solve(mat.data, ...)
x$setinv(inv)
                return(inv)
}
