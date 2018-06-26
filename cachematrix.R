## we start off with the makeCacheMatrix, which calls for a function that will catch any cache inverses. Therefore we set inv to NULL to 
## make sure any previous values assigned to inv are cleared.
## We create a list in order to assign a name to all elements so they match up accordingly to the function that are previously displayed
## This function is created in order to make a matrix that is capable of caching inverses

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y){
		x <<- y
		inv <<- NULL
	}
	get <- function()x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function ()inv
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve takes the inverse from the object of MakeCacheMatrix, which in this case is the matrix
## This coding should retrieve inverse if it has already been calculated in MakeCacheMatrix

cacheSolve <- function(x, ...) { 
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat,...)
	x$setInverse(inv)
}
