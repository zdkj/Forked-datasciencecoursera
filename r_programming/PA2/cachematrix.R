## Put comments here that give an overall description of what your
## functions do

## return an 'object' of cache matrix

makeCacheMatrix <- function(x = matrix()) {
	mat <- x
	inv <- NULL
	set = function(x){
		mat <<- x
		inv <<- NULL
	}
	get = function(){
		mat
	}

	setinv = function(x){
		inv <<- x
	}
	getinv = function(){
		inv
	}
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## given an cache matrix return the inverse of x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
       	if(!is.null(m)){
       		#getting cached matrix
       		return (m)
       	}else{
       		inv <- solve(x$get())
       		x$setinv(inv)
       		return (inv)
       	}
}
