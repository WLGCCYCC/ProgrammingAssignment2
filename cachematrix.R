## makeCacheMatrix stores uninversed matrix and inversed matrix
## cacheSolve uses makeCacheMatrix to get inversed matrix

## makeCacheMatrix returns a list of four functions:
##"get","set","getIM","setIM"
## the first two functions are for store and return universed matrix
## the last two functions are for store and return inversed matrix
## They  do not perform any calculation or evaluation

makeCacheMatrix <- function(x = matrix()) {    
	## makeCacheMatrix takes a matrix as an input
	
	IM  <- NULL                             
	## IM(Inversed Matrix)
	
	set <- function(y) {
		
		x<<-y
		## set takes an new matrix and replaces the old one with 
		## the new one
		
		IM<<- null
		## IM will be reset to null for catching new inversed matrix
	}
	
	get <- function() x
	## get simply returns the "uninversed" matrix
	
	setIM<- function(inversedMatrix) IM<<-inversedMatrix
	## setIM takes an inversed matrix and stores it in IM
	
	getIM<- function() IM
	## getIM simply returns the inversed matrix
	
	list(set=set, get=get, setIM=setIM, getIM=getIM )
	## return all four functions
	
}


## cacheSolve takes an input from makeCacheMatrix
## The argument "..." will be passed to function "solve"

cacheSolve <- function(x, ...) {
	IM <- x$getIM()
	## cache the value in IM
	
	if(!is.null(IM)) {
		
		message("getting cached data")
		
		return(IM)
		## If IM already has an inversed matrix, simply return that matrix
		## and end the function
		
	}
	
	##If IM is empty then use "get" to extract the matrix and inverse it
	data <- x$get()
	
	IM <- solve(data, ...)
	
	x$setIM(IM)
	
	IM
}