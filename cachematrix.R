## These 2 functions are set designed to store and return values of 
## inputed matrix (must be inverasble), and its inverse
## I.e. z<-matrix(1:4,2,2) we create some matrix
## a<-makeCacheMatrix(z) - creates object
## (a) with matrix (z) saved inside
## cacheSolve(a) - used 1st time counts and store matrix inverted to matrix (z)
## within (a) object. Next time; if we won't change oryginal matrix saved inside 
## (a) object function will just call its inverse from cache memory associated with (a) object 

## makeCacheMatrix function creates matrix-based object 
## (list of functions on matricies explicite) able to store
## given matrix and its inverse matrix 'within' the object.
## Use cacheSolve to count, store and call inversed matrix.
makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setSolve<-function(m_inverse)m<<-m_inverse
	getSolve<-function()m
	list(get=get,set=set,getSolve=getSolve,setSolve=setSolve)
}

## cacheSolve returns an inverse matrix from cache 
## (from variables set within makeCacheMatrix function), 
## or if imposible, solve it, so this function alows us to skip 
## evaluating process if it has been done alredy
## works only on makeCacheMatrix object
cacheSolve <- function(x, ...) {
	m=x$getSolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setSolve(m)
	m
}
