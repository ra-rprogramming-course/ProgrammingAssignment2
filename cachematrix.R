#############################################
###This is a solution by Robert Arthur for###
###the R programming coursera course	  ###
###programming assignment #2 for week 3	  ###
#############################################



#For this assignment we are supposed to 
#do something similar to the example provided
#of caching a mean, but instead do so on a 
#matrix we supply with creating/caching its
#inverse. This is to show us how R can do
#data caching and storing data outside the 
#current environment.

#As such, the following function makeCacheMatrix
#takes data provided by the user as a matrix "x"
#and:
#sets value of matrix
#can get value of matrix
#sets the value of the matrix's inverse
#can get vaue of the matrix's inverse

makeCacheMatrix <- function(x = matrix())  {
	m  <- NULL
	
	set <- function(y){
		x <<- y		#allows user to set a new matrix as x from command line
		m <<- NULL	#keeps M set to null
	}
	
	get <- function() x
	setinverse <- function(inverse) m <<- inverse #sets(caches) inverse of x
	getinverse <- function() m #spits out the inverse matrix 
	
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse) #stores our options for the user to see as a list
	
}


#next to complete the assignment,
#we create cacheSolve, which returns the inverse of the matrix.
#It will check to see if we have already created an inverse
#and if so, it will return the cached inverse and exits
#If not, it will then create and cache an inverse for the matrix
#using the setinverse() tool we made in makeCacheMatrix

cacheSolve <- function(x, ...){
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data") #you will only see this if we have a cached inverse already
		return(m)
	}
	#run these next steps if no inverse exists
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m) #calls makeCacheMatrix's setinverse() function with the solve command
	m #returns m
}


##Below I have copy/pasted the results of a test run:
##
#> z = matrix(c(1,2,3,4),2,2)
#> z
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> test=makeCacheMatrix(z)
#> test$get()
#
#We see here that test$get() has returned matrix Z
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#> test$getinverse()
#NULL
#We can see there is no inverse as of now.
#> cacheSolve(test)
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#Note that we did not see "getting cached data.", thus
#we do not expect aynthing cached at this time
#cacheSolve(test)
#getting cached data
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#test$getinverse()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#
#We can see that the inverse of matrix z stored in test
#has indeed been stored and cached, and everything is working
#properly.