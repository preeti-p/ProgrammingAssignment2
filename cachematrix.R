#Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than compute it repeatedly

#This function creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL 
        set<-function(y)
         {
                x<<-y
                inv<<-NULL
         }
         get<-function()x
         setInv<-function(inverse) inv<<-inverse
         getInv<-function(inv)
         list(set=set,get=get,setInv=setInv,getInv=getInv) 
}



##This function computes the inverse of matrix returned by makeCacheMatrix above

##If inverse has already been calculated then cacheSolve retrieve the inverse from cache.
cacheSolve <- function(x, ...) {
        inv<-x$getInv()
        if(!is.null(inv))
        {
                message("Getting cached data")
                return(inv)
        }
        mat<-x$get()
	inv<-solve(mat,...)
	x$setInv(inv)
	inv
}
