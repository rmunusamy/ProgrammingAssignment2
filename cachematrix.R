## Program to demonstrate caching
## 

## makeCacheMatrix function returns a list of functions that can get or set the values for a Matrix variable
## and its Inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv<<-NULL
        getMat<-function()x
        setMat<-function(y){
                x<<-y
                Inv<<-NULL
        }
        getInv<-function(x)Inv
        setInv<-function(x)Inv<<-x
        list(getMat=getMat,setMat=setMat,getInv=getInv,setInv=setInv)
}


## cacheSolve checks if an Inverse of a matrix is already in cache and returns the same.  If not in cache,it calculates the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        res<-x$getInv()
        if (!is.null(res)){
                # return cached data
                print("Returning from cache..")
                return(res)
        }
        else{
                v<-x$getMat()        
                res<-solve(v)
                x$setInv(res)
                res
        }
                
}
