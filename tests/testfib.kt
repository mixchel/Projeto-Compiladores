fun main(){
    var x : Int = 0
    var y : Int = 1
    var result : Int = 0
    var n : Int = 20
    if(n < 0){
         return
    }
    while(true){
         result = x + y
         x = y
         y = result
         n = n - 1
    }
    print(result)
}