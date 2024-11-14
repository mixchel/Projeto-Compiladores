fun main(){
    x = 0
    y = 1
    result = 0
    n = 20
    if(n < 0){
         print("Invalid number")
    }
    while(true){
         result = x + y
         x = y
         y = result
         n = n - 1
    }
    print(result)
}