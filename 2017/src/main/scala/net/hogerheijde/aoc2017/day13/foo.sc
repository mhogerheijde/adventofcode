def modInv(a: Int, m: Int, x:Int = 1, y:Int = 0) : Int = if (m == 0) x else modInv(m, a%m, y, x - y*(a/m))




val modulo = (4 - 1) * 2

4 % modulo


//modInv(1, 1)
//modInv(1, 2)
//modInv(1, 3)
//modInv(1, 4)
//modInv(1, 2)
//modInv(2, 2)
//modInv(3, 2)
//modInv(4, 2)

//5 % 3
//modInv(5, 4, 3)