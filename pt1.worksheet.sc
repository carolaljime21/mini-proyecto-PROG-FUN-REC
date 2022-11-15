def calcularError(a: Double, b: Double): Double = math.abs(a - b)
//SIMPSON 1/3
def integracion(a:Double, b:Double, f:Double => Double) : Double = {
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}
val f1 = integracion(3, 5, x => (-1*math.pow(x,2) + 8*x  - 12))
val f2 = integracion(0, 2, x => 3*(math.pow(x,2)))
val f3 = integracion(-1, 1, x => (x + 2*math.pow(x,2) - math.pow(x,3) + 5*math.pow(x,4)))
val f4 = integracion(1, 2, x=> (2*x + 1)/(math.pow(x,2) + x))
val f5 = integracion(0, 1, x=> math.pow((math.E),x))
val f6 = integracion(2, 3, x=>(1/(math.sqrt(x-1))))
val f7 = integracion(0, 1, x=> (1/(1+math.pow(x,2))))
val f8 = integracion(0, 1, x=> 1/(1 + (math.pow(x, 2))))

val es1 = calcularError(7.33, f1)
val es2 = calcularError(8, f2)
val es3 = calcularError(3.33, f3)
val es4 = calcularError(3.33, f3)
val es5 = calcularError(1.09861, f4)
val es6 = calcularError(1.71828, f5)
val es7 = calcularError(0.828427,f6)
val es8 = calcularError(0.785398, f7)

  //SIMPSON 1/3 COMPUESTA
  def simpCompuesta(a:Double, b:Double, f:Double => Double) : Double = {
    val n = 2
    val h = (b-a)/n
    
    h/3 * (1 to n/2).map(j => (f(a+2 * j * h - 2) + 4*f(a+2 * j * h - 1) + f((a+2 * j * h)))).sum
}
val comp1 = simpCompuesta(3, 5, x => (-1*math.pow(x,2) + 8*x  - 12))
val comp2 = simpCompuesta(0, 2, x => 3*(math.pow(x,2)))
