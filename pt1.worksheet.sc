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

 def calcularError(a: Double, b: Double): Double = math.abs(a - b)

  val error1 = calcularError(7.33, f1)
  val error2 = calcularError(8, f2)
  val error3 = calcularError(3.33, f3)
  val error4 = calcularError(3.33, f3)
  val error5 = calcularError(1.09861, f4)
  val error6 = calcularError(1.71828, f5)
  val error7 = calcularError(0.828427,f6)
  val error8 = calcularError(0.785398, f7)