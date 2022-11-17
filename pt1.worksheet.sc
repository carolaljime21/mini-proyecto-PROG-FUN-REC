//FUNCION PARA CALCULAR EL ERROR
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
val f7 = integracion(0, 1, x=> 1/(1 + (math.pow(x, 2))))

val ers1 = calcularError(7.33, f1)
val ers2 = calcularError(8, f2)
val ers3 = calcularError(3.33, f3)
val ers4 = calcularError(3.33, f4)
val ers5 = calcularError(1.09861, f5)
val ers6 = calcularError(1.71828, f6)
val ers7 = calcularError(0.828427,f7)

//SIMPSON 1/3 COMPUESTA

//SIMPSON EXTENDIDA
def simpExtendida(a: Double, b: Double, f: Double => Double) : Double = {
  val n = 2 * (b-a)
  val h = ((b-a)/n)
  val i = f(a) + 4 * (1 to (n-1).toInt by 2).map(c => f(a + c * h)).sum //SUMATORIA IMPAR
  val j = f(b) + 2 * (2 to (n-2).toInt by 2).map(c => f(a + c * h)).sum //SUMATORIA PAR
  (h*(i+j))/3
}

val ex1 = simpExtendida(3, 5, x => (-1*math.pow(x,2) + 8*x  - 12))
val ex2 = simpExtendida(0, 2, x => 3*(math.pow(x,2)))
val ex3 = simpExtendida(-1, 1, x => (x + 2*math.pow(x,2) - math.pow(x,3) + 5*math.pow(x,4)))
val ex4 = simpExtendida(1, 2, x=> (2*x + 1)/(math.pow(x,2) + x))
val ex5 = simpExtendida(0, 1, x=> math.pow((math.E),x))
val ex6 = simpExtendida(2, 3, x=>(1/(math.sqrt(x-1))))
val ex7 = simpExtendida(0, 1, x=> (1/(1+math.pow(x,2))))

val ere1 = calcularError(7.33, ex1)
val ere2 = calcularError(8, ex2)
val ere3 = calcularError(3.33, ex3)
val ere4 = calcularError(3.33, ex4)
val ere5 = calcularError(1.09861, ex5)
val ere6 = calcularError(1.71828, ex6)
val ere7 = calcularError(0.828427,ex7)

