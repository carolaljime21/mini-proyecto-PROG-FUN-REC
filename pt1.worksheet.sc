//CAROLINA ALVARADO JIMENES - MINIPROYECTO
// INTEGRAR POR METODO DE SIMPSON: 1/3, COMPUESTA Y SIMPLE

//FUNCION PARA CALCULAR EL ERROR ---------------------------------------------------------------------
def calcularError(a: Double, b: Double): Double = math.abs(a - b)

//SIMPSON 1/3 ----------------------------------------------------------------------------------------
def integracion(a:Double, b:Double, f:Double => Double) : Double = {
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
}
val s1 = integracion(3, 5, x => (-1*math.pow(x,2) + 8*x  - 12))
val s2 = integracion(0, 2, x => 3*(math.pow(x,2)))
val s3 = integracion(-1, 1, x => (x + 2*math.pow(x,2) - math.pow(x,3) + 5*math.pow(x,4)))
val s4 = integracion(1, 2, x=> (2*x + 1)/(math.pow(x,2) + x))
val s5 = integracion(0, 1, x=> math.pow((math.E),x))
val s6 = integracion(2, 3, x=>(1/(math.sqrt(x-1))))
val s7 = integracion(0, 1, x=> 1/(1 + (math.pow(x, 2))))

val ers1 = calcularError(7.33, s1)
val ers2 = calcularError(8, s2)
val ers3 = calcularError(3.33, s3)
val ers4 = calcularError(3.33, s4)
val ers5 = calcularError(1.09861, s5)
val ers6 = calcularError(1.71828, s6)
val ers7 = calcularError(0.828427,s7)

//SIMPSON 1/3 COMPUESTA ------------------------------------------------------------------------------
def simpCompuesta(a: Double, b: Double, f: Double => Double) : Double = {
  val nIntervalos = 2
  val h = (b-a)/nIntervalos
  val varx = (j: Double) => (a+(j*h))
  val x = (c: Double) => f(varx(2*c-2) + 4*f(varx(2*c-1)) + f(varx(2*c)))

  //FORMULA FINAL
  (1 to 2).map(x(_))((h).toInt/3)
}

val comp1 = simpCompuesta(3, 5, x => (-1*math.pow(x,2) + 8*x  - 12))
val comp2 = simpCompuesta(0, 2, x => 3*(math.pow(x,2)))
val comp3 = simpCompuesta(-1, 1, x => (x + 2*math.pow(x,2) - math.pow(x,3) + 5*math.pow(x,4)))
val comp4 = simpCompuesta(1, 2, x=> (2*x + 1)/(math.pow(x,2) + x))
val comp5 = simpCompuesta(0, 1, x=> math.pow((math.E),x))
val comp6 = simpCompuesta(2, 3, x=>(1/(math.sqrt(x-1))))
val comp7 = simpCompuesta(0, 1, x=> (1/(1+math.pow(x,2))))

val erc1 = calcularError(7.33, comp1)
val erc2 = calcularError(8, comp2)
val erc3 = calcularError(3.33, comp3)
val erc4 = calcularError(3.33, comp4)
val erc5 = calcularError(1.09861, comp5)
val erc6 = calcularError(1.71828, comp6)
val erc7 = calcularError(0.828427, comp7)

//SIMPSON EXTENDIDA ----------------------------------------------------------------------------------
def simpExtendida(a: Double, b: Double, f: Double => Double) : Double = {
  val nIntervalos = 2 * (b-a)
  val h = ((b-a)/nIntervalos)
  val i = f(a) + 4 * (1 to (nIntervalos-1).toInt by 2).map(c => f(a + c * h)).sum //SUMATORIA IMPAR
  val j = f(b) + 2 * (2 to (nIntervalos-2).toInt by 2).map(c => f(a + c * h)).sum //SUMATORIA PAR

  //FORMULA FINAL
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

