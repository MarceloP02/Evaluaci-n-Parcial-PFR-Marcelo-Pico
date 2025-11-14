// Ejercicio 1

def myMethod1Pass(datos: List[Double]): Double = {
  val logs: List[Double] =
    datos.map((x: Double) => math.log(x))

  val init: (Double, Double, Int) = (0.0, 0.0, 0)
  val acumulado: (Double, Double, Int) =
    logs.foldLeft(init) {
      case ((s: Double, s2: Double, k: Int), l: Double) =>
        (s + l, s2 + l * l, k + 1)
    }

  val sum: Double = acumulado._1
  val sum2: Double = acumulado._2
  val n: Int = acumulado._3

  val mu: Double = sum / n.toDouble
  val varianza: Double = (sum2 / n.toDouble) - (mu * mu)
  val desviacionEstandar: Double = math.sqrt(varianza)
  desviacionEstandar
}
// Ejercicio 2

def ajustarPrecios(precios: List[Double], politica: Double => Double): List[Double] = {
  val resultado: List[Double] = precios.map((p: Double) => politica(p))
  resultado
}
val IVA_ECUADOR: Double = 0.15

def politicaIvaEcuador(p: Double): Double = {
  val ajustado: Double = p * (1.0 + IVA_ECUADOR)
  ajustado
}

def politicaDescuento(p: Double): Double = {
  val ajustado: Double = p * 0.95
  ajustado
}

def redondear(p: Double): Double = {
  val v: Double = Math.round(p * 100.0) / 100.0
  v
}
val precios: List[Double] = List(9.99, 25.0, 120.0)

val conIva: List[Double] = ajustarPrecios(precios, politicaIvaEcuador)

def ivaMasRedondeo(p: Double): Double = {
  val conIvaLocal: Double = politicaIvaEcuador(p)
  val redondeado: Double = redondear(conIvaLocal)
  redondeado
}
val conIvaYRedondeo: List[Double] = ajustarPrecios(precios, ivaMasRedondeo)
val conDescuento: List[Double] = ajustarPrecios(precios, politicaDescuento)

//Ejercicio 3
def generadorIncrementador(incremento: Int): Int => Int = {
  val f: Int => Int = (x: Int) => {
    val resultado: Int = x + incremento
    resultado
  }
  f
}




