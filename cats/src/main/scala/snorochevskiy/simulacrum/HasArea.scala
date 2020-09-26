package snorochevskiy.simulacrum

import simulacrum.{op, typeclass}

@typeclass trait HasArea[A] {
  @op("area") def calcArea(a: A): Double
}
