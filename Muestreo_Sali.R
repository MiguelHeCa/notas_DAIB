
salamanca = data.table(
  Barrio = c(
    "Centro-Mercado San Juan",
    "Gran Vía-Canalejas",
    "Antiguo",
    "Prosperidad-Rollo",
    "Salas Pombo",
    "Carmelitas-Oeste",
    "Pizarrales",
    "Vidal",
    "Garrido",
    "Tejares",
    "San José-La Vega"
  ),
  N_i = c(
    6632,
    3539,
    856,
    5561,
    3318,
    4330,
    3786,
    3104,
    13690,
    976,
    2285
  ),
  n_i = c(
    85,
    45,
    11,
    71,
    43,
    56,
    49,
    40,
    176,
    12,
    29
  )
)

salamanca

error_estrat = function(N, n, z = 2, p = 0.5, q = 0.5) {
  z * sqrt( ((p*q)/n) * ((N-n)/(N-1)))
}

salamanca[, .(Barrio, N_i, n_i, error = error_estrat(N_i, n_i))]



2 * sqrt(((0.5*0.5)/85) * ((6632 - 85)/ (6632-1)))

error_estrat(N = 6632, n = 85)
