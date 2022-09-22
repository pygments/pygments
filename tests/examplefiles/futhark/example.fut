local let dotprod [n] 't
                  (mul: t -> t -> t) (add: t -> t -> t) (zero: t)
                  (a: [n]t) (b: [n]t): t =
  map2 mul a b |> reduce add zero

module matmul (F: numeric) = {
  type t = F.t
  open F

  let matmul [n1][n2][m] (xss: [n1][m]t) (yss: [m][n2]t): *[n1][n2]t =
    map (\xs -> map (\ys -> dotprod (*) (+) (i32 0) xs ys) <| transpose yss) xss
}

module matmul32 = matmul f32

let main [n][m] (xss: [n][m]f32) (yss: [m][n]f32): ?[a][b].[a][b]f32 =
  matmul32.matmul xss yss
