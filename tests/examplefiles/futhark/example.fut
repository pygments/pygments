module matmul (F: numeric) = {
  type t = F.t
  open F

  local let dotprod [n] (a: [n]t) (b: [n]t): t =
  map2 (*) a b |> reduce (+) (i32 0)


  let matmul [n1][n2][m] (xss: [n1][m]t) (yss: [m][n2]t): [n1][n2]t =
    map (\xs -> map (\ys -> dotprod xs ys) <| transpose yss) xss
}

module matmul32 = matmul f32

let main [n][m] (xss: [n][m]f32) (yss: [m][n]f32): *[n][n]f32 =
  matmul32.matmul xss yss
