import Data.Ord

catenary_length :: Float -> Float -> Float
catenary_length a width = 2 * a * (sinh (width / (2 * a)))

make_catenary_error_function :: Float -> Float -> (Float -> Float)
make_catenary_error_function width length =
  \a -> catenary_length a width - length

get_a_lower_bound :: (Float -> Float) -> Float -> Float
get_a_lower_bound f a_0 =
  case f a_0 < 0 of
    True -> a_0
    False -> get_a_lower_bound f (a_0 * 2)

get_an_upper_bound :: (Float -> Float) -> Float -> Float
get_an_upper_bound f a_0 =
  case f a_0 > 0 of
    True -> a_0
    False -> get_an_upper_bound f (a_0 * 0.5)

secant_method :: (Float -> Float) -> Float -> Float -> Float -> Float
secant_method err_f lower upper max_err =
  let curr_midpoint = (lower + upper) * 0.5
      error = err_f curr_midpoint
  in
  case abs error < max_err of
    True -> curr_midpoint
    False ->
      case error < 0 of
        True -> secant_method err_f curr_midpoint upper max_err
        False -> secant_method err_f lower curr_midpoint max_err

start_secant_method :: (Float -> Float) -> Float -> Float -> Float
start_secant_method err_f a_init max_err =
  case compare (err_f a_init) 0 of
    EQ -> a_init
    LT ->
      let upper = get_an_upper_bound err_f a_init
          lower = upper * 2
      in
        secant_method err_f lower upper max_err

    GT ->
      let lower = get_a_lower_bound err_f a_init
          upper = lower * 0.5
      in
        secant_method err_f lower upper max_err

make_catenary_constant :: Float -> Float -> Float
make_catenary_constant width length =
  let err_f = make_catenary_error_function width length
  in
    -- note that a_init and max_err must be greater than zero
    start_secant_method err_f 16.0 0.001

points_on_catenary :: Float -> Int -> Float -> [(Float, Float)]
points_on_catenary a num_points width =
  let step = width / (fromIntegral (num_points - 1))
      start = -width / 2
      indices = [0..(num_points-1)]
      catenary a x = a * cosh(x / a)
      x_coords = map (\i -> start + (fromIntegral i) * step) indices
      y_coords' = map (catenary a) x_coords
      min_point = catenary a 0
      y_coords = map (\y -> y - min_point) y_coords'
  in
    zip x_coords y_coords

length_extension :: Float
length_extension = 5.0 / 16.0 * 2

points_for_catenary :: Float -> Float -> Int -> (Float, Float, [(Float, Float)])
points_for_catenary width length n =
  let a = make_catenary_constant width length
      points = points_on_catenary a n width
      length' = catenary_length a (width + length_extension)
  in
    (a, length', points)
