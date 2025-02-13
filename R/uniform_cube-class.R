#' Uniform Cube Sampling
#'
#' @description This class defines a Sampler for generating points uniformly within a cube.
#'
#' @details The `UniformCube` class inherits from the `Sampler` class and
#' provides functionality to sample points uniformly within a cube.
#'
#' @examples
#' # Create a new UniformCube Sampler
#' Sampler <- UniformCube()
#'
#' @include sampler.R
#' @export
UniformCube <- S7::new_class(
  "UniformCube",
  parent = Sampler
)
