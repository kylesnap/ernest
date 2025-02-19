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
  parent = ErnestLRPS
)

#' @name format
#' @export
S7::method(format, UniformCube) <-
  function(x, digits = max(3L, getOption("digits") - 3L), ...) {
    cli::cli_format_method({
      cli::cli_dl(c("Method" = "Uniform Hypercube", sampler_info))
    })
  }
