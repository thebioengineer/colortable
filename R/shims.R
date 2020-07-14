register_shim <- function(shim,base,class,envir = getNamespace("colortable")){
  registerS3method(
    genname = base,
    class = class,
    method = shim,
    envir = envir
    )
}

colortable_shims <- function() {
  register_shim(
    "print.data.frame_shim",
    "print",
    "data.frame")
  register_shim(
    "knit_print.data.frame_shim",
    "knit_print",
    "data.frame")
}
