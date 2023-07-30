
predict.ipp.point <- function(ipp.raster, selected.point) {
  raster::extract(ipp.raster, selected.point)
}

predict.maxent.point <- function(model, raster.vals) {
  dismo::predict(model, x=raster.vals)
}

predict.point <- function(model, raster.vals) {
  predict(model, newdata = raster.vals, type = "prob")
}
