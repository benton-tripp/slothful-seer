mask.pred <- function(p.r, r) {
  crs(p.r) <- crs(r)
  crop(p.r, extent(r)) %>%
    resample(., r, method="bilinear") %>%
    mask(., r)
}
