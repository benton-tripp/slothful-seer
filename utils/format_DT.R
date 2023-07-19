format.datatable <- function(dt, signif=NULL, searching=T, 
                             font.size="20px", page.length=10, 
                             paging=F) {
  out <- datatable(
    dt,
    filter='none',
    selection='none',
    rownames=F,
    options=list(
      paging=paging,
      searching=searching,
      orderMulti=T,
      info=F,
      lengthChange = F,
      pageLength=page.length
    )
  ) %>%
    formatStyle(columns=names(dt), `font-size`=font.size)
  if (!is.null(signif)) out <- out %>% 
      formatSignif(columns=names(dt)[!(names(dt) %in% c("variable"))], 
                   digits=signif)
  out
}