read_and_save_ED2.2 = function(there,place,yeara,yearz,
                                         ED2srcdir = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2.2/ED2/R-utils")
  {

  here           = getwd()    # Current directory.

  yearbeg        = as.numeric(substring(yeara,1,4))    # First year to consider
  yearend        = as.numeric(substring(yearz,1,4))
  monthbeg       = as.numeric(substring(yeara,6,7))
  monthend       = as.numeric(substring(yearz,6,7))

  ntimes = floor(yearend-yearbeg)*12+(monthend-monthbeg)

  slz.min        = -30.0

  sasmonth_read  = seq(1:12)
  sasmonth       = c(2,5,8,11)

  emean.line     = TRUE
  srcdir <<- ED2srcdir

  source(file.path(srcdir,"load.everything.r"))

  thispoi = locations(where=place,here=there,yearbeg=yearbeg,yearend=yearend,monthbeg=monthbeg)
  inpref = file.path(there,place)

  datum      = create.monthly( ntimes  = ntimes,
                               montha  = monthbeg,
                               yeara   = yearbeg,
                               inpref  = inpref,
                               slz.min = slz.min)

  datum = read.q.files(datum=datum,ntimes=ntimes,tresume=1,sasmonth=sasmonth_read)
  ed22.rdata = file.path(there,paste0(place,".RData"))
  cat(" + Saving data to ",basename(ed22.rdata),"...","\n")
  save(datum,file=ed22.rdata)

}
