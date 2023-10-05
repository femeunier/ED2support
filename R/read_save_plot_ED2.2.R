read_and_plot_ED2_Q2R = function(there,place,yeara,yearz,ED2srcdir = "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/ED2.2/ED2/R-utils"){
  
  here           = getwd()    # Current directory.
  
  #yeara='2004/12/01'
  #yearz='2005/01/31'
  
  yearbeg        = as.numeric(substring(yeara,1,4))    # First year to consider
  yearend        = as.numeric(substring(yearz,1,4))
  monthbeg       = as.numeric(substring(yeara,6,7))
  monthend       = as.numeric(substring(yearz,6,7))
  
  ntimes = floor(yearend-yearbeg)*12+(monthend-monthbeg)
  #ntimes=ntimes-ntimes%%12
 
  slz.min        = -30.0 
  
  sasmonth_read  = seq(1:12)
  sasmonth       = c(2,5,8,11)

  emean.line     = TRUE 
  srcdir <<- ED2srcdir
 
  source(file.path(srcdir,"load.everything.r"))
  thispoi = locations(where=place,here=there,yearbeg=yearbeg,yearend=yearend
                      ,monthbeg=monthbeg)
  inpref = file.path(there,place)
  
  datum      = create.monthly( ntimes  = ntimes
                               , montha  = monthbeg
                               , yeara   = yearbeg
                               , inpref  = inpref
                               , slz.min = slz.min) 
  datum = read.q.files(datum=datum,ntimes=ntimes,tresume=1,sasmonth=sasmonth_read)
  ed22.rdata = file.path(there,paste0(place,".RData"))
  cat(" + Saving data to ",basename(ed22.rdata),"...","\n")
  save(datum,file=ed22.rdata)
  
  outroot=file.path(there,'Figures')

  if (! file.exists(outroot)) dir.create(outroot)

  n.density      = 256            # Number of density points
  idbh.type           = 1             # Type of DBH class
                                    # 1 -- Every 10 cm until 100cm; > 100cm
                                    # 2 -- 0-10; 10-20; 20-35; 35-50; 50-70; > 70 (cm)
                                    # 3 -- 0-10; 10-35; 35-55; > 55 (cm)
klight              = 1             # Weighting factor for maximum carbon balance
corr.growth.storage = 1             # Correction factor to be applied to growth and storage respiration

#----- Plot options. ----------------------------------------------------------------------#
outform        = "pdf"                  # Formats for output file.  Supported formats are:
depth          = 96                     # PNG resolution, in pixels per inch
paper          = "a4"                   # Paper size, to define the plot shape
ptsz           = 17                     # Font size.
lwidth         = 2.5                    # Line width
plotgrid       = TRUE                   # Should I plot the grid in the background? 
sasfixlimits   = FALSE                  # Use a fixed scale for size and age-structure
fcgrid         = TRUE                   # Include a grid on the filled contour plots?
ncolsfc        = 80                     # Target number of colours for filled contour.
hovgrid        = TRUE                   # Include a grid on the Hovmoller plots?
legwhere       = "topleft"              # Where should I place the legend?
inset          = 0.01                   # Inset between legend and edge of plot region.
scalleg        = 0.40                   # Expand y limits by this relative amount to fit the legend
cex.main       = 0.8                    # Scale coefficient for the title
theta          = 315.                   # Azimuth for perspective projection
phi            = 30.                    # Vertical angle for perspective projection
ltheta         = -210.                  # Azimuth angle for light
shade          = 0.125                  # Shade intensity
expz           = 0.5                    # Expansion factor for Z axis
cexmin         = 0.5                    # Minimum "head" size of the lollipop
cexmax         = 3.0                    # Maximum "head" size of the lollipop
ylnudge         = 0.05                  # Nudging factor for ylimit
ptype          = "l"                    # Type of plot
ptyped         = "p"                    # Type of plot
ptypeb         = "o"                    # Type of plot
drought.mark   = FALSE                  # Put a background to highlight droughts?
drought.yeara  = 2004                   # First year that has drought
drought.yearz  = 2004                   # Last year that has drought
months.drought = 1                      # Months with drought
ibackground    = 0                      # Background settings (check load_everything.r)

#==========================================================================================#


#thispoi = locations(where=place,here=there,yearbeg=yearbeg,monthbeg=monthbeg)
inpref = file.path(there,place)
outmain = paste(outroot,place,sep="/")
outpref = paste(outmain,"monthly",sep="/")
lieu    = thispoi$lieu
iata    = thispoi$iata
  
if (is.na(thispoi$iata)){
  suffix  = place
} else {
  suffix  = thispoi$iata
}

yeara   = thispoi$yeara
yearz   = thispoi$yearz
meszz   = thispoi$monz

if (! file.exists(outmain)) dir.create(outmain)
if (! file.exists(outpref)) dir.create(outpref)

#============================================================================#
# Read model outputs

#datum      = create.monthly( ntimes  = ntimes
#                             , montha  = monthbeg
#                             , yeara   = yearbeg
#                             , inpref  = inpref
#                             , slz.min = slz.min) 

#datum = read.q.files(datum=datum,ntimes=ntimes,tresume=1,sasmonth=sasmonth_read)

#============================================================================#
#----- Convert model outputs
#----- Copy some dimensions to scalars. -------------------------------------#

nzg        = datum$nzg
nzs        = datum$nzs
ndcycle    = datum$ndcycle
isoilflg   = datum$isoilflg
slz        = datum$slz
slxsand    = datum$slxsand
slxclay    = datum$slxclay
ntext      = datum$ntext
soil.prop  = datum$soil.prop
dslz       = datum$dslz
soil.depth = datum$soil.depth
soil.dry   = datum$soil.dry
soil.poro  = datum$soil.poro
ka         = datum$ka
kz         = datum$kz

outform = tolower(outform)
nout    = length (outform)

#---------------------------------------------------------------------------------------#
#      Define a suitable scale for those time series that uses datum$tomonth...         #
#---------------------------------------------------------------------------------------#
whenplot6 = pretty.time(datum$tomonth,n=6)
whenplot8 = pretty.time(datum$tomonth,n=8)
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------#
#      Define a suitable scale for diurnal cycle...                                     #
#---------------------------------------------------------------------------------------#
thisday = seq(from=0,to=ndcycle,by=1) * 24 / rep(ndcycle,ndcycle+1)
uplot = list()
uplot$levels = c(0,4,8,12,16,20,24)
uplot$n      = 7
uplot$scale  = "hours"
uplot$padj   = rep(0,times=uplot$n)
#---------------------------------------------------------------------------------------#

size = plotsize(proje=FALSE,paper=paper)

#---------------------------------------------------------------------------------------#
#      Define a suitable scale for soil profile layers...                               #
#---------------------------------------------------------------------------------------#
znice  = -pretty.log(-slz,n=8)
znice  = sort(c(znice,slz[1],slz[nzg]))
sel    = znice >= slz[1] & znice <= slz[nzg]
znice  = znice[sel]
zat    = -log(-znice)
nznice = length(znice)
znice  = sprintf("%.2f",znice)
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
#      Define a suitable scale for monthly means...                                     #
#---------------------------------------------------------------------------------------#
montmont  = seq(from=1,to=12,by=1)
mplot        = list()
mplot$levels = montmont
mplot$labels = capwords(mon2mmm(montmont))
mplot$n      = 12
mplot$scale  = "months"
mplot$padj   = rep(0,times=mplot$n)
#---------------------------------------------------------------------------------------#




#----- Make some shorter versions of some variables. -----------------------------------#
mfac   = datum$month
emean  = datum$emean
emsqu  = datum$emsqu
qmean  = datum$qmean
qmsqu  = datum$qmsqu
szpft  = datum$szpft
lu     = datum$lu
patch  = datum$patch
cohort = datum$cohort
#---------------------------------------------------------------------------------------#

#---------------------------------------------------------------------------------------#
#     Find the mean and standard deviation.  For variables for which we did not track   #
# the mean sum of squares, the standard deviation is just the standard deviation of the #
# means, otherwise we convert the mean sum of squares:                                  #
#              ____________________          _____________________________________      #
#             / SUM_i[X_i - Xm]^2           /  / SUM_i[X_i^2]         \      1          #
# sigma = \  /  ------------------   =  \  /  |  ------------  - Xm^2 | ---------       #
#          \/       N - 1                \/    \      N               /   1 - 1/N       #
#                                                                                       #
# srnonm1 is the square root of 1 / (1 - 1/N)                                           #
#     Find the standard deviation.                                                      #
#---------------------------------------------------------------------------------------#
cat ("    - Finding the monthly means...","\n")

srnorm1 = sqrt(1./(1. - 1. / datum$montable))
srnorm1[!is.finite(srnorm1)] = 0.

mmean = list()
msdev = list()
for (vname in names(emean)){
  if (vname %in% names(emsqu)){
    has.emsqu = any(is.finite(emsqu[[vname]]))
  }else{
    has.emsqu = FALSE
  }#end if
  
  #------------------------------------------------------------------------------------#
  #     Soil variables are multi-dimensional.  Use qapply.  Otherwise, check whether   #
  # the mean sum of squares is available or not.                                       #
  #------------------------------------------------------------------------------------#
  if (vname %in% c("soil.temp","soil.water","soil.mstpot","soil.extracted")){
    mmean[[vname]] = qapply(X=emean[[vname]], INDEX=mfac, DIM=1, FUN=mean, na.rm=TRUE)
    msdev[[vname]] = qapply(X=emean[[vname]], INDEX=mfac, DIM=1, FUN=sd  , na.rm=TRUE)
  }else if (has.emsqu){
    mmean[[vname]] = tapply(X=emean[[vname]], INDEX=mfac, FUN=mean, na.rm=TRUE)
    mmsqu          = tapply(X=emsqu[[vname]], INDEX=mfac, FUN=mean, na.rm=TRUE)
    msdev[[vname]] = sqrt  ( mmsqu - mmean[[vname]]^ 2 ) * srnorm1
  }else{
    mmean[[vname]] = tapply(X=emean[[vname]], INDEX=mfac, FUN=mean, na.rm=TRUE)
    msdev[[vname]] = tapply(X=emean[[vname]], INDEX=mfac, FUN=sd  , na.rm=TRUE)
  }#end if
  #------------------------------------------------------------------------------------#
  
  
  #----- Fix the bad data. ------------------------------------------------------------#
  bad.mmean = ! is.finite(mmean[[vname]])
  bad.msdev = ! is.finite(msdev[[vname]])
  mmean[[vname]][bad.mmean] = NA
  msdev[[vname]][bad.msdev] = 0.
  #------------------------------------------------------------------------------------#
}#end for
#---------------------------------------------------------------------------------------#




#---------------------------------------------------------------------------------------#
#      Here we find the Mean diurnal cycle for each month, then compute the standard    #
# deviation, similar to the monthly mean.                                               #
#---------------------------------------------------------------------------------------#
cat ("    - Aggregating the monthly mean of the diurnal cycle...","\n")
umean              = list()
usdev              = list()
for (vname in names(qmean)){
  
  #------------------------------------------------------------------------------------#
  #     Soil variables are multi-dimensional.  Use qapply.  Otherwise, check whether   #
  # the mean sum of squares is available or not.                                       #
  #------------------------------------------------------------------------------------#
  if (vname %in% names(qmsqu)){
    umean[[vname]] = qapply(X=qmean[[vname]], INDEX=mfac, DIM=1,FUN=mean, na.rm=TRUE)
    umsqu          = qapply(X=qmsqu[[vname]], INDEX=mfac, DIM=1,FUN=mean, na.rm=TRUE)
    usdev[[vname]] = sqrt  ( umsqu - umean[[vname]]^ 2 ) * srnorm1
  }else{
    umean[[vname]] = qapply(X=qmean[[vname]], INDEX=mfac, DIM=1,FUN=mean, na.rm=TRUE)
    usdev[[vname]] = qapply(X=qmean[[vname]], INDEX=mfac, DIM=1,FUN=sd  , na.rm=TRUE)
  }#end if
  #------------------------------------------------------------------------------------#
  
  
  #----- Fix the bad data. ------------------------------------------------------------#
  bad.umean = ! is.finite(umean[[vname]])
  bad.usdev = ! is.finite(usdev[[vname]])
  umean[[vname]][bad.umean] = NA
  usdev[[vname]][bad.usdev] = 0.
  #------------------------------------------------------------------------------------#
}#end for
#---------------------------------------------------------------------------------------#





#---------------------------------------------------------------------------------------#
#      Estimate NPP and NEE standard deviation.                                         #
#---------------------------------------------------------------------------------------#
usdev$nee  = sqrt(usdev$cflxca^2     + usdev$cflxst^2                        )
usdev$reco = sqrt(usdev$plant.resp^2 + usdev$het.resp^2                      )
usdev$evap = sqrt(usdev$wflxgc^2     + usdev$wflxlc^2    + usdev$wflxwc^2    )
#---------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------#
#     Remove all elements of the DBH/PFT class that do not have a single valid cohort   #
# at any given time.                                                                    #
#---------------------------------------------------------------------------------------#
empty = is.na(szpft$nplant) | szpft$nplant == 0
for (vname in names(szpft)) szpft[[vname]][empty] = NA
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
#     Replace the mortality and recruitment exponential rates by the "interests" rates. #
#---------------------------------------------------------------------------------------#
szpft$mort          = 100. * (1.0 - exp(- szpft$mort         )      )
szpft$dimort        = 100. * (1.0 - exp(- szpft$dimort       )      )
szpft$ncbmort       = 100. * (1.0 - exp(- szpft$ncbmort      )      )
szpft$recrpft       = 100. * (      exp(  szpft$recr         ) - 1.0)
szpft$agb.mort      = 100. * (1.0 - exp(- szpft$agb.mort     )      )
szpft$agb.dimort    = 100. * (1.0 - exp(- szpft$agb.dimort   )      )
szpft$agb.ncbmort   = 100. * (1.0 - exp(- szpft$agb.ncbmort  )      )
szpft$agb.recrpft   = 100. * (      exp(  szpft$agb.recr     ) - 1.0)
szpft$bsa.mort      = 100. * (1.0 - exp(- szpft$bsa.mort     )      )
szpft$bsa.dimort    = 100. * (1.0 - exp(- szpft$bsa.dimort   )      )
szpft$bsa.ncbmort   = 100. * (1.0 - exp(- szpft$bsa.ncbmort  )      )
szpft$bsa.recrpft   = 100. * (      exp(  szpft$bsa.recr     ) - 1.0)
#---------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------#
#      Find the patch density function for all patch-level data.                        #
#---------------------------------------------------------------------------------------#
cat ("    - Finding the distribution function of patch properties...","\n")
patchpdf = list()
for (pp in 1:nplotpatch){
  this        = plotpatch[[pp]]
  vname       = this$vnam
  col.scheme  = get(this$col.scheme)(n=ncolsfc)
  emean.area  = patch$area
  emean.vname = patch[[vname]]
  mmean.area  = tapply(X=emean.area ,INDEX=mfac,FUN=unlist)
  mmean.vname = tapply(X=emean.vname,INDEX=mfac,FUN=unlist)
  
  #----- Find the range for which we find the density function. -----------------------#
  low.vname   = min(unlist(emean.vname),na.rm=TRUE)
  high.vname  = max(unlist(emean.vname),na.rm=TRUE)
  #------------------------------------------------------------------------------------#
  
  
  
  #----- Find the density function for each time. -------------------------------------#
  edfun.now   = mapply( FUN      = density.safe
                        , x        = emean.vname
                        , weights  = emean.area
                        , MoreArgs = list(n=n.density,from=low.vname,to=high.vname)
  )#end mapply
  mdfun.now   = mapply( FUN      = density.safe
                        , x        = mmean.vname
                        , weights  = mmean.area
                        , MoreArgs = list(n=n.density,from=low.vname,to=high.vname)
  )#end mapply
  #------------------------------------------------------------------------------------#
  
  
  
  
  #----- Save the density function. ---------------------------------------------------#
  edfun        = list()
  edfun$x      = chron(datum$when)
  edfun$y      = seq(from=low.vname,to=high.vname,length.out=n.density)
  edfun$z      = t(sapply(X=edfun.now["y",],FUN=cbind))
  #------------------------------------------------------------------------------------#
  
  
  
  
  #----- Save the density function. ---------------------------------------------------#
  mdfun        = list()
  mdfun$x      = montmont
  mdfun$y      = seq(from=low.vname,to=high.vname,length.out=n.density)
  mdfun$z      = t(sapply(X=mdfun.now["y",],FUN=cbind))
  #------------------------------------------------------------------------------------#
  
  
  
  #----- Remove tiny values (even with log scale values can be very hard to see. ------#
  bye         = is.finite(edfun$z) & edfun$z < 1.e-6 * max(unlist(edfun$z),na.rm=TRUE)
  edfun$z[bye] = NA
  #------------------------------------------------------------------------------------#
  
  
  #----- Remove tiny values (even with log scale values can be very hard to see. ------#
  bye         = is.finite(mdfun$z) & mdfun$z < 1.e-6 * max(unlist(mdfun$z),na.rm=TRUE)
  mdfun$z[bye] = NA
  #------------------------------------------------------------------------------------#
  patchpdf[[vname]] = list(edensity=edfun,mdensity=mdfun)
}#end for 
#---------------------------------------------------------------------------------------#




#----- Find which PFTs, land uses and transitions we need to consider ------------------#
pftave  = apply( X      = szpft$agb[,ndbh+1,]
                 , MARGIN = 2
                 , FUN    = mean
                 , na.rm  = TRUE
)#end apply
luave   = apply( X      = lu$agb 
                 , MARGIN = 2
                 , FUN    = mean
                 , na.rm  = TRUE
)#end apply
distave = apply(X=lu$dist,MARGIN=c(2,3),FUN=mean)
selpft  = is.finite(pftave ) & pftave  > 0.
sellu   = is.finite(luave  ) & luave   > 0.
seldist = is.finite(distave) & distave > 0.
n.selpft  = sum(selpft )
n.sellu   = sum(sellu  )
n.seldist = sum(seldist)
#---------------------------------------------------------------------------------------#



#=======================================================================================#
#=======================================================================================#
#=======================================================================================#
#      Plotting section begins here...                                                  #
#---------------------------------------------------------------------------------------#
cat ("    - Plotting figures...","\n")
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
#      Time series by PFT.                                                              #
#---------------------------------------------------------------------------------------#
for (v in 1:ntspftdbh){
  thistspft   = tspftdbh[[v]]
  vnam        = thistspft$vnam
  description = thistspft$desc
  unit        = thistspft$e.unit
  plog        = thistspft$plog
  plotit      = thistspft$pft
  
  #----- Check whether the user wants to have this variable plotted. ------------------#
  if (plotit && any(selpft)){
    
    #---------------------------------------------------------------------------------#
    #    Check whether the time series directory exists.  If not, create it.          #
    #---------------------------------------------------------------------------------#
    outdir = paste(outpref,"tspft",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + ",description," time series for all PFTs...","\n")
    
    #----- Load variable -------------------------------------------------------------#
    thisvar = szpft[[vnam]][,ndbh+1,]
    if (plog){
      #----- Eliminate non-positive values in case it is a log plot. ----------------#
      thisvar[thisvar <= 0] = NA
    }#end if
    #---------------------------------------------------------------------------------#
    
    
    
    #----- Loop over output formats. -------------------------------------------------#
    for (o in 1:nout){
      #----- Open file. -------------------------------------------------------------#
      fichier = paste(outdir,"/",vnam,"-",suffix,".",outform[o],sep="")
      if(outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      #------------------------------------------------------------------------------#
      
      
      #------------------------------------------------------------------------------#
      #     Find the limit, make some room for the legend, and in case the field is  #
      # a constant, nudge the limits so the plot command will not complain.          #
      #------------------------------------------------------------------------------#
      xlimit = pretty.xylim(u=as.numeric(datum$tomonth),fracexp=0.0,is.log=FALSE)
      ylimit = pretty.xylim(u=thisvar[,selpft]         ,fracexp=0.0,is.log=plog )
      if (plog){
        xylog    = "y"
        ydrought = c( exp(ylimit[1] * sqrt(ylimit[1]/ylimit[2]))
                      , exp(ylimit[2] * sqrt(ylimit[2]/ylimit[1]))
        )#end c
      }else{
        xylog    = ""
        ydrought = c(ylimit[1] - 0.5 * diff(ylimit), ylimit[2] + 0.5 * diff(ylimit))
      }#end if
      #------------------------------------------------------------------------------#
      
      
      #----- Plot settings. ---------------------------------------------------------#
      letitre       = paste(description,lieu,sep=" - ")
      ley           = desc.unit(desc=description,unit=unit)
      cols          = pft$colour[selpft]
      legs          = pft$name  [selpft]
      #------------------------------------------------------------------------------#
      
      
      #------------------------------------------------------------------------------#
      #     Split the plot into two windows.                                         #
      #------------------------------------------------------------------------------#
      par(par.user)
      layout(mat=rbind(2,1),heights=c(5,1))
      #------------------------------------------------------------------------------#
      
      
      
      #------------------------------------------------------------------------------#
      #      First plot: legend.                                                     #
      #------------------------------------------------------------------------------#
      par(mar=c(0.1,4.6,0.,2.1))
      plot.new()
      plot.window(xlim=c(0,1),ylim=c(0,1))
      legend( x      = "bottom"
              , inset  = 0.0
              , legend = legs
              , col    = cols
              , lwd    = lwidth
              , ncol   = min(pretty.box(n.selpft)$ncol,3)
              , title  = expression(bold("Plant Functional Type"))
      )#end legend
      #------------------------------------------------------------------------------#
      
      #------------------------------------------------------------------------------#
      #      Main plot.                                                              #
      #------------------------------------------------------------------------------#
      par(mar=c(4.6,4.6,4.1,2.1))
      plot.new()
      plot.window(xlim=xlimit,ylim=ylimit,log=xylog)
      axis(side=1,at=whenplot8$levels,labels=whenplot8$labels,padj=whenplot8$padj)
      axis(side=2,las=1)
      box()
      title(main=letitre,xlab="Year",ylab=ley,cex.main=0.7)
      if (drought.mark){
        for (n in 1:ndrought){
          rect(xleft  = drought[[n]][1],ybottom = ydrought[1]
               ,xright = drought[[n]][2],ytop    = ydrought[2]
               ,col    = grid.colour,border=NA)
        }#end for
      }#end if
      #----- Plot grid. -------------------------------------------------------------#
      if (plotgrid){ 
        abline(v=whenplot8$levels,h=axTicks(side=2),col=grid.colour,lty="solid")
      }#end if
      #----- Plot lines. ------------------------------------------------------------#
      for (n in 1:(npft+1)){
        if (selpft[n]){
          lines(datum$tomonth,thisvar[,n],type="l",col=pft$colour[n],lwd=lwidth)
        }#end if
      }#end for
      #------------------------------------------------------------------------------#
      
      
      #----- Close the device. ------------------------------------------------------#
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy=clean.tmp()
      #------------------------------------------------------------------------------#
    } #end for outform
  }#end if (tseragbpft)
} #end for tseries
#---------------------------------------------------------------------------------------#


#---------------------------------------------------------------------------------------#
#      Time series by DBH, by PFT.                                                      #
#---------------------------------------------------------------------------------------#
#----- Find the PFTs to plot. ----------------------------------------------------------#
pftuse  = which(apply(X=szpft$nplant,MARGIN=3,FUN=sum,na.rm=TRUE) > 0)
pftuse  = pftuse[pftuse != (npft+1)]
for (v in 1:ntspftdbh){
  thistspftdbh   = tspftdbh[[v]]
  vnam           = thistspftdbh$vnam
  description    = thistspftdbh$desc
  unit           = thistspftdbh$e.unit
  plog           = thistspftdbh$plog
  plotit         = thistspftdbh$pftdbh
  
  #----- Load variable ----------------------------------------------------------------#
  thisvar = szpft[[vnam]]
  if (plog){
    xylog="y"
    badlog = is.finite(thisvar) & thisvar <= 0
    thisvar[badlog] = NA
  }else{
    xylog=""
  }#end if
  #----- Check whether the user wants to have this variable plotted. ------------------#
  if (plotit && length(pftuse) > 0 && any(is.finite(thisvar))){
    
    #---------------------------------------------------------------------------------#
    #    Check whether the time series directory exists.  If not, create it.          #
    #---------------------------------------------------------------------------------#
    outdir = paste(outpref,"tsdbh",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    outvar = paste(outdir,vnam,sep="/")
    if (! file.exists(outvar)) dir.create(outvar)
    #---------------------------------------------------------------------------------#
    
    cat("      + ",description," time series for DBH class...","\n")
    
    
    #---------------------------------------------------------------------------------#
    #     Find the limit, make some room for the legend, and in case the field is a   #
    # constant, nudge the limits so the plot command will not complain.               #
    #---------------------------------------------------------------------------------#
    xlimit = pretty.xylim(u=as.numeric(datum$tomonth),fracexp=0.0,is.log=FALSE)
    ylimit = pretty.xylim(u=thisvar[,,pftuse]        ,fracexp=0.0,is.log=plog )
    if (plog){
      xylog    = "y"
      ydrought = c( exp(ylimit[1] * sqrt(ylimit[1]/ylimit[2]))
                    , exp(ylimit[2] * sqrt(ylimit[2]/ylimit[1]))
      )#end c
    }else{
      xylog    = ""
      ydrought = c(ylimit[1] - 0.5 * diff(ylimit), ylimit[2] + 0.5 * diff(ylimit))
    }#end if
    #---------------------------------------------------------------------------------#
    
    
    
    
    #---------------------------------------------------------------------------------#
    #       Loop over plant functional types.                                         #
    #---------------------------------------------------------------------------------#
    for (p in pftuse){
      pftlab = paste("pft-",sprintf("%2.2i",p),sep="")
      
      cat("        - ",pft$name[p],"\n")
      
      
      #----- Loop over output formats. ----------------------------------------------#
      for (o in 1:nout){
        #----- Open file. ----------------------------------------------------------#
        fichier = paste(outvar,"/",vnam,"-",pftlab,"-",suffix,".",outform[o],sep="")
        if(outform[o] == "x11"){
          X11(width=size$width,height=size$height,pointsize=ptsz)
        }else if(outform[o] == "png"){
          png(filename=fichier,width=size$width*depth,height=size$height*depth
              ,pointsize=ptsz,res=depth)
        }else if(outform[o] == "eps"){
          postscript(file=fichier,width=size$width,height=size$height
                     ,pointsize=ptsz,paper=size$paper)
        }else if(outform[o] == "pdf"){
          pdf(file=fichier,onefile=FALSE
              ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
        }#end if
        #---------------------------------------------------------------------------#
        
        
        
        #-----  Plot annotation. ---------------------------------------------------#
        letitre = paste(description,pft$name[p],lieu,sep=" - ")
        ley     = desc.unit(desc=description,unit=unit)
        #---------------------------------------------------------------------------#
        
        
        #---------------------------------------------------------------------------#
        #     Split the plot into two windows.                                      #
        #---------------------------------------------------------------------------#
        par(par.user)
        layout(mat=rbind(2,1),heights=c(5,1))
        #---------------------------------------------------------------------------#
        
        
        
        #---------------------------------------------------------------------------#
        #      First plot: legend.                                                  #
        #---------------------------------------------------------------------------#
        par(mar=c(0.1,4.6,0.1,2.1))
        plot.new()
        plot.window(xlim=c(0,1),ylim=c(0,1))
        legend( x      = "bottom"
                , inset  = 0.0
                , bg     = background
                , legend = dbhnames
                , col    = dbhcols
                , ncol   = min(pretty.box(ndbh+1)$ncol,3)
                , title  = expression(bold("DBH class"))
                , lwd    = lwidth
        )#end legend
        #---------------------------------------------------------------------------#
        
        
        
        #---------------------------------------------------------------------------#
        #      Main plot.                                                           #
        #---------------------------------------------------------------------------#
        par(mar=c(4.6,4.6,4.1,2.1))
        plot.new()
        plot.window(xlim=xlimit,ylim=ylimit,log=xylog)
        axis(side=1,at=whenplot8$levels,labels=whenplot8$labels,padj=whenplot8$padj)
        axis(side=2,las=1)
        box()
        title(main=letitre,xlab="Year",ylab=ley,cex.main=0.7)
        if (drought.mark){
          for (n in 1:ndrought){
            rect(xleft  = drought[[n]][1],ybottom = ydrought[1]
                 ,xright = drought[[n]][2],ytop    = ydrought[2]
                 ,col    = grid.colour,border=NA)
          }#end for
        }#end if
        #----- Plot grid. ----------------------------------------------------------#
        if (plotgrid){ 
          abline(v=whenplot8$levels,h=axTicks(side=2),col=grid.colour,lty="solid")
        }#end if
        #----- Plot lines. ---------------------------------------------------------#
        for (d in seq(from=1,to=ndbh+1,by=1)){
          lines(datum$tomonth,thisvar[,d,p],type="l",col=dbhcols[d],lwd=lwidth)
        }#end for
        #---------------------------------------------------------------------------#
        
        
        #----- Close the device. ---------------------------------------------------#
        if (outform[o] == "x11"){
          locator(n=1)
          dev.off()
        }else{
          dev.off()
        }#end if
        dummy=clean.tmp()
        #---------------------------------------------------------------------------#
      }#end for outform
      #------------------------------------------------------------------------------#
    }#end for (p in pftuse)
    #---------------------------------------------------------------------------------#
  }#end if (tseragbpft)
  #------------------------------------------------------------------------------------#
} #end for tseries
#---------------------------------------------------------------------------------------#



#---------------------------------------------------------------------------------------#
#   Plot the time series diagrams showing months and years.                             #
#---------------------------------------------------------------------------------------#
cat("      * Plot some time series with groups of variables...","\n")
for (hh in 1:ntheme){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  themenow     = theme[[hh]]
  vnames       = themenow$vnam  
  description  = themenow$desc  
  lcolours     = themenow$colour
  llwd         = themenow$lwd
  if (emean.line){
    ltype        = "l"
  }else{
    ltype        = themenow$type 
  }#end if
  plog         = themenow$plog
  prefix       = themenow$prefix
  group        = themenow$title
  unit         = themenow$unit  
  legpos       = themenow$legpos
  plotit       = themenow$emean
  ylimit.fix   = themenow$emean.lim
  
  if (plotit){
    
    #---------------------------------------------------------------------------------#
    #    Check whether the time series directory exists.  If not, create it.          #
    #---------------------------------------------------------------------------------#
    outdir = paste(outpref,"theme_emean",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + ",group," time series for several variables...","\n")
    
    
    #----- Define the number of layers. ----------------------------------------------#
    nlayers   = length(vnames)
    #---------------------------------------------------------------------------------#
    
    
    
    #---------------------------------------------------------------------------------#
    #     Find the limit, make some room for the legend, and in case the field is a   #
    # constant, nudge the limits so the plot command will not complain.               #
    #---------------------------------------------------------------------------------#
    xlimit   = pretty.xylim(u=as.numeric(datum$tomonth),fracexp=0.0,is.log=FALSE)
    if (any(! is.finite(ylimit.fix))){
      ylimit    = NULL
      for (l in 1:nlayers) ylimit  = c(ylimit,emean[[vnames[l]]])
      ylimit = pretty.xylim(u=ylimit,fracexp=0.0,is.log=plog)
    }else{
      ylimit = ylimit.fix
    }#end if
    if (plog){
      xylog    = "y"
      ydrought = c( exp(ylimit[1] * sqrt(ylimit[1]/ylimit[2]))
                    , exp(ylimit[2] * sqrt(ylimit[2]/ylimit[1]))
      )#end c
    }else{
      xylog    = ""
      ydrought = c(ylimit[1] - 0.5 * diff(ylimit), ylimit[2] + 0.5 * diff(ylimit))
    }#end if
    #---------------------------------------------------------------------------------#
    
    
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    
    #----- Loop over formats. --------------------------------------------------------#
    for (o in 1:nout){
      #------ Open file. ------------------------------------------------------------#
      fichier = paste(outdir,"/",prefix,"-",suffix,".",outform[o],sep="")
      if(outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      #------------------------------------------------------------------------------#
      
      
      
      #----- Plot settings. ---------------------------------------------------------#
      letitre = paste(" Time series: ",group,"\n",lieu,sep="")
      ley     = desc.unit(desc=description,unit=unit)
      #------------------------------------------------------------------------------#
      
      
      #------------------------------------------------------------------------------#
      #     Split the plot into two windows.                                         #
      #------------------------------------------------------------------------------#
      par(par.user)
      layout(mat=rbind(2,1),heights=c(5,1))
      #------------------------------------------------------------------------------#
      
      
      
      #------------------------------------------------------------------------------#
      #      First plot: legend.                                                     #
      #------------------------------------------------------------------------------#
      par(mar=c(0.1,4.6,0.1,2.1))
      plot.new()
      plot.window(xlim=c(0,1),ylim=c(0,1))
      legend( x      = "bottom"
              , inset  = 0.0
              , legend = description
              , col    = lcolours
              , lwd    = llwd
              , ncol   = min(3,pretty.box(nlayers)$ncol)
              , xpd    = TRUE
      )#end legend
      #------------------------------------------------------------------------------#
      
      
      
      #------------------------------------------------------------------------------#
      #      Main plot.                                                              #
      #------------------------------------------------------------------------------#
      par(mar=c(4.1,4.6,4.1,2.1))
      plot.new()
      plot.window(xlim=xlimit,ylim=ylimit,log=xylog)
      axis(side=1,at=whenplot8$levels,labels=whenplot8$labels,padj=whenplot8$padj)
      axis(side=2,las=1)
      box()
      title(main=letitre,xlab="Year",ylab=ley,cex.main=0.7)
      if (drought.mark){
        for (n in 1:ndrought){
          rect(xleft  = drought[[n]][1],ybottom = ydrought[1]
               ,xright = drought[[n]][2],ytop    = ydrought[2]
               ,col    = grid.colour,border=NA)
        }#end for
      }#end if
      #----- Plot grid. -------------------------------------------------------------#
      if (plotgrid){ 
        abline(v=whenplot8$levels,h=axTicks(side=2),col=grid.colour,lty="solid")
      }#end if
      #----- Plot lines. ------------------------------------------------------------#
      for (l in 1:nlayers){
        thisvar = emean[[vnames[l]]]
        points(x=datum$tomonth,y=thisvar,col=lcolours[l],lwd=llwd[l],type=ltype
               ,pch=16,cex=0.8)
      }#end for
      #------------------------------------------------------------------------------#
      
      
      #----- Close the device. ------------------------------------------------------#
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy=clean.tmp()
      #------------------------------------------------------------------------------#
    } #end for outform
    #---------------------------------------------------------------------------------#
  }#end if plotit
  #------------------------------------------------------------------------------------#
}#end for ntser
#---------------------------------------------------------------------------------------#




#---------------------------------------------------------------------------------------#
#   Plot the time series diagrams showing months and years.                             #
#---------------------------------------------------------------------------------------#
cat("      * Plot some monthly means of groups of variables ...","\n")
for (hh in 1:ntheme){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  themenow     = theme[[hh]]
  vnames       = themenow$vnam  
  description  = themenow$desc  
  lcolours     = themenow$colour
  llwd         = themenow$lwd
  ltype        = themenow$type
  plog         = themenow$plog
  prefix       = themenow$prefix
  group        = themenow$title
  unit         = themenow$unit  
  legpos       = themenow$legpos
  plotit       = themenow$mmean
  ylimit.fix   = themenow$mmean.lim
  
  if (plotit){
    
    #---------------------------------------------------------------------------------#
    #    Check whether the time series directory exists.  If not, create it.          #
    #---------------------------------------------------------------------------------#
    outdir = paste(outpref,"theme_mmean",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + ",group," time series for several variables...","\n")
    
    
    #----- Define the number of layers. ----------------------------------------------#
    nlayers   = length(vnames)
    #---------------------------------------------------------------------------------#
    
    
    
    #---------------------------------------------------------------------------------#
    #     Find the limit, make some room for the legend, and in case the field is a   #
    # constant, nudge the limits so the plot command will not complain.               #
    #---------------------------------------------------------------------------------#
    xlimit    = pretty.xylim(u=montmont,fracexp=0.0,is.log=plog)
    if (any (! is.finite(ylimit.fix))){
      ylimit    = NULL
      for (l in 1:nlayers) ylimit  = c(ylimit,mmean[[vnames[l]]])
      ylimit = pretty.xylim(u=ylimit,fracexp=0.0,is.log=plog)
    }else{
      ylimit = ylimit.fix
    }#end if
    if (plog){
      xylog    = "y"
      ydrought = c( exp(ylimit[1] * sqrt(ylimit[1]/ylimit[2]))
                    , exp(ylimit[2] * sqrt(ylimit[2]/ylimit[1]))
      )#end c
    }else{
      xylog    = ""
      ydrought = c(ylimit[1] - 0.5 * diff(ylimit), ylimit[2] + 0.5 * diff(ylimit))
    }#end if
    #---------------------------------------------------------------------------------#
    
    
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    
    #----- Loop over formats. --------------------------------------------------------#
    for (o in 1:nout){
      #------ Open file. ------------------------------------------------------------#
      fichier = paste(outdir,"/",prefix,"-",suffix,".",outform[o],sep="")
      if(outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      #------------------------------------------------------------------------------#
      
      
      
      #----- Plot settings. ---------------------------------------------------------#
      letitre = paste(" Time series: ",group,"\n",lieu,sep="")
      ley     = desc.unit(desc=description,unit=unit)
      #------------------------------------------------------------------------------#
      
      
      #------------------------------------------------------------------------------#
      #     Split the plot into two windows.                                         #
      #------------------------------------------------------------------------------#
      par(par.user)
      layout(mat=rbind(2,1),heights=c(5,1))
      #------------------------------------------------------------------------------#
      
      
      
      #------------------------------------------------------------------------------#
      #      First plot: legend.                                                     #
      #------------------------------------------------------------------------------#
      par(mar=c(0.1,4.6,0.1,2.1))
      plot.new()
      plot.window(xlim=c(0,1),ylim=c(0,1))
      legend( x      = "bottom"
              , inset  = 0.0
              , legend = description
              , col    = lcolours
              , lwd    = llwd
              , pch    = 16
              , ncol   = min(3,pretty.box(nlayers)$ncol)
              , cex    = 0.9*cex.ptsz
              , xpd    = TRUE
      )#end legend
      #------------------------------------------------------------------------------#
      
      
      
      #------------------------------------------------------------------------------#
      #      Main plot.                                                              #
      #------------------------------------------------------------------------------#
      par(mar=c(4.1,4.6,4.1,2.1))
      plot.new()
      plot.window(xlim=xlimit,ylim=ylimit,log=xylog)
      axis(side=1,at=mplot$levels,labels=substring(mplot$labels,1,1),padj=mplot$padj)
      axis(side=2,las=1)
      box()
      title(main=letitre,xlab="Year",ylab=ley,cex.main=0.7)
      if (drought.mark){
        for (n in 1:ndrought){
          rect(xleft  = drought[[n]][1],ybottom = ydrought[1]
               ,xright = drought[[n]][2],ytop    = ydrought[2]
               ,col    = grid.colour,border=NA)
        }#end for
      }#end if
      #----- Plot grid. -------------------------------------------------------------#
      if (plotgrid){ 
        abline(v=mplot$levels,h=axTicks(side=2),col=grid.colour,lty="solid")
      }#end if
      #----- Plot lines. ------------------------------------------------------------#
      for (l in 1:nlayers){
        thisvar = mmean[[vnames[l]]]
        points(x=montmont,y=thisvar,col=lcolours[l],lwd=llwd[l],type=ltype
               ,pch=16,cex=0.8)
      }#end for
      #------------------------------------------------------------------------------#
      
      
      #----- Close the device. ------------------------------------------------------#
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy=clean.tmp()
      #------------------------------------------------------------------------------#
    } #end for outform
    #---------------------------------------------------------------------------------#
  }#end if plotit
  #------------------------------------------------------------------------------------#
}#end for ntser
#---------------------------------------------------------------------------------------#




#---------------------------------------------------------------------------------------#
#   Plot the climatology of the mean diurnal cycle.                                     #
#---------------------------------------------------------------------------------------#
cat("      * Plot the mean diel of groups of variables...","\n")
for (hh in 1:ntheme){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  themenow     = theme[[hh]]
  vnames       = themenow$vnam  
  description  = themenow$desc  
  lcolours     = themenow$colour
  llwd         = themenow$lwd
  ltype        = themenow$type
  plog         = themenow$plog
  prefix       = themenow$prefix
  group        = themenow$title
  unit         = themenow$unit  
  legpos       = themenow$legpos
  plotit       = themenow$qmean 
  if (plog){ 
    xylog = "y"
  }else{
    xylog = ""
  }#end if
  
  
  if (plotit){
    
    #---------------------------------------------------------------------------------#
    #    Check whether the time series directory exists.  If not, create it.          #
    #---------------------------------------------------------------------------------#
    outdir   = paste(outpref,"theme_qmean",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    outtheme = paste(outdir,prefix,sep="/")
    if (! file.exists(outtheme)) dir.create(outtheme)
    cat("      + ",group," diurnal cycle for several variables...","\n")
    
    
    #----- Define the number of layers. ----------------------------------------------#
    nlayers   = length(vnames)
    xlimit    = range(thisday)
    ylimit    = NULL
    for (l in 1:nlayers) ylimit = c(ylimit,umean[[vnames[l]]])
    ylimit = pretty.xylim(u=ylimit,fracexp=0.0,is.log=FALSE)
    #---------------------------------------------------------------------------------#
    
    
    #---------------------------------------------------------------------------------#
    #      Loop over all months.                                                      #
    #---------------------------------------------------------------------------------#
    for (pmon in 1:12){
      cmon    = sprintf("%2.2i",pmon)
      namemon = mlist[pmon]
      
      #------------------------------------------------------------------------------#
      #     Check if the directory exists.  If not, create it.                       #
      #------------------------------------------------------------------------------#
      
      #----- Loop over formats. -----------------------------------------------------#
      for (o in 1:nout){
        #------ Open file. ---------------------------------------------------------#
        fichier = paste(outtheme,"/",prefix,"-",cmon,"-",suffix,".",outform[o]
                        ,sep="")
        if(outform[o] == "x11"){
          X11(width=size$width,height=size$height,pointsize=ptsz)
        }else if(outform[o] == "png"){
          png(filename=fichier,width=size$width*depth,height=size$height*depth
              ,pointsize=ptsz,res=depth)
        }else if(outform[o] == "eps"){
          postscript(file=fichier,width=size$width,height=size$height
                     ,pointsize=ptsz,paper=size$paper)
        }else if(outform[o] == "pdf"){
          pdf(file=fichier,onefile=FALSE
              ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
        }#end if
        #---------------------------------------------------------------------------#
        
        
        
        #----- Plot settings. ------------------------------------------------------#
        letitre = paste(group," - ",lieu,"\n"
                        ,"Mean diurnal cycle - ",namemon,sep="")
        ley     = desc.unit(desc=description,unit=unit)
        #---------------------------------------------------------------------------#
        
        
        #---------------------------------------------------------------------------#
        #     Split the plot into two windows.                                      #
        #---------------------------------------------------------------------------#
        par(par.user)
        layout(mat=rbind(2,1),heights=c(5,1))
        #------------------------------------------------------------------------------#
        
        
        
        #---------------------------------------------------------------------------#
        #      First plot: legend.                                                  #
        #---------------------------------------------------------------------------#
        par(mar=c(0.1,4.6,0.1,2.1))
        plot.new()
        plot.window(xlim=c(0,1),ylim=c(0,1))
        legend( x      = "bottom"
                , inset  = 0.0
                , legend = description
                , col    = lcolours
                , lwd    = llwd
                , ncol   = min(3,pretty.box(nlayers)$ncol)
                , xpd    = TRUE
        )#end legend
        #---------------------------------------------------------------------------#
        
        
        
        #---------------------------------------------------------------------------#
        #      Main plot.                                                           #
        #---------------------------------------------------------------------------#
        par(mar=c(4.1,4.6,4.1,2.1))
        plot.new()
        plot.window(xlim=xlimit,ylim=ylimit,log=xylog)
        axis(side=1,at=uplot$levels,labels=uplot$labels,padj=uplot$padj)
        axis(side=2,las=1)
        box()
        title(main=letitre,xlab="Year",ylab=ley,cex.main=0.7)
        if (drought.mark){
          for (n in 1:ndrought){
            rect(xleft  = drought[[n]][1],ybottom = ydrought[1]
                 ,xright = drought[[n]][2],ytop    = ydrought[2]
                 ,col    = grid.colour,border=NA)
          }#end for
        }#end if
        #----- Plot grid. ----------------------------------------------------------#
        if (plotgrid){ 
          abline(v=uplot$levels,h=axTicks(side=2),col=grid.colour,lty="solid")
        }#end if
        #----- Plot lines. ---------------------------------------------------------#
        for (l in 1:nlayers){
          thisvar = umean[[vnames[l]]]
          thisvar = cbind(thisvar[,ndcycle],thisvar)
          points(x=thisday,y=thisvar[pmon,],col=lcolours[l]
                 ,lwd=llwd[l],type=ltype,pch=16)
        }#end for
        #---------------------------------------------------------------------------#
        
        
        #----- Close the device. ---------------------------------------------------#
        if (outform[o] == "x11"){
          locator(n=1)
          dev.off()
        }else{
          dev.off()
        }#end if
        dummy=clean.tmp()
        #---------------------------------------------------------------------------#
      } #end for outform
      #------------------------------------------------------------------------------#
    }#end for pmon
    #---------------------------------------------------------------------------------#
  }#end if plotit
  #------------------------------------------------------------------------------------#
}#end for ntser
#---------------------------------------------------------------------------------------#





#---------------------------------------------------------------------------------------#
#   Plot the climatology of the soil properties.                                        #
#---------------------------------------------------------------------------------------#
for (v in 1:nsoilplot){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  thissoil    = soilplot[[v]]
  vnam        = thissoil$vnam
  description = thissoil$desc
  unit        = thissoil$unit
  vcscheme    = thissoil$csch
  pnlog       = thissoil$pnlog
  plotit      = thissoil$mmean
  
  if (plotit){
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    outdir  =  paste(outpref,"soil_mmean",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + Climatology profile of ",description,"...","\n")
    
    #----- Find the number of rows and columns, and the axes. ------------------------#
    monaxis  = sort(unique(datum$month))
    soilaxis = slz
    nmon     = length(monaxis)
    nsoil    = nzg
    
    #----- Save the meaningful months and years. -------------------------------------#
    monat   = 1:12
    monlab  = substring(month.abb,1,1)
    
    #----- Convert the vector data into an array. ------------------------------------#
    vararr  = mmean[[vnam]]
    
    #----- Copy Decembers ans Januaries to make the edges buffered. ------------------#
    january  = vararr[1,]
    january  = c(january,january[nzg],january[nzg])
    
    december = vararr[12,]
    december = c(december[1],december[1],december)
    
    #----- Bind first and last year to the array, to make the edges buffered. ---------#
    varbuff  = cbind(vararr[,1],vararr,vararr[,nzg])
    varbuff  = rbind(december,varbuff,january)
    
    #----------------------------------------------------------------------------------#
    #   Expand the month and year axes.  Make the -------------------------------------------#
    monaxis  = c(min(monaxis)-1,monaxis,max(monaxis)+1)
    soilaxis = -log(-1.0 * c( slz[1]*(slz[1]/slz[2])
                              , soilaxis
                              , slz[nzg]*(slz[nzg]/slz[nzg-1]) ))
    
    if (pnlog){
      vrange  = range(varbuff,na.rm=TRUE)
      vlevels = pretty.log(x=vrange,n=ncolsfc,forcelog=TRUE)
      vnlev   = length(vlevels)
    }else{
      vrange  = range(varbuff,na.rm=TRUE)
      vlevels = pretty(x=vrange,n=ncolsfc)
      vnlev   = length(vlevels)
    }#end if
    
    #----- Loop over formats. --------------------------------------------------------#
    for (o in 1:nout){
      fichier = paste(outdir,"/",vnam,"-",suffix,".",outform[o],sep="")
      if(outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      
      letitre = paste(description," - ",lieu,sep="")
      ley     = desc.unit(desc="Soil depth",unit=untab$m)
      lacle   = desc.unit(desc=NULL,unit=unit)
      par(par.user)
      sombreado(x=monaxis,y=soilaxis,z=varbuff,levels=vlevels,nlevels=vnlev
                ,colour.palette=get(vcscheme)
                ,plot.title=title(main=letitre,xlab="Month",ylab=ley,cex.main=0.7)
                ,key.title=title(main=lacle,cex.main=0.8)
                ,key.log=pnlog
                ,useRaster=TRUE
                ,plot.axes={axis(side=1,at=monat,labels=monlab)
                  axis(side=2,at=zat,labels=znice)
                  if (fcgrid){
                    abline(h=zat,v=monat,col=grid.colour,lty="dotted")
                  }#end if fcgrid
                }#end plot.axes
      )
      
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy = clean.tmp()
    } #end for outform
  }#end if plotit
}#end for (v in 1:nsoilplot)
#---------------------------------------------------------------------------------------#





#---------------------------------------------------------------------------------------#
#   Plot the climatology of the soil properties.                                        #
#---------------------------------------------------------------------------------------#
for (sts in 1:nsoilplot){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  thissoil    = soilplot[[sts]]
  vnam        = thissoil$vnam
  description = thissoil$desc
  unit        = thissoil$unit
  vcscheme    = thissoil$csch
  pnlog       = thissoil$pnlog
  plotit      = thissoil$emean
  
  if (plotit){
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    outdir  =  paste(outpref,"soil_emean",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + Time series profile of ",description,"...","\n")
    
    #----- Find the number of rows and columns, and the axes. ------------------------#
    timeaxis  = datum$tomonth
    soilaxis  = slz
    nmon      = length(timeaxis)
    nsoil     = nzg
    
    #----- Convert the vector data into an array. ------------------------------------#
    vararr  = emean[[vnam]]
    
    #----- Copy Decembers ans Januaries to make the edges buffered. ------------------#
    first    = vararr[1,]
    first    = c(first,first[nzg],first[nzg])
    
    last     = vararr[ntimes,]
    last     = c(last[1],last[1],last)
    
    #----- Bind first and last year to the array, to make the edges buffered. --------#
    varbuff  = cbind(vararr[,1],vararr,vararr[,nzg])
    varbuff  = rbind(first,varbuff,last)
    
    #---------------------------------------------------------------------------------#
    #      Expand the month and year axes.  Make the first and last time equal time   #
    # steps.                                                                          #
    #---------------------------------------------------------------------------------#
    dwhen    = as.numeric(datum$tomonth[2]-datum$tomonth[1])
    whenaxis = c(chron(as.numeric(datum$tomonth[1]-dwhen))
                 ,timeaxis
                 ,chron(as.numeric(datum$tomonth[ntimes]+dwhen)))
    soilaxis = -log(-1.0 * c( slz[1]*(slz[1]/slz[2])
                              , soilaxis
                              , slz[nzg]*(slz[nzg]/slz[nzg-1]) ))
    
    if (pnlog){
      vrange  = range(varbuff,na.rm=TRUE)
      vlevels = pretty.log(x=vrange,n=ncolsfc,forcelog=TRUE)
      vnlev   = length(vlevels)
    }else{
      vrange  = range(varbuff,na.rm=TRUE)
      vlevels = pretty(x=vrange,n=ncolsfc)
      vnlev   = length(vlevels)
    }#end if
    
    #----- Loop over formats. --------------------------------------------------------#
    for (o in 1:nout){
      fichier = paste(outdir,"/",vnam,"-",suffix,".",outform[o],sep="")
      if(outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      
      letitre = paste(description," - ",lieu,sep="")
      ley     = desc.unit(desc="Soil depth",unit=untab$m)
      lacle   = desc.unit(desc=NULL,unit=unit)
      par(par.user)
      sombreado(x=whenaxis,y=soilaxis,z=varbuff,levels=vlevels,nlevels=vnlev
                ,colour.palette=get(vcscheme)
                ,plot.title=title(main=letitre,xlab="Month",ylab=ley,cex.main=0.7)
                ,key.title=title(main=lacle,cex.main=0.8)
                ,key.log=pnlog
                ,useRaster=TRUE
                ,plot.axes={axis(side=1,at=whenplot6$levels
                                 ,labels=whenplot6$labels,padj=whenplot6$padj)
                  axis(side=2,at=zat,labels=znice)
                  if (fcgrid){
                    abline(h=zat,v=whenplot6$levels,col=grid.colour
                           ,lty="dotted")
                  }#end if fcgrid
                }#end plot.axes
      )#end sombreado
      
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy = clean.tmp()
    } #end for outform
  }#end if plotit
}#end for nhov
#---------------------------------------------------------------------------------------#





#---------------------------------------------------------------------------------------#
#   Plot a filled contour plot showing months and years.                                #
#---------------------------------------------------------------------------------------#
for (v in 1:nsqueeze){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  thisfillc   = squeeze[[v]]
  vnam        = thisfillc$vnam
  description = thisfillc$desc
  unit        = thisfillc$unit
  vcscheme    = thisfillc$col.scheme
  plotit      = thisfillc$fco.mmean
  
  #------------------------------------------------------------------------------------#
  #     Find the first and the last full years.  These will be the actual first and    #
  # last year only if the years are complete, otherwise the first and the last year    #
  # will be taken out.                                                                 #
  #------------------------------------------------------------------------------------#
  if (monthbeg == 1){
    yearaa = yeara
  }else{
    yearaa = yeara + 1
  }# end if
  if (meszz == 12){
    yearzz = yearz
  }else{
    yearzz = yearz - 1
  }#end if
  sel      = datum$year >= yearaa & datum$year <= yearzz
  twoyears = sum(sel) >= 24
  
  if (plotit && twoyears){
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    outdir  =  paste(outpref,"fillc_mmean",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + ",description," time series in filled contour...","\n")
    
    #----- Load this variable into "thisvar". ----------------------------------------#
    thisvar = emean[[vnam]]
    
    #----- Find the number of rows and columns, and the axes. ------------------------#
    monaxis = sort(unique(datum$month[sel]))
    yraxis  = sort(unique(datum$year[sel]))
    nmon    = length(monaxis)
    nyear   = length(yraxis)
    
    #----- Save the meaningful months and years. -------------------------------------#
    monat   = 1:12
    monlab  = c("J","F","M","A","M","J","J","A","S","O","N","D")
    yrat    = pretty(yraxis)
    
    #----- Convert the vector data into an array. ------------------------------------#
    vararr  = array(thisvar[sel],c(nmon,nyear))
    
    #----- Copy Decembers ans Januaries to make the edges buffered. ------------------#
    january  = vararr[1,]
    january  = c(january,january[nyear],january[nyear])
    
    december = vararr[12,]
    december = c(december[1],december[1],december)
    
    #----- Bind first and last year to the array, to make the edges buffered. --------#
    varbuff  = cbind(vararr[,1],vararr,vararr[,nyear])
    varbuff  = rbind(december,varbuff,january)
    
    #----- Expand the month and year axes. -------------------------------------------#
    monaxis = c(min(monaxis)-1,monaxis,max(monaxis)+1)
    yraxis  = c(min(yraxis)-1,yraxis,max(yraxis)+1)
    
    vrange  = range(varbuff,na.rm=TRUE)
    vlevels = pretty(x=vrange,n=ncolsfc)
    vnlev   = length(vlevels)
    
    #----- Loop over formats. --------------------------------------------------------#
    for (o in 1:nout){
      fichier = paste(outdir,"/",vnam,"-",suffix,".",outform[o],sep="")
      if(outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      
      letitre = paste(description," - ",lieu,sep="")
      lacle   = desc.unit(desc=NULL,unit=unit)
      par(par.user)
      sombreado(x=monaxis,y=yraxis,z=varbuff,levels=vlevels,nlevels=vnlev
                ,colour.palette=get(vcscheme)
                ,plot.title=title(main=letitre,xlab="Month",ylab="Year",cex.main=0.7)
                ,key.title=title(main=lacle,cex.main=0.8)
                ,useRaster=TRUE
                ,plot.axes={axis(side=1,at=monat,labels=monlab)
                  axis(side=2,at=yrat)
                  if (fcgrid){
                    for (yl in yrat){
                      abline(h=yl,col=grid.colour,lty="dotted")
                    } #end for yl
                    for (ml in monat){
                      abline(v=ml,col=grid.colour,lty="dotted")
                    } #end for ml
                  }#end if fcgrid
                }#end plot.axes
      )
      
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy = clean.tmp()
    } #end for outform
  }#end if plotit
}#end for nhov
#---------------------------------------------------------------------------------------#






#---------------------------------------------------------------------------------------#
#   Plot the filled contour diagrams showing time of day and time.                      #
#---------------------------------------------------------------------------------------#
for (v in 1:nsqueeze){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  thisfillc   = squeeze[[v]]
  vnam        = thisfillc$vnam
  description = thisfillc$desc
  unit        = thisfillc$unit
  vcscheme    = thisfillc$col.scheme
  plotit      = thisfillc$fco.qmean
  
  if (plotit){
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    outdir  =  paste(outpref,"fillc_qmean",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + ",description," time series of diurnal cycle...","\n")
    
    #----- Load this variable into "thisvar". ----------------------------------------#
    vararr   = qmean[[vnam]]
    
    #----- Copy Decembers ans Januaries to make the edges buffered. ------------------#
    firsthr  = vararr[,1]
    firsthr  = c(firsthr,firsthr[ntimes],firsthr[ntimes])
    
    lasthr   = vararr[,ndcycle]
    lasthr   = c(lasthr[1],lasthr[1],lasthr)
    
    #----- Bind first and last year to the array, to make the edges buffered. --------#
    varbuff  = rbind(vararr[1,],vararr,vararr[ntimes,])
    varbuff  = cbind(lasthr,varbuff,firsthr)
    
    #----- Expand the month and year axes. -------------------------------------------#
    hraxis    = seq(from=0,to=ndcycle+1,by=1) * 24 / ndcycle
    dwhen     = datum$tomonth[2]-datum$tomonth[1]
    whenaxis  = c(datum$tomonth[1]-dwhen,datum$tomonth,datum$tomonth[ntimes]+dwhen)
    huplot    = pretty.time(whenaxis,n=8)
    
    vrange  = range(varbuff,na.rm=TRUE)
    vlevels = pretty(x=vrange,n=ncolsfc)
    vnlev   = length(vlevels)
    
    #----- Loop over formats. --------------------------------------------------------#
    for (o in 1:nout){
      fichier = paste(outdir,"/",vnam,"-",suffix,".",outform[o],sep="")
      if(outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      
      letitre = paste("Mean diurnal cycle \n ",description," - ",lieu,sep="")
      ley     = desc.unit(desc="Time of day",unit=untab$gmt)
      lacle   = desc.unit(desc=NULL         ,unit=unit)
      par(par.user)
      sombreado(x=whenaxis,y=hraxis,z=varbuff,levels=vlevels,nlevels=vnlev
                ,colour.palette=get(vcscheme)
                ,plot.title=title(main=letitre,ylab=ley,xlab="Time",cex.main=0.7)
                ,key.title=title(main=lacle,cex.main=0.8)
                ,useRaster=TRUE
                ,plot.axes={axis(side=1,at=huplot$level,labels=huplot$labels)
                  axis(side=2,at=uplot$levels,labels=uplot$labels)
                  if (fcgrid){
                    abline(v=huplot$levels,h=uplot$levels
                           ,col=grid.colour,lty="dotted")
                  }#end if fcgrid
                }#end plot.axes
      )
      
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy = clean.tmp()
    } #end for outform
  }#end if plotit
}#end for nhov
#---------------------------------------------------------------------------------------#




#---------------------------------------------------------------------------------------#
#   Plot the monthly boxplots.                                                          #
#---------------------------------------------------------------------------------------#
for (v in 1:nsqueeze){
  
  #----- Retrieve variable information from the list. ---------------------------------#
  thisbplot   = squeeze[[v]]
  vnam        = thisbplot$vnam
  description = thisbplot$desc
  unit        = thisbplot$unit
  plotit      = thisbplot$box.plot
  
  if (plotit){
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    outdir  =  paste(outpref,"boxplot",sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    cat("      + ",description," box plot...","\n")
    
    #----- Load this variable into "thisvar". ----------------------------------------#
    thisvar = emean[[vnam]]
    
    for (o in 1:nout){
      fichier = paste(outdir,"/",vnam,"-",suffix,".",outform[o],sep="")
      if (outform[o] == "x11"){
        X11(width=size$width,height=size$height,pointsize=ptsz)
      }else if(outform[o] == "png"){
        png(filename=fichier,width=size$width*depth,height=size$height*depth
            ,pointsize=ptsz,res=depth)
      }else if(outform[o] == "eps"){
        postscript(file=fichier,width=size$width,height=size$height
                   ,pointsize=ptsz,paper=size$paper)
      }else if(outform[o] == "pdf"){
        pdf(file=fichier,onefile=FALSE
            ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
      }#end if
      
      
      ylimit  = pretty.xylim(u=thisvar,fracexp=0.0,is.log=FALSE)
      letitre = paste(description,lieu,sep=" - ")
      ley     = desc.unit(desc=description,unit=unit)
      par(par.user)
      plot(mmonth,thisvar,main=letitre,ylim=ylimit,cex.main=0.7
           ,xlab="Time",ylab=ley,las=1)
      
      if (outform[o] == "x11"){
        locator(n=1)
        dev.off()
      }else{
        dev.off()
      }#end if
      dummy = clean.tmp()
    } #end for outform
  }#end if
}#end for nbox
#---------------------------------------------------------------------------------------#





#---------------------------------------------------------------------------------------#
#      Bar plot by DBH class.                                                           #
#---------------------------------------------------------------------------------------#
cat("    + Bar plot by DBH classes...","\n")
monbplot    = which(nummonths(datum$tomonth) %in% sasmonth)
nmonbplot   = length(monbplot)
pftuse      = which(apply(X=szpft$nplant,MARGIN=3,FUN=sum,na.rm=TRUE) > 0)
pftuse      = pftuse[pftuse != (npft+1)]
npftuse     = length(pftuse)
pftname.use = pft$name  [pftuse]
pftcol.use  = pft$colour[pftuse]
for (v in 1:ntspftdbh){
  #----- Load settings for this variable.----------------------------------------------#
  thisbar     = tspftdbh[[v]]
  vnam        = thisbar$vnam
  description = thisbar$desc
  unit        = thisbar$unit
  stacked     = thisbar$stack
  plotit      = thisbar$bar.plot
  plog        = thisbar$plog
  if (plog){
    xylog   = "y"
    stacked = FALSE
  }else{
    xylog   = ""
  }#end if
  #------------------------------------------------------------------------------------#
  
  
  #------------------------------------------------------------------------------------#
  #      Check whether to plot this 
  #------------------------------------------------------------------------------------#
  if (plotit){
    cat("      - ",description,"...","\n")
    
    
    #---------------------------------------------------------------------------------#
    #     Retrieve the variable, and keep only the part that is usable.               #
    #---------------------------------------------------------------------------------#
    thisvnam                  = szpft[[vnam]][monbplot,,]
    thisvnam                  = thisvnam [,,pftuse]
    thisvnam                  = thisvnam [,-(ndbh+1),]
    
    thisvnam[is.na(thisvnam)] = 0.
    thiswhen                  = datum$tomonth[monbplot]
    #---------------------------------------------------------------------------------#
    
    
    #---------------------------------------------------------------------------------#
    #      Find the limits for the plots.  We use the same axis so it is easier to    #
    # compare different times.                                                        #
    #---------------------------------------------------------------------------------#
    if (stacked){
      ylimit   = c(0,max(apply(X=thisvnam,MARGIN=c(1,2),FUN=sum,na.rm=TRUE)))
    }else{
      ylimit   = range(x=thisvnam,na.rm=TRUE)
    }#end if
    ylimit = pretty.xylim(u=ylimit,fracexp=0.0,is.log=plog)
    #---------------------------------------------------------------------------------#
    
    
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    barplotdir = paste(outpref,"barplot_dbh",sep="/")
    if (! file.exists(barplotdir)) dir.create(barplotdir)
    outdir = paste(barplotdir,vnam,sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    #---------------------------------------------------------------------------------#
    
    
    
    #---------------------------------------------------------------------------------#
    #      Loop over all possible months.                                             #
    #---------------------------------------------------------------------------------#
    for (m in 1:nmonbplot){
      
      #----- Find which year we are plotting. ---------------------------------------#
      cmonth    = sprintf("%2.2i",(nummonths(thiswhen[m])))
      cyear     = sprintf("%4.4i",(numyears(thiswhen[m])))
      mm        = as.numeric(cmonth)
      yy        = as.numeric(cyear)
      whentitle = paste(mon2mmm(mm,cap1=TRUE),cyear,sep="-")
      #------------------------------------------------------------------------------#
      
      
      #----- Loop over output formats. ----------------------------------------------#
      for (o in 1:nout){
        #------ Open the plot. -----------------------------------------------------#
        fichier = paste(outdir,"/",vnam,"-",cyear,"-",cmonth,"-",suffix
                        ,".",outform[o],sep="")
        if (outform[o] == "x11"){
          X11(width=size$width,height=size$height,pointsize=ptsz)
        }else if(outform[o] == "png"){
          png(filename=fichier,width=size$width*depth,height=size$height*depth
              ,pointsize=ptsz,res=depth)
        }else if(outform[o] == "eps"){
          postscript(file=fichier,width=size$width,height=size$height
                     ,pointsize=ptsz,paper=size$paper)
        }else if(outform[o] == "pdf"){
          pdf(file=fichier,onefile=FALSE
              ,width=size$width,height=size$height,pointsize=ptsz,paper=size$paper)
        }#end if
        #---------------------------------------------------------------------------#
        
        
        #------ Set up the title and axis labels. ----------------------------------#
        letitre = paste(lieu,"\n",description," - Time : ",whentitle,sep="")
        lexlab  = "DBH Classes"
        leylab  = desc.unit(desc=description,unit=unit)
        #---------------------------------------------------------------------------#
        
        
        #------ Split window. ------------------------------------------------------#
        par(par.user)
        layout(mat=rbind(2,1),heights=c(5,1))
        #---------------------------------------------------------------------------#
        
        
        
        #------ Legend. ------------------------------------------------------------#
        par(mar=c(0.1,4.6,0.1,2.1))
        plot.new()
        plot.window(xlim=c(0,1),ylim=c(0,1))
        legend( x      = "bottom"
                , inset  = 0.0
                , legend = pftname.use
                , fill   = pftcol.use
                , ncol   = min(3,pretty.box(n.selpft)$ncol)
                , title  = expression(bold("Plant functional type"))
                , cex    = cex.ptsz
                , bg     = background
                , xpd    = TRUE
        )#end legend
        #---------------------------------------------------------------------------#
        
        
        #----- Plot all monthly means together. ------------------------------------#
        par(mar=c(4.1,4.6,4.1,2.1))
        barplot(height=t(thisvnam[m,,]),names.arg=dbhnames[1:ndbh],width=1.0
                ,main=letitre,xlab=lexlab,ylab=leylab,ylim=ylimit,legend.text=FALSE
                ,beside=(! stacked),col=pftcol.use,log=xylog
                ,border=grey.fg,xpd=FALSE,cex.main=cex.main,las=1)
        if (plotgrid & (! stacked)){
          xgrid=0.5+(1:ndbh)*(1+npftuse)
          abline(v=xgrid,col=grid.colour,lty="solid")
        }#end if
        box()
        #---------------------------------------------------------------------------#
        
        
        
        #---------------------------------------------------------------------------#
        #     Close the device.                                                     #
        #---------------------------------------------------------------------------#
        if (outform[o] == "x11"){
          locator(n=1)
          dev.off()
        }else{
          dev.off()
        }#end if
        dummy = clean.tmp()
        #---------------------------------------------------------------------------#
      } #end for outform
      #------------------------------------------------------------------------------#
    }#end for
    #---------------------------------------------------------------------------------#
  }#end if
  #------------------------------------------------------------------------------------#
}#end for
#---------------------------------------------------------------------------------------#





#---------------------------------------------------------------------------------------#
#    Plot the 3-D size and age structure of various variables.                          #
#---------------------------------------------------------------------------------------#
for (v in 1:ntspftdbh){
  #----- Retrieve variable information from the list. ---------------------------------#
  thissas     = tspftdbh[[v]]
  vnam        = thissas$vnam
  description = thissas$desc
  unit        = thissas$i.unit
  plotit      = thissas$sas
  plog        = thissas$plog
  
  #----- If this variable is to be plotted, then go through this if block. ------------#
  if (plotit){
    
    cat("      + Size and age structure plot: ",description,"...","\n")
    
    #---------------------------------------------------------------------------------#
    #     Check if the directory exists.  If not, create it.                          #
    #---------------------------------------------------------------------------------#
    sasdir = paste(outpref,"sas",sep="/")
    if (! file.exists(sasdir)) dir.create(sasdir)
    outdir = paste(sasdir,vnam,sep="/")
    if (! file.exists(outdir)) dir.create(outdir)
    #---------------------------------------------------------------------------------#
    
    
    #----- Load this list into "thislist". -------------------------------------------#
    varco =  cohort[[vnam]]
    #---------------------------------------------------------------------------------#
    
    
    #---------------------------------------------------------------------------------#
    #      Loop over all times.                                                       #
    #---------------------------------------------------------------------------------#
    for (ww in names(cohort$age)){
      
      #----- Find which year we are plotting. ---------------------------------------#
      cmonth   = substring(ww,7,8)
      thisyear = substring(ww,2,5)
      mm       = as.numeric(cmonth)
      yy       = as.numeric(thisyear)
      
      #----- Retrieve variable list, age, DBH, and PFT for this year. ---------------#
      ageww   = cohort$age   [[ww]]
      if (any(ageww <= 0,na.rm=TRUE)){
        minww = min(ageww,na.rm=TRUE)
        ageww = ageww - minww + 0.01
      }#end if
      dbhww   = cohort$dbh   [[ww]]
      pftww   = cohort$pft   [[ww]]
      varww   = varco        [[ww]]
      popww   = cohort$nplant[[ww]] * cohort$area[[ww]]
      
      #------------------------------------------------------------------------------#
      #     We only plot the SAS figures when the polygon is not an absolute desert. #
      #------------------------------------------------------------------------------#
      if (any (! is.na(varww))){
        #---------------------------------------------------------------------------#
        #      Find the range.  If the user wants the range to be fixed, then use   #
        # the global range, otherwise, simply use the range for this year.          #
        #---------------------------------------------------------------------------#
        if (sasfixlimits){
          xlimit  = pretty.xylim(u=unlist(cohort$age),fracexp=0.0,is.log=TRUE )
          ylimit  = pretty.xylim(u=unlist(cohort$dbh),fracexp=0.0,is.log=FALSE)
          zlimit  = pretty.xylim(u=unlist(varco)     ,fracexp=0.0,is.log=plog )
          popmin  = min  (unlist(cohort$nplant * cohort$area), na.rm=TRUE)
          popmax  = max  (unlist(cohort$nplant * cohort$area), na.rm=TRUE)
        }else{
          xlimit  = pretty.xylim(u=ageww             ,fracexp=0.0,is.log=TRUE )
          ylimit  = pretty.xylim(u=dbhww             ,fracexp=0.0,is.log=FALSE)
          zlimit  = pretty.xylim(u=varww             ,fracexp=0.0,is.log=plog )
          popmin  = min  (popww  ,na.rm=TRUE)
          popmax  = max  (popww  ,na.rm=TRUE)
        }#end if
        #---------------------------------------------------------------------------#
        
        
        #----- Define the scale-dependent population size. -------------------------#
        cexww = cexmin + (cexmax - cexmin) * log(popww/popmin) / log(popmax/popmin)
        #---------------------------------------------------------------------------#
        
        
        
        #----- Define the floor location. ------------------------------------------#
        if ((zlimit[1] > 0) != (zlimit[2] > 0)){
          floor3d = 0.
        }else if (zlimit[1] > 0){
          floor3d = zlimit[1]
        }else{
          floor3d = zlimit[2]
        }#end if
        #---------------------------------------------------------------------------#
        
        
        
        #----- Define the grid information for the 3-D plot. -----------------------#
        xlabels = pretty.log(xlimit,n=5)
        ylabels = pretty(ylimit,n=5)
        zlabels = if(plog){pretty.log(zlimit,n=5)}else{pretty(zlimit,n=5)}
        xat     = log(xlabels)
        yat     = ylabels
        zat     = if(plog){log(zlabels)}else{zlabels}
        xlimit  = range(x=xat)
        ylimit  = range(x=yat)
        zlimit  = range(x=zat)
        xfloor  = seq(from=xlimit[1],to=xlimit[2],length.out=16)
        yfloor  = seq(from=ylimit[1],to=ylimit[2],length.out=16)
        zfloor  = matrix(floor3d,nrow=length(xfloor),ncol=length(yfloor))
        #---------------------------------------------------------------------------#
        
        
        
        #----- Expand the lines to make the lollipops. -----------------------------#
        ncohnow  = length(varww)
        ageww    = rep(ageww,each=3)
        dbhww    = rep(dbhww,each=3)
        pftww    = rep(pftww,each=3)
        varww    = as.vector(rbind(rep(floor3d,times=ncohnow)
                                   ,varco[[ww]]
                                   ,rep(NA,times=ncohnow)))
        xww      = log(ageww)
        yww      = dbhww
        zww      = if(plog){log(varww)}else{varww}
        pchww    = rep(c(NA,16,NA),times=ncohnow)
        cexww    = rep(cexww,each=3)
        colww    = pft$colour[pftww]
        
        pftin   = sort(unique(cohort$pft[[ww]]))
        colleg  = pft$colour[pftin]
        pftleg  = pft$name  [pftin]
        #---------------------------------------------------------------------------#
        
        
        
        #---------------------------------------------------------------------------#
        #   Plot annotation.                                                        #
        #---------------------------------------------------------------------------#
        letitre = paste(description," - ",lieu,
                        "\n Time :",mlist[mm],"/",thisyear,sep=" ")
        lexlab  = desc.unit(desc="Gap age",unit=untab$yr)
        leylab  = desc.unit(desc="DBH",unit=untab$cm)
        lezlab  = desc.unit(desc=description,unit=unit)
        #---------------------------------------------------------------------------#
        
        
        #----- Loop over output formats. -------------------------------------------#
        for (o in 1:nout){
          #----- Open file. -------------------------------------------------------#
          fichier = paste(outdir,"/",vnam,"-",thisyear,"-",cmonth,"-",suffix
                          ,".",outform[o],sep="")
          if (outform[o] == "x11"){
            X11(width=size$width,height=size$height,pointsize=ptsz)
          }else if(outform[o] == "png"){
            png(filename=fichier,width=size$width*depth,height=size$height*depth
                ,pointsize=ptsz,res=depth)
          }else if(outform[o] == "eps"){
            postscript(file=fichier,width=size$width,height=size$height
                       ,pointsize=ptsz,paper=size$paper)
          }else if(outform[o] == "pdf"){
            pdf(file=fichier,onefile=FALSE
                ,width=size$width,height=size$height,pointsize=ptsz
                ,paper=size$paper)
          }#end if
          #------------------------------------------------------------------------#
          
          
          #----- Split the domain into 2. -----------------------------------------#
          par(par.user)
          layout(mat=rbind(2,1),heights=c(5,1))
          #------------------------------------------------------------------------#
          
          
          #------------------------------------------------------------------------#
          #     Plot legend.                                                       #
          #------------------------------------------------------------------------#
          par(mar=c(0.1,0.1,0.1,0.1))
          plot.new()
          plot.window(xlim=c(0,1),ylim=c(0,1))
          legend( x      = "center"
                  , inset  = 0.0
                  , legend = pftleg
                  , fill   = colleg
                  , ncol   = min(4,pretty.box(length(pftleg))$ncol)
                  , title  = expression(bold("Plant functional type"))
                  , cex    = cex.ptsz
                  , xpd    = TRUE
          )#end legend
          #------------------------------------------------------------------------#
          
          
          #------------------------------------------------------------------------#
          #     Plot the 3-D plot.                                                 #
          #------------------------------------------------------------------------#
          par(mar=c(1.1,1.1,4.1,1.1))
          pout = perspx( x         = xfloor
                         , y         = yfloor
                         , z         = zfloor
                         , xlim      = xlimit
                         , ylim      = ylimit
                         , zlim      = zlimit
                         , theta     = theta
                         , phi       = phi
                         , col       = gcol
                         , expand    = expz
                         , ticktype  = "detailed"
                         , border    = NA
                         , shade     = shade
                         , ltheta    = ltheta
                         , main      = letitre
                         , cex.main  = 0.8*cex.ptsz
                         , axes      = FALSE
          )#end perspx
          #----- Add axes. --------------------------------------------------------#
          paxis3d(edge="X--",pmat=pout,at=xat,cex=0.9*cex.ptsz,labels=xlabels)
          paxis3d(edge="Y--",pmat=pout,at=yat,cex=0.9*cex.ptsz,labels=ylabels)
          paxis3d(edge="Z-+",pmat=pout,at=zat,cex=0.9*cex.ptsz,labels=zlabels)
          mtext3d(edge="X--",pmat=pout,labels=lexlab,cex=cex.ptsz,srt=theta+90)
          mtext3d(edge="Y--",pmat=pout,labels=leylab,cex=cex.ptsz,srt=theta)
          mtext3d(edge="Z-+",pmat=pout,labels=lezlab,cex=cex.ptsz,srt=-75)
          #------------------------------------------------------------------------#
          
          
          #----- Add the cohorts. -------------------------------------------------#
          lines (trans3d(x=xww,y=yww,z=zww,pmat=pout),type="l",col=grey.fg,lwd=2)
          points(trans3d(x=xww,y=yww,z=zww,pmat=pout),type="p",pch=pchww
                 ,col=colww,cex=cexww)
          #------------------------------------------------------------------------#
          
          
          
          #----- Close the device. ------------------------------------------------#
          if (outform[o] == "x11"){
            locator(n=1)
            dev.off()
          }else{
            dev.off()
          }#end if
          dummy = clean.tmp()
          #------------------------------------------------------------------------#
        }#end for outform
        #---------------------------------------------------------------------------#
      }#end if is.na(varww)
      #------------------------------------------------------------------------------#
    }#end for nameco
    #---------------------------------------------------------------------------------#
  }#end if
  #------------------------------------------------------------------------------------#
}#end for npsas





}
