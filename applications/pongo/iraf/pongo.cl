#  IRAF initialisation file for the PONGO package.

#  Peter W. Draper 29-APR-1997.

package pongo

#  ------------------
#  Fundermental tasks
#  ------------------
task annotate = "pongo$pongo_mon.e"
task arc = "pongo$pongo_mon.e"
task avedat = "pongo$pongo_mon.e"
task begpongo = "pongo$pongo_mon.e"
hidetask begpongo
task boxframe = "pongo$pongo_mon.e"
task ccmath = "pongo$pongo_mon.e"
task change = "pongo$pongo_mon.e"
task clear = "pongo$pongo_mon.e"
task cllog = "pongo$pongo_mon.e"
hidetask cllog
task curse = "pongo$pongo_mon.e"
task drawpoly = "pongo$pongo_mon.e"
task ellipses = "pongo$pongo_mon.e"
task endpongo = "pongo$pongo_mon.e"
hidetask endpongo
task errorbar = "pongo$pongo_mon.e"
task fitcurve = "pongo$pongo_mon.e"
task fitline = "pongo$pongo_mon.e"
task getglobal = "pongo$pongo_mon.e"
hidetask getglobal
task getpoint = "pongo$pongo_mon.e"
task gpoints = "pongo$pongo_mon.e"
task grid = "pongo$pongo_mon.e"
task gt_circle = "pongo$pongo_mon.e"
task inquire = "pongo$pongo_mon.e"
task label = "pongo$pongo_mon.e"
task limits = "pongo$pongo_mon.e"
task palette = "pongo$pongo_mon.e"
task paper = "pongo$pongo_mon.e"
task plotfun = "pongo$pongo_mon.e"
task plothist = "pongo$pongo_mon.e"
task prim = "pongo$pongo_mon.e"
task pvect = "pongo$pongo_mon.e"
task readf = "pongo$pongo_mon.e"
task setglobal = "pongo$pongo_mon.e"
hidetask setglobal
task vect = "pongo$pongo_mon.e"
task viewport = "pongo$pongo_mon.e"
task world = "pongo$pongo_mon.e"
task writei = "pongo$pongo_mon.e"
task wtext = "pongo$pongo_mon.e"
task unsetglobal = "pongo$pongo_mon.e"
hidetask unsetglobal

#  ----------
#  Procedures (equivalents for ICL aliases and procs).
#  ----------

task begplot = "pongo$begplot.cl"
task endplot = "pongo$endplot.cl"
task $advance = "pongo$advance.cl"
task bin = "pongo$bin.cl"
task clog = "pongo$clog.cl"
task $connect = "pongo$connect.cl"
task data = "pongo$data.cl"
task degtor = "pongo$degtor.cl"
task device = "pongo$device.cl"
task dlimits = "pongo$dlimits.cl"
task draw = "pongo$draw.cl"
task $erase = "pongo$erase.cl"
task errx  ="pongo$errx.cl"
task erry  ="pongo$erry.cl"
task excolumn = "pongo$excolumn.cl"
task expand = "pongo$expand.cl"
task eycolumn = "pongo$eycolumn.cl"
task fillsty = "pongo$fillsty.cl"
task font = "pongo$font.cl"
task histogram = "pongo$histogram.cl"
task labcolumn = "pongo$labcolumn.cl"
task ltype = "pongo$ltype.cl"
task lweight = "pongo$lweight.cl"
task mark = "pongo$mark.cl"
task move = "pongo$move.cl"
task mtext = "pongo$mtext.cl"
task pen = "pongo$pen.cl"
task pcolumn = "pongo$pcolumn.cl"
task points = "pongo$points.cl"
task ptext = "pongo$ptext.cl"
task ptinfo = "pongo$ptinfo.cl"
task radiate = "pongo$radiate.cl"
task $resetpongo = "pongo$resetpongo.cl"
task rtodeg = "pongo$rtodeg.cl"
task setproj = "pongo$setproj.cl"
task $showpongo = "pongo$showpongo.cl"
task sizeplot = "pongo$sizeplot.cl"
task symcolumn = "pongo$symcolumn.cl"
task text = "pongo$text.cl"
task vport = "pongo$vport.cl"
task vp_bh = "pongo$vp_bh.cl"
task vp_bl = "pongo$vp_bl.cl"
task vp_br = "pongo$vp_br.cl"
task vp_th = "pongo$vp_th.cl"
task vp_tl = "pongo$vp_tl.cl"
task vp_tr = "pongo$vp_tr.cl"
task vsize = "pongo$vsize.cl"
task $vstand = "pongo$vstand.cl"
task $wnad = "pongo$wnad.cl"
task xerr = "pongo$xerr.cl"
task $xlinear = "pongo$xlinear.cl"
task $xlogarithm = "pongo$xlogarithm.cl"
task xcolumn = "pongo$xcolumn.cl"
task xoffset = "pongo$xoffset.cl"
task xscale = "pongo$xscale.cl"
task ycolumn = "pongo$ycolumn.cl"
task yerr = "pongo$yerr.cl"
task $ylinear = "pongo$ylinear.cl"
task $ylogarithm = "pongo$ylogarithm.cl"
task yoffset = "pongo$yoffset.cl"
task yscale = "pongo$yscale.cl"
task zcolumn = "pongo$zcolumn.cl"
task zscale = "pongo$zscale.cl"

#  Demo procedures

task spectrum = "pongo$spectrum.cl"
hidetask spectrum
task errors = "pongo$errors.cl"
hidetask errors
task histogramtest = "pongo$histogramtest.cl"
hidetask histogramtest
task ppdotdiag = "pongo$ppdotdiag.cl"
hidetask ppdotdiag
task ellipsetest = "pongo$ellipsetest.cl"
hidetask ellipsetest
task projections = "pongo$projections.cl"
hidetask projections
task radec = "pongo$radec.cl"
hidetask radec
task cover = "pongo$cover.cl"
hidetask cover
task vector = "pongo$vector.cl"
hidetask vector
task interactive = "pongo$interactive.cl"
hidetask interactive
task agi = "pongo$agi.cl"
hidetask agi
task pongo_demo = "pongo$pongo_demo.cl"
hidetask pongo_demo

clbye()
