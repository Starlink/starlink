#!/bin/csh -v

set list2 = ( abconv abline alasin alasout aperture bfft bsmult caldiv \
              centers cmplx2i cmplx2m cmplx2r cmplxadd cmplxconj cmplxdiv \
              cmplxfilt cmplxmult cmplxsub cosbell cspike emlt extin \
              fet321 ff ffcross fft figs321 figs322 figs422 figs423 \
              figs424 figsee figsflux fitset fitskeys flconv foto \
              fwconv  gauss gspike i2cmplx impos interp irconv irflat \
              irflux linterp mask mcfit peak r2cmplx rcgs2 rdfits \
              spflux spied spifit wdfits rdipso wdipso table )

set list3 = (  arc cdist echarc echfind echmask echmerge echselect findsp \
               flaircomp flairext iarc iscrunch iscruni maskext offdist \
               overpf polext sdist skyliner vachel xcopi xcopy )

set list4 = ( arcdisp arcgendb arcident arclocat ascin ascout bbody correl \
              editext evalfit fillcube fitbb fitgauss fitpoly fittri grow \
              grow moments movie resamp speccont specgrid specplot subset xtract )

set list5 = ( arc2d arcsdi cadd changed comb crigauss cscan csub cube2long \
              fib2cube fibdisp fibsep fitcont himage iscan longslit vig )

foreach f ( $list2 )
  mv ${f}.par ../figaro2/
  mv ${f}.ifl ../figaro2/
end

foreach f ( $list3 )
  mv ${f}.par ../figaro3/
  mv ${f}.ifl ../figaro3/
end

foreach f ( $list4 )
  mv ${f}.par ../figaro4/
  mv ${f}.ifl ../figaro4/
end

foreach f ( $list5 )
  mv ${f}.par ../figaro5/
  mv ${f}.ifl ../figaro5/
end

exit