{+
{  Name:
{     demo.icl

{  Purpose:
{     Demonstration procedure for Specdre.

{  Type of Module:
{     ICL procedure

{  Invocation:
{     load FIG_DIR:demo_specdre

{  Arguments:
{     none.

{  Description:
{     This list of ICL commands performs a representative test version
{     0.7 of the Specdre package of applications.

{  Prior requirements:
{     You will need access to an Xwindows display.

{  Authors:
{     hme: Horst Meyerdierks (UoE, Starlink)
{     acc: Anne Charles (RAL)
{     {enter_new_authors_here}

{  History:
{     17 Jul 1992 (hme):
{        Original version.
{     01 Jul 1993 (hme):
{        Include arc* applications.
{     28 Nov 1995 (hme):
{        Convert to Unix (rm, mv, cp commands).
{     24 Nov 1997 (acc):
{        Because of merger of SPECDRE with FIGARO:
{          changed SPECDRE_DIR to FIG_DIR,
{          changed `resample' to `resamp'.
{     {enter_further_changes_here}

{  Bugs:
{     {note_any_bugs_here}

{-
{.

bbody 1000 logar=true xstart=12 xstep=0.05 xend=17 ~
   xlabel=frequency xunit=log10(Hz) out=bb1000 accept
bbody  2000 logar=true in=bb1000  out=bb2000 accept
bbody  3000 logar=true in=bb1000  out=bb3000 accept
bbody  4000 logar=true in=bb1000  out=bb4000 accept
bbody  5000 logar=true in=bb1000  out=bb5000 accept
bbody  6000 logar=true in=bb1000  out=bb6000 accept
bbody  7000 logar=true in=bb1000  out=bb7000 accept
bbody  8000 logar=true in=bb1000  out=bb8000 accept
bbody  9000 logar=true in=bb1000  out=bb9000 accept
bbody 10000 logar=true in=bb1000 out=bb10000 accept

editext list bb1000 accept

editext create bb1000
editext list bb1000 accept

editext "set specvals" bb1000
{editext "set coord" bb1000
editext list bb1000 accept

editext "create results _REAL _DOUBLE default 1 2" bb1000
editext list bb1000 accept

resamp s bb1000 bb1000_2 12.025 0.05 17.025 varuse=no accept
!mv bb1000_2.sdf bb1000.sdf
editext list bb1000 accept

specplot in=bb10000 colour=1 overlay=n clear=y world=[12,16,0,20] ~
   lin=n bin=t northo=2 device=xwindows reset accept
specplot in=bb1000  colour=8 overlay=y clear=n ~
   lin=y bin=f axes=0000 tick=0000 numl=0000 text=0000 accept
specplot in=bb2000  colour=7 overlay=y accept
specplot in=bb4000  colour=3 overlay=y accept
specplot in=bb8000  colour=4 overlay=y accept

grow new=true in=bb2000 out=bball stapix=[0,2] endpix=[0,2] ~
   size=[0,10] expand=[0,1]
editext list bball accept

grow new=false in=bb1000 out=bball stapix=[0,1] endpix=[0,1] expand=[0,1]
editext list bball accept

grow new=false in=bb3000 out=bball stapix=[0,3] endpix=[0,3] expand=[0,1]
grow new=false in=bb4000 out=bball stapix=[0,4] endpix=[0,4] expand=[0,1]
grow new=false in=bb5000 out=bball stapix=[0,5] endpix=[0,5] expand=[0,1]
grow new=false in=bb6000 out=bball stapix=[0,6] endpix=[0,6] expand=[0,1]
grow new=false in=bb7000 out=bball stapix=[0,7] endpix=[0,7] expand=[0,1]
grow new=false in=bb8000 out=bball stapix=[0,8] endpix=[0,8] expand=[0,1]
grow new=false in=bb9000 out=bball stapix=[0,9] endpix=[0,9] expand=[0,1]
grow new=false in=bb10000 out=bball stapix=[0,10] endpix=[0,10] expand=[0,1]

subset bball(,2:) rows2toend
editext "delete specvals" rows2toend
xtract varuse=no in=rows2toend colaps=[0,1] out=extracted
resamp s ~
   "bball(,2),bball(,3),bball(,4),bball(,5),bball(,6),bball(,7),bball(,8),bball(,9),bball(,10)" ~
   out=resampled varuse=no accept
specplot error=yes in=extracted overlay=n clear=y device=xwindows reset accept
specplot in=resampled clear=n overlay=y device=xwindows colour=3 mark=1 accept

editext "delete results" bball
editext list bball accept

ascout in=bball(1,) out=column1.dat
ascin in=column1.dat out=column1 colaxes=1 coldata=2 accept
!rm -f column1.dat
subset bball(1,3:9) subsetcol1
specplot in=column1 overlay=n clear=y device=xwindows reset accept
specplot in=bball(1,) overlay=y clear=n colour=7 accept
specplot in=subsetcol1 overlay=y clear=n colour=4 accept

fitpoly varuse=no order=0 dialog=n comp=1 ~
   device=xwindows in=bball(,1) accept
fitpoly varuse=no order=1 comp=1 device=xwindows in=bball(,2) accept
fitpoly varuse=no order=2 comp=1 device=xwindows in=bball(,3) accept
fitpoly varuse=no order=3 comp=1 device=xwindows in=bball(,4) accept
fitpoly varuse=no order=4 comp=1 device=xwindows in=bball(,5) accept
fitpoly varuse=no order=5 comp=1 device=xwindows in=bball(,6) accept
fitpoly varuse=no order=6 comp=1 device=xwindows in=bball(,7) accept
fitpoly varuse=no order=7 comp=1 device=xwindows in=bball(,8) accept
fitpoly varuse=no order=7 comp=1 device=xwindows in=bball(,9) accept
fitpoly varuse=no order=7 comp=1 device=xwindows in=bball(,10) accept

specplot in=bball.more.specdre.results(4,1,) clear=y colour=1 ~
   height=1.2 roman=true northo=4 ~
   bottom="Temperature axis pixel count" ~
   left="0-th coefficient of polynomial" accept
editext list bball accept

fitgauss varuse=no in=bball(,5) device=xwindows ~
   ncomp=1 cont=-500 centre=15 peak=540 fwhm=10 cf=0 pf=0 wf=0 ~
   comp=2 accept

correl varuse=false inlist="bball(,2),bball(,6)" out=correl.dat ~
   logfil=/dev/null accept
ascin in=correl.dat out=correl colaxes=1 coldata=2 accept
!rm -f correl.dat
specplot in=correl clear=y colour=1 bin=false lin=false mark=3 height=1 ~
   roman=false text=++0+ bottom="B\d\gn\u(2000 K) [log10(Jy/sr)]" ~
   left="B\d\gn\u(6000 K) [log10(Jy/sr)]" ~
   right="Two Planck curves compared" accept

!cp $FIG_DIR/demo.arc $FIG_DIR/demo.sdf .
!chmod 644 demo.arc demo.sdf
arcgendb demo.arc demo_arc
arclocat demo 2 50 mode=Gauss dialog=n
arcident demo demo2 demo_arc wrange=[3900,4000]
arcdisp demo2 2 dialog=n fdb=demo_arc

specplot demo clear=t axes=++00 tick=--00 text=++00 bottom=pixel left=counts ~
   labspc=[4,6,4,6] world=[0.5,2039.5,-10,800] reset accept
specplot demo2 clear=f colour=3 axes=00++ tick=00-- text=00++ numl=00++ ~
   top="wavelength [\A]" right="Specdre demonstration" ~
   labspc=[4,6,4,6] world=[3921.9,3988.33,-110,700] accept

ascout in=demo2.more.specdre.specvals out=dispcurv.dat
ascin in=dispcurv.dat out=dispcurv colaxes=1 coldata=2 accept
!rm -f dispcurv.dat
specplot dispcurv bottom=pixel left="wavelength [\A]" northo=3 ~
   clear=t reset accept

!rm -f bb1000.sdf bb10000.sdf bb2000.sdf bb3000.sdf bb4000.sdf bb5000.sdf bb6000.sdf bb7000.sdf bb8000.sdf bb9000.sdf bball.sdf column1.sdf extracted.sdf subsetcol1.sdf rows2toend.sdf resampled.sdf correl.sdf demo.arc demo_arc.sdf demo.sdf demo2.sdf dispcurv.sdf

{  End of demo.icl
