{+
{  Name:
{     kappa.icl
{
{  Purpose:
{     Defines KAPPA commands and help for ICL usage.
{
{  Type of Module:
{     ICL procedure
{
{  Invocation:
{     load $KAPPA_DIR/kappa
{
{  Description:
{     This procedure starts KAPPA for use from UNIX ICL by defining
{     the KAPPA commands needed to execute each application or to get
{     help about KAPPA.
{
{  Authors:
{     MJC: Malcolm J. Currie (STARLINK)
{     DSB: David S. Berry (STARLINK)
{     {enter_new_authors_here}
{
{  History:
{     1994 May 18 (MJC):
{        Original version.
{     1994 June 23 (MJC):
{        Changed the definition of kaphelp to point to the ICL help 
{        command, and make help kappa pick up the 0-level help using
{        the new ICL option in defhelp.
{     1995 August 19 (MJC):
{        Added version 0.9 tasks.  Divided into three monoliths.
{        Removed withdrawn tasks: COMPRESS, NUMBA, PICK2D, STATS2D,
{        THRESH0; and inserted defstrings indicating their replacements.
{        Renamed GAUSS to GAUSMOOTH and CLEANER to FFCLEAN.
{     1995 October 28 (MJC):
{        Added FILLBAD.  Renamed SHIFT to SLIDE, but retain alias.
{     1995 October 31 (MJC):
{        Added PICEMPTY, PICENTRY, and PICVIS.
{     1996 June 12 (MJC):
{        Added NAG usage message, as supplied by Martin Bly.
{     1997 May 31 (MJC):
{        Removed NAG usage message along with COLUMNAR and HIDE.
{        Inserted DRAWSIG, FITSMOD, FITSEXIST, FITSVAL, FITSWRITE,
{        KSTEST, and SUBSTITUTE.  Also withdrew BLINK, IDUNZOOM,
{        IDVISIBLE.
{     7-OCT-1998 (DSB):
{        Added PROFILE, LISTSHOW, LISTMAKE, WCSALIGN, WCSADD, WCSATTRIB, 
{        WCSCOPY, WCSFRAME and WCSREMOVE for V0.13.
{     {enter_further_changes_here}
{
{  Bugs:
{     {note_any_bugs_here}
{
{-

{ Define KAPPA commands, including aliases for the now non-existent ones.
{ Use the monolith.  Note kaphelp is a defstring to use ICL help because
{ kaphelp doesn't work from UNIX ICL.  Also use defstrings to report
{ withdrawn applications.
define   add        $KAPPA_DIR/kappa_mon
define   aper(add)  $KAPPA_DIR/kappa_mon
define   ardg(en)   $KAPPA_DIR/kappa_mon
define   ardm(ask)  $KAPPA_DIR/kappa_mon
define   bloc(k)    $KAPPA_DIR/kappa_mon
define   cadd       $KAPPA_DIR/kappa_mon
define   calc       $KAPPA_DIR/kappa_mon
define   calp(ol)   $KAPPA_DIR/kappa_mon
define   cdiv       $KAPPA_DIR/kappa_mon
define   cent(roid) $KAPPA_DIR/kappa_mon
define   chpi(x)    $KAPPA_DIR/kappa_mon
define   cmul(t)    $KAPPA_DIR/kappa_mon
define   compad(d)  $KAPPA_DIR/kappa_mon
define   compav(e)  $KAPPA_DIR/kappa_mon
define   compi(ck)  $KAPPA_DIR/kappa_mon
define   conv(olve) $KAPPA_DIR/kappa_mon
define   cref(rame) $KAPPA_DIR/kappa_mon
define   csub       $KAPPA_DIR/kappa_mon
define   div        $KAPPA_DIR/kappa_mon
define   errc(lip)  $KAPPA_DIR/kappa_mon
define   exp1(0)    $KAPPA_DIR/kappa_mon
define   expe       $KAPPA_DIR/kappa_mon
define   expo(n)    $KAPPA_DIR/kappa_mon
define   ffcl(ean)  $KAPPA_DIR/kappa_mon
define   fill(bad)  $KAPPA_DIR/kappa_mon
define   fitsu(rface) $KAPPA_DIR/kappa_mon
define   flip       $KAPPA_DIR/kappa_mon
define   four(ier)  $KAPPA_DIR/kappa_mon
define   gausm(ooth) $KAPPA_DIR/kappa_mon
define   glit(ch)   $KAPPA_DIR/kappa_mon
define   glob(als)  $KAPPA_DIR/kappa_mon
define   hista(t)   $KAPPA_DIR/kappa_mon
define   histe(q)   $KAPPA_DIR/kappa_mon
define   histo(gram) $KAPPA_DIR/kappa_mon
define   kste(st)   $KAPPA_DIR/kappa_mon
define   lapl(ace)  $KAPPA_DIR/kappa_mon
define   listmake   $KAPPA_DIR/kappa_mon
define   listshow   $KAPPA_DIR/kappa_mon
define   log1(0)    $KAPPA_DIR/kappa_mon
define   loga(r)    $KAPPA_DIR/kappa_mon
define   loge       $KAPPA_DIR/kappa_mon
define   look       $KAPPA_DIR/kappa_mon
define   lucy       $KAPPA_DIR/kappa_mon
define   makes(urface) $KAPPA_DIR/kappa_mon
define   mani(c)    $KAPPA_DIR/kappa_mon
define   math(s)    $KAPPA_DIR/kappa_mon
define   medi(an)   $KAPPA_DIR/kappa_mon
define   mem2(d)    $KAPPA_DIR/mem2d
define   mosa(ic)   $KAPPA_DIR/kappa_mon
define   msta(ts)   $KAPPA_DIR/kappa_mon
define   mult       $KAPPA_DIR/kappa_mon
define   nogl(obals) $KAPPA_DIR/kappa_mon
define   noma(gic)  $KAPPA_DIR/kappa_mon
define   norm(alize) $KAPPA_DIR/kappa_mon
define   numb       $KAPPA_DIR/kappa_mon
define   outs(et)   $KAPPA_DIR/kappa_mon
define   parg(et)   $KAPPA_DIR/kappa_mon
define   past(e)    $KAPPA_DIR/kappa_mon
define   pixd(upe)  $KAPPA_DIR/kappa_mon
define   pow        $KAPPA_DIR/kappa_mon
define   profile    $KAPPA_DIR/kappa_mon
define   psf        $KAPPA_DIR/kappa_mon
define   quil(t)    $KAPPA_DIR/kappa_mon
define   rift       $KAPPA_DIR/kappa_mon
define   rota(te)   $KAPPA_DIR/kappa_mon
define   segm(ent)  $KAPPA_DIR/kappa_mon
define   setma(gic) $KAPPA_DIR/kappa_mon
define   shad(ow)   $KAPPA_DIR/kappa_mon
define   shif(t)    $KAPPA_DIR/kappa_mon slide
define   shod(ev)   $KAPPA_DIR/kappa_mon gdnames
define   slid(e)    $KAPPA_DIR/kappa_mon
define   sqor(st)   $KAPPA_DIR/kappa_mon
define   stats      $KAPPA_DIR/kappa_mon
define   sub        $KAPPA_DIR/kappa_mon
define   subs(titute) $KAPPA_DIR/kappa_mon
define   surf(it)   $KAPPA_DIR/kappa_mon
define   thre(sh)   $KAPPA_DIR/kappa_mon
define   trand(at)  $KAPPA_DIR/kappa_mon
define   trani(nvert) $KAPPA_DIR/kappa_mon
define   tranj(oin)  $KAPPA_DIR/kappa_mon
define   tranm(ake)  $KAPPA_DIR/kappa_mon
define   trans(former) $KAPPA_DIR/kappa_mon
define   trant(race) $KAPPA_DIR/kappa_mon
define   trig       $KAPPA_DIR/kappa_mon
define   wcsalign   $KAPPA_DIR/kappa_mon
define   wien(er)   $KAPPA_DIR/kappa_mon
define   zapl(in)   $KAPPA_DIR/kappa_mon

defstring kaph(elp) !$KAPPA_DIR/kaphelp
defstring numba  print NUMBA has been withdrawn.  Use NUMB.
defstring pick2(d) print PICK2D has been superseded by NDFCOPY and SETBOUND
defstring stats2(d) print STATS2D has been superseded by STATS
defstring thresh0  print THRESH0 has been withdrawn.  Use THRESH.

{ Define KAPVIEW commands, including aliases for the now non-existent ones.
{ Use the monolith.
{define   colu(mnar) $KAPPA_DIR/kapview_mon
define   contou(r)  $KAPPA_DIR/kapview_mon
define   contov(er) $KAPPA_DIR/kapview_mon
define   crel(ut)   $KAPPA_DIR/kapview_mon
define   cursor     $KAPPA_DIR/kapview_mon
define   defpi(c)   $KAPPA_DIR/kapview_mon picdef
define   disp(lay)  $KAPPA_DIR/kapview_mon
define   draw(sig)  $KAPPA_DIR/kapview_mon
define   elpr(of)   $KAPPA_DIR/kapview_mon
define   gdcl(ear)  $KAPPA_DIR/kapview_mon
define   gdna(mes)  $KAPPA_DIR/kapview_mon
define   gdse(t)    $KAPPA_DIR/kapview_mon
define   gdst(ate)  $KAPPA_DIR/kapview_mon
define   grey(plot) $KAPPA_DIR/kapview_mon
{define   hide       $KAPPA_DIR/kapview_mon
define   idcl(ear)  $KAPPA_DIR/kapview_mon
define   idin(visible) $KAPPA_DIR/kapview_mon
define   idpan      $KAPPA_DIR/kapview_mon
define   idpaz(o)   $KAPPA_DIR/kapview_mon
define   idre(set)  $KAPPA_DIR/kapview_mon
define   idse(t)    $KAPPA_DIR/kapview_mon
define   idst(ate)  $KAPPA_DIR/kapview_mon
define   insp(ect)  $KAPPA_DIR/kapview_mon
define   linp(lot)  $KAPPA_DIR/kapview_mon
define   luta(ble)  $KAPPA_DIR/kapview_mon
define   lutfl(ip)  $KAPPA_DIR/kapview_mon
define   luthi(lite) $KAPPA_DIR/kapview_mon
define   lutro(t)   $KAPPA_DIR/kapview_mon
define   lutsa(ve)  $KAPPA_DIR/kapview_mon
define   lutt(weak) $KAPPA_DIR/kapview_mon
define   lutv(iew)  $KAPPA_DIR/kapview_mon
define   mlin(plot) $KAPPA_DIR/kapview_mon
define   ovcl(ear)  $KAPPA_DIR/kapview_mon
define   ovse(t)    $KAPPA_DIR/kapview_mon
define   pald(ef)   $KAPPA_DIR/kapview_mon
define   pale(ntry) $KAPPA_DIR/kapview_mon
define   palr(ead)  $KAPPA_DIR/kapview_mon
define   pals(ave)  $KAPPA_DIR/kapview_mon
define   picc(ur)   $KAPPA_DIR/kapview_mon
define   picd(ef)   $KAPPA_DIR/kapview_mon
define   picem(pty) $KAPPA_DIR/kapview_mon
define   pice(ntire) $KAPPA_DIR/kapview_mon
define   pici(n)    $KAPPA_DIR/kapview_mon
define   piclab(el) $KAPPA_DIR/kapview_mon
define   picli(st)  $KAPPA_DIR/kapview_mon
define   pics(el)   $KAPPA_DIR/kapview_mon
define   pict(rans) $KAPPA_DIR/kapview_mon
define   picv(is)   $KAPPA_DIR/kapview_mon
define   shod(ev)   $KAPPA_DIR/kapview_mon gdnames
define   snap(shot) $KAPPA_DIR/kapview_mon
define   turb(ocont) $KAPPA_DIR/kapview_mon
define   twea(k)    $KAPPA_DIR/kapview_mon
define   vecp(lot)  $KAPPA_DIR/kapview_mon

{ Define the NDFPACK commands.
define   axco(nv)   $KAPPA_DIR/ndfpack_mon
define   axla(bel)  $KAPPA_DIR/ndfpack_mon
define   axun(its)  $KAPPA_DIR/ndfpack_mon
define   eras(e)    $KAPPA_DIR/ndfpack_mon
define   fitsd(in)  $KAPPA_DIR/ndfpack_mon
define   fitsexp    $KAPPA_DIR/ndfpack_mon
define   fitsim(p)  $KAPPA_DIR/ndfpack_mon
define   fitsin     $KAPPA_DIR/ndfpack_mon
define   fitsl(ist) $KAPPA_DIR/ndfpack_mon
define   fitsm(od)  $KAPPA_DIR/ndfpack_mon
define   fitst(ext) $KAPPA_DIR/ndfpack_mon
define   hisc(om)   $KAPPA_DIR/ndfpack_mon
define   hisl(ist)  $KAPPA_DIR/ndfpack_mon
define   hiss(et)   $KAPPA_DIR/ndfpack_mon
define   nati(ve)   $KAPPA_DIR/ndfpack_mon
define   ndfc(opy)  $KAPPA_DIR/ndfpack_mon
define   ndft(race) $KAPPA_DIR/ndfpack_mon
define   seta(xis)  $KAPPA_DIR/ndfpack_mon
define   setba(d)   $KAPPA_DIR/ndfpack_mon
define   setbb      $KAPPA_DIR/ndfpack_mon
define   setbo(und) $KAPPA_DIR/ndfpack_mon
define   setex(t)   $KAPPA_DIR/ndfpack_mon
define   setl(abel) $KAPPA_DIR/ndfpack_mon
define   setno(rm)  $KAPPA_DIR/ndfpack_mon
define   seto(rigin) $KAPPA_DIR/ndfpack_mon
define   setsk(y)   $KAPPA_DIR/ndfpack_mon
define   setti(tle) $KAPPA_DIR/ndfpack_mon
define   setty(pe)  $KAPPA_DIR/ndfpack_mon
define   setu(nits) $KAPPA_DIR/ndfpack_mon
define   setv(ar)   $KAPPA_DIR/ndfpack_mon
define   wcsadd     $KAPPA_DIR/ndfpack_mon
define   wcsattrib  $KAPPA_DIR/ndfpack_mon
define   wcscopy    $KAPPA_DIR/ndfpack_mon
define   wcsframe   $KAPPA_DIR/ndfpack_mon
define   wcsremove  $KAPPA_DIR/ndfpack_mon

defstring fitsed(it) !$KAPPA_DIR/fitsedit.csh
defstring fitsh(ead) !$KAPPA_DIR/fitshead.csh


{ Define KAPPA's ICL procedures
set nosave
defproc  lutb(gyrw) $KAPPA_DIR/kappa_proc.icl
defproc  lutcol     $KAPPA_DIR/kappa_proc.icl
defproc  lutcon(t)  $KAPPA_DIR/kappa_proc.icl
defproc  lutfc      $KAPPA_DIR/kappa_proc.icl
defproc  lutg(rey)  $KAPPA_DIR/kappa_proc.icl
defproc  luthe(at)  $KAPPA_DIR/kappa_proc.icl
defproc  luti(kon)  $KAPPA_DIR/kappa_proc.icl
defproc  lutn(eg)   $KAPPA_DIR/kappa_proc.icl
defproc  lutra(mps) $KAPPA_DIR/kappa_proc.icl
defproc  lutre(ad)  $KAPPA_DIR/kappa_proc.icl
defproc  lutsp(ec)  $KAPPA_DIR/kappa_proc.icl
defproc  lutz(ebra) $KAPPA_DIR/kappa_proc.icl

{ Define other synonyms.
defstring fitsexi(st) fitsmod edit=exist mode=interface
defstring fitsv(al) fitsmod edit=print mode=interface
defstring fitsw(rite) fitsmod edit=write mode=interface position=!

defstring picb(ase) piclist picnum=1
defstring picd(ata) piclist name=data picnum=last
defstring picf(rame) piclist name=frame picnum=last
defstring picg(rid) picdef array 1.0 prefix=""
defstring piclas(t) piclist picnum=last
defstring picx(y)   picdef xy 1.0

{ Define KAPPA's help from ICL.  Note that kaphelp points to help kappa
{ because kaphelp doesn't work from UNIX ICL.
defhelp  add        $KAPPA_HELP
defhelp  aperadd    $KAPPA_HELP
defhelp  ardgen     $KAPPA_HELP
defhelp  ardmask    $KAPPA_HELP
defhelp  axconv     $KAPPA_HELP
defhelp  axlabel    $KAPPA_HELP
defhelp  axunits    $KAPPA_HELP
defhelp  block      $KAPPA_HELP
defhelp  cadd       $KAPPA_HELP
defhelp  calc       $KAPPA_HELP
defhelp  calpol     $KAPPA_HELP
defhelp  cdiv       $KAPPA_HELP
defhelp  centroid   $KAPPA_HELP
defhelp  chpix      $KAPPA_HELP
defhelp  cmult      $KAPPA_HELP
defhelp  compadd    $KAPPA_HELP
defhelp  compave    $KAPPA_HELP
defhelp  compick    $KAPPA_HELP
defhelp  contour    $KAPPA_HELP
defhelp  contover   $KAPPA_HELP
defhelp  convolve   $KAPPA_HELP
defhelp  creframe   $KAPPA_HELP
defhelp  crelut     $KAPPA_HELP
defhelp  csub       $KAPPA_HELP
defhelp  cursor     $KAPPA_HELP
defhelp  defpic     $KAPPA_HELP picdef
defhelp  display    $KAPPA_HELP
defhelp  div        $KAPPA_HELP
defhelp  drawsig    $KAPPA_HELP
defhelp  elprof     $KAPPA_HELP
defhelp  erase      $KAPPA_HELP
defhelp  errclip    $KAPPA_HELP
defhelp  exp10      $KAPPA_HELP
defhelp  expe       $KAPPA_HELP
defhelp  expon      $KAPPA_HELP
defhelp  ffclean    $KAPPA_HELP
defhelp  fillbad    $KAPPA_HELP
defhelp  fitsdin    $KAPPA_HELP
defhelp  fitsedit   $KAPPA_HELP
defhelp  fitsexp    $KAPPA_HELP
defhelp  fitshead   $KAPPA_HELP
defhelp  fitsimp    $KAPPA_HELP
defhelp  fitsin     $KAPPA_HELP
defhelp  fitslist   $KAPPA_HELP
defhelp  fitsmod    $KAPPA_HELP
defhelp  fitstext   $KAPPA_HELP
defhelp  fitsurface $KAPPA_HELP
defhelp  flip       $KAPPA_HELP
defhelp  fourier    $KAPPA_HELP
defhelp  gausmooth  $KAPPA_HELP
defhelp  gdclear    $KAPPA_HELP
defhelp  gdnames    $KAPPA_HELP
defhelp  gdset      $KAPPA_HELP
defhelp  gdstate    $KAPPA_HELP
defhelp  glitch     $KAPPA_HELP
defhelp  globals    $KAPPA_HELP
defhelp  greyplot   $KAPPA_HELP
defhelp  hiscom     $KAPPA_HELP
defhelp  hislist    $KAPPA_HELP
defhelp  hisset     $KAPPA_HELP
defhelp  histat     $KAPPA_HELP
defhelp  histeq     $KAPPA_HELP
defhelp  histogram  $KAPPA_HELP
defhelp  idclear    $KAPPA_HELP
defhelp  idinvisible $KAPPA_HELP
defhelp  idpan      $KAPPA_HELP
defhelp  idpazo     $KAPPA_HELP
defhelp  idreset    $KAPPA_HELP
defhelp  idset      $KAPPA_HELP
defhelp  idstate    $KAPPA_HELP
defhelp  inspect    $KAPPA_HELP
defhelp  kaphelp    $KAPPA_HELP
defhelp  kappa      $KAPPA_HELP 0
defhelp  kstest     $KAPPA_HELP
defhelp  laplace    $KAPPA_HELP
defhelp  linplot    $KAPPA_HELP
defhelp  listmake   $KAPPA_HELP
defhelp  listshow   $KAPPA_HELP
defhelp  log10      $KAPPA_HELP
defhelp  logar      $KAPPA_HELP
defhelp  loge       $KAPPA_HELP
defhelp  look       $KAPPA_HELP
defhelp  lucy       $KAPPA_HELP
defhelp  lutable    $KAPPA_HELP
defhelp  lutbgyrw   $KAPPA_HELP
defhelp  lutcol     $KAPPA_HELP
defhelp  lutcont    $KAPPA_HELP
defhelp  lutfc      $KAPPA_HELP
defhelp  lutflip    $KAPPA_HELP
defhelp  lutgrey    $KAPPA_HELP
defhelp  lutheat    $KAPPA_HELP
defhelp  luthilite  $KAPPA_HELP
defhelp  lutikon    $KAPPA_HELP
defhelp  lutneg     $KAPPA_HELP
defhelp  lutramps   $KAPPA_HELP
defhelp  lutread    $KAPPA_HELP
defhelp  lutrot     $KAPPA_HELP
defhelp  lutsave    $KAPPA_HELP
defhelp  lutspec    $KAPPA_HELP
defhelp  luttweak   $KAPPA_HELP
defhelp  lutview    $KAPPA_HELP
defhelp  lutzebra   $KAPPA_HELP
defhelp  makesurface $KAPPA_HELP
defhelp  manic      $KAPPA_HELP
defhelp  maths      $KAPPA_HELP
defhelp  median     $KAPPA_HELP
defhelp  mem2d      $KAPPA_HELP
defhelp  mlinplot   $KAPPA_HELP
defhelp  mosaic     $KAPPA_HELP
defhelp  mstats     $KAPPA_HELP
defhelp  mult       $KAPPA_HELP
defhelp  native     $KAPPA_HELP
defhelp  ndfcopy    $KAPPA_HELP
defhelp  ndftrace   $KAPPA_HELP
defhelp  noglobals  $KAPPA_HELP
defhelp  nomagic    $KAPPA_HELP
defhelp  normalize  $KAPPA_HELP
defhelp  numb       $KAPPA_HELP
defhelp  outset     $KAPPA_HELP
defhelp  ovclear    $KAPPA_HELP
defhelp  ovset      $KAPPA_HELP
defhelp  paldef     $KAPPA_HELP
defhelp  palentry   $KAPPA_HELP
defhelp  palread    $KAPPA_HELP
defhelp  palsave    $KAPPA_HELP
defhelp  parget     $KAPPA_HELP
defhelp  paste      $KAPPA_HELP
defhelp  piccur     $KAPPA_HELP
defhelp  picdef     $KAPPA_HELP
defhelp  picempty   $KAPPA_HELP
defhelp  picentire  $KAPPA_HELP
defhelp  picin      $KAPPA_HELP
defhelp  piclabel   $KAPPA_HELP
defhelp  piclist    $KAPPA_HELP
defhelp  picsel     $KAPPA_HELP
defhelp  pictrans   $KAPPA_HELP
defhelp  picvis     $KAPPA_HELP
defhelp  pixdupe    $KAPPA_HELP
defhelp  pow        $KAPPA_HELP
defhelp  profile    $KAPPA_HELP
defhelp  psf        $KAPPA_HELP
defhelp  quilt      $KAPPA_HELP
defhelp  rift       $KAPPA_HELP
defhelp  rotate     $KAPPA_HELP
defhelp  segment    $KAPPA_HELP
defhelp  setaxis    $KAPPA_HELP
defhelp  setbad     $KAPPA_HELP
defhelp  setbb      $KAPPA_HELP
defhelp  setbound   $KAPPA_HELP
defhelp  setext     $KAPPA_HELP
defhelp  setlabel   $KAPPA_HELP
defhelp  setmagic   $KAPPA_HELP
defhelp  setnorm    $KAPPA_HELP
defhelp  setorigin  $KAPPA_HELP
defhelp  setsky     $KAPPA_HELP
defhelp  settitle   $KAPPA_HELP
defhelp  settype    $KAPPA_HELP
defhelp  setunits   $KAPPA_HELP
defhelp  setvar     $KAPPA_HELP
defhelp  shadow     $KAPPA_HELP
defhelp  shift      $KAPPA_HELP slide
defhelp  shodev     $KAPPA_HELP gdnames
defhelp  slide      $KAPPA_HELP
defhelp  snapshot   $KAPPA_HELP
defhelp  sqorst     $KAPPA_HELP
defhelp  stats      $KAPPA_HELP
defhelp  sub        $KAPPA_HELP
defhelp  substitute $KAPPA_HELP
defhelp  surfit     $KAPPA_HELP
defhelp  thresh     $KAPPA_HELP
defhelp  trandat    $KAPPA_HELP
defhelp  trandat    $KAPPA_HELP
defhelp  traninvert $KAPPA_HELP
defhelp  tranjoin   $KAPPA_HELP
defhelp  tranmake   $KAPPA_HELP
defhelp  transformer $KAPPA_HELP
defhelp  trantrace  $KAPPA_HELP
defhelp  trig       $KAPPA_HELP
defhelp  turbocont  $KAPPA_HELP
defhelp  tweak      $KAPPA_HELP
defhelp  vecplot    $KAPPA_HELP
defhelp  wcsadd     $KAPPA_HELP
defhelp  wcsalign   $KAPPA_HELP
defhelp  wcsattrib  $KAPPA_HELP
defhelp  wcscopy    $KAPPA_HELP
defhelp  wcsframe   $KAPPA_HELP
defhelp  wcsremove  $KAPPA_HELP
defhelp  wiener     $KAPPA_HELP
defhelp  zaplin     $KAPPA_HELP

{defhelp  columnar   $KAPPA_HELP
{defhelp  hide       $KAPPA_HELP

{ Define help for synonyms.
defhelp  fitsexist  $KAPPA_HELP
defhelp  fitsval    $KAPPA_HELP
defhelp  fitswrite  $KAPPA_HELP
defhelp  picbase    $KAPPA_HELP
defhelp  picdata    $KAPPA_HELP
defhelp  picframe   $KAPPA_HELP
defhelp  picgrid    $KAPPA_HELP
defhelp  piclast    $KAPPA_HELP
defhelp  picxy      $KAPPA_HELP

{ Define the full commands for KAPPA.
define   kap_add        $KAPPA_DIR/kappa_mon add
define   kap_aper(add)  $KAPPA_DIR/kappa_mon aperadd
define   kap_ardg(en)   $KAPPA_DIR/kappa_mon ardgen
define   kap_ardm(ask)  $KAPPA_DIR/kappa_mon ardmask
define   kap_bloc(k)    $KAPPA_DIR/kappa_mon block
define   kap_cadd       $KAPPA_DIR/kappa_mon cadd
define   kap_calc       $KAPPA_DIR/kappa_mon calc
define   kap_calp(ol)   $KAPPA_DIR/kappa_mon calpol
define   kap_cdiv       $KAPPA_DIR/kappa_mon cdiv
define   kap_cent(roid) $KAPPA_DIR/kappa_mon centroid
define   kap_chpi(x)    $KAPPA_DIR/kappa_mon chpix
define   kap_cmul(t)    $KAPPA_DIR/kappa_mon cmult
define   kap_compad(d)  $KAPPA_DIR/kappa_mon compadd
define   kap_compav(e)  $KAPPA_DIR/kappa_mon compave
define   kap_compi(ck)  $KAPPA_DIR/kappa_mon compick
define   kap_conv(olve) $KAPPA_DIR/kappa_mon convolve
define   kap_cref(rame) $KAPPA_DIR/kappa_mon creframe
define   kap_csub       $KAPPA_DIR/kappa_mon csub
define   kap_div        $KAPPA_DIR/kappa_mon div
define   kap_errc(lip)  $KAPPA_DIR/kappa_mon errclip
define   kap_exp1(0)    $KAPPA_DIR/kappa_mon exp10
define   kap_expe       $KAPPA_DIR/kappa_mon expe
define   kap_expo(n)    $KAPPA_DIR/kappa_mon expon
define   kap_ffcl(ean)  $KAPPA_DIR/kappa_mon ffclean
define   kap_fill(bad)  $KAPPA_DIR/kappa_mon fillbad
define   kap_fitsu(rface) $KAPPA_DIR/kappa_mon fitsurface
define   kap_flip       $KAPPA_DIR/kappa_mon flip
define   kap_four(ier)  $KAPPA_DIR/kappa_mon fourier
define   kap_gausm(ooth) $KAPPA_DIR/kappa_mon gausmooth
define   kap_glit(ch)   $KAPPA_DIR/kappa_mon glitch
define   kap_glob(als)  $KAPPA_DIR/kappa_mon globals
define   kap_hista(t)   $KAPPA_DIR/kappa_mon histat
define   kap_histe(q)   $KAPPA_DIR/kappa_mon histeq
define   kap_histo(gram) $KAPPA_DIR/kappa_mon histogram
define   kap_kste(st)   $KAPPA_DIR/kappa_mon kstest
define   kap_lapl(ace)  $KAPPA_DIR/kappa_mon laplace
define   kap_listmake   $KAPPA_DIR/kappa_mon listmake
define   kap_listshow   $KAPPA_DIR/kappa_mon listshow
define   kap_log1(0)    $KAPPA_DIR/kappa_mon log10
define   kap_loga(r)    $KAPPA_DIR/kappa_mon logar
define   kap_loge       $KAPPA_DIR/kappa_mon loge
define   kap_look       $KAPPA_DIR/kappa_mon look
define   kap_lucy       $KAPPA_DIR/kappa_mon lucy
define   kap_makes(urface) $KAPPA_DIR/kappa_mon makesurface
define   kap_mani(c)    $KAPPA_DIR/kappa_mon manic
define   kap_math(s)    $KAPPA_DIR/kappa_mon maths
define   kap_medi(an)   $KAPPA_DIR/kappa_mon median
define   kap_mem2(d)    $KAPPA_DIR/mem2d
define   kap_mosa(ic)   $KAPPA_DIR/kappa_mon mosaic
define   kap_msta(ts)   $KAPPA_DIR/kappa_mon mstats
define   kap_mult       $KAPPA_DIR/kappa_mon mult
define   kap_nogl(obals) $KAPPA_DIR/kappa_mon noglobals
define   kap_noma(gic)  $KAPPA_DIR/kappa_mon nomagic
define   kap_norm(alize) $KAPPA_DIR/kappa_mon normalize
define   kap_numb       $KAPPA_DIR/kappa_mon numb
define   kap_outs(et)   $KAPPA_DIR/kappa_mon outset
define   kap_parg(et)   $KAPPA_DIR/kappa_mon parget
define   kap_past(e)    $KAPPA_DIR/kappa_mon paste
define   kap_pixd(upe)  $KAPPA_DIR/kappa_mon pixdupe
define   kap_pow        $KAPPA_DIR/kappa_mon pow
define   kap_profile    $KAPPA_DIR/kappa_mon profile
define   kap_psf        $KAPPA_DIR/kappa_mon psf
define   kap_quil(t)    $KAPPA_DIR/kappa_mon quilt
define   kap_rift       $KAPPA_DIR/kappa_mon rift
define   kap_rota(te)   $KAPPA_DIR/kappa_mon rotate
define   kap_segm(ent)  $KAPPA_DIR/kappa_mon segment
define   kap_setma(gic) $KAPPA_DIR/kappa_mon setmagic
define   kap_shad(ow)   $KAPPA_DIR/kappa_mon shadow
define   kap_shif(t)    $KAPPA_DIR/kappa_mon slide
define   kap_shod(ev)   $KAPPA_DIR/kappa_mon gdnames
define   kap_slid(e)    $KAPPA_DIR/kappa_mon slide
define   kap_sqor(st)   $KAPPA_DIR/kappa_mon sqorst
define   kap_stats      $KAPPA_DIR/kappa_mon stats
define   kap_sub        $KAPPA_DIR/kappa_mon sub
define   kap_subs(titute) $KAPPA_DIR/kappa_mon substitute
define   kap_surf(it)   $KAPPA_DIR/kappa_mon surfit
define   kap_thre(sh)   $KAPPA_DIR/kappa_mon thresh
define   kap_trand(at)  $KAPPA_DIR/kappa_mon trandat
define   kap_trani(nvert) $KAPPA_DIR/kappa_mon traninvert
define   kap_tranj(oin)  $KAPPA_DIR/kappa_mon tranjoin
define   kap_tranm(ake)  $KAPPA_DIR/kappa_mon tranmake
define   kap_trans(former) $KAPPA_DIR/kappa_mon transformer
define   kap_trant(race) $KAPPA_DIR/kappa_mon trantrace
define   kap_trig       $KAPPA_DIR/kappa_mon trig
define   kap_wcsalign   $KAPPA_DIR/kappa_mon wcsalign
define   kap_wien(er)   $KAPPA_DIR/kappa_mon wiener
define   kap_zapl(in)   $KAPPA_DIR/kappa_mon zaplin

defstring kap_kaph(elp) !$KAPPA_DIR/kaphelp
defstring kap_numba  print NUMBA has been withdrawn.  Use NUMB.
defstring kap_pick2(d) print PICK2D has been superseded by NDFCOPY and SETBOUND
defstring kap_stats2(d) print STATS2D has been superseded by STATS
defstring kap_thresh0  print THRESH0 has been withdrawn.  Use THRESH.

{ Define full commands for KAPVIEW.
{define   kap_colu(mnar) $KAPPA_DIR/kapview_mon columnar
define   kap_contou(r)  $KAPPA_DIR/kapview_mon contour
define   kap_contov(er) $KAPPA_DIR/kapview_mon contover
define   kap_crel(ut)   $KAPPA_DIR/kapview_mon crelut
define   kap_cursor     $KAPPA_DIR/kapview_mon cursor
define   kap_defpi(c)   $KAPPA_DIR/kapview_mon picdef
define   kap_disp(lay)  $KAPPA_DIR/kapview_mon display
define   kap_draw(sig)  $KAPPA_DIR/kapview_mon drawsig
define   kap_elpr(of)   $KAPPA_DIR/kapview_mon elprof
define   kap_gdcl(ear)  $KAPPA_DIR/kapview_mon gdclear
define   kap_gdna(mes)  $KAPPA_DIR/kapview_mon gdnames
define   kap_gdse(t)    $KAPPA_DIR/kapview_mon gdset
define   kap_gdst(ate)  $KAPPA_DIR/kapview_mon gdstate
define   kap_grey(plot) $KAPPA_DIR/kapview_mon greypplot
{define   kap_hide       $KAPPA_DIR/kapview_mon hide
define   kap_idcl(ear)  $KAPPA_DIR/kapview_mon idclear
define   kap_idin(visible) $KAPPA_DIR/kapview_mon idinvisible
define   kap_idpan      $KAPPA_DIR/kapview_mon idpan
define   kap_idpaz(o)   $KAPPA_DIR/kapview_mon idpazo
define   kap_idre(set)  $KAPPA_DIR/kapview_mon idreset
define   kap_idse(t)    $KAPPA_DIR/kapview_mon idset
define   kap_idst(ate)  $KAPPA_DIR/kapview_mon idstate
define   kap_insp(ect)  $KAPPA_DIR/kapview_mon inspect
define   kap_linp(lot)  $KAPPA_DIR/kapview_mon linplot
define   kap_luta(ble)  $KAPPA_DIR/kapview_mon lutable
define   kap_lutfl(ip)  $KAPPA_DIR/kapview_mon lutflip
define   kap_luthi(lite) $KAPPA_DIR/kapview_mon luthilite
define   kap_lutro(t)   $KAPPA_DIR/kapview_mon lutrot
define   kap_lutsa(ve)  $KAPPA_DIR/kapview_mon lutsave
define   kap_lutt(weak) $KAPPA_DIR/kapview_mon luttweak
define   kap_lutv(iew)  $KAPPA_DIR/kapview_mon lutview
define   kap_mlin(plot) $KAPPA_DIR/kapview_mon mlinplot
define   kap_ovcl(ear)  $KAPPA_DIR/kapview_mon ovclear
define   kap_ovse(t)    $KAPPA_DIR/kapview_mon ovset
define   kap_pald(ef)   $KAPPA_DIR/kapview_mon paldef
define   kap_pale(ntry) $KAPPA_DIR/kapview_mon palentry
define   kap_palr(ead)  $KAPPA_DIR/kapview_mon palread
define   kap_pals(ave)  $KAPPA_DIR/kapview_mon palsave
define   kap_picc(ur)   $KAPPA_DIR/kapview_mon piccur
define   kap_picd(ef)   $KAPPA_DIR/kapview_mon picdef
define   kap_picem(pty) $KAPPA_DIR/kapview_mon picempty
define   kap_picen(tire) $KAPPA_DIR/kapview_mon picentire
define   kap_pici(n)    $KAPPA_DIR/kapview_mon picin
define   kap_piclab(el) $KAPPA_DIR/kapview_mon piclabel
define   kap_picli(st)  $KAPPA_DIR/kapview_mon piclist
define   kap_pics(el)   $KAPPA_DIR/kapview_mon picsel
define   kap_pict(rans) $KAPPA_DIR/kapview_mon pictrans
define   kap_picv(is)   $KAPPA_DIR/kapview_mon picvis
define   kap_shod(ev)   $KAPPA_DIR/kapview_mon gdnames
define   kap_snap(shot) $KAPPA_DIR/kapview_mon snapshot
define   kap_turb(ocont) $KAPPA_DIR/kapview_mon turbocont
define   kap_twea(k)    $KAPPA_DIR/kapview_mon tweak
define   kap_vecp(lot)  $KAPPA_DIR/kapview_mon vecplot

{ Define the NDFPACK commands.
define   kap_axco(nv)   $KAPPA_DIR/ndfpack_mon axconv
define   kap_axla(bel)  $KAPPA_DIR/ndfpack_mon axlabel
define   kap_axun(its)  $KAPPA_DIR/ndfpack_mon axunits
define   kap_eras(e)    $KAPPA_DIR/ndfpack_mon erase
define   kap_fitsd(in)  $KAPPA_DIR/ndfpack_mon fitsdin
define   kap_fitsexp    $KAPPA_DIR/ndfpack_mon fitsexp
define   kap_fitsim(p)  $KAPPA_DIR/ndfpack_mon fitsimp
define   kap_fitsin     $KAPPA_DIR/ndfpack_mon fitsin
define   kap_fitsl(ist) $KAPPA_DIR/ndfpack_mon fitslist
define   kap_fitsm(od)  $KAPPA_DIR/ndfpack_mon fitsmod
define   kap_fitst(ext) $KAPPA_DIR/ndfpack_mon fitstext
define   kap_hisc(om)   $KAPPA_DIR/ndfpack_mon hiscom
define   kap_hisl(ist)  $KAPPA_DIR/ndfpack_mon hislist
define   kap_hiss(et)   $KAPPA_DIR/ndfpack_mon hisset
define   kap_nati(ve)   $KAPPA_DIR/ndfpack_mon native
define   kap_ndfc(opy)  $KAPPA_DIR/ndfpack_mon ndfcopy
define   kap_ndft(race) $KAPPA_DIR/ndfpack_mon ndftrace
define   kap_seta(xis)  $KAPPA_DIR/ndfpack_mon setaxis
define   kap_setba(d)   $KAPPA_DIR/ndfpack_mon setbad
define   kap_setbb      $KAPPA_DIR/ndfpack_mon setbb
define   kap_setbo(und) $KAPPA_DIR/ndfpack_mon setbound
define   kap_setex(t)   $KAPPA_DIR/ndfpack_mon setext
define   kap_setl(abel) $KAPPA_DIR/ndfpack_mon setlabel
define   kap_setno(rm)  $KAPPA_DIR/ndfpack_mon setnorm
define   kap_seto(rigin) $KAPPA_DIR/ndfpack_mon setorigin
define   kap_setsk(y)   $KAPPA_DIR/ndfpack_mon setsky
define   kap_setti(tle) $KAPPA_DIR/ndfpack_mon settitle
define   kap_setty(pe)  $KAPPA_DIR/ndfpack_mon settype
define   kap_setu(nits) $KAPPA_DIR/ndfpack_mon setunits
define   kap_setv(ar)   $KAPPA_DIR/ndfpack_mon setvar
define   kap_wcsadd     $KAPPA_DIR/ndfpack_mon wcsadd
define   kap_wcsattrib  $KAPPA_DIR/ndfpack_mon wcsattrib
define   kap_wcscopy    $KAPPA_DIR/ndfpack_mon wcscopy
define   kap_wcsframe   $KAPPA_DIR/ndfpack_mon wcsframe
define   kap_wcsremove  $KAPPA_DIR/ndfpack_mon wcsremove

defstring kap_fitsed(it) !$KAPPA_DIR/fitsedit.csh
defstring kap_fitsh(ead) !$KAPPA_DIR/fitshead.csh

{ Define KAPPA's ICL procedures for the full names.
set nosave
defproc  kap_lutb(gyrw) $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutcol     $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutcon(t)  $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutfc      $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutg(rey)  $KAPPA_DIR/kappa_proc.icl
defproc  kap_luthe(at)  $KAPPA_DIR/kappa_proc.icl
defproc  kap_luti(kon)  $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutn(eg)   $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutra(mps) $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutre(ad)  $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutsp(ec)  $KAPPA_DIR/kappa_proc.icl
defproc  kap_lutz(ebra) $KAPPA_DIR/kappa_proc.icl

{ Define full names for other synonyms.
defstring kap_fitsexi(st) fitsmod edit=exist mode=interface
defstring kap_fitsv(al) fitsmod edit=print mode=interface
defstring kap_fitsw(rite) fitsmod edit=write mode=interface position=!
defstring kap_fitsexi(st) fitsmod mode=exist
defstring kap_fitsv(al) fitsmod mode=print
defstring kap_fitsw(rite) fitsmod mode=write
defstring kap_picb(ase) piclist picnum=1
defstring kap_picd(ata) piclist name=data picnum=last
defstring kap_picf(rame) piclist name=frame picnum=last
defstring kap_picg(rid) picdef array 1.0 prefix=""
defstring kap_piclas(t) piclist picnum=last
defstring kap_picx(y)   picdef xy 1.0

{ Define KAPPA's help from ICL.
defhelp  kap_add        $KAPPA_HELP add
defhelp  kap_aperadd    $KAPPA_HELP aperadd
defhelp  kap_ardgen     $KAPPA_HELP ardgen
defhelp  kap_ardmask    $KAPPA_HELP ardmask
defhelp  kap_axconv     $KAPPA_HELP axconv
defhelp  kap_axlabel    $KAPPA_HELP axlabel
defhelp  kap_axunits    $KAPPA_HELP axunits
defhelp  kap_block      $KAPPA_HELP block
defhelp  kap_cadd       $KAPPA_HELP cadd
defhelp  kap_calc       $KAPPA_HELP calc
defhelp  kap_calpol     $KAPPA_HELP calpol
defhelp  kap_cdiv       $KAPPA_HELP cdiv
defhelp  kap_centroid   $KAPPA_HELP centroid
defhelp  kap_chpix      $KAPPA_HELP chpix
defhelp  kap_cmult      $KAPPA_HELP cmult
defhelp  kap_compadd    $KAPPA_HELP compadd
defhelp  kap_compave    $KAPPA_HELP compave
defhelp  kap_compick    $KAPPA_HELP compick
defhelp  kap_contour    $KAPPA_HELP contour
defhelp  kap_contover   $KAPPA_HELP contover
defhelp  kap_convolve   $KAPPA_HELP convolve
defhelp  kap_creframe   $KAPPA_HELP creframe
defhelp  kap_crelut     $KAPPA_HELP crelut
defhelp  kap_csub       $KAPPA_HELP csub
defhelp  kap_cursor     $KAPPA_HELP cursor
defhelp  kap_defpic     $KAPPA_HELP picdef
defhelp  kap_display    $KAPPA_HELP display
defhelp  kap_div        $KAPPA_HELP div
defhelp  kap_drawsig    $KAPPA_HELP drawsig
defhelp  kap_elprof     $KAPPA_HELP elprof
defhelp  kap_erase      $KAPPA_HELP erase
defhelp  kap_errclip    $KAPPA_HELP errclip
defhelp  kap_exp10      $KAPPA_HELP exp10
defhelp  kap_expe       $KAPPA_HELP expe
defhelp  kap_expon      $KAPPA_HELP expon
defhelp  kap_ffclean    $KAPPA_HELP ffclean
defhelp  kap_fillbad    $KAPPA_HELP fillbad
defhelp  kap_fitsdin    $KAPPA_HELP fitsdin
defhelp  kap_fitsedit   $KAPPA_HELP fitsedit
defhelp  kap_fitsexp    $KAPPA_HELP fitsexp
defhelp  kap_fitshead   $KAPPA_HELP fitshead
defhelp  kap_fitsimp    $KAPPA_HELP fitsimp
defhelp  kap_fitsin     $KAPPA_HELP fitsin
defhelp  kap_fitslist   $KAPPA_HELP fitslist
defhelp  kap_fitsmod    $KAPPA_HELP fitsmod
defhelp  kap_fitstext   $KAPPA_HELP fitstext
defhelp  kap_fitsurface $KAPPA_HELP fitsurface
defhelp  kap_flip       $KAPPA_HELP flip
defhelp  kap_fourier    $KAPPA_HELP fourier
defhelp  kap_gausmooth  $KAPPA_HELP gausmooth
defhelp  kap_gdclear    $KAPPA_HELP gdclear
defhelp  kap_gdnames    $KAPPA_HELP gdnames
defhelp  kap_gdset      $KAPPA_HELP gdset
defhelp  kap_gdstate    $KAPPA_HELP gdstate
defhelp  kap_glitch     $KAPPA_HELP glitch
defhelp  kap_globals    $KAPPA_HELP globals
defhelp  kap_greyplot   $KAPPA_HELP greyplot
defhelp  kap_hiscom     $KAPPA_HELP hiscom
defhelp  kap_hislist    $KAPPA_HELP hislist
defhelp  kap_hisset     $KAPPA_HELP hisset
defhelp  kap_histat     $KAPPA_HELP histat
defhelp  kap_histeq     $KAPPA_HELP histeq
defhelp  kap_histogram  $KAPPA_HELP histogram
defhelp  kap_idclear    $KAPPA_HELP idclear
defhelp  kap_idinvisible $KAPPA_HELP idinvisible
defhelp  kap_idpan      $KAPPA_HELP idpan
defhelp  kap_idpazo     $KAPPA_HELP idpazo
defhelp  kap_idreset    $KAPPA_HELP idreset
defhelp  kap_idset      $KAPPA_HELP idset
defhelp  kap_idstate    $KAPPA_HELP idstate
defhelp  kap_inspect    $KAPPA_HELP inspect
defhelp  kap_kaphelp    $KAPPA_HELP kaphelp
defhelp  kap_kappa      $KAPPA_HELP 0
defhelp  kap_kstest     $KAPPA_HELP kstest
defhelp  kap_laplace    $KAPPA_HELP laplace
defhelp  kap_linplot    $KAPPA_HELP linplot
defhelp  kap_listmake   $KAPPA_HELP listmake
defhelp  kap_listshow   $KAPPA_HELP listshow
defhelp  kap_log10      $KAPPA_HELP log10
defhelp  kap_logar      $KAPPA_HELP logar
defhelp  kap_loge       $KAPPA_HELP loge
defhelp  kap_look       $KAPPA_HELP look
defhelp  kap_lucy       $KAPPA_HELP lucy
defhelp  kap_lutable    $KAPPA_HELP lutable
defhelp  kap_lutbgyrw   $KAPPA_HELP lutbgyrw
defhelp  kap_lutcol     $KAPPA_HELP lutcol
defhelp  kap_lutcont    $KAPPA_HELP lutcont
defhelp  kap_lutfc      $KAPPA_HELP lutfc
defhelp  kap_lutflip    $KAPPA_HELP lutflip
defhelp  kap_lutgrey    $KAPPA_HELP lutgrey
defhelp  kap_lutheat    $KAPPA_HELP lutheat
defhelp  kap_luthilite  $KAPPA_HELP luthilite
defhelp  kap_lutikon    $KAPPA_HELP lutikon
defhelp  kap_lutneg     $KAPPA_HELP lutneg
defhelp  kap_lutramps   $KAPPA_HELP lutramps
defhelp  kap_lutread    $KAPPA_HELP lutread
defhelp  kap_lutrot     $KAPPA_HELP lutrot
defhelp  kap_lutsave    $KAPPA_HELP lutsave
defhelp  kap_lutspec    $KAPPA_HELP lutspec
defhelp  kap_luttweak   $KAPPA_HELP luttweak
defhelp  kap_lutview    $KAPPA_HELP lutview
defhelp  kap_lutzebra   $KAPPA_HELP lutzebra
defhelp  kap_makesurface $KAPPA_HELP makesurface
defhelp  kap_manic      $KAPPA_HELP manic
defhelp  kap_maths      $KAPPA_HELP maths
defhelp  kap_median     $KAPPA_HELP median
defhelp  kap_mem2d      $KAPPA_HELP mem2d
defhelp  kap_mlinplot   $KAPPA_HELP mlinplot
defhelp  kap_mosaic     $KAPPA_HELP mosaic
defhelp  kap_mstats     $KAPPA_HELP mstats
defhelp  kap_mult       $KAPPA_HELP mult
defhelp  kap_native     $KAPPA_HELP native
defhelp  kap_ndfcopy    $KAPPA_HELP ndfcopy
defhelp  kap_ndftrace   $KAPPA_HELP ndftrace
defhelp  kap_noglobals  $KAPPA_HELP noglobals
defhelp  kap_nomagic    $KAPPA_HELP nomagic
defhelp  kap_normalize  $KAPPA_HELP normalize
defhelp  kap_numb       $KAPPA_HELP numb
defhelp  kap_numba      $KAPPA_HELP numba
defhelp  kap_outset     $KAPPA_HELP outset
defhelp  kap_ovclear    $KAPPA_HELP ovclear
defhelp  kap_ovset      $KAPPA_HELP ovset
defhelp  kap_paldef     $KAPPA_HELP paldef
defhelp  kap_palentry   $KAPPA_HELP palentry
defhelp  kap_palread    $KAPPA_HELP palread
defhelp  kap_palsave    $KAPPA_HELP palsave
defhelp  kap_parget     $KAPPA_HELP parget
defhelp  kap_paste      $KAPPA_HELP paste
defhelp  kap_piccur     $KAPPA_HELP piccur
defhelp  kap_picdef     $KAPPA_HELP picdef
defhelp  kap_picempty   $KAPPA_HELP picempty
defhelp  kap_picentire  $KAPPA_HELP picentire
defhelp  kap_picin      $KAPPA_HELP picin
defhelp  kap_pick2d     $KAPPA_HELP pick2d
defhelp  kap_piclabel   $KAPPA_HELP piclabel
defhelp  kap_piclist    $KAPPA_HELP piclist
defhelp  kap_picsel     $KAPPA_HELP picsel
defhelp  kap_pictrans   $KAPPA_HELP pictrans
defhelp  kap_picvis     $KAPPA_HELP picvis
defhelp  kap_pixdupe    $KAPPA_HELP pixdupe
defhelp  kap_pow        $KAPPA_HELP pow
defhelp  kap_profile    $KAPPA_HELP profile
defhelp  kap_psf        $KAPPA_HELP psf
defhelp  kap_quilt      $KAPPA_HELP quilt
defhelp  kap_rift       $KAPPA_HELP rift
defhelp  kap_rotate     $KAPPA_HELP rotate
defhelp  kap_segment    $KAPPA_HELP segment
defhelp  kap_setaxis    $KAPPA_HELP setaxis
defhelp  kap_setbad     $KAPPA_HELP setbad
defhelp  kap_setbb      $KAPPA_HELP setbb
defhelp  kap_setbound   $KAPPA_HELP setbound
defhelp  kap_setext     $KAPPA_HELP setext
defhelp  kap_setlabel   $KAPPA_HELP setlabel
defhelp  kap_setmagic   $KAPPA_HELP setmagic
defhelp  kap_setnorm    $KAPPA_HELP setnorm
defhelp  kap_setorigin  $KAPPA_HELP setorigin
defhelp  kap_setsky     $KAPPA_HELP setsky
defhelp  kap_settitle   $KAPPA_HELP settitle
defhelp  kap_settype    $KAPPA_HELP settype
defhelp  kap_setunits   $KAPPA_HELP setunits
defhelp  kap_setvar     $KAPPA_HELP setvar
defhelp  kap_shadow     $KAPPA_HELP shadow
defhelp  kap_shift      $KAPPA_HELP slide
defhelp  kap_shodev     $KAPPA_HELP gdnames
defhelp  kap_slide      $KAPPA_HELP slide
defhelp  kap_snapshot   $KAPPA_HELP snapshot
defhelp  kap_sqorst     $KAPPA_HELP sqorst
defhelp  kap_stats      $KAPPA_HELP stats
defhelp  kap_stats2d    $KAPPA_HELP stats2d
defhelp  kap_sub        $KAPPA_HELP sub
defhelp  kap_substitute $KAPPA_HELP substitute
defhelp  kap_surfit     $KAPPA_HELP surfit
defhelp  kap_thresh     $KAPPA_HELP thresh
defhelp  kap_thresh0    $KAPPA_HELP thresh0
defhelp  kap_trandat    $KAPPA_HELP trandat
defhelp  kap_traninvert $KAPPA_HELP traninvert
defhelp  kap_tranjoin   $KAPPA_HELP tranjoin
defhelp  kap_tranmake   $KAPPA_HELP tranmake
defhelp  kap_transformer $KAPPA_HELP transformer
defhelp  kap_trantrace  $KAPPA_HELP trantrace
defhelp  kap_trig       $KAPPA_HELP trig
defhelp  kap_turbocont  $KAPPA_HELP turbocont
defhelp  kap_tweak      $KAPPA_HELP tweak
defhelp  kap_vecplot    $KAPPA_HELP vecplot
defhelp  kap_wcsadd     $KAPPA_HELP wcsadd
defhelp  kap_wcsalign   $KAPPA_HELP wcsalign
defhelp  kap_wcsattrib  $KAPPA_HELP wcsattrib
defhelp  kap_wcsframe   $KAPPA_HELP wcsframe
defhelp  kap_wcsremove  $KAPPA_HELP wcsremove
defhelp  kap_wiener     $KAPPA_HELP wiener
defhelp  kap_zaplin     $KAPPA_HELP zaplin

{defhelp  kap_columnar   $KAPPA_HELP columnar
{defhelp  kap_hide       $KAPPA_HELP hide

{ Define help for full-name synonyms.
defhelp  kap_fitsexist  $KAPPA_HELP fitsexist
defhelp  kap_fitsval    $KAPPA_HELP fitsval
defhelp  kap_fitswrite  $KAPPA_HELP fitsexist
defhelp  kap_picbase    $KAPPA_HELP picbase
defhelp  kap_picdata    $KAPPA_HELP picdata
defhelp  kap_picframe   $KAPPA_HELP picframe
defhelp  kap_picgrid    $KAPPA_HELP picgrid
defhelp  kap_piclast    $KAPPA_HELP piclast
defhelp  kap_picxy      $KAPPA_HELP picxy

{ Announce that the KAPPA commands are available.

print " "
print "   KAPPA commands are now available -- (Version PKG_VERS)"
print " "
print "   Type `help kappa' or `kaphelp' for help on KAPPA commands."
print '   Type "showme sun95" to browse the hypertext documentation'
print " "
