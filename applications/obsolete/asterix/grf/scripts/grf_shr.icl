proc old_graphics
  defstring XLAB(el)    label xaxis=y yaxis=n
  defstring YLAB(el)    label xaxis=n yaxis=y
  defstring XAX(is)     axes xaxis=y yaxis=n
  defstring YAX(is)     axes yaxis=y xaxis=n
  defstring XLOG        axes xaxis=y yaxis=n log=y
  defstring YLOG        axes yaxis=y xaxis=n log=y
  defstring XYLOG       axes xaxis=y yaxis=y log=y
  defstring AMULTI      multi add=y delete=n
  defstring DMULTI      multi delete=y add=n
  defstring COLBAR      pixel cbar=y
  defstring ALEG(end)   legend add=y delete=n
  defstring DLEG(end)   legend delete=y add=n
  defstring GREYSC(ale) print "GREYSCALE not available on UNIX in old graphics"
endproc

proc new_graphics
  defstring XLAB(el)    gset xla
  defstring YLAB(el)    gset yla
  defstring XAX(is)     gset xax
  defstring YAX(is)     gset yax
  defstring XLOG        gset xax log=y
  defstring YLOG        gset yax log=y
  defstring XYLOG       print "xylog not defined in new graphics"
  defstring AMULTI      gmulti add
  defstring DMULTI      gmulti del
  defstring COLBAR      gset key
  defstring ALEG(end)   gset tit
  defstring DLEG(end)   gset tit cancel
  defstring GREYSC(ale) gset col table=2
  defstring GSH(ow)     gset show
  defstring GCAN(cel)   gset cancel
endproc


define	GD(raw)		$AST_BIN/grf_mono
define	GS(et)		$AST_BIN/grf_mono
define	GL(oad)		$AST_BIN/grf_mono
define	GC(lose)	$AST_BIN/grf_mono
define	GM(ulti)	$AST_BIN/grf_mono

define  GDEV(ices)      $AST_BIN/grf_mono
defstring	DS(how) 	GDEV SHOW
defstring	DO(pen) 	GDEV OPEN
defstring	DCLOSE   	GDEV CLOSE
defstring	DE(rase)	GDEV ERASE
defstring       DL(ist)         GDEV LIST
defstring       DSPOOL          $AST_BIN/dspool

define	G_VP(ort)	$AST_BIN/grf_mono PG_BOX
define	G_WI(ndow)	$AST_BIN/grf_mono PG_WINDOW
define	G_BO(x)		$AST_BIN/grf_mono PG_BOX
define	G_MO(ve)	$AST_BIN/grf_mono PG_MOVE
define	G_PL(ot)	$AST_BIN/grf_mono PG_DRAW
define	G_LI(ne)	$AST_BIN/grf_mono PG_LINE
define	G_PO(int)	$AST_BIN/grf_mono PG_POINT
define	G_LA(bel)	$AST_BIN/grf_mono PG_LABEL
define	G_TE(xt)	$AST_BIN/grf_mono PG_PTEXT
define  G_CU(rs)        $AST_BIN/grf_mono

{
{ New graphics shortforms by default
{
new_graphics


{
{ Image processing stuff
{
define  ILOAD          $AST_BIN/grf_mono
define  ILOAD1D        $AST_BIN/grf_mono
define  ICLOSE         $AST_BIN/grf_mono
define  IBGND          $AST_BIN/grf_mono
define  IXCORR         $AST_BIN/grf_mono
define  IDISP(lay)     $AST_BIN/grf_mono
define  IPIX(el)       $AST_BIN/grf_mono
define  ISCALE         $AST_BIN/grf_mono
define  IBOX           $AST_BIN/grf_mono
define  ICIRC(le)      $AST_BIN/grf_mono
define  IWHOLE         $AST_BIN/grf_mono
define  IBROW(se)      $AST_BIN/grf_mono
define  ICLEAR         $AST_BIN/grf_mono
define  ICONT(our)     $AST_BIN/grf_mono
define  INOISE         $AST_BIN/grf_mono
define  IUNDO	       $AST_BIN/grf_mono
define  IMARK          $AST_BIN/grf_mono
define  IMASK          $AST_BIN/grf_mono
define  ISLICE         $AST_BIN/grf_mono
define  ISTATS         $AST_BIN/grf_mono
define  ICOL(our)      $AST_BIN/grf_mono
define  IGRID          $AST_BIN/grf_mono
define  IPATCH         $AST_BIN/grf_mono
define  ISAVE          $AST_BIN/grf_mono
define  ISAVE1D        $AST_BIN/grf_mono
define  IRAD(ial)      $AST_BIN/grf_mono
define  IPLOT          $AST_BIN/grf_mono
define  ITIT(le)       $AST_BIN/grf_mono
define  ILAB(els)      $AST_BIN/grf_mono
define  ITEXT          $AST_BIN/grf_mono
define  IPSF           $AST_BIN/grf_mono
define  IHIST          $AST_BIN/grf_mono
define  IAZIM(uth)     $AST_BIN/grf_mono
define  IPEAKS         $AST_BIN/grf_mono
define  IBLUR          $AST_BIN/grf_mono
define  ICURR(ent)     $AST_BIN/grf_mono
define  IMODE          $AST_BIN/grf_mono
define  IPOS(it)       $AST_BIN/grf_mono
define  IKEY           $AST_BIN/grf_mono
define  ICENT(roid)    $AST_BIN/grf_mono
define  ISEP           $AST_BIN/grf_mono
define  ISURF(ace)     $AST_BIN/grf_mono
define  IPOLAR         $AST_BIN/grf_mono
define  IMOSAIC        $AST_BIN/grf_mono
define  IZED           $AST_BIN/grf_mono
define  IARITH         $AST_BIN/grf_mono
define  IEX(clude)     $AST_BIN/grf_mono
define  IREG(ion)      $AST_BIN/grf_mono
define  IZOOM          $AST_BIN/grf_mono
define  ICACHE         $AST_BIN/grf_mono

defstring IREDISP(lay)   IDISPLAY NEXT=n
defstring IREPLOT        IPLOT NEXT=n
defstring IBLURG         IBLUR GAUSS=y
defstring IBLURB         IBLUR GAUSS=n
defstring IADD           IARITH ADD
defstring ISUB           IARITH SUB
defstring IMULT          IARITH MULT
defstring IDIV           IARITH DIV
defstring IBOXSTATS      ISTATS BOX
defstring ICIRCSTATS     ISTATS CIRC
defstring IQPIX(el)      IPIXEL QUAL
defstring IUNZOOM        IZOOM MODE=OFF

defproc   ICNTSUB        $AST_ETC/icntsub
defproc   IDTODMS        $AST_ETC/i_proc
defproc   IDTOHMS        $AST_ETC/i_proc
defproc   IDMSTOD        $AST_ETC/i_proc
defproc   IHMSTOD        $AST_ETC/i_proc
