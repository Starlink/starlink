#!/bin/csh
#+
#  Name:
#     kappa.csh
#
#  Purpose:
#     Sets up aliases for the KAPPA package.
#
#  Type of Module:
#     C-shell script
#
#  Invocation:
#     source kappa.csh
#
#  Description:
#     This procedure defines an alias for each KAPPA command.  The string
#     INSTALL_BIN is replaced by the path of the directory containing
#     the package executable files when the package is installed.  The
#     string HELP_DIR is likewise replaced by the path to the directory
#     containing help libraries.
#
#     This procedure also initialises ADAM, if this has not already been
#     done.
#
#  Authors:
#     Malcolm J. Currie (STARLINK)
#     {enter_new_authors_here}
#
#  History:
#     1992 June 12 (MJC):
#        Original version.
#     1992 November 26 (MJC):
#        Sourced the lutread commands.
#     1992 December 4 (MJC):
#        Added axconv, noglobals, and ovclear.
#     1993 January 8 (MJC):
#        Added fitsdin.
#     1993 April 16 (MJC):
#        Added fitsin.
#     1993 April 23 (MJC):
#        Added mem2d.
#     1995 August 19 (MJC):
#        Revised to conform to Martin Bly's template startup script,
#        including tokens for the HELP location and version number.
#        Added Version 0.9 tasks.  Removed withdrawn tasks: compress,
#        numba, pick2d, stats2d, thresh0; and inserted echo strings
#        indicating their replacements (except for compress).
#        Renamed gauss to gausmooth and cleaner to ffclean.
#
#        Introduced the PKG_VERS token. 
#     1995 October 28 (MJC):
#        Added FILLBAD.  Renamed SHIFT to SLIDE.
#     1995 October 31 (MJC):
#        Added PICEMPTY, PICENTRY, and PICVIS.
#     1996 June 12 (MJC):
#        Added NAG usage message, as supplied by Martin Bly.
#     1997 May 31 (MJC):
#        Removed NAG usage message along with COLUMNAR and HIDE.
#        Inserted DRAWSIG, FITSMOD, FITSEXIST, FITSVAL, FITSWRITE,
#        KSTEST, and SUBSTITUTE.  Also withdrew BLINK, IDUNZOOM,
#        IDVISIBLE.
#     {enter_further_changes_here}
#
#  Bugs:
#     {note_any_bugs_here}
#
#-
#
#  Prepare to run ADAM applications if this has not been done already.
#  ===================================================================
#
#  Here look to see if there is ADAM_USER directory.  If there is not
#  check whether or not there is an adam file that is not a directory.
#  If there is, issue a warning and exit.  Otherwise create the required
#  directory.
#
if (-d ${HOME}/adam) then
   echo -n
else
   if (-f ${HOME}/adam) then
      echo "You have a file called adam in your home directory.  Please rename it"
      echo "since adam must be a directory for ADAM files."
      exit
   else
      mkdir ${HOME}/adam
   endif
endif

#
#  Set up an environment variable pointing to the help library.
#  This is referred to within the application interface files.

setenv KAPPA_HELP INSTALL_HELP/kappa

#
#  Define symbols for each application and script.
#  ===============================================
alias add       $KAPPA_DIR/add
alias aperadd   $KAPPA_DIR/aperadd
alias ardgen    $KAPPA_DIR/ardgen
alias ardmask   $KAPPA_DIR/ardmask
alias axconv    $KAPPA_DIR/axconv
alias axlabel   $KAPPA_DIR/axlabel
alias axunits   $KAPPA_DIR/axunits
alias block     $KAPPA_DIR/block
alias cadd      $KAPPA_DIR/cadd
alias calc      $KAPPA_DIR/calc
alias calpol    $KAPPA_DIR/calpol
alias cdiv      $KAPPA_DIR/cdiv
alias centroid  $KAPPA_DIR/centroid
alias chpix     $KAPPA_DIR/chpix
#alias columnar  $KAPPA_DIR/columnar
alias cmult     $KAPPA_DIR/cmult
alias compadd   $KAPPA_DIR/compadd
alias compave   $KAPPA_DIR/compave
alias compick   $KAPPA_DIR/compick
# Cannot alias compress to print a message indicating which command to
# use now, as it clashes with the UNIX command of the same name.
alias contour   $KAPPA_DIR/contour
alias contover  $KAPPA_DIR/contover
alias convolve  $KAPPA_DIR/convolve
alias creframe  $KAPPA_DIR/creframe
alias crelut    $KAPPA_DIR/crelut
alias csub      $KAPPA_DIR/csub
alias cursor    $KAPPA_DIR/cursor
alias defpic    $KAPPA_DIR/picdef
alias display   $KAPPA_DIR/display
alias div       $KAPPA_DIR/div
alias drawsig   $KAPPA_DIR/drawsig
alias elprof    $KAPPA_DIR/elprof
alias erase     $KAPPA_DIR/erase
alias errclip   $KAPPA_DIR/errclip
alias exp10     $KAPPA_DIR/exp10
alias expe      $KAPPA_DIR/expe
alias expon     $KAPPA_DIR/expon
alias ffclean   $KAPPA_DIR/ffclean
alias fillbad   $KAPPA_DIR/fillbad
alias fitsdin   $KAPPA_DIR/fitsdin
alias fitsedit  $KAPPA_DIR/fitsedit.csh
alias fitsexp   $KAPPA_DIR/fitsexp
alias fitshead  $KAPPA_DIR/fitshead.csh
alias fitsimp   $KAPPA_DIR/fitsimp
alias fitsin    $KAPPA_DIR/fitsin
alias fitslist  $KAPPA_DIR/fitslist
alias fitsmod   $KAPPA_DIR/fitsmod
alias fitsurface $KAPPA_DIR/fitsurface
alias fitstext  $KAPPA_DIR/fitstext
alias flip      $KAPPA_DIR/flip
alias fourier   $KAPPA_DIR/fourier
alias gausmooth $KAPPA_DIR/gausmooth
alias gdclear   $KAPPA_DIR/gdclear
alias gdnames   $KAPPA_DIR/gdnames
alias gdset     $KAPPA_DIR/gdset
alias gdstate   $KAPPA_DIR/gdstate
alias glitch    $KAPPA_DIR/glitch
alias globals   $KAPPA_DIR/globals
alias greyplot  $KAPPA_DIR/greyplot
#alias hide      $KAPPA_DIR/hide
alias hiscom    $KAPPA_DIR/hiscom
alias hislist   $KAPPA_DIR/hislist
alias hisset    $KAPPA_DIR/hisset
alias histat    $KAPPA_DIR/histat
alias histeq    $KAPPA_DIR/histeq
alias histogram $KAPPA_DIR/histogram
alias idclear   $KAPPA_DIR/idclear
alias idinvisible $KAPPA_DIR/idinvisible
alias idpazo    $KAPPA_DIR/idpazo
alias idset     $KAPPA_DIR/idset
alias idstate   $KAPPA_DIR/idstate
alias inspect   $KAPPA_DIR/inspect
alias kaphelp   $KAPPA_DIR/kaphelp
alias kstest    $KAPPA_DIR/kstest
alias laplace   $KAPPA_DIR/laplace
alias linplot   $KAPPA_DIR/linplot
alias log10     $KAPPA_DIR/log10
alias logar     $KAPPA_DIR/logar
alias loge      $KAPPA_DIR/loge
alias look      $KAPPA_DIR/look
alias lucy      $KAPPA_DIR/lucy
alias lutable   $KAPPA_DIR/lutable
alias lutflip   $KAPPA_DIR/lutflip
alias luthilite $KAPPA_DIR/luthilite
alias lutrot    $KAPPA_DIR/lutrot
alias lutsave   $KAPPA_DIR/lutsave
alias luttweak  $KAPPA_DIR/luttweak
alias lutview   $KAPPA_DIR/lutview
alias makesurface $KAPPA_DIR/makesurface
alias manic     $KAPPA_DIR/manic
alias maths     $KAPPA_DIR/maths
alias median    $KAPPA_DIR/median
alias mem2d     $KAPPA_DIR/mem2d
alias mlinplot  $KAPPA_DIR/mlinplot
alias mosaic    $KAPPA_DIR/mosaic
alias mstats    $KAPPA_DIR/mstats
alias mult      $KAPPA_DIR/mult
alias native    $KAPPA_DIR/native
alias ndfcopy   $KAPPA_DIR/ndfcopy
alias ndftrace  $KAPPA_DIR/ndftrace
alias noglobals $KAPPA_DIR/noglobals
alias nomagic   $KAPPA_DIR/nomagic
alias normalize $KAPPA_DIR/normalize
alias numb      $KAPPA_DIR/numb
alias numba     echo numba is withdrawn.  Use numb instead.
alias outset    $KAPPA_DIR/outset
alias ovclear   $KAPPA_DIR/ovclear
alias ovset     $KAPPA_DIR/ovset
alias paldef    $KAPPA_DIR/paldef
alias palentry  $KAPPA_DIR/palentry
alias palread   $KAPPA_DIR/palread
alias palsave   $KAPPA_DIR/palsave
alias parget    $KAPPA_DIR/parget
alias paste     $KAPPA_DIR/paste
alias piccur    $KAPPA_DIR/piccur
alias picdef    $KAPPA_DIR/picdef
alias picempty  $KAPPA_DIR/picempty
alias picentire $KAPPA_DIR/picentire
alias picin     $KAPPA_DIR/picin
alias pick2d    echo pick2d is withdrawn.  Use ndfcopy or setbound.
alias piclabel  $KAPPA_DIR/piclabel
alias piclist   $KAPPA_DIR/piclist
alias picsel    $KAPPA_DIR/picsel
alias pictrans  $KAPPA_DIR/pictrans
alias picvis    $KAPPA_DIR/picvis
alias pixdupe   $KAPPA_DIR/pixdupe
alias pow       $KAPPA_DIR/pow
alias psf       $KAPPA_DIR/psf
alias quilt     $KAPPA_DIR/quilt
alias rift      $KAPPA_DIR/rift
alias rotate    $KAPPA_DIR/rotate
alias segment   $KAPPA_DIR/segment
alias setaxis   $KAPPA_DIR/setaxis
alias setbad    $KAPPA_DIR/setbad
alias setbb     $KAPPA_DIR/setbb
alias setbound  $KAPPA_DIR/setbound
alias setext    $KAPPA_DIR/setext
alias setlabel  $KAPPA_DIR/setlabel
alias setmagic  $KAPPA_DIR/setmagic
alias setnorm   $KAPPA_DIR/setnorm
alias setorigin $KAPPA_DIR/setorigin
alias setsky    $KAPPA_DIR/setsky
alias settitle  $KAPPA_DIR/settitle
alias settype   $KAPPA_DIR/settype
alias setunits  $KAPPA_DIR/setunits
alias setvar    $KAPPA_DIR/setvar
alias shadow    $KAPPA_DIR/shadow
alias slide     $KAPPA_DIR/slide
alias snapshot  $KAPPA_DIR/snapshot
alias sqorst    $KAPPA_DIR/sqorst
alias stats     $KAPPA_DIR/stats
alias stats2d   echo stats2d is withdrawn.  Use stats instead.
alias sub       $KAPPA_DIR/sub
alias substitute $KAPPA_DIR/substitute
alias surfit    $KAPPA_DIR/surfit
alias thresh    $KAPPA_DIR/thresh
alias thresh0   echo thresh0 is withdrawn.  Use thresh instead.
alias trandat   $KAPPA_DIR/trandat
alias traninvert $KAPPA_DIR/traninvert
alias tranjoin  $KAPPA_DIR/tranjoin
alias tranmake  $KAPPA_DIR/tranmake
alias transformer $KAPPA_DIR/transformer
alias trantrace $KAPPA_DIR/trantrace
alias trig      $KAPPA_DIR/trig
alias turbocont $KAPPA_DIR/turbocont
alias tweak     $KAPPA_DIR/tweak
alias vecplot   $KAPPA_DIR/vecplot
alias wiener    $KAPPA_DIR/wiener
alias zaplin    $KAPPA_DIR/zaplin
#
# Define procedures and commands requiring the above symbols.
#
alias fitsexist   'fitsmod edit=exist mode=interface'
alias fitsval     'fitsmod edit=print mode=interface'
alias fitswrite   'fitsmod edit=write mode=interface position=\!'
alias lutbgyrw    'lutable mapping=linear coltab=external lut=$KAPPA_DIR/bgyrw_lut'
alias lutcol      'lutable mapping=linear coltab=colour'
alias lutcont     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/cont_lut nn'
alias lutfc       'lutable mapping=linear coltab=external lut=$KAPPA_DIR/fc_lut nn'
alias lutgrey     'lutable mapping=linear coltab=grey'
alias lutheat     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/heat_lut'
alias lutikon     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/ikon_lut nn'
alias lutneg      'lutable mapping=linear coltab=negative'
alias lutramps    'lutable mapping=linear coltab=external lut=$KAPPA_DIR/ramps_lut nn'
alias lutread     'set lutread_pars = "\!*"; source $KAPPA_DIR/lutread.csh'
alias lutspec     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/spectrum_lut'
alias lutzebra    'lutable mapping=linear coltab=external lut=$KAPPA_DIR/zebra_lut nn'
alias picbase     'piclist picnum=1'
alias picdata     'piclist name=data picnum=last'
alias picframe    'piclist name=frame picnum=last'
alias picgrid     'picdef array 1.0 prefix=\"\"'
alias piclast     'piclist picnum=last'
alias picxy       'picdef xy 1.0'
#
#  Now repeat for the full command names.
#
alias kap_add       $KAPPA_DIR/add
alias kap_aperadd   $KAPPA_DIR/aperadd
alias kap_ardgen    $KAPPA_DIR/ardgen
alias kap_ardmask   $KAPPA_DIR/ardmask
alias kap_axconv    $KAPPA_DIR/axconv
alias kap_axlabel   $KAPPA_DIR/axlabel
alias kap_axunits   $KAPPA_DIR/axunits
alias kap_block     $KAPPA_DIR/block
alias kap_cadd      $KAPPA_DIR/cadd
alias kap_calc      $KAPPA_DIR/calc
alias kap_calpol    $KAPPA_DIR/calpol
alias kap_cdiv      $KAPPA_DIR/cdiv
alias kap_centroid  $KAPPA_DIR/centroid
alias kap_chpix     $KAPPA_DIR/chpix
#alias kap_columnar  $KAPPA_DIR/columnar
alias kap_cmult     $KAPPA_DIR/cmult
alias kap_compadd   $KAPPA_DIR/compadd
alias kap_compave   $KAPPA_DIR/compave
alias kap_compick   $KAPPA_DIR/compick
alias kap_compress  echo compress is withdrawn.  Use compave instead.
alias kap_contour   $KAPPA_DIR/contour
alias kap_contover  $KAPPA_DIR/contover
alias kap_convolve  $KAPPA_DIR/convolve
alias kap_creframe  $KAPPA_DIR/creframe
alias kap_crelut    $KAPPA_DIR/crelut
alias kap_csub      $KAPPA_DIR/csub
alias kap_cursor    $KAPPA_DIR/cursor
alias kap_defpic    $KAPPA_DIR/picdef
alias kap_display   $KAPPA_DIR/display
alias kap_drawsig   $KAPPA_DIR/drawsig
alias kap_div       $KAPPA_DIR/div
alias kap_elprof    $KAPPA_DIR/elprof
alias kap_erase     $KAPPA_DIR/erase
alias kap_errclip   $KAPPA_DIR/errclip
alias kap_exp10     $KAPPA_DIR/exp10
alias kap_expe      $KAPPA_DIR/expe
alias kap_expon     $KAPPA_DIR/expon
alias kap_ffclean   $KAPPA_DIR/ffclean
alias kap_fillbad   $KAPPA_DIR/fillbad
alias kap_fitsdin   $KAPPA_DIR/fitsdin
alias kap_fitsedit  $KAPPA_DIR/fitsedit.csh
alias kap_fitsexp   $KAPPA_DIR/fitsexp
alias kap_fitshead  $KAPPA_DIR/fitshead.csh
alias kap_fitsimp   $KAPPA_DIR/fitsimp
alias kap_fitsin    $KAPPA_DIR/fitsin
alias kap_fitslist  $KAPPA_DIR/fitslist
alias kap_fitsmod   $KAPPA_DIR/fitsmod
alias kap_fitsurface $KAPPA_DIR/fitsurface
alias kap_fitstext  $KAPPA_DIR/fitstext
alias kap_flip      $KAPPA_DIR/flip
alias kap_fourier   $KAPPA_DIR/fourier
alias kap_gausmooth $KAPPA_DIR/gausmooth
alias kap_gdclear   $KAPPA_DIR/gdclear
alias kap_gdnames   $KAPPA_DIR/gdnames
alias kap_gdset     $KAPPA_DIR/gdset
alias kap_gdstate   $KAPPA_DIR/gdstate
alias kap_glitch    $KAPPA_DIR/glitch
alias kap_globals   $KAPPA_DIR/globals
alias kap_greyplot  $KAPPA_DIR/greyplot
#alias kap_hide      $KAPPA_DIR/hide
alias kap_hiscom    $KAPPA_DIR/hiscom
alias kap_hislist   $KAPPA_DIR/hislist
alias kap_hisset    $KAPPA_DIR/hisset
alias kap_histat    $KAPPA_DIR/histat
alias kap_histeq    $KAPPA_DIR/histeq
alias kap_histogram $KAPPA_DIR/histogram
alias kap_idclear   $KAPPA_DIR/idclear
alias kap_idinvisible $KAPPA_DIR/idinvisible
alias kap_idpazo    $KAPPA_DIR/idpazo
alias kap_idset     $KAPPA_DIR/idset
alias kap_idstate   $KAPPA_DIR/idstate
alias kap_inspect   $KAPPA_DIR/inspect
alias kap_kaphelp   $KAPPA_DIR/kaphelp
alias kap_kstest    $KAPPA_DIR/kstest
alias kap_laplace   $KAPPA_DIR/laplace
alias kap_linplot   $KAPPA_DIR/linplot
alias kap_log10     $KAPPA_DIR/log10
alias kap_logar     $KAPPA_DIR/logar
alias kap_loge      $KAPPA_DIR/loge
alias kap_look      $KAPPA_DIR/look
alias kap_lucy      $KAPPA_DIR/lucy
alias kap_lutable   $KAPPA_DIR/lutable
alias kap_lutflip   $KAPPA_DIR/lutflip
alias kap_luthilite $KAPPA_DIR/luthilite
alias kap_lutrot    $KAPPA_DIR/lutrot
alias kap_lutsave   $KAPPA_DIR/lutsave
alias kap_luttweak  $KAPPA_DIR/luttweak
alias kap_lutview   $KAPPA_DIR/lutview
alias kap_makesurface $KAPPA_DIR/makesurface
alias kap_manic     $KAPPA_DIR/manic
alias kap_maths     $KAPPA_DIR/maths
alias kap_median    $KAPPA_DIR/median
alias kap_mem2d     $KAPPA_DIR/mem2d
alias kap_mlinplot  $KAPPA_DIR/mlinplot
alias kap_mosaic    $KAPPA_DIR/mosaic
alias kap_mstats    $KAPPA_DIR/mstats
alias kap_mult      $KAPPA_DIR/mult
alias kap_native    $KAPPA_DIR/native
alias kap_ndfcopy   $KAPPA_DIR/ndfcopy
alias kap_ndftrace  $KAPPA_DIR/ndftrace
alias kap_noglobals $KAPPA_DIR/noglobals
alias kap_nomagic   $KAPPA_DIR/nomagic
alias kap_normalize $KAPPA_DIR/normalize
alias kap_numb      $KAPPA_DIR/numb
alias kap_numba     echo numba is withdrawn.  Use numb instead.
alias kap_outset    $KAPPA_DIR/outset
alias kap_ovclear   $KAPPA_DIR/ovclear
alias kap_ovset     $KAPPA_DIR/ovset
alias kap_paldef    $KAPPA_DIR/paldef
alias kap_palentry  $KAPPA_DIR/palentry
alias kap_palread   $KAPPA_DIR/palread
alias kap_palsave   $KAPPA_DIR/palsave
alias kap_parget    $KAPPA_DIR/parget
alias kap_paste     $KAPPA_DIR/paste
alias kap_piccur    $KAPPA_DIR/piccur
alias kap_picdef    $KAPPA_DIR/picdef
alias kap_picempty  $KAPPA_DIR/picempty
alias kap_picentire $KAPPA_DIR/picentire
alias kap_picin     $KAPPA_DIR/picin
alias kap_pick2d    echo pick2d is withdrawn.  Use ndfcopy or setbound.
alias kap_piclabel  $KAPPA_DIR/piclabel
alias kap_piclist   $KAPPA_DIR/piclist
alias kap_picsel    $KAPPA_DIR/picsel
alias kap_pictrans  $KAPPA_DIR/pictrans
alias kap_picvis    $KAPPA_DIR/picvis
alias kap_pixdupe   $KAPPA_DIR/pixdupe
alias kap_pow       $KAPPA_DIR/pow
alias kap_psf       $KAPPA_DIR/psf
alias kap_quilt     $KAPPA_DIR/quilt
alias kap_rift      $KAPPA_DIR/rift
alias kap_rotate    $KAPPA_DIR/rotate
alias kap_segment   $KAPPA_DIR/segment
alias kap_setaxis   $KAPPA_DIR/setaxis
alias kap_setbad    $KAPPA_DIR/setbad
alias kap_setbb     $KAPPA_DIR/setbb
alias kap_setbound  $KAPPA_DIR/setbound
alias kap_setext    $KAPPA_DIR/setext
alias kap_setlabel  $KAPPA_DIR/setlabel
alias kap_setmagic  $KAPPA_DIR/setmagic
alias kap_setnorm   $KAPPA_DIR/setnorm
alias kap_setorigin $KAPPA_DIR/setorigin
alias kap_setsky    $KAPPA_DIR/setsky
alias kap_settitle  $KAPPA_DIR/settitle
alias kap_settype   $KAPPA_DIR/settype
alias kap_setunits  $KAPPA_DIR/setunits
alias kap_setvar    $KAPPA_DIR/setvar
alias kap_shadow    $KAPPA_DIR/shadow
alias kap_slide     $KAPPA_DIR/slide
alias kap_snapshot  $KAPPA_DIR/snapshot
alias kap_sqorst    $KAPPA_DIR/sqorst
alias kap_stats     $KAPPA_DIR/stats
alias kap_stats2d   echo stats2d is withdrawn.  Use stats instead.
alias kap_sub       $KAPPA_DIR/sub
alias kap_substitute $KAPPA_DIR/substitute
alias kap_surfit    $KAPPA_DIR/surfit
alias kap_thresh    $KAPPA_DIR/thresh
alias kap_thresh0   echo thresh0 is withdrawn.  Use thresh instead.
alias kap_trandat   $KAPPA_DIR/trandat
alias kap_traninvert $KAPPA_DIR/traninvert
alias kap_tranjoin  $KAPPA_DIR/tranjoin
alias kap_tranmake  $KAPPA_DIR/tranmake
alias kap_transformer $KAPPA_DIR/transformer
alias kap_trantrace $KAPPA_DIR/trantrace
alias kap_trig      $KAPPA_DIR/trig
alias kap_turbocont $KAPPA_DIR/turbocont
alias kap_tweak     $KAPPA_DIR/tweak
alias kap_vecplot   $KAPPA_DIR/vecplot
alias kap_wiener    $KAPPA_DIR/wiener
alias kap_zaplin    $KAPPA_DIR/zaplin
#
# Define procedures and commands requiring the above symbols.
#
alias kap_fitsexist   'fitsmod edit=exist mode=interface'
alias kap_fitsval     'fitsmod edit=print mode=interface'
alias kap_fitswrite   'fitsmod edit=write mode=interface position=\!'
alias kap_lutbgyrw    'lutable mapping=linear coltab=external lut=$KAPPA_DIR/bgyrw_lut'
alias kap_lutcol      'lutable mapping=linear coltab=colour'
alias kap_lutcont     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/cont_lut nn'
alias kap_lutfc       'lutable mapping=linear coltab=external lut=$KAPPA_DIR/fc_lut nn'
alias kap_lutgrey     'lutable mapping=linear coltab=grey'
alias kap_lutheat     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/heat_lut'
alias kap_lutikon     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/ikon_lut nn'
alias kap_lutneg      'lutable mapping=linear coltab=negative'
alias kap_lutramps    'lutable mapping=linear coltab=external lut=$KAPPA_DIR/ramps_lut nn'
alias kap_lutread     'set lutread_pars = "\!*"; source $KAPPA_DIR/lutread.csh'
alias kap_lutspec     'lutable mapping=linear coltab=external lut=$KAPPA_DIR/spectrum_lut'
alias kap_lutzebra    'lutable mapping=linear coltab=external lut=$KAPPA_DIR/zebra_lut nn'
alias kap_picbase     'piclist picnum=1'
alias kap_picdata     'piclist name=data picnum=last'
alias kap_picframe    'piclist name=frame picnum=last'
alias kap_picgrid     'picdef array 1.0 prefix=\"\"'
alias kap_piclast     'piclist picnum=last'
alias kap_picxy       'picdef xy 1.0'

#
#  Tell the user that KAPPA commands are now available.

echo " "
echo "   KAPPA commands are now available -- (Version PKG_VERS)"
echo " "
echo "   Type kaphelp for help on KAPPA commands"
echo '   Type "showme sun95" to browse the hypertext documentation'
echo " "

#
#  Exit the procedure.
# end
