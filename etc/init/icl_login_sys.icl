{  ICL_LOGIN_SYS.ICL - Initial setup of Starlink ADAM applications packages.
{
{  Method:
{
{    Display ICL Version
{
{    Define the source of help on `PACKAGES'
{
{    Define (hidden) procedure NOTINSTALLED for Packages not installed
{    Define (hidden) procedure NOTAVAILABLE for Packages not available
{    Define (hidden) procedure NOPORT for Packages not Ported
{
{    For each "standard" ADAM applications package:
{      * Define the source of introductory help.
{      * Define the Package Startup Command. 
{
{    Define (hidden) procedure ADAM_OPTIONS to search for installed Options
{    and define appropriate startup command.
{
{    Run ADAM_OPTIONS procedure
{
{    Inform the user of HELP commands 
{
{    Define (hidden) procedure ADAM_LOCAL to search for LADAM_PACKAGES:.ICL
{    and LOAD it if it exists
{
{    Run ADAM_LOCAL procedure
{
{ End of Method
{
{    For full details see SSN/64.
{
{  Authors:
{    AJC: A J Chipperfield  (Starlink)
{    BRADC: Brad Cavanagh (JAC, Hawaii)
{    BLY: M J Bly (Starlink, RAL)
{    TIMJ: Tim Jenness (JAC, Hawaii)
{    MJC: Malcolm J. Currie (Starlink, RAL)
{
{  History:
{    12.09.1994 (AJC):
{       Original - adapted from VMS system
{    16.03.1995 (BLY):
{       Modified to reflect current Unix packages availablility.
{    11.05.1995 (BLY):
{       Removed definition of LS to avoid clash with shell ls command.
{    18.07.1995 (BLY):
{       Modified ASTERIX startup for Asterix v1.8.
{    06.12.1995 (BLY):
{       Added WFCPACK.
{    14.12.1995 (BLY):
{       Modified CCDPACK startup for CCDpack v2.0
{    18.12.1995 (BLY):
{       Modified CONVERT startup for CONVERT v0.6U,
{       Moved FIGARO startup to 'Options Set'.
{    22.02.1996 (BLY):
{       Moved KAPPA startup to 'Options Set'.
{    14.10.1996 (BLY):
{       Added IRCAMPACK for Linux.
{    18.08.1997 (BLY):
{       Integrated version.
{    13.01.1999 (BLY):
{       Added EXTRACTOR.
{    25.10.1999 (BLY):
{       Added POLPACK.
{    24.04.2000 (BLY):
{       Corrected POLPACK definitions.
{    31.01.2002 (BLY):
{       Added XRT definitions.
{       Removed TWODSPEC definitions.
{    21.08.2005 (TIMJ):
{       Photom now uses photom.icl
{    2008 July 2 (MJC):
{       Added DATACUBE definitions.
{    2009 May 6 (BRADC):
<       Added CUPID definitions.

{  Display ICL Version
VERSION

{  Define the source of help on "PACKAGES"  }
defhelp packages $ADAM_PACKAGES 0

{  Define a procedure to be run to print a helpful message if a user
{  attempts to obey a package startup command for a package which is
{  not installed.
HIDDEN PROC NOTINSTALLED PACKAGE
  PRINT
  PRINT " "(PACKAGE) "is not installed at this site."
  PRINT " If you really want it, contact your Site Manager."
  PRINT
ENDPROC

{  Define a procedure to be run to print a helpful message if a user
{  attempts to obey a package startup command for a package which is
{  not yet avaliable for ICL but is available for shell.
HIDDEN PROC NOTAVAILABLE PACKAGE
  PRINT
  PRINT " "(PACKAGE) "is not available for use with ICL yet."
  PRINT " Please use the shell version which is available."
  PRINT
ENDPROC

{  Define a procedure to be run to print a helpful message if a user
{  attempts to obey a package startup command for a package which is
{  not going to be ported.
HIDDEN PROC NOPORT PACKAGE
  PRINT
  PRINT " "(PACKAGE) "is either not ported to Unix or not"
  PRINT " going to be ported to Unix."
  PRINT
ENDPROC

{  Define a procedure to be run to print a helpful message if a user
{  attempts to obey a package startup command for a package which is
{  not going to be ported.
HIDDEN PROC NOTICL PACKAGE
  PRINT
  PRINT " "(PACKAGE) "does not run under ICL"
  PRINT
ENDPROC

{
{  STANDARD Packages
{

{  Definitions for HDSTRACE
defhelp hdstrace $HDSTRACE_HELP HDSTRACE
define hdstrace $HDSTRACE_DIR/hdstrace

{  Definitions for MISCELLANEOUS bits and bobs
defstring ikonpaint noport IKONPAINT
defstring tapedism noport TAPEDISM
defstring tapemount noport TAPEMOUNT

{  Definitions for UTILITIES
defhelp utilities $ADAM_PACKAGES UTILITIES

{  OPTION Packages  }

{  Define a procedure to determine if Optional Packages are installed. }
HIDDEN PROC ADAM_OPTIONS

{  Method:

{    For each "optional" package:
{      * Define the source of introductory help.
{      * If: ICL package startup available
{            Define Package Startup Command(s)
{        Else If: Shell package startup available
{            Display not ICL message
{        Else:             
{            Define as Not Installed
{        End If.

{  Print status message.
PRINT
PRINT "Loading installed package definitions..."
PRINT

{  ASTERIX
defhelp asterix $ADAM_PACKAGES ASTERIX
if file_exists("$AST_ROOT/startup.icl")
   defstring asterix load $AST_ROOT/startup.icl
else
   defstring asterix notinstalled ASTERIX
endif

{  ATOOLS
if file_exists("$ATOOLS_DIR/atools.icl") 
   defstring atools load $ATOOLS_DIR/atools.icl
elseif file_exists("$ATOOLS_DIR/atools.csh")
   defstring atools notavailable ATOOLS
else
   defstring atools notinstalled ATOOLS
endif

{  CATPAC
defhelp catpac $ADAM_PACKAGES CATPAC
IF FILE_EXISTS("$CATPAC_DIR/catpac.icl") 
   defstring catpac load $CATPAC_DIR/catpac.icl
   defstring catpacdemo load $CATPAC_DIR/catpacdemo
ELSE
   defstring catpac notinstalled CATPAC
   defstring catpacdemo notinstalled CATPAC
ENDIF

{  CCDPACK
defhelp ccdpack $ADAM_PACKAGES CCDPACK
if file_exists("$CCDPACK_DIR/ccdpack.icl") 
   defstring ccdpack load $CCDPACK_DIR/ccdpack.icl
elseif file_exists("$CCDPACK_DIR/ccdpack.csh")
   defstring ccdpack notavailable CCDPACK
else
   defstring ccdpack notinstalled CCDPACK
endif

{  CONVERT
defhelp convert $ADAM_PACKAGES CONVERT
if file_exists("$CONVERT_DIR/convert.icl")
   defstring convert(start) load $CONVERT_DIR/convert.icl
elseif file_exists("$CONVERT_DIR/convert.csh")
   defstring convert(start) notavailable CONVERT
else
   defstring convert(start) notinstalled CONVERT
endif

{  CUPID
defhelp cupid $ADAM_PACKAGES CUPID
if file_exists("$CUPID_DIR/cupid.icl")
   defstring cupid load $CUPID_DIR/cupid.icl
elseif file_exists("$CUPID_DIR/cupid.csh")
   defstring cupid notavailable CUPID
else
   defstring cupid notinstalled CUPID
endif

{  DAOPHOT
defhelp daophot $ADAM_PACKAGES DAOPHOT
if file_exists("$DAOPHOT_DIR/daophot_init") 
   defstring daophot print " DAOPHOT should be run from the shell"
else
   defstring daophot notinstalled DAOPHOT
endif

{  DATACUBE
defhelp datacube $ADAM_PACKAGES DATACUBE
if file_exists("$DATACUBE_DIR/datacube.icl")
   defstring datacube(start) load $DATACUBE_DIR/datacube.icl
elseif file_exists("$DATACUBE_DIR/datacube.csh")
   defstring datacube(start) notavailable DATACUBE
else
   defstring datacube(start) notinstalled DATACUBE
endif

{  ESP
defhelp esp $ADAM_PACKAGES ESP
if file_exists("$ESP_DIR/esp.icl") 
   defstring esp load $ESP_DIR/esp.icl
elseif file_exists("$ESP_DIR/esp.csh")
   defstring esp notavailable ESP
else
   defstring esp notinstalled ESP
endif

{  EXTRACTOR
defhelp extractor $ADAM_PACKAGES EXTRACTOR
if file_exists("$EXTRACTOR_DIR/extractor.icl")
   defstring extractor load $EXTRACTOR_DIR/extractor.icl
elseif file_exists("$EXTRACTOR_DIR/extractor.csh")
   defstring extractor notavailable EXTRACTOR
else
   defstring extractor notinstalled EXTRACTOR
endif

{  FIGARO
defhelp figaro $ADAM_PACKAGES FIGARO
if file_exists("$FIG_DIR/figaro.icl")
   defstring figaro load $FIG_DIR/figaro.icl
elseif file_exists("$FIG_DIR/figaro.csh")
   defstring figaro notavailable FIGARO
else
    defstring figaro notinstalled FIGARO
endif

{  HDSTOOLS
defhelp hdstools $ADAM_PACKAGES HDSTOOLS
if file_exists("$HDSTOOLS_DIR/hdstools.icl")
   defstring hdstools load $HDSTOOLS_DIR/hdstools.icl
elseif file_exists("$HDSTOOLS_DIR/hdstools.csh")
   defstring hdstools notavailable HDSTOOLS
else
    defstring hdstools notinstalled HDSTOOLS
endif

{  IRAS90
defhelp iras90 $ADAM_PACKAGES IRAS90
if file_exists("$IRAS90_DIR/iras90.icl")
   defstring iras90 load $IRAS90_DIR/iras90.icl
elseif file_exists("$IRAS90_DIR/iras90")
   defstring iras90 notavailable IRAS90
else
   defstring iras90 notinstalled IRAS90
endif

{  IRCAMPACK
{ defhelp ircampack $ADAM_PACKAGES IRAS90
if file_exists("$IRCAMPACK_DIR/ircampack.icl")
   defstring ircampack load $IRCAMPACK_DIR/ircampack.icl
elseif file_exists("$IRCAMPACK_DIR/ircampack.csh")
   defstring ircampack notavailable IRCAMPACK
else
   defstring ircampack notinstalled IRCAMPACK
endif

{  JCMTDR
defhelp jcmtdr $ADAM_PACKAGES JCMTDR
if file_exists("$JCMTDR_DIR/jcmtdr.icl")
   defstring jcmtdr load $JCMTDR_DIR/jcmtdr.icl
else
   defstring jcmtdr notinstalled JCMTDR
endif

{  KAPPA
defhelp kappa $ADAM_PACKAGES KAPPA
if file_exists("$KAPPA_DIR/kappa.icl") 
   defstring kappa load $KAPPA_DIR/kappa.icl
elseif file_exists("$KAPPA_DIR/kappa.csh")
   defstring kappa notavailable KAPPA
else
   defstring kappa notinstalled KAPPA
endif

{  NDPROGS
defhelp ndprogs $ADAM_PACKAGES NDPROGS
if file_exists("$NDPROGS_DIR/ndprogs.icl") 
   defstring ndprogs load $NDPROGS_DIR/ndprogs.icl
elseif file_exists("$NDPROGS_DIR/ndprogs.csh")
   defstring ndprogs notavailable NDPROGS
else
   defstring ndprogs notinstalled NDPROGS
endif

{  PHOTOM
defhelp photom $ADAM_PACKAGES PHOTOM
if file_exists("$PHOTOM_DIR/photom.icl") 
   defstring photom load $PHOTOM_DIR/photom.icl
elseif file_exists("$PHOTOM_DIR/photom.csh")
   defstring photom notavailable PHOTOM
else
   defstring photom notinstalled PHOTOM
endif

{  PISA
defhelp pisa $ADAM_PACKAGES PISA
if file_exists("$PISA_DIR/pisa.icl") 
   defstring pisa load $PISA_DIR/pisa.icl
elseif file_exists("$PISA_DIR/start")
   defstring pisa notavailable PISA
else
   defstring pisa notinstalled PISA
endif

{  POLPACK
defhelp polpack $ADAM_PACKAGES POLPACK
if file_exists("$POLPACK_DIR/polpack.icl") 
   defstring polpack load $POLPACK_DIR/polpack.icl
elseif file_exists("$POLPACK_DIR/polpack.csh")
   defstring polpack notavailable POLPACK
else
   defstring polpack notinstalled POLPACK
endif

{  PONGO
defhelp pongo $ADAM_PACKAGES PONGO
if file_exists("$PONGO_BIN/pongo.icl")
   defstring pongo load $PONGO_BIN/pongo.icl
else
   defstring pongo notavailable PONGO
endif

{  SCAR
defhelp scar $ADAM_PACKAGES SCAR
defstring SCAR noport SCAR

{  SMURF
defhelp smurf $ADAM_PACKAGES SMURF
if file_exists("$SMURF_DIR/smurf.icl")
   defstring smurf load $SMURF_DIR/smurf.icl
elseif file_exists("$SMURF_DIR/smurf.csh")
   defstring smurf notavailable SMURF
else
   defstring smurf notinstalled SMURF
endif

{  SPECDRE
{  Withdrawn Spring-1998, merged with FIGARO
defhelp specdre $ADAM_PACKAGES SPECDRE
if file_exists("$FIG_DIR/specdre.icl") 
   defstring specdre load $FIG_DIR/specdre.icl
elseif file_exists("$FIG_DIR/specdre.csh")
   defstring specdre notavailable SPECDRE
else
   defstring specdre notinstalled SPECDRE
endif

{  SPECX
defstring specx noticl SPECX

{  Definitions for SST
defhelp sst $ADAM_PACKAGES SST
if file_exists("$SST_DIR/sst.icl")
   defstring sst load $SST_DIR/sst.icl
elseif file_exists("$SST_DIR/sst.csh")
   defstring sst notavailable SST
else
   defstring sst notinstalled SST
endif


{  STARMAN
defhelp starman $ADAM_PACKAGES STARMAN
if file_exists("$STARMAN_DIR/starman.icl") 
   defstring starman load $STARMAN_DIR/starman.icl
elseif file_exists("$STARMAN_DIR/starman.csh")
   defstring starman notavailable STARMAN
else
   defstring starman notinstalled STARMAN
endif

{  SURF
defhelp surf $ADAM_PACKAGES SURF
if file_exists("$SURF_DIR/surf.icl")
   defstring surf load $SURF_DIR/surf.icl
elseif file_exists("$SURF_DIR/surf.csh")
   defstring surf notavailable SURF
else
   defstring surf notinstalled SURF
endif

{ TSP
defhelp tsp $ADAM_PACKAGES TSP
if file_exists("$TSP_DIR/tsp.icl") 
   defstring tsp load $TSP_DIR/tsp.icl
elseif file_exists("$TSP_DIR/tsp.csh")
   defstring tsp notavailable TSP
else
   defstring tsp notinstalled TSP
endif

{  TWODSPEC
{  Withdrawn Autumn-1999, merged with FIGARO

{ WFCPACK
defhelp wfcpack $ADAM_PACKAGES WFCPACK
if file_exists("$WFCBIN/wfcpack.icl") 
   defstring wfcpack load $WFCBIN/wfcpack.icl
elseif file_exists("$WFCBIN/wfcpack.csh")
   defstring wfcpack notavailable WFCPACK
else
   defstring wfcpack notinstalled WFCPACK
endif

{ XRT
defhelp xrt $ADAM_PACKAGES XRT
if file_exists("$XRT_BIN/xrt.icl")
   defstring xrt load $XRT_BIN/xrt.icl
elseif file_exists("$XRT_BIN/xrt.csh")
   defstring xrt notavailable XRT
else
   defstring xrt notinstalled XRT
endif

ENDPROC

{  Run ADAM_OPTIONS procedure  }
ADAM_OPTIONS

{  Startup Message  }
PRINT   "  - Type HELP package_name for help on specific Starlink packages"
PRINT   "  -   or HELP PACKAGES for a list of all Starlink packages"

{  LOCAL Packages  }

{  Define a procedure to determine if LADAM_PACKAGES.ICL file exists, 
{  and load it if it does.
HIDDEN PROC ADAM_LOCAL
IF FILE_EXISTS("$LADAM_PACKAGES")
   PRINT 
   PRINT "Loading local package definitions..."
   PRINT 
   LOAD $LADAM_PACKAGES
ENDIF
ENDPROC

{  Run ADAM_LOCAL procedure  }
ADAM_LOCAL


PRINT   "  - Type HELP [command] for help on ICL and its commands"
PRINT

