C-----------------------------------------------------------------------
C            Include file for GRPCKG
C Modifications:
C   29-Jan-1985 - add HP2648 (KS/TJP).
C   16-Sep-1985 - remove tabs (TJP).
C   30-Dec-1985 - add PS, VPS (TJP).
C   27-May-1987 - remove ARGS, NULL, PS, VPS, QMS, VQMS, HIDMP,
C                 HP7221, GRINL (TJP).
C    6-Jun-1987 - remove PRTX, TRILOG, VERS, VV (TJP).
C   11-Jun-1987 - remove remaining built-in devices (TJP).
C    5-Jul-1987 - replace GRINIT, GRPLTD by GRSTAT.
C   16-Aug-1987 - remove obsolete variables.
C    9-Sep-1989 - add SAVE statement.
C   26-Nov-1990 - remove GRCTYP.
C    5-Jan-1993 - add GRADJU.
C    1-Sep-1994 - add GRGCAP.
C   21-Dec-1995 - increase GRIMAX to 8.
C   30-Apr-1997 - remove GRC{XY}SP
C-----------------------------------------------------------------------
C
C Parameters:
C   GRIMAX : maximum number of concurrent devices
C   GRFNMX : maximum length of file names
C   GRCXSZ : default width of chars (pixels)
C   GRCYSZ : default height of chars (pixels)
C
      INTEGER   GRIMAX, GRFNMX
      REAL      GRCXSZ, GRCYSZ
      PARAMETER (GRIMAX = 8)
      PARAMETER (GRFNMX = 90)
      PARAMETER (GRCXSZ =  7.0, GRCYSZ =  9.0)
C
C Common blocks:
C   GRCIDE : identifier of current plot
C   GRGTYP : device type of current plot
C The following are qualified by a plot id:
C   GRSTAT : 0 => workstation closed
C            1 => workstation open
C            2 => picture open
C   GRPLTD :
C   GRDASH : software dashing in effect?
C   GRUNIT : unit associated with id
C   GRFNLN : length of filename
C   GRTYPE : device type
C   GRXMXA : x size of plotting surface
C   GRYMXA : y size of plotting surface
C   GRXMIN : blc of plotting window
C   GRYMIN : ditto
C   GRXMAX : trc of plotting window
C   GRYMAX : ditto
C   GRSTYL : line style (integer code)
C   GRWIDT : line width (integer code)
C   GRCCOL : current color index (integer code)
C   GRMNCI : minimum color index on this device
C   GRMXCI : maximum color index on this device
C   GRCMRK : marker number
C   GRXPRE : previous (current) pen position (x)
C   GRYPRE : ditto (y)
C   GRXORG : transformation variables (GRTRAN)
C   GRYORG : ditto
C   GRXSCL : ditto
C   GRYSCL : ditto
C   GRCSCL : character scaling factor
C   GRCFAC :
C   GRCFNT : character font
C   GRFILE : file name (character)
C   GRGCAP : device capabilities (character)
C   GRPXPI : pixels per inch in x
C   GRPYPI : pixels per inch in y
C   GRADJU : TRUE if GRSETS (PGPAP) has been called
C
      INTEGER   GRCIDE, GRGTYP
      LOGICAL   GRPLTD(GRIMAX), GRDASH(GRIMAX), GRADJU(GRIMAX)
      INTEGER   GRSTAT(GRIMAX)
      INTEGER   GRUNIT(GRIMAX), GRFNLN(GRIMAX), GRTYPE(GRIMAX),
     1          GRXMXA(GRIMAX), GRYMXA(GRIMAX), 
     2          GRSTYL(GRIMAX), GRWIDT(GRIMAX), GRCCOL(GRIMAX),
     3          GRCMRK(GRIMAX), GRIPAT(GRIMAX), GRCFNT(GRIMAX),
     4          GRMNCI(GRIMAX), GRMXCI(GRIMAX)
      REAL      GRXMIN(GRIMAX), GRYMIN(GRIMAX),
     1          GRXMAX(GRIMAX), GRYMAX(GRIMAX)
      REAL      GRXPRE(GRIMAX), GRYPRE(GRIMAX), GRXORG(GRIMAX),
     1          GRYORG(GRIMAX), GRXSCL(GRIMAX), GRYSCL(GRIMAX),
     2          GRCSCL(GRIMAX), GRCFAC(GRIMAX), GRPOFF(GRIMAX),
     3          GRPATN(GRIMAX,8),GRPXPI(GRIMAX),GRPYPI(GRIMAX)
      COMMON /GRCM00/ GRCIDE, GRGTYP, GRSTAT, GRPLTD, GRUNIT,
     1                GRFNLN, GRTYPE, GRXMXA, GRYMXA, GRXMIN, GRYMIN,
     2                GRXMAX, GRYMAX, GRWIDT, GRCCOL, GRSTYL,
     3                GRXPRE, GRYPRE, GRXORG, GRYORG, GRXSCL, GRYSCL,
     4                GRCSCL, GRCFAC, GRDASH, GRPATN, GRPOFF,
     5                GRIPAT, GRCFNT, GRCMRK, GRPXPI, GRPYPI, GRADJU,
     6                GRMNCI, GRMXCI
C
      CHARACTER*(GRFNMX) GRFILE(GRIMAX)
      CHARACTER*11       GRGCAP(GRIMAX)
      COMMON /GRCM01/ GRFILE, GRGCAP
      SAVE /GRCM00/, /GRCM01/
C-----------------------------------------------------------------------
