*+  RAT_SUB - subroutines for reading RFITS/RDAT
*-
*+RAT_FITSKEY - looks up the keyword for variuos versions of FITS
       SUBROUTINE RAT_FITSKEY(ORIGIN,VERSION,NAME,KEY,STATUS)
* Description :
*   Look up a given keyword for a particular fits version
*   The key will be the default or the last key read upto and including
*   the version number
* Authors : Jeremy Ashley
* History : 15-Jun-1994 Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Parameters:
      CHARACTER*(*) ORIGIN
      CHARACTER*(*) VERSION
      CHARACTER*(*) NAME
      CHARACTER*(*) KEY
* Status :
      INTEGER STATUS
* Local constants
      INTEGER NVALUES
      PARAMETER (NVALUES = 38)
* Local variables :
      CHARACTER*50 VALUES(NVALUES)
      CHARACTER*3 T_ORI,T_VER
      CHARACTER*12 T_KEY, T_NAM
      REAL R_VER1, R_VER2
      INTEGER I
      INTEGER MATCH
* Data statements :
      DATA VALUES(1)   /'RDF:   :TITLE      :OBJECT'/
      DATA VALUES(2)   /'RDF:   :TARGET     :OBJECT'/
      DATA VALUES(3)   /'RDF:   :CSTART     :TCRVL3'/
      DATA VALUES(4)   /'RDF:   :CEND       :TALEN3'/
      DATA VALUES(5)   /'RDF:   :AXIS_RA    :TCRVL1'/
      DATA VALUES(6)   /'RDF:   :AXIS_DEC   :TCRVL2'/
      DATA VALUES(7)   /'RDF:   :ROLLCI     :TCROT2'/
      DATA VALUES(8)   /'RDF:   :PIXEL      :TCDLT2'/
      DATA VALUES(9)   /'RDF:   :XDEND      :TALEN6'/
      DATA VALUES(10)  /'RDF:   :YDEND      :TALEN7'/
      DATA VALUES(11)  /'RDF:   :TEND       :LIVETIME'/
      DATA VALUES(12)  /'RDF:   :XEND       :TALEN1'/
      DATA VALUES(13)  /'RDF:   :YEND       :TALEN2'/
      DATA VALUES(14)  /'RDF:   :FDSIZEX    :TALEN1'/
      DATA VALUES(15)  /'RDF:   :FDSIZEY    :TALEN2'/
      DATA VALUES(16)  /'RDF:   :EVENTS     :NAXIS2'/
      DATA VALUES(17)  /'RDF:   :OBSERVER   :OBSERVER'/
      DATA VALUES(18)  /'RDF:   :OBSERVATORY:TELESCOP'/
      DATA VALUES(19)  /'RDF:   :INSTRUMENT : '/
      DATA VALUES(20)  /'RDF:   :DETECTOR   :INSTRUME'/
      DATA VALUES(21)  /'RDF:   :FILTER     :FILTER'/
      DATA VALUES(22)  /'RDF:   :XPUNITS    : '/
      DATA VALUES(23)  /'RDF:   :YPUNITS    : '/
      DATA VALUES(24)  /'RDF:   :MJDREFI    :MJDREFI'/
      DATA VALUES(25)  /'RDF:   :MJDREFF    :MJDREFF'/
      DATA VALUES(26)  /'RDF:   :DATE_OBS   :DATE-OBS'/
      DATA VALUES(27)  /'RDF:   :TIME_OBS   :TIME-OBS'/
      DATA VALUES(28)  /'RDF:   :DATE_END   :DATE_END'/
      DATA VALUES(29)  /'RDF:   :TIME_END   :TIME_END'/
      DATA VALUES(30)  /'RDF:   :SKYCX      :TCRPX1'/
      DATA VALUES(31)  /'RDF:   :SKYCY      :TCRPX2'/
      DATA VALUES(32)  /'RDF:2.3:XEND       :TLMAX1'/
      DATA VALUES(33)  /'RDF:2.3:YEND       :TLMAX2'/
      DATA VALUES(34)  /'RDF:2.3:CEND       :TLMAX3'/
      DATA VALUES(35)  /'RDF:2.3:FDSIZEX    :TLMAX2'/
      DATA VALUES(36)  /'RDF:2.3:FDSIZEY    :TLMAX3'/
      DATA VALUES(37)  /'RDF:2.3:XDEND      :TLMAX6'/
      DATA VALUES(38)  /'RDF:2.3:YDEND      :TLMAX7'/
*-
      IF (STATUS.NE.SAI__OK) RETURN

      CALL CHR_CTOR(VERSION,R_VER2,STATUS)
***** lookup fits keyword
      MATCH = 0
      DO I = 1, NVALUES
         T_ORI = VALUES(I)(1:3)
         T_VER = VALUES(I)(5:7)
         T_NAM = VALUES(I)(9:19)
         T_KEY = VALUES(I)(21:50)
         CALL CHR_CTOR(T_VER,R_VER1,STATUS)
*        check for matches, ' ' matches anything
         IF (((T_ORI.EQ.' ').OR.(T_ORI.EQ.ORIGIN)).AND.
     &       ((T_VER.EQ.' ').OR.(R_VER1.LE.R_VER2)).AND.
     &       ((T_NAM.EQ.' ').OR.(T_NAM.EQ.NAME))) MATCH = I
      ENDDO
*
      IF (MATCH.EQ.0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('ORI',ORIGIN)
         CALL MSG_SETC('VER',VERSION)
         CALL MSG_SETC('NAM',NAME)
         CALL MSG_PRNT('** No defined values for ^ORI ^VER, ^NAM **')
      ELSE
         KEY = VALUES(MATCH)(21:50)
      ENDIF

999   CONTINUE
      END

*+GHISTINF Gets and decodes history item header from the MPE FITS FILE
       SUBROUTINE GHISTINF(IUNIT,ITEM,START,CARDS,ITEMS,PACK,STATUS)
*
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Jeremy Ashley
* History :
*     28-Sep-1993 Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
* Parameters:
      INTEGER IUNIT		! Logical unit of FITS file
      CHARACTER*(*) ITEM	! Name of history item
      INTEGER START		! Card number of history data
      INTEGER CARDS		! number of cards used for data
      INTEGER ITEMS		! number of data items
      INTEGER PACK		! number of items per line
* Status :
      INTEGER STATUS
* functions
      LOGICAL CHR_ISDIG
      INTEGER CHR_INDEX
* Local variables :
      CHARACTER*80 CARD
      INTEGER CARDNO
      INTEGER MAXKEYS
      INTEGER DUMMY

      IF (STATUS.NE.SAI__OK) RETURN

      CALL FTGHSP(IUNIT,MAXKEYS,DUMMY,STATUS)
      DO CARDNO = 1,MAXKEYS
         CALL FTGREC(IUNIT,CARDNO,CARD,STATUS)
         IF (CHR_INDEX(CARD,ITEM).NE.0) THEN
            START = CARDNO + 1
            CALL CHR_CTOI(CARD(44:48),ITEMS,STATUS)
            IF (CHR_ISDIG(card(52:52))) THEN
               CALL CHR_CTOI(CARD(51:52),PACK,STATUS)
            ELSE
               CALL CHR_CTOI(CARD(51:51),PACK,STATUS)
            ENDIF
            GOTO 999
         ENDIF
      ENDDO
      STATUS = SAI__WARN
      CALL MSG_SETC('NAME',ITEM)
      CALL MSG_PRNT('HISTORY RECORD "^NAME" NOT FOUND')
*      CALL ERR_REP(' ','HISTORY RECORD "^NAME" NOT FOUND',STATUS)
999   CONTINUE
      END

*+GHISTnI  Gets history items (INTEGER) from the MPE FITS FILE
       SUBROUTINE GHISTnI(IUNIT,ITEM,VALUES,MAXVAL,COUNT,STATUS)
*
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Jeremy Ashley
* History :
*     28-Sep-1993 Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
* Parameters:
      INTEGER IUNIT		! Logical unit of FITS file
      CHARACTER*(*) ITEM	! Name of history item
      INTEGER MAXVAL
      INTEGER VALUES(MAXVAL)	! items value
      INTEGER COUNT
* Status :
      INTEGER STATUS
* Local variables :
      CHARACTER*80 CARD
      INTEGER START,CARDS,ITEMS,PACK,FINISH,LINES,I

      IF (STATUS.NE.SAI__OK) RETURN

      COUNT = 0
      CALL GHISTINF(IUNIT,ITEM,START,CARDS,ITEMS,PACK,STATUS)
      IF (STATUS.NE.SAI__OK) RETURN

      IF (ITEMS.GT.MAXVAL) ITEMS = MAXVAL
      FINISH = ((ITEMS-1)/PACK)+START
      DO LINES = START,FINISH
        CALL FTGREC(IUNIT,LINES,CARD,STATUS)
        CARD = CARD(10:)
        IF ((ITEMS-COUNT).LT.PACK) PACK = ITEMS-COUNT
        READ(CARD,*) (VALUES(I), I = COUNT+1,COUNT+PACK)
        COUNT = COUNT+PACK
      ENDDO
      END

*+GHISTnD  Gets history items (DOUBLE) from the MPE FITS FILE
       SUBROUTINE GHISTnD(IUNIT,ITEM,VALUES,MAXVAL,COUNT,STATUS)
*
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Jeremy Ashley
* History :
*     28-Sep-1993 Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
* Parameters:
      INTEGER IUNIT		! Logical unit of FITS file
      CHARACTER*(*) ITEM	! Name of history item
      INTEGER MAXVAL
      DOUBLE PRECISION VALUES(MAXVAL)	! items value
      INTEGER COUNT
* Status :
      INTEGER STATUS
* Local variables :
      CHARACTER*80 CARD
      INTEGER START,CARDS,ITEMS,PACK,FINISH,LINES,I

      IF (STATUS.NE.SAI__OK) RETURN

      COUNT = 0
      CALL GHISTINF(IUNIT,ITEM,START,CARDS,ITEMS,PACK,STATUS)
      IF (STATUS.NE.SAI__OK) RETURN

      IF (ITEMS.GT.MAXVAL) ITEMS = MAXVAL
      FINISH = ((ITEMS-1)/PACK)+START
      DO LINES = START,FINISH
        CALL FTGREC(IUNIT,LINES,CARD,STATUS)
        CARD = CARD(10:)
        IF ((ITEMS-COUNT).LT.PACK) PACK = ITEMS-COUNT
        READ(CARD,*) (VALUES(I), I = COUNT+1,COUNT+PACK)
        COUNT = COUNT+PACK
      ENDDO
      END

*+GHIST1S  Gets a history item string from the MPE FITS FILE
       SUBROUTINE GHIST1S(IUNIT,ITEM,VALUE,STATUS)
*
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Jeremy Ashley
* History :
*     28-Sep-1993 Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
* Parameters:
      INTEGER IUNIT		! Logical unit of FITS file
      CHARACTER*(*) ITEM	! Name of history item
      CHARACTER*(*) VALUE	! items value
* Status :
      INTEGER STATUS
* Local variables :
      CHARACTER*80 CARD
      INTEGER START,CARDS,ITEMS,PACK

      IF (STATUS.NE.SAI__OK) RETURN

      CALL GHISTINF(IUNIT,ITEM,START,CARDS,ITEMS,PACK,STATUS)
      CALL FTGREC(IUNIT,START,CARD,STATUS)
      VALUE = CARD(10:)
999   CONTINUE
      END

*+GHIST1I  Gets a history item Integer from the MPE FITS FILE
       SUBROUTINE GHIST1I(IUNIT,ITEM,VALUE,STATUS)
*
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Jeremy Ashley
* History :
*     28-Sep-1993 Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Parameters:
      INTEGER IUNIT		! Logical unit of FITS file
      CHARACTER*(*) ITEM	! Name of history item
      INTEGER VALUE		! items value
* Status :
      INTEGER STATUS
* Local variables :
      CHARACTER*80 CARD

      IF (STATUS.NE.SAI__OK) RETURN

      CALL GHIST1S(IUNIT,ITEM,CARD,STATUS)
      CALL CHR_CTOI(CARD,VALUE,STATUS)
      END

*+GHIST1R  Gets a history item Real from the MPE FITS FILE
       SUBROUTINE GHIST1R(IUNIT,ITEM,VALUE,STATUS)
*
* Description :
* Deficiencies :
* Bugs :
* Authors :
*     Jeremy Ashley
* History :
*     28-Sep-1993 Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Parameters:
      INTEGER IUNIT		! Logical unit of FITS file
      CHARACTER*(*) ITEM	! Name of history item
      REAL VALUE		! items value
* Status :
      INTEGER STATUS
* Local variables :
      CHARACTER*80 CARD

      IF (STATUS.NE.SAI__OK) RETURN

      CALL GHIST1S(IUNIT,ITEM,CARD,STATUS)
      CALL CHR_CTOR(CARD,VALUE,STATUS)
      END


*+  RAT_GETXRTHEAD - Open header file and read HEAD structure
      SUBROUTINE RAT_GETXRTHEAD(RTNAME,HEAD,STATUS)
*
* Description :
*  Opens index file and reads header information
* Method :
*     <description of how the subroutine works - for programmer info>
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Author: Jeremy Ashley 1993-November
* History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'AST_SYS_PAR'
*    Include files
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Parameters :
      CHARACTER*(*) RTNAME		! Rootname of files
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator


      IF (STATUS.NE.SAI__OK) RETURN

      CALL HDS_OPEN(RTNAME(1:CHR_LEN(RTNAME))//'_hdr','READ',LOC,STATUS)
      CALL RAT_GETHEAD(LOC, 'HEAD', HEAD, STATUS)
      CALL HDS_CLOSE(LOC, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
         CALL MSG_SETC('INAME',RTNAME(1:CHR_LEN(RTNAME))//'_hdr')
         CALL MSG_PRNT('** Error reading header info from ^INAME **')
      ENDIF

      RETURN
      END

*+  RAT-PUTHEAD - Create HEAD structure
      SUBROUTINE RAT_PUTHEAD(LOC,OBJECT,HEAD,STATUS)
*
* Description :
*  Write the contents of the HEAD structure into a HDS container.
* Method :
*     <description of how the subroutine works - for programmer info>
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Author: Jeremy Ashley 1993-sept
* History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Include files
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Parameters :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator
      CHARACTER*(*)            OBJECT                ! Structured name
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   ALOC                   ! locator
*
***** Create HEAD structure
      CALL HDX_CREATE(LOC,OBJECT,0,0,OBJECT,ALOC,STATUS)
      CALL HDX_FIND(LOC,OBJECT,ALOC,STATUS)

      CALL HDX_PUTD(ALOC,'AXIS_RA',1,HEAD.AXIS_RA,STATUS)
      CALL HDX_PUTD(ALOC,'AXIS_DEC',1,HEAD.AXIS_DEC,STATUS)
      CALL HDX_PUTD(ALOC,'ROLLCI',1,HEAD.ROLLCI,STATUS)
      CALL HDX_PUTR(ALOC,'PIXEL',1,HEAD.PIXEL,STATUS)
      CALL HDX_PUTI(ALOC,'NTRANGE',1,HEAD.NTRANGE,STATUS)
      CALL HDX_PUTD(ALOC,'BASE_SCTIME',1,HEAD.BASE_SCTIME,STATUS)
      CALL HDX_PUTR(ALOC,'TSTART',MAXRAN,HEAD.TSTART,STATUS)
      CALL HDX_PUTR(ALOC,'TEND',MAXRAN,HEAD.TEND,STATUS)
      CALL HDX_PUTI(ALOC,'ASTART',1,HEAD.ASTART,STATUS)
      CALL HDX_PUTI(ALOC,'AEND',1,HEAD.AEND,STATUS)
      CALL HDX_PUTI(ALOC,'CSTART',1,HEAD.CSTART,STATUS)
      CALL HDX_PUTI(ALOC,'CEND',1,HEAD.CEND,STATUS)
      CALL HDX_PUTI(ALOC,'XSTART',1,HEAD.XSTART,STATUS)
      CALL HDX_PUTI(ALOC,'XEND',1,HEAD.XEND,STATUS)
      CALL HDX_PUTI(ALOC,'YSTART',1,HEAD.YSTART,STATUS)
      CALL HDX_PUTI(ALOC,'YEND',1,HEAD.YEND,STATUS)
      CALL HDX_PUTI(ALOC,'XDSTART',1,HEAD.XDSTART,STATUS)
      CALL HDX_PUTI(ALOC,'XDEND',1,HEAD.XDEND,STATUS)
      CALL HDX_PUTI(ALOC,'YDSTART',1,HEAD.YDSTART,STATUS)
      CALL HDX_PUTI(ALOC,'YDEND',1,HEAD.YDEND,STATUS)
      CALL HDX_PUTI(ALOC,'NEVENTS',MAXMAPS,HEAD.NEVENTS,STATUS)
      CALL HDX_PUTI(ALOC,'EVSTART',MAXMAPS,HEAD.EVSTART,STATUS)
      CALL HDX_PUTI(ALOC,'IEVLEN',1,HEAD.IEVLEN,STATUS)
      CALL HDX_PUTI(ALOC,'IRECLN',1,HEAD.IRECLN,STATUS)
      CALL HDX_PUTI(ALOC,'IFDSZX',1,HEAD.IFDSZX,STATUS)
      CALL HDX_PUTI(ALOC,'IFDSZY',1,HEAD.IFDSZY,STATUS)
      CALL HDX_PUTI(ALOC,'XSMAP',MAXMAPS,HEAD.XSMAP,STATUS)
      CALL HDX_PUTI(ALOC,'YSMAP',MAXMAPS,HEAD.YSMAP,STATUS)
      CALL HDX_PUTI(ALOC,'ISMNUX',1,HEAD.ISMNUX,STATUS)
      CALL HDX_PUTI(ALOC,'ISMNUY',1,HEAD.ISMNUY,STATUS)
      CALL HDX_PUTI(ALOC,'ISMTNU',1,HEAD.ISMTNU,STATUS)
      CALL HDX_PUTI(ALOC,'IEVTNU',1,HEAD.IEVTNU,STATUS)
      CALL HDX_PUTI(ALOC,'NHEAD',1,HEAD.NHEAD,STATUS)
      CALL HDX_PUTC(ALOC,'OBS_MODE',1,HEAD.OBS_MODE,STATUS)
      CALL HDX_PUTC(ALOC,'OBC_MODE',1,HEAD.OBC_MODE,STATUS)
      CALL HDX_PUTC(ALOC,'TARGET',1,HEAD.TARGET,STATUS)
      CALL HDX_PUTC(ALOC,'OBSERVER',1,HEAD.OBSERVER,STATUS)
      CALL HDX_PUTC(ALOC,'OBSERVATORY',1,HEAD.OBSERVATORY,STATUS)
      CALL HDX_PUTC(ALOC,'INSTRUMENT',1,HEAD.INSTRUMENT,STATUS)
      CALL HDX_PUTC(ALOC,'DETECTOR',1,HEAD.DETECTOR,STATUS)
      CALL HDX_PUTC(ALOC,'FILTER',1,HEAD.FILTER,STATUS)
      CALL HDX_PUTC(ALOC,'TITLE',1,HEAD.TITLE,STATUS)
      CALL HDX_PUTC(ALOC,'XPUNITS',1,HEAD.XPUNITS,STATUS)
      CALL HDX_PUTC(ALOC,'YPUNITS',1,HEAD.YPUNITS,STATUS)
      CALL HDX_PUTD(ALOC,'BASE_MJD',1,HEAD.BASE_MJD,STATUS)
      CALL HDX_PUTD(ALOC,'END_MJD',1,HEAD.END_MJD,STATUS)
      CALL HDX_PUTD(ALOC,'SCCONV',1,HEAD.SCCONV,STATUS)
      CALL HDX_PUTR(ALOC,'SKYCX',1,HEAD.SKYCX,STATUS)
      CALL HDX_PUTR(ALOC,'SKYCY',1,HEAD.SKYCY,STATUS)
      CALL HDX_PUTI(ALOC,'NSPOT',1,HEAD.NSPOT,STATUS)
      CALL HDX_PUTI(ALOC,'XSPOT',MAXSPOT,HEAD.XSPOT,STATUS)
      CALL HDX_PUTI(ALOC,'YSPOT',MAXSPOT,HEAD.YSPOT,STATUS)
      CALL HDX_PUTI(ALOC,'SPOTRAD',MAXSPOT,HEAD.SPOTRAD,STATUS)
      CALL HDX_PUTC(ALOC,'SASS_DATE',1,HEAD.SASS_DATE,STATUS)
      CALL HDX_PUTC(ALOC,'ORIGIN',1,HEAD.ORIGIN,STATUS)

      CALL DAT_ANNUL(ALOC,STATUS)

***** Test for any writing errors
      IF (STATUS.NE.SAI__OK) THEN
      ENDIF
      END


*+  RAT-GETHEAD - Read HEAD structure
      SUBROUTINE RAT_GETHEAD(LOC,OBJECT,HEAD,STATUS)
*
* Description :
*   Read the contents of the HEAD structure from a HDS container into
*   the fortran data structure.
* Method :
*     <description of how the subroutine works - for programmer info>
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Author: Jeremy Ashley 1993-sept
* History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Include files
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Parameters :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator
      CHARACTER*(*)            OBJECT                ! Structured name
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   ALOC                   ! locator
      INTEGER DUMMY			! Value not used
*
      CALL HDX_FIND(LOC,OBJECT,ALOC,STATUS)

      CALL CMP_GET0D(ALOC,'AXIS_RA',HEAD.AXIS_RA,STATUS)
      CALL CMP_GET0D(ALOC,'AXIS_DEC',HEAD.AXIS_DEC,STATUS)
      CALL CMP_GET0D(ALOC,'ROLLCI',HEAD.ROLLCI,STATUS)
      CALL CMP_GET0R(ALOC,'PIXEL',HEAD.PIXEL,STATUS)
      CALL CMP_GET0I(ALOC,'NTRANGE',HEAD.NTRANGE,STATUS)
      CALL CMP_GET0D(ALOC,'BASE_SCTIME',HEAD.BASE_SCTIME,STATUS)
      CALL CMP_GET1R(ALOC,'TSTART',MAXRAN,HEAD.TSTART,DUMMY,STATUS)
      CALL CMP_GET1R(ALOC,'TEND',MAXRAN,HEAD.TEND,DUMMY,STATUS)
      CALL CMP_GET0I(ALOC,'ASTART',HEAD.ASTART,STATUS)
      CALL CMP_GET0I(ALOC,'AEND',HEAD.AEND,STATUS)
      CALL CMP_GET0I(ALOC,'CSTART',HEAD.CSTART,STATUS)
      CALL CMP_GET0I(ALOC,'CEND',HEAD.CEND,STATUS)
      CALL CMP_GET0I(ALOC,'XSTART',HEAD.XSTART,STATUS)
      CALL CMP_GET0I(ALOC,'XEND',HEAD.XEND,STATUS)
      CALL CMP_GET0I(ALOC,'YSTART',HEAD.YSTART,STATUS)
      CALL CMP_GET0I(ALOC,'YEND',HEAD.YEND,STATUS)
      CALL CMP_GET0I(ALOC,'XDSTART',HEAD.XDSTART,STATUS)
      CALL CMP_GET0I(ALOC,'XDEND',HEAD.XDEND,STATUS)
      CALL CMP_GET0I(ALOC,'YDSTART',HEAD.YDSTART,STATUS)
      CALL CMP_GET0I(ALOC,'YDEND',HEAD.YDEND,STATUS)
      CALL CMP_GET1I(ALOC,'NEVENTS',MAXMAPS,HEAD.NEVENTS,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'EVSTART',MAXMAPS,HEAD.EVSTART,DUMMY,STATUS)
      CALL CMP_GET0I(ALOC,'IEVLEN',HEAD.IEVLEN,STATUS)
      CALL CMP_GET0I(ALOC,'IRECLN',HEAD.IRECLN,STATUS)
      CALL CMP_GET0I(ALOC,'IFDSZX',HEAD.IFDSZX,STATUS)
      CALL CMP_GET0I(ALOC,'IFDSZY',HEAD.IFDSZY,STATUS)
      CALL CMP_GET1I(ALOC,'XSMAP',MAXMAPS,HEAD.XSMAP,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'YSMAP',MAXMAPS,HEAD.YSMAP,DUMMY,STATUS)
      CALL CMP_GET0I(ALOC,'ISMNUX',HEAD.ISMNUX,STATUS)
      CALL CMP_GET0I(ALOC,'ISMNUY',HEAD.ISMNUY,STATUS)
      CALL CMP_GET0I(ALOC,'ISMTNU',HEAD.ISMTNU,STATUS)
      CALL CMP_GET0I(ALOC,'IEVTNU',HEAD.IEVTNU,STATUS)
      CALL CMP_GET0I(ALOC,'NHEAD',HEAD.NHEAD,STATUS)
      CALL CMP_GET0C(ALOC,'OBS_MODE',HEAD.OBS_MODE,STATUS)
      CALL CMP_GET0C(ALOC,'OBC_MODE',HEAD.OBC_MODE,STATUS)
      CALL CMP_GET0C(ALOC,'TARGET',HEAD.TARGET,STATUS)
      CALL CMP_GET0C(ALOC,'OBSERVER',HEAD.OBSERVER,STATUS)
      CALL CMP_GET0C(ALOC,'OBSERVATORY',HEAD.OBSERVATORY,STATUS)
      CALL CMP_GET0C(ALOC,'INSTRUMENT',HEAD.INSTRUMENT,STATUS)
      CALL CMP_GET0C(ALOC,'DETECTOR',HEAD.DETECTOR,STATUS)
      CALL CMP_GET0C(ALOC,'FILTER',HEAD.FILTER,STATUS)
      CALL CMP_GET0C(ALOC,'TITLE',HEAD.TITLE,STATUS)
      CALL CMP_GET0C(ALOC,'XPUNITS',HEAD.XPUNITS,STATUS)
      CALL CMP_GET0C(ALOC,'YPUNITS',HEAD.YPUNITS,STATUS)
      CALL CMP_GET0D(ALOC,'BASE_MJD',HEAD.BASE_MJD,STATUS)
      CALL CMP_GET0D(ALOC,'END_MJD',HEAD.END_MJD,STATUS)
      CALL CMP_GET0D(ALOC,'SCCONV',HEAD.SCCONV,STATUS)
      CALL CMP_GET0R(ALOC,'SKYCX',HEAD.SKYCX,STATUS)
      CALL CMP_GET0R(ALOC,'SKYCY',HEAD.SKYCY,STATUS)
      CALL CMP_GET0I(ALOC,'NSPOT',HEAD.NSPOT,STATUS)
      CALL CMP_GET1I(ALOC,'XSPOT',MAXSPOT,HEAD.XSPOT,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'YSPOT',MAXSPOT,HEAD.YSPOT,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'SPOTRAD',MAXSPOT,HEAD.SPOTRAD,DUMMY,STATUS)
      CALL CMP_GET0C(ALOC,'SASS_DATE',HEAD.SASS_DATE,STATUS)
      CALL CMP_GET0C(ALOC,'ORIGIN',HEAD.ORIGIN,STATUS)
      CALL DAT_ANNUL(ALOC,STATUS)

***** Test for any reading errors
      IF (STATUS.NE.SAI__OK) THEN
      ENDIF

      END

*+  RAT-PUTHEAD - Create Index structure
      SUBROUTINE RAT_PUTINDEX(LOC,OBJECT,INDEX,STATUS)
*
* Description :
*   Data structure indexing event data.  (not yet implemented)
* Method :
*     <description of how the subroutine works - for programmer info>
* Deficiencies :
*     <description of any deficiencies>
* Bugs :
*     <description of any "bugs" which have not been fixed>
* Author: Jeremy Ashley 1993-sept
* History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Include files
*    Parameters :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator
      CHARACTER*(*)            OBJECT                ! Structured name
      INTEGER                  INDEX                 ! Index structure
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   ALOC                   ! locator
*
***** Create INDEX structure
      CALL HDX_CREATE(LOC,OBJECT,0,0,OBJECT,ALOC,STATUS)
      CALL HDX_FIND(LOC,OBJECT,ALOC,STATUS)
      CALL DAT_ANNUL(ALOC,STATUS)
      END

      SUBROUTINE RAT_GETINDEX(LOC,OBJECT,HEAD,STATUS)
* (not implemented yet)
      END





*+  RAT_LOOKUP - Returns the column names associated with tables
      CHARACTER*20 FUNCTION RAT_LOOKUP(ORIGIN,DETECTOR,TABLE,COLUMN)
*    Description :
*     <description of what the function does - for user info>
*    Deficiencies :
*    Bugs :
*    Authors :
*     Jeremy Ashley (LTVAD::JKA)
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      CHARACTER*(*) ORIGIN       ! Original data format
      CHARACTER*(*) DETECTOR     ! Detector type
      CHARACTER*(*) TABLE        ! table name
      CHARACTER*(*) COLUMN       ! column name: 'EXTNAME' for file extension
*    Function declarations :
*    Local constants :
      INTEGER N_VALUE
       PARAMETER (N_VALUE=103)
*    Local variables :
      INTEGER I
      CHARACTER*20 T_VAL
      CHARACTER*10 T_ORI,T_DET,T_TAB,T_COL
      CHARACTER*50 VALUES(N_VALUE)
*    Local data :
      DATA VALUES( 1) /'   :    :HEADER :EXTNAME  _hdr' /
      DATA VALUES( 2) /'MPE:    :STDEVT :EXTNAME   ' /
      DATA VALUES( 3) /'US :    :STDEVT :EXTNAME  _events' /
      DATA VALUES( 4) /'RDF:    :STDEVT :EXTNAME  _stdevt' /
      DATA VALUES( 5) /'MPE:    :REJEVT :EXTNAME  _dif ' /
      DATA VALUES( 6) /'US :    :REJEVT :EXTNAME  _rejevt' /
      DATA VALUES( 7) /'RDF:    :REJEVT :EXTNAME  _rejevt' /
      DATA VALUES( 8) /'US :PSPC:ASPECT :EXTNAME  _aspqu' /
      DATA VALUES( 9) /'US :HRI :ASPECT :EXTNAME  _ao' /
      DATA VALUES(10) /'RDF:    :ASPECT :EXTNAME  _aspect' /
      DATA VALUES(11) /'   :    :ASPECT :EXTNAME  _att' /
      DATA VALUES(12) /'US :    :EVRATE :EXTNAME  _evrat' /
      DATA VALUES(13) /'RDF:    :EVRATE :EXTNAME  _evrate' /
      DATA VALUES(14) /'   :    :EVRATE :EXTNAME  _evr' /
      DATA VALUES(15) /'US :    :ORBIT  :EXTNAME  _sporb' /
      DATA VALUES(16) /'RDF:    :ORBIT  :EXTNAME  _ephem' /
      DATA VALUES(17) /'   :    :ORBIT  :EXTNAME  _orb' /
      DATA VALUES(18) /'   :    :QUALITY:EXTNAME  _qua' /
      DATA VALUES(19) /'   :    :HOTSPOT:EXTNAME  _hot' /
      DATA VALUES(20) /'US :PSPC:ASPECT :TIME     it2_asp' /
      DATA VALUES(21) /'US :HRI :ASPECT :TIME     aspect_time' /
      DATA VALUES(22) /'US :PSPC:EVRATE :TIME     iti_evr' /
      DATA VALUES(23) /'US :PSPC:ASPDATA:TIME     it1_cas' /
      DATA VALUES(24) /'MPE:    :       :XPIX     xpix' /
      DATA VALUES(25) /'   :    :       :XPIX     x' /
      DATA VALUES(26) /'MPE:    :       :YPIX     ypix' /
      DATA VALUES(27) /'   :    :       :YPIX     y' /
      DATA VALUES(28) /'MPE:    :       :XDET     xdet' /
      DATA VALUES(29) /'US :    :       :XDET     dx' /
      DATA VALUES(30) /'RDF:PSPC:       :XDET     detx' /
      DATA VALUES(31) /'RDF:HRI :       :XDET     rawx' /
      DATA VALUES(32) /'MPE:    :       :YDET     ydet' /
      DATA VALUES(33) /'US :    :       :YDET     dy' /
      DATA VALUES(34) /'RDF:PSPC:       :YDET     dety' /
      DATA VALUES(35) /'RDF:HRI :       :YDET     rawy' /
      DATA VALUES(36) /'MPE:    :       :PHA      raw_ampl' /
      DATA VALUES(37) /'   :    :       :PHA      pha' /
      DATA VALUES(38) /'MPE:PSPC:       :PI       ampl' /
      DATA VALUES(39) /'MPE:HRI :       :PI       ampl' / ! cheat for HRI
      DATA VALUES(40) /'   :    :       :PI       pi' /
      DATA VALUES(41) /'US :PSPC:ASPECT :ASPERR   iql_asp' /
      DATA VALUES(42) /'US :HRI :ASPECT :ASPERR   asp_error' /
      DATA VALUES(43) /'RDF:    :ASPECT :ASPERR   asp_qual' /
      DATA VALUES(44) /'   :    :ASPECT :ASPERR   asp_err' /
      DATA VALUES(45) /'US :PSPC:EVRATE :MVRATE   iac_evr' /
      DATA VALUES(46) /'RDF:PSPC:EVRATE :MVRATE   mv_aco' /
      DATA VALUES(47) /'   :PSPC:EVRATE :MVRATE   ee_mv' /
      DATA VALUES(48) /'   :HRI :EVRATE :MVRATE    ' / ! no equivelent for HRI
      DATA VALUES(49) /'US :PSPC:EVRATE :XACC     iax_evr' /
      DATA VALUES(50) /'RDF:PSPC:EVRATE :XACC     xacc' /
      DATA VALUES(51) /'   :PSPC:EVRATE :XACC     ee_axe' /
      DATA VALUES(52) /'   :HRI :EVRATE :XACC      ' / ! no equivelent for HRI
      DATA VALUES(53) /'US :PSPC:EVRATE :XTRANSM  iqe_evr' /
      DATA VALUES(54) /'RDF:PSPC:EVRATE :XTRANSM  xtransm' /
      DATA VALUES(55) /'   :PSPC:EVRATE :XTRANSM  ee_aexe' /
      DATA VALUES(56) /'   :HRI :EVRATE :XTRANSM   ' / ! no equivelent for HRI
      DATA VALUES(57) /'US :PSPC:EVRATE :A2_AL    ia2_evr' /
      DATA VALUES(58) /'RDF:PSPC:EVRATE :A2_AL    a2_al' /
      DATA VALUES(59) /'   :PSPC:EVRATE :A2_AL    ee_a2ll' /
      DATA VALUES(60) /'   :HRI :EVRATE :A2_AL     ' / ! no equivelent for HRI
      DATA VALUES(61) /'   :    :HOTSPOT:XSPOT    xspot' /
      DATA VALUES(62) /'   :    :HOTSPOT:YSPOT    yspot' /
      DATA VALUES(63) /'   :    :HOTSPOT:SPOTRAD  spotrad' /
      DATA VALUES(64) /'RDF:    :ORBIT  :MJD_INT  mjd_int' /
      DATA VALUES(65) /'RDF:    :ORBIT  :MJD_FRAC mjd_frac' /
      DATA VALUES(66) /'US :    :ORBIT  :DATE     iut1_so' /
      DATA VALUES(67) /'   :    :ORBIT  :DATE     date' /
      DATA VALUES(68) /'US :    :ORBIT  :DAYSEC   iut2_so' /
      DATA VALUES(69) /'   :    :ORBIT  :DAYSEC   daysec' /
      DATA VALUES(70) /'US :    :ORBIT  :XSAT     isat_sox' /
      DATA VALUES(71) /'RDF:    :ORBIT  :XSAT     sat_x' /
      DATA VALUES(72) /'   :    :ORBIT  :XSAT     xsatellite' /
      DATA VALUES(73) /'US :    :ORBIT  :YSAT     isat_soy' /
      DATA VALUES(74) /'RDF:    :ORBIT  :YSAT     sat_y' /
      DATA VALUES(75) /'   :    :ORBIT  :YSAT     ysatellite' /
      DATA VALUES(76) /'US :    :ORBIT  :ZSAT     isat_soz' /
      DATA VALUES(77) /'RDF:    :ORBIT  :ZSAT     sat_z' /
      DATA VALUES(78) /'   :    :ORBIT  :ZSAT     zsatellite' /
      DATA VALUES(79) /'US :    :ORBIT  :LAT      ilat_so' /
      DATA VALUES(80) /'RDF:    :ORBIT  :LAT      lat_nort' /
      DATA VALUES(81) /'   :    :ORBIT  :LAT      latitude' /
      DATA VALUES(82) /'US :    :ORBIT  :LONG     ilon_so' /
      DATA VALUES(83) /'RDF:    :ORBIT  :LONG     lon_east' /
      DATA VALUES(84) /'   :    :ORBIT  :LONG     longitude' /

      DATA VALUES(85) /'US :PSPC:ASPDATA:EXTNAME  _coras' /
      DATA VALUES(86) /'RDF:PSPC:ASPDATA:EXTNAME  _aspect' /
      DATA VALUES(87) /'   :PSPC:ASPDATA:EXTNAME  _att' /

      DATA VALUES(88) /'US :PSPC:ASPDATA:XOFFSET  ixn_cas' /
      DATA VALUES(89) /'US :PSPC:ASPDATA:YOFFSET  iyn_cas' /
      DATA VALUES(90) /'US :PSPC:ASPDATA:ROLL     iro_cas' /

      DATA VALUES(91) /'RDF:PSPC:ASPDATA:XOFFSET  ra_cas' /
      DATA VALUES(92) /'RDF:PSPC:ASPDATA:YOFFSET  dec_cas' /
      DATA VALUES(93) /'RDF:PSPC:ASPDATA:ROLL     roan_cas' /

      DATA VALUES(94) /'   :PSPC:ASPDATA:XOFFSET  xoffset' /
      DATA VALUES(95) /'   :PSPC:ASPDATA:YOFFSET  yoffset' /
      DATA VALUES(96) /'   :PSPC:ASPDATA:ROLL     roll' /

      DATA VALUES(97) /'US :PSPC:EVRATE :A1_AL    ia1_evr' /
      DATA VALUES(98) /'RDF:PSPC:EVRATE :A1_AL    a1_al' /
      DATA VALUES(99) /'   :PSPC:EVRATE :A1_AL    ee_a1ll' /
      DATA VALUES(100)/'US :HRI :EVRATE :PS_VALID ps_valid' /
      DATA VALUES(101)/'   :    :       :TIME     time' /
      DATA VALUES(102)/'RDF:HRI :EVRATE :PS_VALID secondary' /
      DATA VALUES(103)/'   :    :       :TIME     time' /
*-
*
      RAT_LOOKUP = ' '
*
*     lookup values
      DO I=1,N_VALUE
         T_ORI = VALUES(I)(1:3)
         T_DET = VALUES(I)(5:8)
         T_TAB = VALUES(I)(10:16)
         T_COL = VALUES(I)(18:25)
         T_VAL = VALUES(I)(27:)
*
*        check for matches, ' ' matches anything
*        only look at 3 character on detector (PSPC=PSPCB,PSPCC)
         IF (((T_ORI.EQ.' ').OR.(T_ORI.EQ.ORIGIN)).AND.
     &       ((T_DET.EQ.' ').OR.(T_DET(1:3).EQ.DETECTOR(1:3))).AND.
     &       ((T_TAB.EQ.' ').OR.(T_TAB.EQ.TABLE)).AND.
     &       ((T_COL.EQ.' ').OR.(T_COL.EQ.COLUMN))) GOTO 100
      ENDDO
*
*     show message about lookup failure
      CALL MSG_SETC('COL',COLUMN)
      CALL MSG_PRNT('Lookup failed for ^COL in RAT_LOOKUP')
      CALL MSG_SETC('ORI',ORIGIN)
      CALL MSG_SETC('DET',DETECTOR)
      CALL MSG_SETC('TAB',TABLE)
      CALL MSG_PRNT('no defined values for ^ORI ^DET ^TAB')
      GOTO 999

100   CONTINUE
      RAT_LOOKUP = T_VAL

999   CONTINUE

      END


*+RAT_HDLOOKUP  Lookup table and column names using HEADER information
	SUBROUTINE RAT_HDLOOKUP(HEAD, TABLE, COLUMN, VALUE, STATUS)
* Description :
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     Jeremy Ashley
* History :
*     23-Apr-1994   Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
* Import :
      RECORD /XRT_HEAD/ HEAD          ! Header structure for subsequent use
      CHARACTER*(*) TABLE             ! Table name
      CHARACTER*(*) COLUMN            ! Column name
* Export :
      CHARACTER*(*) VALUE
* Status :
      INTEGER STATUS
* Function declarations :
      CHARACTER*20 RAT_LOOKUP
        EXTERNAL RAT_LOOKUP
*-
      IF (STATUS .NE. SAI__OK) RETURN
*
*     Lookup the required information
      VALUE = RAT_LOOKUP(HEAD.ORIGIN,HEAD.DETECTOR,TABLE,COLUMN)

      END


*+RAT_TMCONV  Converts times from MJD to s/c clock
	SUBROUTINE RAT_TMCONV(DATE_OBS, TIME_OBS, DATE_END, TIME_END,
     &                       XS_MJDRD, XS_MJDRF, HEAD, STATUS)
*
* Description :
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     Richard Saxton
* History :
*     12-Aug-1992   Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
* Import :
      CHARACTER*(*) DATE_OBS          ! Date of observation start
      CHARACTER*(*) TIME_OBS          ! Time of observation start
      CHARACTER*(*) DATE_END          ! Date of observation end
      CHARACTER*(*) TIME_END          ! Time of observation end
      INTEGER XS_MJDRD                ! Integer MJD of s/c start date
      DOUBLE PRECISION XS_MJDRF       ! Fractional MJD of s/c start time
* Import-Export :
      RECORD /XRT_HEAD/ HEAD          ! Header structure for subsequent use
* Export :
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
* Local constants :
* Local variables :
      INTEGER HRS,MIN
      INTEGER SEC
      INTEGER YEAR,IMONTH,DAY         ! Start date
      INTEGER IMJD1                   ! MJD of Start date
      INTEGER IMJD2                   ! MJD of end date
      INTEGER K

      DOUBLE PRECISION MJDBIT         ! Fraction of day
      DOUBLE PRECISION MJDOFF         ! Difference in MJDs
*
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN
*
* Convert base MJD
      K=INDEX(DATE_OBS, '/')
      READ(DATE_OBS(K-2:K-1), FMT='(I2)')DAY
      READ(DATE_OBS(K+1:K+2), FMT='(I2)')IMONTH
      READ(DATE_OBS(K+4:K+5), FMT='(I2)')YEAR
*
* The year must be in the form 1990 - so add 1900 if 90.
      YEAR = YEAR + 1900
*
      CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD1)
*
* Convert the time string to MJD
      K=INDEX(TIME_OBS, ':')
      READ(TIME_OBS(K-2:K-1), FMT='(I2)')HRS
      READ(TIME_OBS(K+1:K+2), FMT='(I2)')MIN
      READ(TIME_OBS(K+4:K+5), FMT='(I2)')SEC
*
      MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
      HEAD.BASE_MJD=DBLE(IMJD1) + MJDBIT/86400.0

* Convert end MJD
      K=INDEX(DATE_END, '/')
      READ(DATE_END(K-2:K-1), FMT='(I2)')DAY
      READ(DATE_END(K+1:K+2), FMT='(I2)')IMONTH
      READ(DATE_END(K+4:K+5), FMT='(I2)')YEAR
*
* The year must be in the form 1990 - so add 1900 if 90.
      YEAR = YEAR + 1900
*
      CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD2)
*
* Convert the time string to MJD
      K=INDEX(TIME_END, ':')
      READ(TIME_END(K-2:K-1), FMT='(I2)')HRS
      READ(TIME_END(K+1:K+2), FMT='(I2)')MIN
      READ(TIME_END(K+4:K+5), FMT='(I2)')SEC
*
      MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
      HEAD.END_MJD=DBLE(IMJD2) + MJDBIT/86400.0
*
* Calculate the MJD offset from the start of the Rosat clock
      MJDOFF = HEAD.BASE_MJD - (XS_MJDRD + XS_MJDRF)
*
* Convert to seconds
      HEAD.BASE_SCTIME = MJDOFF * 86400.0
*
* Set the S/C to MJD conversion factor to one
      HEAD.SCCONV=1.0
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from FX_TMCONV',STATUS)
      ENDIF
*
      END

*+  RAT_FITSSTYLE - Determine fits origins from header records
      SUBROUTINE RAT_FITSSTYLE(IUNIT,ORIGIN,STATUS)
*    Description :
*     Looks through header information for recognised keywords
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*       IUNIT  = INTEGER      (R)
*       ORIGIN = CHARACTER(*) (W)
*       STATUS = INTEGER      (U)
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (LTVAD::JKA)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Import :
*     <declarations and descriptions for imported arguments>
      INTEGER IUNIT              ! Logical unit number of FITS file
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
      CHARACTER*(*) ORIGIN       ! FITS file origin (RDF/US/MPE/unknown)
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
*     <declarations for local variables>
      INTEGER FSTATUS            ! FITSIO routines status value
      CHARACTER*80 CARD          ! Keyword value
*    Local data :
*     <any DATA initialisations for local variables>
*-

*     <application code>

      IF (STATUS.NE.SAI__OK) RETURN

      ORIGIN = ' '
*
*     Test for MPE file
      FSTATUS = 0
      CALL FTGCRD(IUNIT,'ORIGIN',CARD,FSTATUS)
      IF (FSTATUS.EQ.0) ORIGIN = 'MPE'
*
*     Test for US file
      FSTATUS = 0
      CALL FTGCRD(IUNIT,'QPOENAME',CARD,FSTATUS)
      IF (FSTATUS.EQ.0) ORIGIN = 'US'
      FSTATUS = 0
      CALL FTGCRD(IUNIT,'XS-CNTRY',CARD,FSTATUS)
      IF (FSTATUS.EQ.0) ORIGIN = 'US'
*
*     Test for RAT file
      FSTATUS = 0
      CALL FTGCRD(IUNIT,'RDF_VERS',CARD,FSTATUS)
      IF (FSTATUS.EQ.0) ORIGIN = 'RDF'

      END


*+  RAT_FITSORIGIN - <brief title for subroutine>
      SUBROUTINE RAT_FITSORIGIN(FITSDIR,ORIGIN,STATUS)
*    Description :
*     Looks in the FITS directory for any files with an extension
*     ending in FITS opens the first one and checks for certain
*     keywords to determin the origins.
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           FITSDIR = CHARACTER*(*) (R)
*           ORIGIN  = CHARACTER*(*) (W)
*           STATUS  = INTEGER       (U)
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     author (LTVAD::JKA)
*    History :
*     date:  changes (institution::username)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
*     <global variables held in named COMMON>
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Import :
      CHARACTER*(*) FITSDIR     ! Raw directory of fits files
*     <declarations and descriptions for imported arguments>
*    Import-Export :
*     <declarations and descriptions for imported/exported arguments>
*    Export :
*     <declarations and descriptions for exported arguments>
      CHARACTER*(*) ORIGIN      ! Type of fite file (RDF/US/MPE/unknown)
*    Status :
      INTEGER STATUS
*    Function declarations :
*     <declarations for function references>
*    Local constants :
*     <local constants defined by PARAMETER>
      INTEGER MAXRAW              ! Maximum number of files found
      PARAMETER (MAXRAW = 500)
*    Local variables :
*     <declarations for local variables>
      CHARACTER*100 FILES(MAXRAW) ! Array to hold FITS filenames
      INTEGER IUNIT               ! Logical unit number for fite file
      INTEGER NFILES,NMPE1,NMPE2  ! Number of files found
      INTEGER BLOCKSIZE           ! Block size of opened FITS file
*    Local data :
*     <any DATA initialisations for local variables>
*-

*     <application code>

      IF (STATUS.NE.SAI__OK) RETURN

*
*     look for MPE file naming format
      CALL UTIL_FINDFILE(FITSDIR,'*.tfits',MAXRAW,FILES,NMPE1,STATUS)
      CALL UTIL_FINDFILE(FITSDIR,'*.mt',MAXRAW,FILES,NMPE2,STATUS)
      IF (NMPE1 + NMPE2.NE.0) THEN
         ORIGIN = 'MPE'
      ELSE
*        look for any file with a fits extension
         CALL UTIL_FINDFILE(FITSDIR,'*.fits',MAXRAW,FILES,NFILES,STATUS)
         IF (NFILES.NE.0) THEN
*           open the first file in the list
            CALL FIO_GUNIT(IUNIT,STATUS)
            CALL FTOPEN(IUNIT,FILES(1),0,BLOCKSIZE,STATUS)
*           determin origin
            CALL RAT_FITSSTYLE(IUNIT,ORIGIN,STATUS)
*           close file
            CALL FTCLOS(IUNIT,STATUS)
            CALL FIO_PUNIT(IUNIT,STATUS)
         ELSE
            ORIGIN = ' '
         ENDIF
      ENDIF

999   CONTINUE

      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_REP(' ','from RAT_FITSORIGIN', STATUS)
      ENDIF

      END



*+RAT_RAWTIM  Reads a RAT header file associated with an XRT observation
	SUBROUTINE RAT_RAWTIM(RTNAME, NSEL, RAWTIM, STATUS)
*
* Description :
*        This routine finds the time ranges in an observation.
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     Richard Saxton
* History :
*     5-Aug-1992   Original
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
* Import :
      CHARACTER*(*) RTNAME             ! Rootname of datafiles
* Import-Export :
      INTEGER NSEL
      DOUBLE PRECISION RAWTIM(MAXRAN*2) ! Raw sorting times
* Export :
* Status :
      INTEGER STATUS
* Function declarations :
* Local constants :
* Local variables :
      RECORD /XRT_HEAD/ HEAD          ! Header structure for subsequent use
*                                     ! and dummy header structure
      INTEGER LP
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN
*
*     Get header information
      CALL RAT_GETXRTHEAD(RTNAME,HEAD,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Copy the times into the right output variables
      NSEL = HEAD.NTRANGE * 2
*
      DO LP=1,HEAD.NTRANGE
         RAWTIM(1+(LP-1)*2) = HEAD.TSTART(LP)
         RAWTIM(LP*2) = HEAD.TEND(LP)
      ENDDO
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from RAT_RAWTIM',STATUS)
      ENDIF
*
      END

*+ RAT_RDHEAD -  Read header information from US FITS file
	SUBROUTINE RAT_RDHEAD(IUNIT,ORIGIN,HEAD,STATUS)
*    Description :
*      Fill the HEAD structure with the fits file information
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed >
*    Authors :
*     Jeremy Ashley
*    History :
*     25-Nov-1993 : Original
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Global parameters
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      INTEGER IUNIT			! Open FITS IO unit
      CHARACTER*5 ORIGIN                ! FITS file type
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
*-
      IF (STATUS.NE.SAI__OK) RETURN
*
      HEAD.ORIGIN = ORIGIN
      IF (ORIGIN.EQ.'US')  CALL RAT_RDUSHEAD(IUNIT,HEAD,STATUS)
      IF (ORIGIN.EQ.'RDF') CALL RAT_RDRDFHEAD(IUNIT,HEAD,STATUS)
      IF (ORIGIN.EQ.'MPE') CALL RAT_RDMPEHEAD(IUNIT,HEAD,STATUS)

      RETURN
      END


*+ RAT_RDUSHEAD -  Read header information from US FITS file
	SUBROUTINE RAT_RDUSHEAD(IUNIT,HEAD,STATUS)
*    Description :
*      Fill the HEAD structure with the fits file information written in
*      the US style.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed >
*    Authors :
*     Jeremy Ashley
*    History :
*     23-Sep-1993   - Converted from old FX_RDHDR
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Global parameters
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      INTEGER IUNIT			! Open FITS IO unit
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER IVAL,XS_MJDRD,XS_EVREF,TSTAT,HTYPE,I
      DOUBLE PRECISION DVAL,XS_MJDRF
      CHARACTER*80 CDUMMY
      CHARACTER*80 CVAL
      CHARACTER*8 DATE_OBS,TIME_OBS,DATE_END,TIME_END
      LOGICAL LDUMMY
*-
      IF (STATUS.NE.SAI__OK) RETURN
*

C - MOVE TO PRIMARY HEADER
      CALL FTMAHD(IUNIT,1,HTYPE,STATUS)

      CALL FTGKYS(IUNIT,'TITLE',HEAD.TITLE,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'OBJECT',CVAL,CDUMMY,STATUS)
      HEAD.TARGET = CVAL(1:20)
      IF (STATUS.NE.0) RETURN

C - MOVE TO EVENTS HEADER
      CALL FTMAHD(IUNIT,4,HTYPE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL MSG_PRNT('Cannot find Table 4, Possibly no EVENT data')
         RETURN
      ENDIF

      CALL FTGKYJ(IUNIT,'XS-MINCH',HEAD.CSTART,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-MAXCH',HEAD.CEND,CDUMMY,STATUS)
*
      CALL FTGKYD(IUNIT,'CRVAL1',HEAD.AXIS_RA,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CRVAL2',HEAD.AXIS_DEC,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CROTA2',HEAD.ROLLCI,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CDELT2',DVAL,CDUMMY,STATUS)
      HEAD.PIXEL = DVAL * 3600.0

      HEAD.XDSTART = 1
      HEAD.YDSTART = 1
      CALL FTGKYJ(IUNIT,'XS-XDET',HEAD.XDEND,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-YDET',HEAD.YDEND,CDUMMY,STATUS)
*
* Get exposure time info
      HEAD.NTRANGE=1
      HEAD.TSTART(1)=0.0
      CALL FTGKYE(IUNIT,'XS-LIVTI',HEAD.TEND(1),CDUMMY,STATUS)

* The next parameters will not exist for image data so ignore errors
      HEAD.XSTART = 1
      HEAD.YSTART = 1
      CALL FTGKYJ(IUNIT,'AXLEN1',HEAD.XEND,CDUMMY,TSTAT)
      CALL FTGKYJ(IUNIT,'AXLEN2',HEAD.YEND,CDUMMY,TSTAT)
      CALL FTGKYJ(IUNIT,'AXLEN1',HEAD.IFDSZX,CDUMMY,TSTAT)
      CALL FTGKYJ(IUNIT,'AXLEN2',HEAD.IFDSZY,CDUMMY,TSTAT)
*
* Dummy up small map values
      HEAD.ISMNUX=1
      HEAD.ISMNUY=1
      HEAD.ISMTNU=1
      HEAD.XSMAP(1)=1
      HEAD.YSMAP(1)=1
*
      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD.IEVTNU,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'XS-SEQPI',CVAL,CDUMMY,STATUS)
      HEAD.OBSERVER = CVAL(1:20)
      CALL FTGKYS(IUNIT,'TELESCOP',HEAD.OBSERVATORY,CDUMMY,STATUS)
      HEAD.INSTRUMENT = 'XRT'
      CALL FTGKYS(IUNIT,'INSTRUME',HEAD.DETECTOR,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-FILTR',IVAL,CDUMMY,STATUS)
      IF (IVAL.EQ.0) HEAD.FILTER = 'OFF'
      IF (IVAL.EQ.1) HEAD.FILTER = 'BORON'
      HEAD.XPUNITS = 'ARCSECONDS'
      HEAD.YPUNITS  = 'ARCSECONDS'
      CALL FTGKYJ(IUNIT,'XS-MJDRD',XS_MJDRD,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'XS-MJDRF',XS_MJDRF,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-OBS',DATE_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-OBS',TIME_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-END',DATE_END,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-END',TIME_END,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-EVREF',XS_EVREF,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX1',HEAD.SKYCX,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX2',HEAD.SKYCY,CDUMMY,STATUS)
      HEAD.NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,HEAD,STATUS)


C - READ TIME RANGE INFORMATION
      CALL FTMAHD(IUNIT,2,HTYPE,STATUS)

      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD.NTRANGE,CDUMMY,STATUS)
C - CHECK NUMBER OF TIME RANGES DOESN'T EXCEED MAXIMUM
      IF (HEAD.NTRANGE.GT.MAXRAN) HEAD.NTRANGE = MAXRAN
      CALL FTGCVE(IUNIT,1,1,1,HEAD.NTRANGE,0,HEAD.TSTART,LDUMMY,STATUS)
      CALL FTGCVE(IUNIT,2,1,1,HEAD.NTRANGE,0,HEAD.TEND,LDUMMY,STATUS)
      DO I = 1,HEAD.NTRANGE
         HEAD.TSTART(I) = HEAD.TSTART(I) - HEAD.BASE_SCTIME
         HEAD.TEND(I) = HEAD.TEND(I) - HEAD.BASE_SCTIME
      ENDDO

*   Set amplitude ranges
      IF (INDEX(HEAD.DETECTOR,'PSPC') .NE. 0) THEN
         HEAD.ASTART = 1
         HEAD.AEND = 256
*
*   US data uses channel 0 which can't be processed by the system
         IF (HEAD.CSTART .EQ. 0) HEAD.CSTART = 1
*
* NB: The US PSPC CPHA channels are defined to be one less than the MPE files.
*
      ELSEIF (INDEX(HEAD.DETECTOR,'HRI') .NE. 0) THEN
         HEAD.ASTART = HEAD.CSTART
         HEAD.AEND = HEAD.CEND
         HEAD.CSTART = 1
         HEAD.CEND = 1
      ENDIF

      RETURN
      END

*+ RAT_RDRDFHEAD -  Read header information from RAT FITS file
	SUBROUTINE RAT_RDRDFHEAD(IUNIT,HEAD,STATUS)
*    Description :
*      Fill the HEAD structure with the fits file information written in
*      the new RAtionalised style.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed >
*    Authors :
*     Jeremy Ashley
*    History :
*     29-Sep-1993   - J.ASHLEY
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Global parameters
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      INTEGER IUNIT			! Open FITS IO unit
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
      REAL MAXVERS
      PARAMETER (MAXVERS = 2.5)
*    Local variables :
      REAL RVERSION
      INTEGER XS_MJDRD,TSTAT,HTYPE,I
      CHARACTER*8 VERSION
      CHARACTER*12 KEYWORD
      DOUBLE PRECISION DVAL,XS_MJDRF
      CHARACTER*80 CDUMMY
      CHARACTER*8 DATE_OBS,TIME_OBS,DATE_END,TIME_END
      LOGICAL LDUMMY
*-
      IF (STATUS.NE.SAI__OK) RETURN
*

C - MOVE TO PRIMARY HEADER
      CALL FTMAHD(IUNIT,1,HTYPE,STATUS)

C - GET FITS FILE VERSION NUMBER
      CALL FTGKYS(IUNIT,'RDF_VERS',VERSION,CDUMMY,STATUS)

C - CHECK FOR NEW RDF VERSIONS
      CALL CHR_CTOR(VERSION,RVERSION,STATUS)
      IF (MAXVERS.LT.RVERSION) THEN
         CALL MSG_SETR('VER',RVERSION)
         CALL MSG_PRNT('** WARNING, new RDF fits version (^VER) **')
      ENDIF

      CALL RAT_FITSKEY('RDF',VERSION,'TITLE',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD.TITLE,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'TARGET',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD.TARGET,CDUMMY,STATUS)

C - MOVE TO EVENTS HEADER
      CALL FTMAHD(IUNIT,3,HTYPE,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'CSTART',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.CSTART,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'CEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.CEND,CDUMMY,STATUS)
*
      CALL RAT_FITSKEY('RDF',VERSION,'AXIS_RA',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,HEAD.AXIS_RA,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'AXIS_DEC',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,HEAD.AXIS_DEC,CDUMMY,STATUS)

* ROTATION ANGLE
      CALL RAT_FITSKEY('RDF',VERSION,'ROLLCI',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,HEAD.ROLLCI,CDUMMY,STATUS)

* DEGREES PER PIXEL
      CALL RAT_FITSKEY('RDF',VERSION,'PIXEL',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,DVAL,CDUMMY,STATUS)
      HEAD.PIXEL = DVAL * 3600.0

      HEAD.XDSTART = 1
      HEAD.YDSTART = 1

      CALL RAT_FITSKEY('RDF',VERSION,'XDEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.XDEND,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'YDEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.YDEND,CDUMMY,STATUS)
*
* Get exposure time info
      HEAD.NTRANGE=1
      HEAD.TSTART(1)=0.0

      CALL RAT_FITSKEY('RDF',VERSION,'TEND',KEYWORD,STATUS)
      CALL FTGKYE(IUNIT,KEYWORD,HEAD.TEND(1),CDUMMY,STATUS)

* The next parameters will not exist for image data so ignore errors
      HEAD.XSTART = 1
      HEAD.YSTART = 1

      CALL RAT_FITSKEY('RDF',VERSION,'XEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.XEND,CDUMMY,TSTAT)

      CALL RAT_FITSKEY('RDF',VERSION,'YEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.YEND,CDUMMY,TSTAT)

      CALL RAT_FITSKEY('RDF',VERSION,'FDSIZEX',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.IFDSZX,CDUMMY,TSTAT)

      CALL RAT_FITSKEY('RDF',VERSION,'FDSIZEY',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.IFDSZY,CDUMMY,TSTAT)
*
* Dummy up small map values
      HEAD.ISMNUX=1
      HEAD.ISMNUY=1
      HEAD.ISMTNU=1
      HEAD.XSMAP(1)=1
      HEAD.YSMAP(1)=1
*
      CALL RAT_FITSKEY('RDF',VERSION,'EVENTS',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD.IEVTNU,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'OBSERVER',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD.OBSERVER,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'OBSERVATORY',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD.OBSERVATORY,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'INSTRUMENT',KEYWORD,STATUS)
      HEAD.INSTRUMENT = 'XRT'

      CALL RAT_FITSKEY('RDF',VERSION,'DETECTOR',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD.DETECTOR,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'FILTER',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD.FILTER,CDUMMY,STATUS)
      IF (HEAD.FILTER.EQ.'NONE') HEAD.FILTER = 'OFF'

      CALL RAT_FITSKEY('RDF',VERSION,'XPUNITS',KEYWORD,STATUS)
      HEAD.XPUNITS = 'ARCSECONDS'

      CALL RAT_FITSKEY('RDF',VERSION,'YPUNITS',KEYWORD,STATUS)
      HEAD.YPUNITS  = 'ARCSECONDS'

      CALL RAT_FITSKEY('RDF',VERSION,'MJDREFI',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,XS_MJDRD,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'MJDREFF',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,XS_MJDRF,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'DATE_OBS',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,DATE_OBS,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'TIME_OBS',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,TIME_OBS,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'DATE_END',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,DATE_END,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'TIME_END',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,TIME_END,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'SKYCX',KEYWORD,STATUS)
      CALL FTGKYE(IUNIT,KEYWORD,HEAD.SKYCX,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'SKYCY',KEYWORD,STATUS)
      CALL FTGKYE(IUNIT,KEYWORD,HEAD.SKYCY,CDUMMY,STATUS)
      HEAD.NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,HEAD,STATUS)


C - READ TIME RANGE INFORMATION
      CALL FTMAHD(IUNIT,2,HTYPE,STATUS)

      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD.NTRANGE,CDUMMY,STATUS)
C - CHECK NUMBER OF TIME RANGES DOESN'T EXCEED MAXIMUM
      IF (HEAD.NTRANGE.GT.MAXRAN) HEAD.NTRANGE = MAXRAN
      CALL FTGCVE(IUNIT,1,1,1,HEAD.NTRANGE,0,HEAD.TSTART,LDUMMY,STATUS)
      CALL FTGCVE(IUNIT,2,1,1,HEAD.NTRANGE,0,HEAD.TEND,LDUMMY,STATUS)
      DO I = 1,HEAD.NTRANGE
         HEAD.TSTART(I) = HEAD.TSTART(I) - HEAD.BASE_SCTIME
         HEAD.TEND(I) = HEAD.TEND(I) - HEAD.BASE_SCTIME
      ENDDO

*   Set amplitude ranges
      IF (INDEX(HEAD.DETECTOR,'PSPC') .NE. 0) THEN
         HEAD.ASTART = 1
         HEAD.AEND = 256
*
*   US data uses channel 0 which can't be processed by the system
         IF (HEAD.CSTART .EQ. 0) HEAD.CSTART = 1
*
* NB: The US PSPC CPHA channels are defined to be one less than the MPE files.
*
      ELSEIF (INDEX(HEAD.DETECTOR,'HRI') .NE. 0) THEN
         HEAD.ASTART = HEAD.CSTART
         HEAD.AEND = HEAD.CEND
         HEAD.CSTART = 1
         HEAD.CEND = 1
      ENDIF


C -   CHECK STATUS
      IF (STATUS.NE.SAI__OK) THEN
         CALL MSG_SETC('KEY',KEYWORD)
         CALL MSG_PRNT('** ERROR, possibly for key lookup: ^KEY **')
      ENDIF

      RETURN
      END

*+RAT_RDMPEHEAD   Reads header information from a MPE FITS file
	SUBROUTINE RAT_RDMPEHEAD(IUNIT, HEAD, STATUS)
*
* Description :
*      Fill the HEAD structure with the fits file information written in
*      the old MPE style.
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     J.Ashley from original by Richard Saxton
* History :
*     5-Jul-1990   Original
*     27-Sep-1993  Rationalised version
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
* Parameters :
      INTEGER IUNIT			! Logical unit for FITS file
      RECORD /XRT_HEAD/ HEAD		! Header structure
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
* Local constants :
* Local variables :
      CHARACTER*80 STR		      ! General purpose string
      CHARACTER*80 DUMMY
      CHARACTER*30 RASTRING           ! String for RA
      CHARACTER*30 DECSTRING          ! String for DEC
      INTEGER HTYPE
      INTEGER SIZE

      CHARACTER*80 DATE               ! String for date
      CHARACTER*3 MONTH               ! Month as a character string
      CHARACTER*80 TIME               ! Time string
      CHARACTER*1 SIGN
      REAL XSIZE,YSIZE
      INTEGER HRS,MIN,DEG,ISEC
      DOUBLE PRECISION SC_END         ! Final spacecraft time
      REAL SEC
      DOUBLE PRECISION ONOFF(MAXRAN*2)! Time ranges used to select data
      DOUBLE PRECISION  END_MJD       ! MJD of end time
      INTEGER LP,K,K2
      INTEGER YEAR,IMONTH,DAY         ! Start date
      INTEGER IMJD1                   ! MJD of Start date
      INTEGER IMJD2                   ! MJD of end date
      INTEGER KR
      DOUBLE PRECISION MJDBIT         ! Fraction of day
      REAL R1,R2                      ! Real variables
*
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN

***** Get the information put into the FITS in a sensible way
      CALL FTMAHD(IUNIT,2,HTYPE,STATUS)
      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD.IEVTNU,DUMMY,STATUS)

***** Read header information put into FITS in an Irrational way
      CALL GHIST1I(IUNIT,'NO_SM_X',HEAD.ISMNUX,STATUS)
      CALL GHIST1I(IUNIT,'NO_SM_Y',HEAD.ISMNUY,STATUS)
      CALL GHIST1S(IUNIT,'DETECTOR_ID',HEAD.DETECTOR,STATUS)
      CALL GHIST1S(IUNIT,'FILTER_ID',HEAD.FILTER,STATUS)
      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD.TARGET,STATUS)
      CALL GHIST1R(IUNIT,'SKY_PIX_SIZE',HEAD.PIXEL,STATUS)
      CALL GHIST1S(IUNIT,'OBS_ID',HEAD.OBSERVER,STATUS)
      CALL GHIST1S(iUNIT,'MISSION_ID',HEAD.OBSERVATORY,STATUS)
      HEAD.INSTRUMENT = 'XRT'

      IF (STATUS.NE.SAI__OK) GOTO 999
***** REM: Files from AO-1 onwards use LAT and LONG for DEC and RA
      CALL GHIST1S(IUNIT,'POINT_LONG',RASTRING,STATUS)
      CALL GHIST1S(IUNIT,'POINT_LAT',DECSTRING,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_ANNUL(STATUS)
         CALL GHIST1S(IUNIT,'POINT_RA',RASTRING,STATUS)
         CALL GHIST1S(IUNIT,'POINT_DEC',DECSTRING,STATUS)
      ENDIF
      IF (STATUS.NE.SAI__OK) GOTO 999
***** CONVERT INTO DECIMAL DEGREES
      READ(RASTRING,FMT='(I2,X,I2,X,F4.1)')HRS,MIN,SEC
      HEAD.AXIS_RA = (HRS*15.0) + (MIN*15.0/60.0) + (SEC*15.0/3600.)
      READ(DECSTRING,FMT='(A1,I2,X,I2,X,I2)')SIGN,DEG,MIN,ISEC
      HEAD.AXIS_DEC = DEG + (MIN/60.0) + (ISEC/3600.)
      IF (SIGN .EQ. '-') HEAD.AXIS_DEC = -HEAD.AXIS_DEC

***** Read roll angle in degrees
      CALL GHIST1S(IUNIT,'XPIX_TO_NORTH',STR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
      KR=INDEX(STR, 'D')
      IF (KR . EQ. 0) THEN
         READ(STR, *)HEAD.ROLLCI
      ELSE
         READ(STR(1:KR-1), *)HEAD.ROLLCI
      ENDIF
***** Convert roll angle to Asterx standard
      HEAD.ROLLCI = 90.0 - HEAD.ROLLCI

      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD.TITLE,STATUS)
      CALL GHIST1S(IUNIT,'OBS_DATE',DATE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
***** Convert first date in date string to an MJD
      K=INDEX(DATE, '-')
      READ(DATE(K+5:K+8), FMT='(I4)')YEAR
***** The year must be in the form 1990 - so add 1900 if 90.
      IF (YEAR .GT. 3000) YEAR = YEAR/100. + 1900
      MONTH=DATE(K+1:K+3)
      CALL CONV_MONTH(MONTH,IMONTH,STATUS)
*
      IF (STATUS .EQ. SAI__OK) THEN
         READ(DATE(K-2:K-1), FMT='(I2)')DAY
         CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD1)
      ELSE
         CALL MSG_PRNT('*Couldnt understand month string*')
      ENDIF
***** Convert second date in date string to an MJD
      K2=INDEX(DATE(K+8:80), '-') + K+7
      READ(DATE(K2+5:K2+8), FMT='(I4)')YEAR
***** The year must be in the form 1990 - so add 1900 if 90.
      IF (YEAR .GT. 3000) YEAR = YEAR/100. + 1900
      MONTH=DATE(K2+1:K2+3)
      CALL CONV_MONTH(MONTH,IMONTH,STATUS)
*
      IF (STATUS .EQ. SAI__OK) THEN
         READ(DATE(K2-2:K2-1), FMT='(I2)')DAY
         CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD2)
      ELSE
         CALL MSG_PRNT('*Couldnt understand month string*')
      ENDIF
*
      CALL GHIST1S(IUNIT,'OBS_UT',TIME,STATUS)
            K=INDEX(TIME, ':')
            READ(TIME(K-2:K-1), FMT='(I2)')HRS
            READ(TIME(K+1:K+2), FMT='(I2)')MIN
            READ(TIME(K+4:K+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            HEAD.BASE_MJD=DBLE(IMJD1) + MJDBIT/86400.0
*
*      Calculate the end UT time
            K2=INDEX(TIME(K+12:80), ':') + K+11
            READ(TIME(K2-2:K2-1), FMT='(I2)')HRS
            READ(TIME(K2+1:K2+2), FMT='(I2)')MIN
            READ(TIME(K2+4:K2+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            END_MJD=DBLE(IMJD2) + MJDBIT/86400.0
            HEAD.END_MJD = END_MJD
*
      CALL GHIST1S(IUNIT,'OBS_CLOCK',STR,STATUS)
      READ(STR, *)HEAD.BASE_SCTIME,SC_END
      CALL GHIST1S(IUNIT,'TLABL003',STR,STATUS)
      HEAD.XPUNITS=STR(17:27)
      CALL GHIST1S(IUNIT,'TLABL004',STR,STATUS)
      HEAD.YPUNITS=STR(17:27)
      CALL GHIST1S(IUNIT,'RAW_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD.ASTART=INT(R1)
      HEAD.AEND=INT(R2)
      CALL GHIST1S(IUNIT,'AMP_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD.CSTART=INT(R1)
      HEAD.CEND=INT(R2)
      CALL GHISTnD(IUNIT,'TIM_SEL',ONOFF,MAXRAN*2,SIZE,STATUS)
      HEAD.NTRANGE = SIZE / 2
      DO LP=1,HEAD.NTRANGE
         HEAD.TSTART(LP)=ONOFF((LP-1)*2+1) - HEAD.BASE_SCTIME
         HEAD.TEND(LP)=ONOFF(LP*2) - HEAD.BASE_SCTIME
      ENDDO
*
      CALL GHIST1S(IUNIT,'SKY_FIELD',STR,STATUS)
      READ(STR, *)HEAD.XSTART,HEAD.YSTART,HEAD.XEND,HEAD.YEND
      HEAD.IFDSZX = HEAD.XEND - HEAD.XSTART
      HEAD.IFDSZY = HEAD.YEND - HEAD.YSTART

*   Get sky pixel centre
      CALL GHIST1R(IUNIT,'SKY_CEN_X',HEAD.SKYCX,STATUS)
      CALL GHIST1R(IUNIT,'SKY_CEN_Y',HEAD.SKYCY,STATUS)
*
      CALL GHIST1S(IUNIT,'DET_FIELD',STR,STATUS)
      READ(STR, *)HEAD.XDSTART,HEAD.YDSTART,HEAD.XDEND,HEAD.YDEND

*  Read in small map pointers
      CALL GHISTnI(IUNIT,'START_SM',HEAD.EVSTART,MAXMAPS,SIZE,STATUS)
      HEAD.ISMTNU=HEAD.ISMNUX*HEAD.ISMNUY

* Calculate the conversion factor between spacecraft time and UT
      HEAD.SCCONV = (END_MJD - HEAD.BASE_MJD) * 86400. /
     &                            (SC_END - HEAD.BASE_SCTIME)
* Create array of small map start positions
      XSIZE = HEAD.IFDSZX / REAL(HEAD.ISMNUX)
      YSIZE = HEAD.IFDSZY / REAL(HEAD.ISMNUY)

      DO LP=1,HEAD.ISMTNU
        HEAD.XSMAP(LP) = HEAD.XSTART+MOD((LP-1),HEAD.ISMNUX) * XSIZE
        HEAD.YSMAP(LP) = HEAD.YSTART+INT((LP-1)/HEAD.ISMNUY) * YSIZE
      ENDDO
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','from RAT_RDMPEHEAD',STATUS)
      ENDIF
*
      END



*+ RAT_RDIMAGE -  Read image header information
	SUBROUTINE RAT_RDIMAGE(IUNIT,ORIGIN,HEAD,STATUS)
*    Description :
*      Read the header information present in an image fits datafile
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed >
*    Authors :
*     Jeremy Ashley
*    History :
*     23-Sep-1993   - Converted from old FX_RDHDR
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Global parameters
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      INTEGER IUNIT			! Open FITS IO unit
      CHARACTER*5 ORIGIN                ! FITS file type
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
*-
      IF (STATUS.NE.SAI__OK) RETURN

      IF (origin.eq.'US')  CALL RAT_RDUSIMAGE(iunit,head,STATUS)
      IF (origin.eq.'RDF') CALL RAT_RDRDFIMAGE(iunit,head,STATUS)
      IF (origin.eq.'MPE') CALL RAT_RDMPEIMAGE(iunit,head,STATUS)

      RETURN
      END

*+ RAT_RDUSIMAGE -  Read image header information from US FITS file
	SUBROUTINE RAT_RDUSIMAGE(IUNIT,HEAD,STATUS)
*    Description :
*      Read the header information present in US image fits datafile required
*      for producing standard ASTERIX image files.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed >
*    Authors :
*     Jeremy Ashley
*    History :
*     23-Sep-1993   - Converted from old FX_RDHDR
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Global parameters
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      INTEGER IUNIT			! Open FITS IO unit
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER IVAL,XS_MJDRD,XS_EVREF,HTYPE
      DOUBLE PRECISION DVAL,XS_MJDRF
      CHARACTER*80 CDUMMY,CVAL
      CHARACTER*8 DATE_OBS,TIME_OBS,DATE_END,TIME_END
*-
      IF (STATUS.NE.SAI__OK) RETURN
*

C - MOVE TO PRIMARY HEADER
      CALL FTMAHD(IUNIT,1,HTYPE,STATUS)

      CALL FTGKYS(IUNIT,'TITLE',HEAD.TITLE,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'OBJECT',CVAL,CDUMMY,STATUS)
      HEAD.TARGET = CVAL(1:20)
      IF (STATUS.NE.0) RETURN

      CALL FTGKYJ(IUNIT,'XS-MINCH',HEAD.CSTART,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-MAXCH',HEAD.CEND,CDUMMY,STATUS)
*
      CALL FTGKYD(IUNIT,'CRVAL1',HEAD.AXIS_RA,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CRVAL2',HEAD.AXIS_DEC,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CROTA2',HEAD.ROLLCI,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CDELT2',DVAL,CDUMMY,STATUS)
      HEAD.PIXEL = DVAL * 3600.0

      HEAD.XDSTART = 1
      HEAD.YDSTART = 1
      CALL FTGKYJ(IUNIT,'XS-XDET',HEAD.XDEND,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-YDET',HEAD.YDEND,CDUMMY,STATUS)
*
* Get exposure time info
      HEAD.NTRANGE=1
      HEAD.TSTART(1)=0.0
      CALL FTGKYE(IUNIT,'XS-LIVTI',HEAD.TEND(1),CDUMMY,STATUS)

      CALL FTGKYS(IUNIT,'XS-SEQPI',CVAL,CDUMMY,STATUS)
      HEAD.OBSERVER = CVAL(1:20)
      CALL FTGKYS(IUNIT,'TELESCOP',HEAD.OBSERVATORY,CDUMMY,STATUS)
      HEAD.INSTRUMENT = 'XRT'
      CALL FTGKYS(IUNIT,'INSTRUME',HEAD.DETECTOR,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-FILTR',IVAL,CDUMMY,STATUS)
      IF (IVAL.EQ.0) HEAD.FILTER = 'OFF'
      IF (IVAL.EQ.1) HEAD.FILTER = 'BORON'
      HEAD.XPUNITS = 'ARCSECONDS'
      HEAD.YPUNITS  = 'ARCSECONDS'
      CALL FTGKYJ(IUNIT,'XS-MJDRD',XS_MJDRD,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'XS-MJDRF',XS_MJDRF,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-OBS',DATE_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-OBS',TIME_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-END',DATE_END,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-END',TIME_END,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-EVREF',XS_EVREF,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX1',HEAD.SKYCX,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX2',HEAD.SKYCY,CDUMMY,STATUS)
      HEAD.NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,HEAD,STATUS)

      RETURN
      END

*+ RAT_RDRDFIMAGE -  Read image header information from RAT FITS file
	SUBROUTINE RAT_RDRDFIMAGE(IUNIT,HEAD,STATUS)
*    Description :
*      Read the header information present in RAT image fits datafile required
*      for producing standard ASTERIX image files.
*    Environment parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*     <description of any "bugs" which have not been fixed >
*     Doesn't actually work yet.  Information required on new Rationalised
*     formats - 25th October 1993 (JKA)
*    Authors :
*     Jeremy Ashley
*    History :
*     29-Sep-1993   - J.ASHLEY
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*     <any INCLUDE files containing global constant definitions>
*    Global variables :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Global parameters
      RECORD /XRT_HEAD/ HEAD 		! Structure to hold header information
      INTEGER IUNIT			! Open FITS IO unit
*    Structure definitions :
*     <specification of FORTRAN structures>
*    Status :
      INTEGER STATUS
*    Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER XS_MJDRD,HTYPE
      CHARACTER*8 RAT_VERS
      DOUBLE PRECISION DVAL,XS_MJDRF
      CHARACTER*80 CDUMMY
      CHARACTER*8 DATE_OBS,TIME_OBS,DATE_END,TIME_END
*-
      IF (STATUS.NE.SAI__OK) RETURN
*

C - MOVE TO PRIMARY HEADER
      CALL FTMAHD(IUNIT,1,HTYPE,STATUS)

C - GET FITS FILE VERSION NUMBER
      CALL FTGKYS(IUNIT,'RDF_VERS',RAT_VERS,CDUMMY,STATUS)

      CALL FTGKYS(IUNIT,'OBJECT',HEAD.TITLE,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'OBJECT',HEAD.TARGET,CDUMMY,STATUS)

      IF (STATUS.NE.0) RETURN
*
*     PI channel (not on HRI)
      CALL FTGKYS(IUNIT,'INSTRUME',HEAD.DETECTOR,CDUMMY,STATUS)

      IF (HEAD.DETECTOR.EQ.'HRI') THEN
         CALL FTGKYJ(IUNIT,'PHAMIN',HEAD.CSTART,CDUMMY,STATUS)
         CALL FTGKYJ(IUNIT,'PHAMAX',HEAD.CEND,CDUMMY,STATUS)
      ELSE
         CALL FTGKYJ(IUNIT,'PIMIN',HEAD.CSTART,CDUMMY,STATUS)
         CALL FTGKYJ(IUNIT,'PIMAX',HEAD.CEND,CDUMMY,STATUS)
      ENDIF
*
*     RA, DEC & ROLL
      CALL FTGKYD(IUNIT,'RA_NOM',HEAD.AXIS_RA,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'DEC_NOM',HEAD.AXIS_DEC,CDUMMY,STATUS)
      HEAD.ROLLCI=0.0
*
*     Degrees per pixel (use CDELT1 or CDELT2)
      CALL FTGKYD(IUNIT,'CDELT2',DVAL,CDUMMY,STATUS)
      IF (STATUS.NE.0) RETURN
      HEAD.PIXEL = DVAL * 3600.0
*
* Get exposure time info
      HEAD.NTRANGE=1
      HEAD.TSTART(1)=0.0
      CALL FTGKYE(IUNIT,'LIVETIME',HEAD.TEND(1),CDUMMY,STATUS)
*
      CALL FTGKYS(IUNIT,'OBSERVER',HEAD.OBSERVER,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TELESCOP',HEAD.OBSERVATORY,CDUMMY,STATUS)
      HEAD.INSTRUMENT = 'XRT'
      CALL FTGKYS(IUNIT,'FILTER',HEAD.FILTER,CDUMMY,STATUS)
      IF (HEAD.FILTER.EQ.'NONE') HEAD.FILTER = 'OFF'
      HEAD.XPUNITS = 'ARCSECONDS'
      HEAD.YPUNITS  = 'ARCSECONDS'
      CALL FTGKYJ(IUNIT,'MJDREFI',XS_MJDRD,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'MJDREFF',XS_MJDRF,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-OBS',DATE_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-OBS',TIME_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE_END',DATE_END,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME_END',TIME_END,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX1',HEAD.SKYCX,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX2',HEAD.SKYCY,CDUMMY,STATUS)
      HEAD.NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,HEAD,STATUS)


*   Set amplitude ranges
      IF (INDEX(HEAD.DETECTOR,'PSPC') .NE. 0) THEN
         HEAD.ASTART = 1
         HEAD.AEND = 256
*
*   US data uses channel 0 which can't be processed by the system
         IF (HEAD.CSTART .EQ. 0) HEAD.CSTART = 1
*
* NB: The US PSPC CPHA channels are defined to be one less than the MPE files.
*
      ELSEIF (INDEX(HEAD.DETECTOR,'HRI') .NE. 0) THEN
         HEAD.ASTART = HEAD.CSTART
         HEAD.AEND = HEAD.CEND
         HEAD.CSTART = 1
         HEAD.CEND = 1
      ENDIF

      RETURN
      END

*+RAT_RDMPEIMAGE   Reads image header information from a MPE FITS file
	SUBROUTINE RAT_RDMPEIMAGE(IUNIT, HEAD, STATUS)
*
* Description :
*      Read the header information present in US image fits datafile required
*      for producing standard ASTERIX image files.
* Environment parameters :
* Method :
* Deficiencies :
* Bugs :
* Authors :
*     J.Ashley from original by Richard Saxton
* History :
*     5-Jul-1990   Original
*     27-Sep-1993  Rationalised version
* Type Definitions :
      IMPLICIT NONE
* Global constants :
      INCLUDE 'SAE_PAR'
* Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
* Parameters :
      INTEGER IUNIT			! Logical unit for FITS file
      RECORD /XRT_HEAD/ HEAD		! Header structure
* Status :
      INTEGER STATUS
* Function declarations :
      INTEGER CHR_LEN
        EXTERNAL CHR_LEN
* Local constants :
* Local variables :
      CHARACTER*80 STR		      ! General purpose string
      CHARACTER*80 DUMMY
      CHARACTER*30 RASTRING           ! String for RA
      CHARACTER*30 DECSTRING          ! String for DEC
      INTEGER HTYPE
      INTEGER SIZE,LP

      CHARACTER*80 DATE               ! String for date
      CHARACTER*3 MONTH               ! Month as a character string
      CHARACTER*80 TIME               ! Time string
      CHARACTER*1 SIGN
      INTEGER HRS,MIN,DEG,ISEC
      DOUBLE PRECISION SC_END         ! Final spacecraft time
      REAL SEC
      DOUBLE PRECISION ONOFF(MAXRAN*2)! Time ranges used to select data
      DOUBLE PRECISION  END_MJD       ! MJD of end time
      INTEGER K,K2
      INTEGER YEAR,IMONTH,DAY         ! Start date
      INTEGER IMJD1                   ! MJD of Start date
      INTEGER IMJD2                   ! MJD of end date
      INTEGER KR
      DOUBLE PRECISION MJDBIT         ! Fraction of day
      REAL R1,R2                      ! Real variables
*
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN

***** Get the information put into the FITS in a sensible way
      CALL FTMAHD(IUNIT,1,HTYPE,STATUS)
      CALL FTGKYS(IUNIT,'CTYPE1',HEAD.XPUNITS,DUMMY,STATUS)
      CALL FTGKYS(IUNIT,'CTYPE2',HEAD.YPUNITS,DUMMY,STATUS)

***** Read header information put into FITS in an Irrational way
      CALL GHIST1S(IUNIT,'DETECTOR_ID',HEAD.DETECTOR,STATUS)
      CALL GHIST1S(IUNIT,'FILTER_ID',HEAD.FILTER,STATUS)
      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD.TARGET,STATUS)
      CALL GHIST1R(IUNIT,'SKY_PIX_SIZE',HEAD.PIXEL,STATUS)
      CALL GHIST1S(IUNIT,'OBS_ID',HEAD.OBSERVER,STATUS)
      CALL GHIST1S(iUNIT,'MISSION_ID',HEAD.OBSERVATORY,STATUS)
      HEAD.INSTRUMENT = 'XRT'

      IF (STATUS.NE.SAI__OK) GOTO 999
***** REM: Files from AO-1 onwards use LAT and LONG for DEC and RA
      CALL GHIST1S(IUNIT,'POINT_LONG',RASTRING,STATUS)
      CALL GHIST1S(IUNIT,'POINT_LAT',DECSTRING,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_ANNUL(STATUS)
         CALL GHIST1S(IUNIT,'POINT_RA',RASTRING,STATUS)
         CALL GHIST1S(IUNIT,'POINT_DEC',DECSTRING,STATUS)
      ENDIF
      IF (STATUS.NE.SAI__OK) GOTO 999
***** CONVERT INTO DECIMAL DEGREES
      READ(RASTRING,FMT='(I2,X,I2,X,F4.1)')HRS,MIN,SEC
      HEAD.AXIS_RA = HRS*15.0 + MIN*15.0/60.0 + SEC*15.0/3600.
      READ(DECSTRING,FMT='(A1,I2,X,I2,X,I2)')SIGN,DEG,MIN,ISEC
      HEAD.AXIS_DEC = DEG + MIN/60.0 + ISEC/3600.
      IF (SIGN .EQ. '-') HEAD.AXIS_DEC = -HEAD.AXIS_DEC

***** Read roll angle in degrees
      CALL GHIST1S(IUNIT,'XPIX_TO_NORTH',STR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
      KR=INDEX(STR, 'D')
      IF (KR . EQ. 0) THEN
         READ(STR, *)HEAD.ROLLCI
      ELSE
         READ(STR(1:KR-1), *)HEAD.ROLLCI
      ENDIF
***** Convert roll angle to Asterx standard
      HEAD.ROLLCI = 90.0 - HEAD.ROLLCI

      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD.TITLE,STATUS)
      CALL GHIST1S(IUNIT,'OBS_DATE',DATE,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
***** Convert first date in date string to an MJD
      K=INDEX(DATE, '-')
      READ(DATE(K+5:K+8), FMT='(I4)')YEAR
***** The year must be in the form 1990 - so add 1900 if 90.
      IF (YEAR .GT. 3000) YEAR = YEAR/100. + 1900
      MONTH=DATE(K+1:K+3)
      CALL CONV_MONTH(MONTH,IMONTH,STATUS)
*
      IF (STATUS .EQ. SAI__OK) THEN
         READ(DATE(K-2:K-1), FMT='(I2)')DAY
         CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD1)
      ELSE
         CALL MSG_PRNT('*Couldnt understand month string*')
      ENDIF
***** Convert second date in date string to an MJD
      K2=INDEX(DATE(K+8:80), '-') + K+7
      READ(DATE(K2+5:K2+8), FMT='(I4)')YEAR
***** The year must be in the form 1990 - so add 1900 if 90.
      IF (YEAR .GT. 3000) YEAR = YEAR/100. + 1900
      MONTH=DATE(K2+1:K2+3)
      CALL CONV_MONTH(MONTH,IMONTH,STATUS)
*
      IF (STATUS .EQ. SAI__OK) THEN
         READ(DATE(K2-2:K2-1), FMT='(I2)')DAY
         CALL CONV_YMDMJD(YEAR, IMONTH, DAY, IMJD2)
      ELSE
         CALL MSG_PRNT('*Couldnt understand month string*')
      ENDIF
*
      CALL GHIST1S(IUNIT,'OBS_UT',TIME,STATUS)
            K=INDEX(TIME, ':')
            READ(TIME(K-2:K-1), FMT='(I2)')HRS
            READ(TIME(K+1:K+2), FMT='(I2)')MIN
            READ(TIME(K+4:K+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            HEAD.BASE_MJD=DBLE(IMJD1) + MJDBIT/86400.0
*
*      Calculate the end UT time
            K2=INDEX(TIME(K+12:80), ':') + K+11
            READ(TIME(K2-2:K2-1), FMT='(I2)')HRS
            READ(TIME(K2+1:K2+2), FMT='(I2)')MIN
            READ(TIME(K2+4:K2+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            END_MJD=DBLE(IMJD2) + MJDBIT/86400.0
            HEAD.END_MJD = END_MJD
*
      CALL GHIST1S(IUNIT,'OBS_CLOCK',STR,STATUS)
      READ(STR, *)HEAD.BASE_SCTIME,SC_END
      CALL GHIST1S(IUNIT,'RAW_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD.ASTART=INT(R1)
      HEAD.AEND=INT(R2)
      CALL GHIST1S(IUNIT,'AMP_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD.CSTART=INT(R1)
      HEAD.CEND=INT(R2)
* if no TIM_SEL then keep current values
      CALL GHISTnD(IUNIT,'TIM_SEL',ONOFF,MAXRAN*2,SIZE,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_ANNUL(STATUS)
      ELSE
         HEAD.NTRANGE = SIZE / 2
         DO LP=1,HEAD.NTRANGE
            HEAD.TSTART(LP)=ONOFF((LP-1)*2+1) - HEAD.BASE_SCTIME
            HEAD.TEND(LP)=ONOFF(LP*2) - HEAD.BASE_SCTIME
         ENDDO
      ENDIF
*   Get sky pixel centre
      CALL GHIST1R(IUNIT,'SKY_CEN_X',HEAD.SKYCX,STATUS)
      CALL GHIST1R(IUNIT,'SKY_CEN_Y',HEAD.SKYCY,STATUS)
* Calculate the conversion factor between spacecraft time and UT
      HEAD.SCCONV = (END_MJD - HEAD.BASE_MJD) * 86400. /
     &                            (SC_END - HEAD.BASE_SCTIME)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','from RAT_RDMPEHEAD',STATUS)
      ENDIF
*
      END




*+RAT_SORT_BIN - Sorts rationalised XRT raw data into a binned data array
      SUBROUTINE RAT_SORT_BIN(HEAD, SRT, BSRT, SDIM1, SDIM2,
     &           SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDIM1,
     &           BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &           NRBIN, NAZBIN, SMASK, BMASK, ELIPA2, ELIPB2,
     &           SDATA, BDATA, SQUAL, BQUAL, STATUS)
*    Description :
*        Sorts events from an XRT hds event datafile into a temporary
*       array of 7 dimensions.
*    History :
*     2-Nov-1988   original (LTVAD::RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'QUAL_PAR'
*    Global parameters
*     ROOTNAME  - hds events filename
*     DIFFILE   - hds diff events filename
*    Status :
      INTEGER STATUS
*    Structures :
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      RECORD /XRT_HEAD/ HEAD                      ! Header values
*
      RECORD /XRT_SCFDEF/ SRT                     ! Source sorting parameters
      RECORD /XRT_SCFDEF/ BSRT                    ! Bckgnd sorting parameters
      INTEGER SDIM1,SDIM2,SDIM3,SDIM4,SDIM5       ! Dimensions of source array
      INTEGER SDIM6,SDIM7                         ! Dimensions of source array
      INTEGER BDIM1,BDIM2,BDIM3,BDIM4,BDIM5       ! Dimensions of bckgnd array
      INTEGER BDIM6,BDIM7                         ! Dimensions of bckgnd array
      INTEGER NRBIN                               ! Number of radial bins
      INTEGER NAZBIN                              ! Number of azimuthal bins
      LOGICAL SMASK(SDIM1,SDIM2)                  ! Source image pixel mask
      LOGICAL BMASK(BDIM1,BDIM2)                  ! Bckgnd image pixel mask
*
*    Import-Export :
      REAL ELIPA2(NRBIN)                          ! Square of elliptical
                                                  ! X axes (for each rad. bin)
      REAL ELIPB2(NRBIN)                          ! Square of elliptical Y axes
      REAL SDATA(SDIM1,SDIM2,SDIM3,SDIM4,SDIM5,SDIM6,SDIM7)  ! S array
      REAL BDATA(BDIM1,BDIM2,BDIM3,BDIM4,BDIM5,BDIM6,BDIM7)  ! b array
      BYTE SQUAL(SDIM1,SDIM2,SDIM3,SDIM4,SDIM5,SDIM6,SDIM7)  ! S quality
      BYTE BQUAL(BDIM1,BDIM2,BDIM3,BDIM4,BDIM5,BDIM6,BDIM7)  ! B quality
*    Export :
*    Functions :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local constants :
      INTEGER MAXBAD
         PARAMETER (MAXBAD=1000)                  ! Max no. of bad time periods
*    Local variables :
      CHARACTER*20 EXT                            ! File extension name
      CHARACTER*80 DNAME                    ! Names of files
      REAL STBAD(MAXBAD), ENBAD(MAXBAD)           ! Bad time periods
      REAL XWIDTH,YWIDTH,TWIDTH,EWIDTH,PWIDTH     ! Binwidth of each axis
      REAL BXWIDTH,BYWIDTH                        ! Binwidth in backgnd box
      REAL XDWID,YDWID                            ! Width of detector axes
      REAL ELAWID,ELBWID                          ! Binwidth of elliptic axes
      REAL T1,T2                                  ! Lower and upper limits of
*                                                 ! a time bin
      INTEGER NBAD                                ! Number of bad time ranges
      INTEGER TLP,LP1,LP2,LP3,LP4,LP6,LP7,INLP,LP
      INTEGER BADEV                               ! No of events in hotspots
      LOGICAL QVAL
      CHARACTER*(DAT__SZLOC) ELOC,DLOC          ! Locators to datafiles
      CHARACTER*(DAT__SZLOC) LOCA(7),SLOCA(7)   ! Locators to data arrays
      INTEGER PTRA(7),NELEMS     ! Pointer to mapped arrays and item count
      INTEGER IX,UPPER,LOWER     ! number of indexes and ranges
*
* Set events in hotspots counter to zero
      BADEV = 0
*
* Calculate bin widths for each axis. NB: SDIM1 can refer to the no. of
* radial bins.
      IF (NRBIN .EQ. 1) THEN
         XWIDTH = (SRT.MAX_X - SRT.MIN_X + 1) / REAL(SDIM1)
         YWIDTH = (SRT.MAX_Y - SRT.MIN_Y + 1) / REAL(SDIM2)
      ELSE
         XWIDTH = (SRT.MAX_X - SRT.MIN_X + 1) / 1.0
         YWIDTH = (SRT.MAX_Y - SRT.MIN_Y + 1) / 1.0
      ENDIF
*
      IF (SDIM1 .EQ. 1 .AND. SDIM2 .EQ. 1) THEN
         BXWIDTH = (BSRT.MAX_X - BSRT.MIN_X + 1) / REAL(BDIM1)
         BYWIDTH = (BSRT.MAX_Y - BSRT.MIN_Y + 1) / REAL(BDIM2)
      ELSE
         BXWIDTH=XWIDTH
         BYWIDTH=YWIDTH
      ENDIF
*
      XDWID = (SRT.MAX_XD - SRT.MIN_XD + 1) / REAL(SDIM3)
      YDWID = (SRT.MAX_YD - SRT.MIN_YD + 1) / REAL(SDIM4)
      TWIDTH = (SRT.MAX_T(SRT.NTIME) - SRT.MIN_T(1)) / REAL(SDIM5)
      PWIDTH = (SRT.MAX_PH - SRT.MIN_PH + 1) / REAL(SDIM6)
      EWIDTH = (SRT.MAX_EN - SRT.MIN_EN + 1) / REAL(SDIM7)
*
* Generate the squares of the elliptical minor and major axes
* for each radial bin (Max values).
      IF (NRBIN .GT. 1) THEN
*   Calculate binwidths for elliptical minor and major axes
         ELAWID = (SRT.ELAMAX - SRT.ELAMIN) / REAL(NRBIN)
         ELBWID = (SRT.ELBMAX - SRT.ELBMIN) / REAL(NRBIN)
*
         DO LP=1,NRBIN
            ELIPA2(LP) = (SRT.ELAMIN + (LP) * ELAWID) **2
            ELIPB2(LP) = (SRT.ELBMIN + (LP) * ELBWID) **2
         ENDDO
      ENDIF
*
*    Open observation event file STDEVT
      CALL RAT_HDLOOKUP(HEAD,'STDEVT','EXTNAME',EXT,STATUS)
      CALL HDS_OPEN(SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//EXT,
     &              'READONLY',ELOC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Generate index into events
C     CALL RAT_INDX_COORD(HEAD,SRT,BSRT,EINDEX,STATUS)

***   Open quality file if wanted and get array of bad times for this obs.
      IF (SRT.QUAL_MORE .OR. SRT.QUAL_LESS) THEN
C         CALL XSORT_BADTIME(SRT,OBS,MAXBAD,NBAD,STBAD,ENBAD,STATUS)
C         IF (STATUS .NE. SAI__OK) GOTO 999
      ENDIF

***   Have the quality constraints been relaxed ?
      IF (SRT.QUAL_LESS) THEN

***      Open observation diff evenfile
         CALL RAT_HDLOOKUP(HEAD,'REJEVT','EXTNAME',EXT,STATUS)
         CALL USI_DEF0C('DIFFILE',SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))
     &                  //EXT,STATUS)
         CALL USI_GET0C('DIFFILE',DNAME,STATUS)
         CALL HDS_OPEN(DNAME,'READONLY',DLOC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

***   Locate the various data arrays
      CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,ELOC,LOCA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   For each index entry
      DO IX = 1,1
         LOWER = 1
         UPPER = HEAD.IEVTNU

***      map the event data arrays into memory
         CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &      NELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

***      Check them against the sort parameters
         CALL RAT_DOIT_BIN(HEAD, SRT, BSRT,%val(PTRA(1)),%val(PTRA(2)),
     &      %val(PTRA(3)), %val(PTRA(4)), %val(PTRA(5)), %val(PTRA(6)),
     &      %val(PTRA(7)), NELEMS, SDATA, SDIM1, SDIM2,
     &      SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDATA, BDIM1,
     &      BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &      SRT.QUAL_MORE, MAXBAD, NBAD, STBAD, ENBAD, XWIDTH, YWIDTH,
     &      XDWID, YDWID, TWIDTH, PWIDTH, EWIDTH, BXWIDTH,
     &      BYWIDTH, NRBIN, NAZBIN, ELIPA2, ELIPB2, BADEV)

***      unmap the arrays & memory
         CALL RAT_UNMAPEVE(SLOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDDO

***   annul the array locators
      CALL RAT_ANNULEVE(LOCA, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Same for the diff data?
      IF (SRT.QUAL_LESS) THEN
***      Locate the various data arrays
         CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,DLOC,LOCA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

***      Find the size of the diff events
         CALL CMP_SIZE(DLOC,'TIME',UPPER,STATUS)
         DO IX = 1,1
            LOWER = 1

***         map the diff event data arrays into memory
            CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &         NELEMS,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999

***         Check them against the sort parameters
            CALL RAT_DOIT_BIN(HEAD,SRT,BSRT,%val(PTRA(1)),%val(PTRA(2)),
     &         %val(PTRA(3)),%val(PTRA(4)),%val(PTRA(5)), %val(PTRA(6)),
     &         %val(PTRA(7)), NELEMS, SDATA, SDIM1, SDIM2,
     &         SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDATA, BDIM1,
     &         BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &         SRT.QUAL_MORE, MAXBAD, NBAD,STBAD, ENBAD, XWIDTH, YWIDTH,
     &         XDWID, YDWID, TWIDTH, PWIDTH, EWIDTH, BXWIDTH,
     &         BYWIDTH, NRBIN, NAZBIN, ELIPA2, ELIPB2, BADEV)

***         unmap the arrays & memory
            CALL RAT_UNMAPEVE(SLOCA, STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999
         ENDDO

***      annul the array locators
         CALL RAT_ANNULEVE(LOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

      IF ( INDEX(HEAD.DETECTOR, 'HRI') .NE. 0) THEN
         CALL MSG_SETI('BAD', BADEV)
         CALL MSG_PRNT('Rejected ^BAD events found in '/
     &                                /'hotspots/deadspots')
      ENDIF

***  Set quality values for each pixel
***   Loop over each time bin
      DO TLP=1,SDIM5
***      Calculate lower and upper times of this bin
         T1=SRT.MIN_T(1) + TWIDTH * (TLP-1.0)
         T2=SRT.MIN_T(1) + TWIDTH * TLP

***      Test if this bin was within any of the on times of the instrument
***      Set the quality value to 0 if the bin is good or 1 if it is bad.
         QVAL=.FALSE.
         DO INLP=1,HEAD.NTRANGE
            IF (HEAD.TSTART(INLP).LT.T2.AND.
     &                         HEAD.TEND(INLP).GT.T1) THEN
               QVAL = .TRUE.
               GOTO 100
            ENDIF
         ENDDO

100      CONTINUE

***      If the time is within the pre-selection windows - check if it
***      is within the windows selected in XSORT
         IF (QVAL) THEN
*
            QVAL=.FALSE.
            DO INLP=1,SRT.NTIME
               IF ( SRT.MIN_T(INLP) .LT. T2 .AND.
     &                         SRT.MAX_T(INLP) .GT. T1 ) THEN
                  QVAL = .TRUE.
                  GOTO 110
               ENDIF
            ENDDO
         ENDIF
*
110      CONTINUE
*
*       Loop over the remaining dimensions of the source and background
*       arrays
         DO LP7=1,SDIM7
           DO LP6=1,SDIM6
            DO LP4=1,SDIM4
             DO LP3=1,SDIM3

               DO LP2=1,SDIM2
                  DO LP1=1,SDIM1
                     IF (.NOT. QVAL .OR. SMASK(LP1,LP2)) THEN
                       SQUAL(LP1,LP2,LP3,LP4,TLP,LP6,LP7)=QUAL__MISSING
                     ENDIF
                  ENDDO
               ENDDO
***            Background quality
               IF (SRT.BCKGND) THEN
                  DO LP2=1,BDIM2
                     DO LP1=1,BDIM1
                       IF (.NOT. QVAL .OR. BMASK(LP1,LP2)) THEN
                        BQUAL(LP1,LP2,LP3,LP4,TLP,LP6,LP7)=QUAL__MISSING
                       ENDIF
                     ENDDO
                  ENDDO
               ENDIF

            ENDDO
           ENDDO
          ENDDO
         ENDDO
      ENDDO

***   Close any opened files
      CALL HDS_CLOSE(ELOC, STATUS)
      IF (SRT.QUAL_LESS) CALL HDS_CLOSE(DLOC, STATUS)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from RAT_SORT_BIN',STATUS)
      ENDIF
*
      END


*+RAT_DOIT_BIN     checks mapped events against sort parameters
      SUBROUTINE RAT_DOIT_BIN(HEAD, SRT, BSRT, TIME, XPIX, YPIX, XDET,
     &              YDET, AMPL, CAMPL, NELEMS, SDATA, SDIM1, SDIM2,
     &              SDIM3, SDIM4, SDIM5, SDIM6, SDIM7, BDATA, BDIM1,
     &              BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7,
     &              QCHECK, MAXBAD, NBAD, STBAD, ENBAD, XWIDTH, YWIDTH,
     &              XDWID, YDWID, TWIDTH, PWIDTH, EWIDTH, BXWIDTH,
     &              BYWIDTH, NRBIN, NAZBIN, ELIPA2, ELIPB2, BADEV)
*    Description :
*    History :
*     30-NOV-93     Copied from original by LTVAD::RDS (LTVAD::JKA)
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      RECORD /XRT_HEAD/ HEAD            ! HEADER values
      RECORD /XRT_SCFDEF/ SRT, BSRT     ! Sort parameters
      INTEGER NELEMS			! length of data arrays
      DOUBLE PRECISION TIME(NELEMS)     ! Event times
      INTEGER XPIX(NELEMS), YPIX(NELEMS)  ! Array of coordinates
      INTEGER XDET(NELEMS), YDET(NELEMS)  ! Array of detector coords
      INTEGER AMPL(NELEMS), CAMPL(NELEMS) ! Array of photon events
      INTEGER SDIM1, SDIM2, SDIM3, SDIM4, SDIM5, SDIM6, SDIM7
      INTEGER BDIM1, BDIM2, BDIM3, BDIM4, BDIM5, BDIM6, BDIM7
      LOGICAL QCHECK                    ! Check quality of each event
      INTEGER MAXBAD,NBAD               ! Dimension & NUMBER of bad arrays
      REAL STBAD(MAXBAD),ENBAD(MAXBAD)  ! Start & End times of bad data
      REAL XWIDTH,YWIDTH,TWIDTH,PWIDTH,EWIDTH     ! Bin widths of axes
      REAL XDWID,YDWID                  ! Bin widths of detector axes
      REAL BXWIDTH, BYWIDTH             ! Width of bckgnd im pixels
      INTEGER NRBIN                     ! Number of output radial bins
      INTEGER NAZBIN                    ! Number of output azim. bins
      REAL ELIPA2(NRBIN),ELIPB2(NRBIN)  ! Squares of elliptic axes
*    Import-Export :
      REAL SDATA(SDIM1,SDIM2,SDIM3,SDIM4,SDIM5,SDIM6,SDIM7)   ! Source array
      REAL BDATA(BDIM1,BDIM2,BDIM3,BDIM4,BDIM5,BDIM6,BDIM7)   ! Source array
      INTEGER BADEV                               ! No of events in hotspots
*    Functions :
      LOGICAL XRT_HSPOT
        EXTERNAL XRT_HSPOT
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER BLP,TLP
      INTEGER XEV,YEV,XDEV,YDEV,AEV,CEV
      DOUBLE PRECISION TEV,BSCTIM
      INTEGER EL1,EL2,EL3,EL4,EL5,EL6,EL7
      INTEGER SCEN_X,SCEN_Y                  ! Centre pixel position of src box
      INTEGER BCEN_X,BCEN_Y                  ! Centre pixel position of bck box
      INTEGER SAVE_YMIN,SAVE_YMAX
*
      REAL SAMIN2,SAMAX2,SBMIN2,SBMAX2       ! Squares of the min. and max.
*                                            ! values for the source ellip axes.
      REAL BAMIN2,BAMAX2,BBMIN2,BBMAX2       !    Same for the background.
      REAL SA2B2I,SA2B2O                     ! Product of the squares of the
*                                            ! inner and outer source axes
      REAL BA2B2I,BA2B2O                     ! Product of the squares of the
*                                            ! inner and outer background axes
      REAL SDIFFX,SDIFFY                     ! X,Y offset from box centre (pix)
      REAL BDIFFX,BDIFFY                     !    Same for the background
      REAL SELPX,SELPY                       ! X,Y position in elliptical cords
      REAL BELPX,BELPY                       !   same for the background box
      REAL SELPX2,SELPY2                     ! Square of the photon X,Y pos.
*                                            ! in source box elliptical coords
      REAL BELPX2,BELPY2                     ! Square of the photon X,Y pos.
*                                            ! in bckgnd box elliptical coords
      REAL SCPHI,SSPHI                       ! Cos and Sine of src angle
      REAL BCPHI,BSPHI                       ! Cos and Sine of bck angle
*
      LOGICAL LHRI                           ! Is it an HRI file ?
      LOGICAL TOK                            ! Is this event within time ranges?
      LOGICAL OK
      INTEGER IX,YMAX                        ! Counter
*-
      SAVE_YMIN = SRT.MIN_Y
      SAVE_YMAX = SRT.MAX_Y

      YMAX = HEAD.YEND

C      IF (.NOT. LMPE) THEN
C         SRT.MIN_Y = YMAX - SRT.MIN_Y + 1
C         SRT.MAX_Y = YMAX - SRT.MAX_Y + 1
C      ENDIF

***   Test if this is an HRI file
      IF (INDEX(HEAD.DETECTOR, 'HRI') .NE. 0) LHRI = .TRUE.

***   Calculate the pixel centres of each box
      SCEN_X = (SRT.MIN_X + SRT.MAX_X) / 2.0
      SCEN_Y = (SRT.MIN_Y + SRT.MAX_Y) / 2.0
      BCEN_X = (BSRT.MIN_X + BSRT.MAX_X) / 2.0
      BCEN_Y = (BSRT.MIN_Y + BSRT.MAX_Y) / 2.0

      BSCTIM = HEAD.BASE_SCTIME

***   Calculate the squares of the elliptical axis - if any
      IF (SRT.SHAPE .EQ. 'C' .OR. SRT.SHAPE .EQ. 'A' .OR.
     &                              SRT.SHAPE .EQ. 'E') THEN
         SAMIN2 = SRT.ELAMIN **2
         SAMAX2 = SRT.ELAMAX **2
         SBMIN2 = SRT.ELBMIN **2
         SBMAX2 = SRT.ELBMAX **2
         BAMIN2 = BSRT.ELAMIN **2
         BAMAX2 = BSRT.ELAMAX **2
         BBMIN2 = BSRT.ELBMIN **2
         BBMAX2 = BSRT.ELBMAX **2

***      Calculate the product of the squares of the two elliptical axes
         SA2B2I = SAMIN2 * SBMIN2
         SA2B2O = SAMAX2 * SBMAX2
         BA2B2I = BAMIN2 * BBMIN2
         BA2B2O = BAMAX2 * BBMAX2

***      Set a local cos and sin of the orientation angle for speed
         SCPHI = SRT.COSPHI
         SSPHI = SRT.SINPHI
         BCPHI = BSRT.COSPHI
         BSPHI = BSRT.SINPHI
      ENDIF

***   Loop over each element in arrays
      DO IX = 1,NELEMS
***      Copy event to simpler variables
         TEV = TIME(IX) - BSCTIM
         XEV = XPIX(IX)
         YEV = YPIX(IX)
         XDEV = XDET(IX)
         YDEV = YDET(IX)
         AEV = AMPL(IX)
         CEV = CAMPL(IX)
***      Fix for HRI no corrected events. set to value '1'
         IF (LHRI) CEV = 1

***      Check if this event is from an HRI hotspot or deadspot
         IF (.NOT. LHRI .OR. XRT_HSPOT(HEAD, XEV, YEV)) THEN

***         If the source box is circular, annular or elliptical, calculate
***         various numbers.
            IF (SRT.SHAPE .EQ. 'C' .OR. SRT.SHAPE .EQ. 'A' .OR.
     &                              SRT.SHAPE .EQ. 'E') THEN

***             calculate the offset in X and Y celestial pixels from the
***             source box centre
                SDIFFX = XEV - SCEN_X
                SDIFFY = YEV - SCEN_Y

***             calculate the position in elliptical coordinates - source box
***             NB: This also handles circles
                SELPX = SDIFFX * SCPHI + SDIFFY * SSPHI
                SELPY = - SDIFFX * SSPHI + SDIFFY * SCPHI
                SELPX2 = SELPX * SELPX
                SELPY2 = SELPY * SELPY

***             calculate the position in elliptical coordinates - backgnd box
                IF (SRT.BCKGND) THEN


***                calculate the offset in X and Y celestial pixels from the
***                background box centre
                   BDIFFX = XEV - BCEN_X
                   BDIFFY = YEV - BCEN_Y

                   BELPX = BDIFFX * BCPHI + BDIFFY * BSPHI
                   BELPY = - BDIFFX * BSPHI + BDIFFY * BCPHI
                   BELPX2 = BELPX * BELPX
                   BELPY2 = BELPY * BELPY

                ENDIF
             ENDIF

***          Check if each event is within the ranges selected
***          Time range:
             TOK = .FALSE.
             DO TLP=1,SRT.NTIME
                IF (SRT.MIN_T(TLP) .LE. TEV .AND. SRT.MAX_T(TLP)
     &                      .GE. TEV) THEN
                   TOK = .TRUE.
                   GOTO 100
                ENDIF
             ENDDO
*
100          CONTINUE
*
             IF (TOK) THEN
*
             IF (SRT.MIN_PH .LE. AEV .AND.SRT.MAX_PH .GE. AEV) THEN
*
             IF (SRT.MIN_EN .LE. CEV .AND.SRT.MAX_EN .GE. CEV) THEN
*
             IF (SRT.MIN_XD .LE. XDEV .AND.SRT.MAX_XD .GE. XDEV) THEN
*
             IF (SRT.MIN_YD .LE. YDEV .AND.SRT.MAX_YD .GE. YDEV) THEN
*
*     If quality limits have been made more strict then check quality
               OK = .TRUE.
*
               IF (QCHECK) THEN
*
*         See if this time is within one of the bad times
                  DO BLP=1,NBAD
                     IF (TEV .GE. STBAD(BLP) .AND.
     &                                      TEV .LE. ENBAD(BLP)) THEN
*           Set flag false and exit loop
                        OK=.FALSE.
                        GOTO 10
                     ENDIF
                  ENDDO
*
10                CONTINUE
*
               ENDIF
*
               IF (OK) THEN
*
*             Check if event is within the source box. Circles are tested
*             as a special case of the elliptical box
                  IF ( (SRT.SHAPE .EQ. 'R' .AND.
     &              (SRT.MIN_X .LE. XEV .AND. SRT.MAX_X .GE. XEV) .AND.
     &              (SRT.MIN_Y .LE. YEV .AND. SRT.MAX_Y .GE. YEV))
     &                                  .OR.
     &              ( (SRT.SHAPE .EQ. 'C' .OR. SRT.SHAPE .EQ. 'A' .OR.
     &                              SRT.SHAPE .EQ. 'E') .AND.
     &              ((SELPX2*SBMIN2 + SELPY2*SAMIN2) .GE. SA2B2I .AND.
     &               (SELPX2*SBMAX2 + SELPY2*SAMAX2) .LE. SA2B2O))) THEN
*
*             Calculate position of data in array
*               The first two dimensions of the array can be either
*               X and Y pixel or RADIAL and AZIMUTHAL bin, depending on
*               the user selection.
                    IF (NRBIN .EQ. 1) THEN
*
*                 X,Y bins:
                      EL1=INT((XEV-SRT.MIN_X)/XWIDTH) + 1
*
*                 Calculate Y element assuming the top is SRT.MIN_Y
                      IF (HEAD.ORIGIN.EQ.'MPE') THEN
                         EL2=SDIM2 - INT((YEV-SRT.MIN_Y)/YWIDTH)
                      ELSE
                         EL2=INT((YEV-SRT.MIN_Y)/YWIDTH) + 1
                      ENDIF
*
                    ELSE
*
*                 Calculate the radial (and azimuthal) bin by checking
*                 each bin individually (how else ??)
                      DO EL1=1,NRBIN
*
                         IF (( SELPX2 / ELIPA2(EL1) +
     &                         SELPY2 / ELIPB2(EL1) ) .LE. 1.0) GOTO 110
                      ENDDO
*
*                 This error message should never be activated
                      CALL MSG_PRNT('Error calculating elliptical - '/
     &                             /'bin refer to author')
*
110                   CONTINUE
*
*                 Set azimuthal bin to zero for now
                      EL2=1
*
                    ENDIF
*
*               Calculate the other elements
                    EL3=INT((XDEV-SRT.MIN_XD)/XDWID) + 1
                    EL4=INT((YDEV-SRT.MIN_YD)/YDWID) + 1
                    EL5=MIN((INT((TEV-SRT.MIN_T(1))/TWIDTH) + 1), SDIM5)
                    EL6=INT((AEV-SRT.MIN_PH)/PWIDTH) + 1
                    EL7=INT((CEV-SRT.MIN_EN)/EWIDTH) + 1
*
                    SDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) =
     &                      SDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) + 1.0
*
*             Check if event is within the background box
                   ELSEIF ( SRT.BCKGND .AND. (BSRT.SHAPE .EQ. 'R' .AND.
     &             (BSRT.MIN_X .LE. XEV .AND. BSRT.MAX_X .GE. XEV) .AND.
     &             (BSRT.MIN_Y .LE. YEV .AND. BSRT.MAX_Y .GE. YEV))
     &                             .OR.
     &             ( (BSRT.SHAPE .EQ. 'C' .OR. BSRT.SHAPE .EQ. 'A' .OR.
     &                              BSRT.SHAPE .EQ. 'E') .AND.
     &             ( (BELPX2*BBMIN2 + BELPY2*BAMIN2) .GE. BA2B2I .AND.
     &               (BELPX2*BBMAX2 + BELPY2*BAMAX2) .LE. BA2B2O))) THEN
*
*                Calculate position of data in array
*                NB: no polar bins in background
                      EL1=INT((XEV-BSRT.MIN_X)/BXWIDTH) + 1

*                 Calculate Y element assuming the top is SRT.MIN_Y
                      IF (HEAD.ORIGIN.EQ.'MPE') THEN
                         EL2=BDIM2 - INT((YEV-BSRT.MIN_Y)/BYWIDTH)
                      ELSE
                         EL2=INT((YEV-BSRT.MIN_Y)/BYWIDTH) + 1
                      ENDIF
*
                      EL3=INT((XDEV-BSRT.MIN_XD)/XDWID) + 1
                      EL4=INT((YDEV-BSRT.MIN_YD)/YDWID) + 1
                      EL5=INT((TEV-BSRT.MIN_T(1))/TWIDTH) + 1
                      EL6=INT((AEV-BSRT.MIN_PH)/PWIDTH) + 1
                      EL7=INT((CEV-BSRT.MIN_EN)/EWIDTH) + 1
*
                      BDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) =
     &                   BDATA(EL1,EL2,EL3,EL4,EL5,EL6,EL7) + 1.0
*
                   ENDIF
                ENDIF
              ENDIF
            ENDIF
           ENDIF
           ENDIF
           ENDIF
          ELSE
            BADEV = BADEV + 1
          ENDIF      ! Hotspot ?
      ENDDO       ! Loop over all records in map
*
999   CONTINUE
*
      SRT.MIN_Y = SAVE_YMIN
      SRT.MAX_Y = SAVE_YMAX
*
      END

*+RAT_SORT_EVE - Sorts XRT data into a binned data array
      SUBROUTINE RAT_SORT_EVE(HEAD, SRT, BSRT, MAXLIM, EVE_X, EVE_Y,
     &                 EVE_XD, EVE_YD, EVE_T, EVE_P, EVE_E, BEVE_X,
     &                 BEVE_Y, BEVE_XD, BEVE_YD, BEVE_T,
     &                 BEVE_P, BEVE_E, TOTEV_SRC, TOTEV_BCK, STATUS)
*    Description :
*        Sorts raw events from an XRT event datafile into event lists
*    History :
*     2-Nov-1988   original (LTVAD::RDS)
*     6-May-1993   now uses double prec. time lists (RDS)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
*    Status :
      INTEGER STATUS
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      RECORD /XRT_SCFDEF/ SRT,BSRT                ! Sort control structures
      RECORD /XRT_HEAD/ HEAD
      INTEGER MAXLIM                              ! Maximum number of events
*    Import-Export :
      REAL EVE_X(MAXLIM)
      REAL EVE_Y(MAXLIM)
      INTEGER EVE_XD(MAXLIM)
      INTEGER EVE_YD(MAXLIM)
      DOUBLE PRECISION EVE_T(MAXLIM)
      INTEGER EVE_P(MAXLIM)
      INTEGER EVE_E(MAXLIM)
      REAL BEVE_X(MAXLIM)
      REAL BEVE_Y(MAXLIM)
      INTEGER BEVE_XD(MAXLIM)
      INTEGER BEVE_YD(MAXLIM)
      DOUBLE PRECISION BEVE_T(MAXLIM)
      INTEGER BEVE_P(MAXLIM)
      INTEGER BEVE_E(MAXLIM)
*    Export :
      INTEGER TOTEV_SRC, TOTEV_BCK                ! Number of events put into
*                                                 ! source and bckgnd lists.
*    Functions :
      INTEGER CHR_LEN
         EXTERNAL CHR_LEN
*    Local constants :
      INTEGER MAXBAD
         PARAMETER (MAXBAD=1000)                  ! Max no. of bad time periods
*    Local variables :
      CHARACTER*20 EXT
      REAL STBAD(MAXBAD), ENBAD(MAXBAD)           ! Bad time periods
      INTEGER NBAD                                ! Number of bad time periods
      INTEGER BADEV                               ! Number of pixels in hotspots
      CHARACTER*80 DNAME                    ! Names of files
      CHARACTER*(DAT__SZLOC) ELOC,DLOC          ! Locators to datafiles
      CHARACTER*(DAT__SZLOC) LOCA(7),SLOCA(7)   ! Locators to data arrays
      INTEGER PTRA(7),NELEMS     ! Pointer to mapped arrays and item count
      INTEGER IX,UPPER,LOWER     ! number of indexes and ranges
*
*-
***   Initialise hotspot counter
      BADEV = 0
*
*    Open observation event file STDEVT from INC_RDF
      CALL RAT_HDLOOKUP(HEAD,'STDEVT','EXTNAME',EXT,STATUS)
      CALL HDS_OPEN(SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))//EXT,
     &              'READONLY',ELOC,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Generate index into events
C     CALL RAT_INDX_COORD(HEAD,SRT,BSRT,INDEX,STATUS)

***   Open quality file if wanted and get array of bad times for this obs.
      IF (SRT.QUAL_MORE .OR. SRT.QUAL_LESS) THEN
C         CALL XSORT_BADTIME(SRT,OBS,MAXBAD,NBAD,STBAD,ENBAD,STATUS)
C         IF (STATUS .NE. SAI__OK) GOTO 999
      ENDIF

***   Have the quality constraints been relaxed ?
      IF (SRT.QUAL_LESS) THEN

***      Open observation diff evenfile
         CALL RAT_HDLOOKUP(HEAD,'REJEVT','EXTNAME',EXT,STATUS)
         CALL USI_DEF0C('DIFFILE',SRT.ROOTNAME(1:CHR_LEN(SRT.ROOTNAME))
     &                  //EXT,STATUS)
         CALL USI_GET0C('DIFFILE',DNAME,STATUS)
         CALL HDS_OPEN(DNAME,'READONLY',DLOC,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

***   Locate the various data arrays
      CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,ELOC,LOCA,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   For each index entry
      DO IX = 1,1
         LOWER = 1
         UPPER = HEAD.IEVTNU

***      map the event data arrays into memory
         CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &      NELEMS,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

***      Check them against the sort parameters
         CALL RAT_DOIT_EVE(HEAD, SRT, BSRT,%val(PTRA(1)),%val(PTRA(2)),
     &      %val(PTRA(3)), %val(PTRA(4)), %val(PTRA(5)), %val(PTRA(6)),
     &      %val(PTRA(7)), NELEMS, MAXLIM, EVE_X, EVE_Y, EVE_XD, EVE_YD,
     &      EVE_T, EVE_P, EVE_E, BEVE_X, BEVE_Y, BEVE_XD, BEVE_YD,
     &      BEVE_T, BEVE_P, BEVE_E, TOTEV_SRC, TOTEV_BCK, SRT.QUAL_MORE,
     &      MAXBAD, NBAD, STBAD, ENBAD, BADEV)

***      unmap the arrays & memory
         CALL RAT_UNMAPEVE(SLOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDDO

***   annul the array locators
      CALL RAT_ANNULEVE(LOCA, STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999

***   Same for the diff data
      IF (SRT.QUAL_LESS) THEN

***      Locate the various data arrays
         CALL RAT_FINDEVE(HEAD.ORIGIN,HEAD.DETECTOR,DLOC,LOCA,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999

***      Find the size of the diff events
         CALL CMP_SIZE(DLOC,'TIME',UPPER,STATUS)
         DO IX = 1,1
            LOWER = 1

***         map the diff event data arrays into memory
            CALL RAT_MAPEVE(LOCA,'READ',LOWER,UPPER,SLOCA,PTRA,
     &         NELEMS,STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999

***         Check them against the sort parameters
            CALL RAT_DOIT_EVE(HEAD,SRT,BSRT,%val(PTRA(1)),%val(PTRA(2)),
     &         %val(PTRA(3)),%val(PTRA(4)),%val(PTRA(5)), %val(PTRA(6)),
     &         %val(PTRA(7)),NELEMS,MAXLIM,EVE_X, EVE_Y, EVE_XD, EVE_YD,
     &         EVE_T, EVE_P, EVE_E, BEVE_X, BEVE_Y, BEVE_XD, BEVE_YD,
     &         BEVE_T, BEVE_P,BEVE_E,TOTEV_SRC,TOTEV_BCK, SRT.QUAL_MORE,
     &         MAXBAD, NBAD, STBAD, ENBAD, BADEV)

***         unmap the arrays & memory
            CALL RAT_UNMAPEVE(SLOCA, STATUS)
            IF (STATUS.NE.SAI__OK) GOTO 999
         ENDDO

***      annul the array locators
         CALL RAT_ANNULEVE(LOCA, STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 999
      ENDIF

      IF (INDEX(HEAD.DETECTOR, 'HRI') .NE. 0) THEN
         CALL MSG_SETI('BAD', BADEV)
         CALL MSG_PRNT('Rejected ^BAD events found in '/
     &                             /'hotspots/deadspots')
      ENDIF

***   Close the files
      CALL HDS_CLOSE(ELOC, STATUS)
      IF (SRT.QUAL_LESS) CALL HDS_CLOSE(DLOC, STATUS)

999   CONTINUE

      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_REP(' ','from RAT_SORT_EVE',STATUS)
      ENDIF

      END

*+RAT_DOIT_EVE    Sorts XRT events into event lists
      SUBROUTINE RAT_DOIT_EVE(HEAD, SRT, BSRT, TIME, XPIX, YPIX, XDET,
     &              YDET, AMPL, CAMPL, NELEMS, MAXLIM, EVE_X, EVE_Y,
     &              EVE_XD, EVE_YD, EVE_T, EVE_P, EVE_E, BEVE_X, BEVE_Y,
     &              BEVE_XD, BEVE_YD, BEVE_T, BEVE_P, BEVE_E, TOTEV_SRC,
     &              TOTEV_BCK, QCHECK, MAXBAD, NBAD, STBAD, ENBAD,
     &              BADEV)
*    Description :
*    History :
*     date:  original (LTVAD::RDS)
*    Type definitions :
      IMPLICIT NONE
*    Structure definitions :
      INCLUDE 'XRTLIB(INC_XRTSRT)'
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      RECORD /XRT_HEAD/ HEAD                      ! Header values
      RECORD /XRT_SCFDEF/ SRT                     ! Source sorting parameters
      RECORD /XRT_SCFDEF/ BSRT                    ! Bckgnd sorting parameters
      INTEGER NELEMS			  ! Number of array elements
      DOUBLE PRECISION TIME(NELEMS)       ! Event times
      INTEGER XPIX(NELEMS), YPIX(NELEMS)  ! Array of coordinates
      INTEGER XDET(NELEMS), YDET(NELEMS)  ! Array of detector coords
      INTEGER AMPL(NELEMS), CAMPL(NELEMS) ! Array of photon events
      LOGICAL QCHECK                              ! Check quality values ?
      INTEGER MAXBAD                              ! Dimension of bad arrays
      INTEGER NBAD
      REAL STBAD(MAXBAD)                          ! Start of bad period
      REAL ENBAD(MAXBAD)                          ! End of bad period
      INTEGER MAXLIM                              ! Event list max extent
*    Import-Export :
      REAL EVE_X(MAXLIM),EVE_Y(MAXLIM)
      INTEGER EVE_XD(MAXLIM),EVE_YD(MAXLIM)
      INTEGER EVE_P(MAXLIM),EVE_E(MAXLIM)         ! Source lists
      DOUBLE PRECISION EVE_T(MAXLIM)
      REAL BEVE_X(MAXLIM),BEVE_Y(MAXLIM)
      INTEGER BEVE_XD(MAXLIM),BEVE_YD(MAXLIM)
      INTEGER BEVE_P(MAXLIM),BEVE_E(MAXLIM)       ! Bckgnd event lists
      DOUBLE PRECISION BEVE_T(MAXLIM)             ! Bckgnd event lists
*
      INTEGER TOTEV_SRC                           ! Number of source events
      INTEGER TOTEV_BCK                           ! Number of bckgnd events
      INTEGER BADEV                               ! Number of events in hotspots
*    Functions :
      INTEGER XRT_HSPOT
         EXTERNAL XRT_HSPOT
*    Local constants :
*     <local constants defined by PARAMETER>
*    Local variables :
      INTEGER BLP,TLP
      INTEGER XEV,YEV,XDEV,YDEV,AEV,CEV
      DOUBLE PRECISION TEV
      INTEGER SCEN_X,SCEN_Y                        ! Pixel centre of src box
      INTEGER BCEN_X,BCEN_Y                        ! Pixel centre of src box
      REAL SAMIN2,SAMAX2,SBMIN2,SBMAX2       ! Squares of the min. and max.
*                                            ! values for the source ellip axes.
      REAL BAMIN2,BAMAX2,BBMIN2,BBMAX2       !    Same for the background.
      REAL SA2B2I,SA2B2O                     ! Product of the squares of the
*                                            ! inner and outer source axes
      REAL BA2B2I,BA2B2O                     ! Product of the squares of the
*                                            ! inner and outer background axes
      REAL SDIFFX,SDIFFY                     ! X,Y offset from box centre (pix)
      REAL BDIFFX,BDIFFY                     !    Same for the background
      REAL SELPX2,SELPY2                     ! Square of the photon X,Y pos.
*                                            ! in source box elliptical coords
      REAL BELPX2,BELPY2                     ! Square of the photon X,Y pos.
*                                            ! in bckgnd box elliptical coords
      REAL SCPHI,SSPHI                       ! Cos and Sine of src orientation
      REAL BCPHI,BSPHI                       ! Cos and Sine of bck orientation
      REAL HPIX60                            ! Pixel size in arcmins
*
      LOGICAL LHRI                           ! Is detector the HRI ?
      LOGICAL TOK                            ! Is this event within time ranges?
      LOGICAL OK
      INTEGER IX
*-
      IF (INDEX(HEAD.DETECTOR, 'HRI') .NE. 0) THEN
         LHRI = .TRUE.
      ELSE
         LHRI = .FALSE.
      ENDIF

***   Calculate the box pixel centres
      SCEN_X = (SRT.MIN_X + SRT.MAX_X) / 2.0
      SCEN_Y = (SRT.MIN_Y + SRT.MAX_Y) / 2.0
      BCEN_X = (BSRT.MIN_X + BSRT.MAX_X) / 2.0
      BCEN_Y = (BSRT.MIN_Y + BSRT.MAX_Y) / 2.0

***   Calculate pixel size in arcmins
      HPIX60 = HEAD.PIXEL / 60.0

***   Calculate the squares of the elliptical axis - if any
      IF (INDEX('CAE', SRT.SHAPE) .NE. 0) THEN
         SAMIN2 = SRT.ELAMIN **2
         SAMAX2 = SRT.ELAMAX **2
         SBMIN2 = SRT.ELBMIN **2
         SBMAX2 = SRT.ELBMAX **2
         BAMIN2 = BSRT.ELAMIN **2
         BAMAX2 = BSRT.ELAMAX **2
         BBMIN2 = BSRT.ELBMIN **2
         BBMAX2 = BSRT.ELBMAX **2

***      Calculate the product of the squares of the two elliptical axes
         SA2B2I = SAMIN2 * SBMIN2
         SA2B2O = SAMAX2 * SBMAX2
         BA2B2I = BAMIN2 * BBMIN2
         BA2B2O = BAMAX2 * BBMAX2

***      Set a local cos and sin of the orientation angle for speed
         SCPHI = SRT.COSPHI
         SSPHI = SRT.SINPHI
         BCPHI = BSRT.COSPHI
         BSPHI = BSRT.SINPHI
      ENDIF

***   Loop over each input record
      DO IX = 1, NELEMS

***      Copy event to simpler variables
         TEV=TIME(IX) - HEAD.BASE_SCTIME
         XEV=XPIX(IX)
         YEV=YPIX(IX)
         XDEV=XDET(IX)
         YDEV=YDET(IX)
         AEV=AMPL(IX)
         CEV=CAMPL(IX)
***      Fix for HRI no corrected events. set to value '1'
         IF (LHRI) CEV = 1

***      Test if this is from an HRI hotspot or deadspot
         IF (.NOT. LHRI .OR. XRT_HSPOT(HEAD, XEV, YEV)) THEN

***         If the source box is circular, annular or elliptical, calculate
***         various numbers.
            IF (INDEX('CAE', SRT.SHAPE) .NE. 0) THEN

***            calculate the offset in X and Y celestial pixels from the
***            source box centre
               SDIFFX = XEV - SCEN_X
               SDIFFY = YEV - SCEN_Y

***            calculate the offset in X and Y celestial pixels from the
***            background box centre
               BDIFFX = XEV - BCEN_X
               BDIFFY = YEV - BCEN_Y

***            calculate the position in elliptical coordinates - source box
***            NB: This also handles circles
               SELPX2 = (SDIFFX * SCPHI + SDIFFY * SSPHI) ** 2
               SELPY2 = (SDIFFX * SSPHI + SDIFFY * SCPHI) ** 2

***            calculate the position in elliptical coordinates - backgnd box
               BELPX2 = (BDIFFX * BCPHI + BDIFFY * BSPHI) ** 2
               BELPY2 = (BDIFFX * BSPHI + BDIFFY * BCPHI) ** 2

            ENDIF

***         Check if each event is within the ranges selected
***         Time range:
            TOK = .FALSE.
            DO TLP=1,SRT.NTIME
               IF (SRT.MIN_T(TLP) .LE. TEV .AND. SRT.MAX_T(TLP)
     &                      .GE. TEV) TOK = .TRUE.
            ENDDO

            IF (TOK) THEN

            IF (SRT.MIN_PH .LE. AEV .AND.SRT.MAX_PH .GE. AEV) THEN

            IF (SRT.MIN_EN .LE. CEV .AND.SRT.MAX_EN .GE. CEV) THEN

            IF (SRT.MIN_XD .LE. XDEV .AND.SRT.MAX_XD .GE. XDEV) THEN

            IF (SRT.MIN_YD .LE. YDEV .AND.SRT.MAX_YD .GE. YDEV) THEN

***            If quality limits have been made more strict then check quality
               OK = .TRUE.

               IF (QCHECK) THEN

***               See if this time is within one of the bad times
                  DO BLP=1,NBAD
                     IF (TEV.GE.STBAD(BLP).AND.TEV.LE.ENBAD(BLP)) THEN
***                    Set flag false and exit loop
                       OK=.FALSE.
                       GOTO 10
                     ENDIF
                  ENDDO

10                CONTINUE

               ENDIF

***            Is quality ok ?
               IF (OK) THEN

***               Check if event is within the source box
                  IF ( (SRT.SHAPE .EQ. 'R' .AND.
     &               (SRT.MIN_X .LE. XEV .AND. SRT.MAX_X .GE. XEV) .AND.
     &               (SRT.MIN_Y .LE. YEV .AND. SRT.MAX_Y .GE. YEV))
     &                                  .OR.
     &               ((INDEX( 'CAE', SRT.SHAPE ) .NE. 0 ) .AND.
     &               ((SELPX2*SBMIN2 + SELPY2*SAMIN2) .GE. SA2B2I .AND.
     &               (SELPX2*SBMAX2 + SELPY2*SAMAX2) .LE. SA2B2O))) THEN

                     TOTEV_SRC=TOTEV_SRC+1

                     EVE_X(TOTEV_SRC)=-(XEV - HEAD.SKYCX)*HPIX60
                     EVE_Y(TOTEV_SRC)=-(YEV - HEAD.SKYCY)*HPIX60
                     EVE_XD(TOTEV_SRC)=XDEV
                     EVE_YD(TOTEV_SRC)=YDEV
                     EVE_T(TOTEV_SRC)=TEV
                     EVE_P(TOTEV_SRC)=AEV
                     EVE_E(TOTEV_SRC)=CEV

***               Check if event is within the bckgnd box
                  ELSEIF ( SRT.BCKGND .AND. (SRT.SHAPE .EQ. 'R' .AND.
     &               (BSRT.MIN_X.LE.XEV .AND. BSRT.MAX_X.GE.XEV).AND.
     &               (BSRT.MIN_Y.LE.YEV .AND. BSRT.MAX_Y.GE.YEV))
     &                             .OR.
     &               ((INDEX( 'CAE', BSRT.SHAPE ) .NE. 0) .AND.
     &               ((BELPX2*BBMIN2 + BELPY2*BAMIN2).GE.BA2B2I .AND.
     &               (BELPX2*BBMAX2 + BELPY2*BAMAX2).LE.BA2B2O))) THEN

                     TOTEV_BCK=TOTEV_BCK+1

                     BEVE_X(TOTEV_BCK)=-XEV*HPIX60
                     BEVE_Y(TOTEV_BCK)=-YEV*HPIX60
                     BEVE_XD(TOTEV_BCK)=XDEV
                     BEVE_YD(TOTEV_BCK)=YDEV
                     BEVE_T(TOTEV_BCK)=TEV
                     BEVE_P(TOTEV_BCK)=AEV
                     BEVE_E(TOTEV_BCK)=CEV

                  ENDIF
               ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
            ENDIF
         ELSE
           BADEV = BADEV + 1
         ENDIF
      ENDDO

999   CONTINUE

      END



*+RAT_FINDEVE - locate event data structures
      SUBROUTINE RAT_FINDEVE(ORI,DET,ELOC,LOCA,STATUS)
*    Description :
*    History :
*     30-Nov-1993   original (LTVAD::JKA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Status :
      INTEGER STATUS
*    Structures :
*    Import :
      CHARACTER*(*) ORI
      CHARACTER*(*) DET                     ! HRI or PSPC
      CHARACTER*(DAT__SZLOC) ELOC           ! Locator to events file
*    Export :
      CHARACTER*(DAT__SZLOC) LOCA(7)        ! Locator to event data
*    Functions
      CHARACTER*20 RAT_LOOKUP
        EXTERNAL RAT_LOOKUP
*    Local variables :
      CHARACTER*10 EVT
      LOGICAL THERE
*
      EVT = 'STDEVT'
*
*     Locate arrays
      CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'TIME'),LOCA(1),STATUS)
      CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'XPIX'),LOCA(2),STATUS)
      CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'YPIX'),LOCA(3),STATUS)
      CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'XDET'),LOCA(4),STATUS)
      CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'YDET'),LOCA(5),STATUS)
      CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'PHA'),LOCA(6),STATUS)
*
*     a special case for HRI.  if it can't find a PI channel then
*     map to the PHA channel
      CALL DAT_THERE(ELOC,RAT_LOOKUP(ORI,DET,EVT,'PI'),THERE,STATUS)
      IF (THERE) THEN
        CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'PI'),LOCA(7),STATUS)
      ELSE
        CALL DAT_FIND(ELOC,RAT_LOOKUP(ORI,DET,EVT,'PHA'),LOCA(7),STATUS)
      ENDIF

      RETURN
      END

*+RAT_MAPEVE - slice and map range of data
      SUBROUTINE RAT_MAPEVE(LOCA,OPER,LOWER,UPPER,SLOCA,PTRA,
     &    NELEM,STATUS)
*    Description :
*    History :
*     30-Nov-1993   original (LTVAD::JKA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global parameters
*    Status :
      INTEGER STATUS
*    Structures :
      INCLUDE 'XRTLIB(INC_XRTHEAD)'
*    Import :
      CHARACTER*(DAT__SZLOC) LOCA(7)     ! Locator to event data
      CHARACTER*(*) OPER                 ! Operating mode
      INTEGER LOWER,UPPER                ! Slicing ranges
*    Export :
      CHARACTER*(DAT__SZLOC) SLOCA(7)    ! Locator to sliced data
      INTEGER PTRA(7)                    ! pointers to mapped data
      INTEGER NELEM                      ! number of elements mapped
*    Local variables :
      INTEGER I

      CALL DAT_SLICE(LOCA(1), 1, LOWER, UPPER, SLOCA(1),STATUS)
      CALL DAT_MAPV(SLOCA(1),'_DOUBLE',OPER,PTRA(1),NELEM,STATUS)
      DO I = 2,7
         CALL DAT_SLICE(LOCA(I), 1, LOWER, UPPER, SLOCA(I),STATUS)
         CALL DAT_MAPV(SLOCA(I),'_INTEGER',OPER,PTRA(I),NELEM,STATUS)
      ENDDO
      RETURN
      END

*+RAT_UNMAPEVE - unmap and annul sliced data locators
      SUBROUTINE RAT_UNMAPEVE(SLOCA, STATUS)
*    Description :
*    History :
*     30-Nov-1993   original (LTVAD::JKA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global parameters
*    Status :
      INTEGER STATUS
*    Structures :
*    Import-Export :
      CHARACTER*(DAT__SZLOC) SLOCA(7)    ! Locator to sliced data
*    Local variables :
      INTEGER I

      DO I = 1,7
         CALL DAT_UNMAP(SLOCA(I), STATUS)
         CALL DAT_ANNUL(SLOCA(I), STATUS)
      ENDDO
      RETURN
      END

*+RAT_ANNULEVE - Annul event locators
      SUBROUTINE RAT_ANNULEVE(LOCA, STATUS)
*    Description :
*    History :
*     30-Nov-1993   original (LTVAD::JKA)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global parameters
*    Status :
      INTEGER STATUS
*    Import-Export :
      CHARACTER*(DAT__SZLOC) LOCA(7)    ! Locator to sliced data
*    Local variables :
      INTEGER I

      DO I = 1,7
         CALL DAT_ANNUL(LOCA(I), STATUS)
      ENDDO
      RETURN
      END
*
