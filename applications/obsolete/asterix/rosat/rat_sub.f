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
*            6 Apr 98 linux port (rjv)
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
         IF ( T_VER.GT.' ')THEN
           CALL CHR_CTOR(T_VER,R_VER1,STATUS)
         ELSE
           R_VER1=0.0
         END IF
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
      SUBROUTINE RAT_GETXRTHEAD(RTNAME,STATUS)
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
*    Include files
      INCLUDE 'XRTHEAD_CMN'
*    Parameters :
      CHARACTER*(*) RTNAME		! Rootname of files
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator
      CHARACTER*132 FILE

      IF (STATUS.NE.SAI__OK) RETURN

      FILE=RTNAME(1:CHR_LEN(RTNAME))//'_hdr'
      CALL HDS_OPEN(FILE,'READ',LOC,STATUS)
      CALL RAT_GETHEAD(LOC, 'HEAD',STATUS)
      CALL HDS_CLOSE(LOC, STATUS)

      IF (STATUS.NE.SAI__OK) THEN
         CALL MSG_SETC('INAME',FILE)
         CALL MSG_PRNT('** Error reading header info from ^INAME **')
      ENDIF

      END

*+  RAT_PUTHEAD - Create HEAD structure
      SUBROUTINE RAT_PUTHEAD(FID,OBJECT,STATUS)
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
*    Include files
      INCLUDE 'XRTHEAD_CMN'
*    Parameters :
      INTEGER			FID
      CHARACTER*(*)            OBJECT                ! Structured name
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator
      CHARACTER*(DAT__SZLOC)   ALOC                   ! locator
*

      IF ( STATUS.NE.SAI__OK ) RETURN

      CALL ADI1_GETLOC( FID, LOC, STATUS )

***** Create HEAD structure
      CALL HDX_CREATE(LOC,OBJECT,0,0,OBJECT,ALOC,STATUS)
      CALL HDX_FIND(LOC,OBJECT,ALOC,STATUS)

      CALL HDX_PUTD(ALOC,'AXIS_RA',1,HEAD_AXIS_RA,STATUS)
      CALL HDX_PUTD(ALOC,'AXIS_DEC',1,HEAD_AXIS_DEC,STATUS)
      CALL HDX_PUTD(ALOC,'ROLLCI',1,HEAD_ROLLCI,STATUS)
      CALL HDX_PUTR(ALOC,'PIXEL',1,HEAD_PIXEL,STATUS)
      CALL HDX_PUTI(ALOC,'NTRANGE',1,HEAD_NTRANGE,STATUS)
      CALL HDX_PUTD(ALOC,'BASE_SCTIME',1,HEAD_BASE_SCTIME,STATUS)
      CALL HDX_PUTD(ALOC,'TSTART',MAXRAN,HEAD_TSTART,STATUS)
      CALL HDX_PUTD(ALOC,'TEND',MAXRAN,HEAD_TEND,STATUS)
      CALL HDX_PUTI(ALOC,'ASTART',1,HEAD_ASTART,STATUS)
      CALL HDX_PUTI(ALOC,'AEND',1,HEAD_AEND,STATUS)
      CALL HDX_PUTI(ALOC,'CSTART',1,HEAD_CSTART,STATUS)
      CALL HDX_PUTI(ALOC,'CEND',1,HEAD_CEND,STATUS)
      CALL HDX_PUTI(ALOC,'XSTART',1,HEAD_XSTART,STATUS)
      CALL HDX_PUTI(ALOC,'XEND',1,HEAD_XEND,STATUS)
      CALL HDX_PUTI(ALOC,'YSTART',1,HEAD_YSTART,STATUS)
      CALL HDX_PUTI(ALOC,'YEND',1,HEAD_YEND,STATUS)
      CALL HDX_PUTI(ALOC,'XDSTART',1,HEAD_XDSTART,STATUS)
      CALL HDX_PUTI(ALOC,'XDEND',1,HEAD_XDEND,STATUS)
      CALL HDX_PUTI(ALOC,'YDSTART',1,HEAD_YDSTART,STATUS)
      CALL HDX_PUTI(ALOC,'YDEND',1,HEAD_YDEND,STATUS)
      CALL HDX_PUTI(ALOC,'NEVENTS',MAXMAPS,HEAD_NEVENTS,STATUS)
      CALL HDX_PUTI(ALOC,'EVSTART',MAXMAPS,HEAD_EVSTART,STATUS)
      CALL HDX_PUTI(ALOC,'IEVLEN',1,HEAD_IEVLEN,STATUS)
      CALL HDX_PUTI(ALOC,'IRECLN',1,HEAD_IRECLN,STATUS)
      CALL HDX_PUTI(ALOC,'IFDSZX',1,HEAD_IFDSZX,STATUS)
      CALL HDX_PUTI(ALOC,'IFDSZY',1,HEAD_IFDSZY,STATUS)
      CALL HDX_PUTI(ALOC,'XSMAP',MAXMAPS,HEAD_XSMAP,STATUS)
      CALL HDX_PUTI(ALOC,'YSMAP',MAXMAPS,HEAD_YSMAP,STATUS)
      CALL HDX_PUTI(ALOC,'ISMNUX',1,HEAD_ISMNUX,STATUS)
      CALL HDX_PUTI(ALOC,'ISMNUY',1,HEAD_ISMNUY,STATUS)
      CALL HDX_PUTI(ALOC,'ISMTNU',1,HEAD_ISMTNU,STATUS)
      CALL HDX_PUTI(ALOC,'IEVTNU',1,HEAD_IEVTNU,STATUS)
      CALL HDX_PUTI(ALOC,'NHEAD',1,HEAD_NHEAD,STATUS)
      CALL HDX_PUTC(ALOC,'OBS_MODE',1,HEAD_OBS_MODE,STATUS)
      CALL HDX_PUTC(ALOC,'OBC_MODE',1,HEAD_OBC_MODE,STATUS)
      CALL HDX_PUTC(ALOC,'TARGET',1,HEAD_TARGET,STATUS)
      CALL HDX_PUTC(ALOC,'OBSERVER',1,HEAD_OBSERVER,STATUS)
      CALL HDX_PUTC(ALOC,'OBSERVATORY',1,HEAD_OBSERVATORY,STATUS)
      CALL HDX_PUTC(ALOC,'INSTRUMENT',1,HEAD_INSTRUMENT,STATUS)
      CALL HDX_PUTC(ALOC,'DETECTOR',1,HEAD_DETECTOR,STATUS)
      CALL HDX_PUTC(ALOC,'FILTER',1,HEAD_FILTER,STATUS)
      CALL HDX_PUTC(ALOC,'TITLE',1,HEAD_TITLE,STATUS)
      CALL HDX_PUTC(ALOC,'XPUNITS',1,HEAD_XPUNITS,STATUS)
      CALL HDX_PUTC(ALOC,'YPUNITS',1,HEAD_YPUNITS,STATUS)
      CALL HDX_PUTD(ALOC,'BASE_MJD',1,HEAD_BASE_MJD,STATUS)
      CALL HDX_PUTD(ALOC,'END_MJD',1,HEAD_END_MJD,STATUS)
      CALL HDX_PUTD(ALOC,'SCCONV',1,HEAD_SCCONV,STATUS)
      CALL HDX_PUTR(ALOC,'SKYCX',1,HEAD_SKYCX,STATUS)
      CALL HDX_PUTR(ALOC,'SKYCY',1,HEAD_SKYCY,STATUS)
      CALL HDX_PUTI(ALOC,'NSPOT',1,HEAD_NSPOT,STATUS)
      CALL HDX_PUTI(ALOC,'XSPOT',MAXSPOT,HEAD_XSPOT,STATUS)
      CALL HDX_PUTI(ALOC,'YSPOT',MAXSPOT,HEAD_YSPOT,STATUS)
      CALL HDX_PUTI(ALOC,'SPOTRAD',MAXSPOT,HEAD_SPOTRAD,STATUS)
      CALL HDX_PUTC(ALOC,'SASS_DATE',1,HEAD_SASS_DATE,STATUS)
      CALL HDX_PUTC(ALOC,'ORIGIN',1,HEAD_ORIGIN,STATUS)

      CALL DAT_ANNUL(ALOC,STATUS)

***** Test for any writing errors
      IF (STATUS.NE.SAI__OK) THEN
      ENDIF
      END


*+  RAT-GETHEAD - Read HEAD structure
      SUBROUTINE RAT_GETHEAD(LOC,OBJECT,STATUS)
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
*    Include files
      INCLUDE 'XRTHEAD_CMN'
*    Parameters :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator
      CHARACTER*(*)            OBJECT                ! Structured name
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   ALOC                   ! locator
      INTEGER DUMMY			! Value not used
*
      IF (STATUS.NE.SAI__OK) RETURN


      CALL HDX_FIND(LOC,OBJECT,ALOC,STATUS)


      CALL CMP_GET0D(ALOC,'AXIS_RA',HEAD_AXIS_RA,STATUS)
      CALL CMP_GET0D(ALOC,'AXIS_DEC',HEAD_AXIS_DEC,STATUS)
      CALL CMP_GET0D(ALOC,'ROLLCI',HEAD_ROLLCI,STATUS)
      CALL CMP_GET0R(ALOC,'PIXEL',HEAD_PIXEL,STATUS)
      CALL CMP_GET0I(ALOC,'NTRANGE',HEAD_NTRANGE,STATUS)
      CALL CMP_GET0D(ALOC,'BASE_SCTIME',HEAD_BASE_SCTIME,STATUS)
      CALL CMP_GET1D(ALOC,'TSTART',MAXRAN,HEAD_TSTART,DUMMY,STATUS)
      CALL CMP_GET1D(ALOC,'TEND',MAXRAN,HEAD_TEND,DUMMY,STATUS)
      CALL CMP_GET0I(ALOC,'ASTART',HEAD_ASTART,STATUS)
      CALL CMP_GET0I(ALOC,'AEND',HEAD_AEND,STATUS)
      CALL CMP_GET0I(ALOC,'CSTART',HEAD_CSTART,STATUS)
      CALL CMP_GET0I(ALOC,'CEND',HEAD_CEND,STATUS)
      CALL CMP_GET0I(ALOC,'XSTART',HEAD_XSTART,STATUS)
      CALL CMP_GET0I(ALOC,'XEND',HEAD_XEND,STATUS)
      CALL CMP_GET0I(ALOC,'YSTART',HEAD_YSTART,STATUS)
      CALL CMP_GET0I(ALOC,'YEND',HEAD_YEND,STATUS)
      CALL CMP_GET0I(ALOC,'XDSTART',HEAD_XDSTART,STATUS)
      CALL CMP_GET0I(ALOC,'XDEND',HEAD_XDEND,STATUS)
      CALL CMP_GET0I(ALOC,'YDSTART',HEAD_YDSTART,STATUS)
      CALL CMP_GET0I(ALOC,'YDEND',HEAD_YDEND,STATUS)
      CALL CMP_GET1I(ALOC,'NEVENTS',MAXMAPS,HEAD_NEVENTS,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'EVSTART',MAXMAPS,HEAD_EVSTART,DUMMY,STATUS)
      CALL CMP_GET0I(ALOC,'IEVLEN',HEAD_IEVLEN,STATUS)
      CALL CMP_GET0I(ALOC,'IRECLN',HEAD_IRECLN,STATUS)
      CALL CMP_GET0I(ALOC,'IFDSZX',HEAD_IFDSZX,STATUS)
      CALL CMP_GET0I(ALOC,'IFDSZY',HEAD_IFDSZY,STATUS)
      CALL CMP_GET1I(ALOC,'XSMAP',MAXMAPS,HEAD_XSMAP,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'YSMAP',MAXMAPS,HEAD_YSMAP,DUMMY,STATUS)
      CALL CMP_GET0I(ALOC,'ISMNUX',HEAD_ISMNUX,STATUS)
      CALL CMP_GET0I(ALOC,'ISMNUY',HEAD_ISMNUY,STATUS)
      CALL CMP_GET0I(ALOC,'ISMTNU',HEAD_ISMTNU,STATUS)
      CALL CMP_GET0I(ALOC,'IEVTNU',HEAD_IEVTNU,STATUS)
      CALL CMP_GET0I(ALOC,'NHEAD',HEAD_NHEAD,STATUS)
      CALL CMP_GET0C(ALOC,'OBS_MODE',HEAD_OBS_MODE,STATUS)
      CALL CMP_GET0C(ALOC,'OBC_MODE',HEAD_OBC_MODE,STATUS)
      CALL CMP_GET0C(ALOC,'TARGET',HEAD_TARGET,STATUS)
      CALL CMP_GET0C(ALOC,'OBSERVER',HEAD_OBSERVER,STATUS)
      CALL CMP_GET0C(ALOC,'OBSERVATORY',HEAD_OBSERVATORY,STATUS)
      CALL CMP_GET0C(ALOC,'INSTRUMENT',HEAD_INSTRUMENT,STATUS)
      CALL CMP_GET0C(ALOC,'DETECTOR',HEAD_DETECTOR,STATUS)
      CALL CMP_GET0C(ALOC,'FILTER',HEAD_FILTER,STATUS)
      CALL CMP_GET0C(ALOC,'TITLE',HEAD_TITLE,STATUS)
      CALL CMP_GET0C(ALOC,'XPUNITS',HEAD_XPUNITS,STATUS)
      CALL CMP_GET0C(ALOC,'YPUNITS',HEAD_YPUNITS,STATUS)
      CALL CMP_GET0D(ALOC,'BASE_MJD',HEAD_BASE_MJD,STATUS)
      CALL CMP_GET0D(ALOC,'END_MJD',HEAD_END_MJD,STATUS)
      CALL CMP_GET0D(ALOC,'SCCONV',HEAD_SCCONV,STATUS)
      CALL CMP_GET0R(ALOC,'SKYCX',HEAD_SKYCX,STATUS)
      CALL CMP_GET0R(ALOC,'SKYCY',HEAD_SKYCY,STATUS)
      CALL CMP_GET0I(ALOC,'NSPOT',HEAD_NSPOT,STATUS)
      CALL CMP_GET1I(ALOC,'XSPOT',MAXSPOT,HEAD_XSPOT,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'YSPOT',MAXSPOT,HEAD_YSPOT,DUMMY,STATUS)
      CALL CMP_GET1I(ALOC,'SPOTRAD',MAXSPOT,HEAD_SPOTRAD,DUMMY,STATUS)
      CALL CMP_GET0C(ALOC,'SASS_DATE',HEAD_SASS_DATE,STATUS)
      CALL CMP_GET0C(ALOC,'ORIGIN',HEAD_ORIGIN,STATUS)
      CALL DAT_ANNUL(ALOC,STATUS)

***** Test for any reading errors
      IF (STATUS.NE.SAI__OK) THEN
        CALL ERR_REP(' ','from RAT_GETINDEX',STATUS)
      ENDIF

      END

*+  RAT-PUTHEAD - Create Index structure
      SUBROUTINE RAT_PUTINDEX(FID,OBJECT,INDEX,STATUS)
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
*    Include files
*    Parameters :
      INTEGER			FID
      CHARACTER*(*)            OBJECT                ! Structured name
      INTEGER                  INDEX                 ! Index structure
*    Status :
      INTEGER STATUS
*    Local constants :
*    Local variables :
      CHARACTER*(DAT__SZLOC)   LOC                   ! Start locator
      CHARACTER*(DAT__SZLOC)   ALOC                   ! locator
*
      CALL ADI1_GETLOC( FID, LOC, STATUS )
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
      DATA VALUES(14) /'MPE:    :EVRATE :EXTNAME  _evr' /
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
      DATA VALUES(100)/'RDF:HRI :EVRATE :PS_VALID secondary' /
      DATA VALUES(101)/'   :HRI :EVRATE :PS_VALID ps_valid' /
      DATA VALUES(102)/'   :    :       :TIME     time' /
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
	SUBROUTINE RAT_HDLOOKUP( TABLE, COLUMN, VALUE, STATUS)
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
* Global variables :
      INCLUDE 'XRTHEAD_CMN'
* Import :
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
      VALUE = RAT_LOOKUP(HEAD_ORIGIN,HEAD_DETECTOR,TABLE,COLUMN)

      END


*+RAT_TMCONV  Converts times from MJD to s/c clock
	SUBROUTINE RAT_TMCONV(DATE_OBS, TIME_OBS, DATE_END, TIME_END,
     &                       XS_MJDRD, XS_MJDRF, STATUS)
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
      INCLUDE 'XRTHEAD_CMN'
* Import :
      CHARACTER*(*) DATE_OBS          ! Date of observation start
      CHARACTER*(*) TIME_OBS          ! Time of observation start
      CHARACTER*(*) DATE_END          ! Date of observation end
      CHARACTER*(*) TIME_END          ! Time of observation end
      INTEGER XS_MJDRD                ! Integer MJD of s/c start date
      DOUBLE PRECISION XS_MJDRF       ! Fractional MJD of s/c start time
* Import-Export :
* Export :
* Status :
      INTEGER STATUS
* Function declarations :
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
      MJDBIT=HRS*3600.0D0 + MIN*60.0D0 + DBLE(SEC)
      HEAD_BASE_MJD=DBLE(IMJD1) + MJDBIT/86400.0D0

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
      MJDBIT=HRS*3600.0D0 + MIN*60.0D0 + DBLE(SEC)
      HEAD_END_MJD=DBLE(IMJD2) + MJDBIT/86400.0D0
*
* Calculate the MJD offset from the start of the Rosat clock
      MJDOFF = HEAD_BASE_MJD - (XS_MJDRD + XS_MJDRF)
*
* Convert to seconds
      HEAD_BASE_SCTIME = MJDOFF * 86400.0D0
*
* Set the S/C to MJD conversion factor to one
      HEAD_SCCONV=1.0D0
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
* Global variables :
      INCLUDE 'XRTHEAD_CMN'
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
*
      INTEGER LP
*-
* Check status :
      IF (STATUS .NE. SAI__OK) RETURN
*
*     Get header information
      CALL RAT_GETXRTHEAD(RTNAME,STATUS)
      IF (STATUS .NE. SAI__OK) GOTO 999
*
* Copy the times into the right output variables
      NSEL = HEAD_NTRANGE * 2
*
      DO LP=1,HEAD_NTRANGE
         RAWTIM(1+(LP-1)*2) = HEAD_TSTART(LP)
         RAWTIM(LP*2) = HEAD_TEND(LP)
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
	SUBROUTINE RAT_RDHEAD(IUNIT,ORIGIN,STATUS)
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
*    Global variables :
      INCLUDE 'XRTHEAD_CMN'
*    Global parameters
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
      HEAD_ORIGIN = ORIGIN
      IF (ORIGIN.EQ.'US')  CALL RAT_RDUSHEAD(IUNIT,STATUS)
      IF (ORIGIN.EQ.'RDF') CALL RAT_RDRDFHEAD(IUNIT,STATUS)
      IF (ORIGIN.EQ.'MPE') CALL RAT_RDMPEHEAD(IUNIT,STATUS)

      RETURN
      END


*+ RAT_RDUSHEAD -  Read header information from US FITS file
	SUBROUTINE RAT_RDUSHEAD(IUNIT,STATUS)
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
*    Global variables :
      INCLUDE 'XRTHEAD_CMN'
*    Global parameters
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

      CALL FTGKYS(IUNIT,'TITLE',HEAD_TITLE,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'OBJECT',CVAL,CDUMMY,STATUS)
      HEAD_TARGET = CVAL(1:20)
      IF (STATUS.NE.0) RETURN

C - MOVE TO EVENTS HEADER
      CALL FTMAHD(IUNIT,4,HTYPE,STATUS)
      IF (STATUS.NE.0) THEN
         CALL MSG_PRNT('Cannot find Table 4, Possibly no EVENT data')
         RETURN
      ENDIF

      CALL FTGKYJ(IUNIT,'XS-MINCH',HEAD_CSTART,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-MAXCH',HEAD_CEND,CDUMMY,STATUS)
*
      CALL FTGKYD(IUNIT,'CRVAL1',HEAD_AXIS_RA,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CRVAL2',HEAD_AXIS_DEC,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CROTA2',HEAD_ROLLCI,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CDELT2',DVAL,CDUMMY,STATUS)
      HEAD_PIXEL = DVAL * 3600.0

      HEAD_XDSTART = 1
      HEAD_YDSTART = 1
      CALL FTGKYJ(IUNIT,'XS-XDET',HEAD_XDEND,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-YDET',HEAD_YDEND,CDUMMY,STATUS)
*
* Get exposure time info
      HEAD_NTRANGE=1
      HEAD_TSTART(1)=0.0D0
      CALL FTGKYD(IUNIT,'XS-LIVTI',HEAD_TEND(1),CDUMMY,STATUS)

* The next parameters will not exist for image data so ignore errors
      HEAD_XSTART = 1
      HEAD_YSTART = 1
      CALL FTGKYJ(IUNIT,'AXLEN1',HEAD_XEND,CDUMMY,TSTAT)
      CALL FTGKYJ(IUNIT,'AXLEN2',HEAD_YEND,CDUMMY,TSTAT)
      CALL FTGKYJ(IUNIT,'AXLEN1',HEAD_IFDSZX,CDUMMY,TSTAT)
      CALL FTGKYJ(IUNIT,'AXLEN2',HEAD_IFDSZY,CDUMMY,TSTAT)
*
* Dummy up small map values
      HEAD_ISMNUX=1
      HEAD_ISMNUY=1
      HEAD_ISMTNU=1
      HEAD_XSMAP(1)=1
      HEAD_YSMAP(1)=1
*
      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD_IEVTNU,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'XS-SEQPI',CVAL,CDUMMY,STATUS)
      HEAD_OBSERVER = CVAL(1:20)
      CALL FTGKYS(IUNIT,'TELESCOP',HEAD_OBSERVATORY,CDUMMY,STATUS)
      HEAD_INSTRUMENT = 'XRT'
      CALL FTGKYS(IUNIT,'INSTRUME',HEAD_DETECTOR,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-FILTR',IVAL,CDUMMY,STATUS)
      IF (IVAL.EQ.0) HEAD_FILTER = 'OFF'
      IF (IVAL.EQ.1) HEAD_FILTER = 'BORON'
      HEAD_XPUNITS = 'ARCSECONDS'
      HEAD_YPUNITS  = 'ARCSECONDS'
      CALL FTGKYJ(IUNIT,'XS-MJDRD',XS_MJDRD,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'XS-MJDRF',XS_MJDRF,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-OBS',DATE_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-OBS',TIME_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-END',DATE_END,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-END',TIME_END,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-EVREF',XS_EVREF,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX1',HEAD_SKYCX,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX2',HEAD_SKYCY,CDUMMY,STATUS)
      HEAD_NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,STATUS)


C - READ TIME RANGE INFORMATION
      CALL FTMAHD(IUNIT,2,HTYPE,STATUS)

      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD_NTRANGE,CDUMMY,STATUS)
C - CHECK NUMBER OF TIME RANGES DOESN'T EXCEED MAXIMUM
      IF (HEAD_NTRANGE.GT.MAXRAN) HEAD_NTRANGE = MAXRAN
      CALL FTGCVD(IUNIT,1,1,1,HEAD_NTRANGE,0,HEAD_TSTART,LDUMMY,STATUS)
      CALL FTGCVD(IUNIT,2,1,1,HEAD_NTRANGE,0,HEAD_TEND,LDUMMY,STATUS)
      DO I = 1,HEAD_NTRANGE
         HEAD_TSTART(I) = HEAD_TSTART(I) - HEAD_BASE_SCTIME
         HEAD_TEND(I) = HEAD_TEND(I) - HEAD_BASE_SCTIME
      ENDDO

*   Set amplitude ranges
      IF (INDEX(HEAD_DETECTOR,'PSPC') .NE. 0) THEN
         HEAD_ASTART = 1
         HEAD_AEND = 256
*
*   US data uses channel 0 which can't be processed by the system
         IF (HEAD_CSTART .EQ. 0) HEAD_CSTART = 1
*
* NB: The US PSPC CPHA channels are defined to be one less than the MPE files.
*
      ELSEIF (INDEX(HEAD_DETECTOR,'HRI') .NE. 0) THEN
         HEAD_ASTART = HEAD_CSTART
         HEAD_AEND = HEAD_CEND
         HEAD_CSTART = 1
         HEAD_CEND = 1
      ENDIF

      RETURN
      END

*+ RAT_RDRDFHEAD -  Read header information from RAT FITS file
	SUBROUTINE RAT_RDRDFHEAD(IUNIT,STATUS)
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
*    Global variables :
      INCLUDE 'XRTHEAD_CMN'
*    Global parameters
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
      CALL FTGKYS(IUNIT,KEYWORD,HEAD_TITLE,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'TARGET',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD_TARGET,CDUMMY,STATUS)

C - MOVE TO EVENTS HEADER
      CALL FTMAHD(IUNIT,3,HTYPE,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'CSTART',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_CSTART,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'CEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_CEND,CDUMMY,STATUS)
*
      CALL RAT_FITSKEY('RDF',VERSION,'AXIS_RA',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,HEAD_AXIS_RA,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'AXIS_DEC',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,HEAD_AXIS_DEC,CDUMMY,STATUS)

* ROTATION ANGLE
      CALL RAT_FITSKEY('RDF',VERSION,'ROLLCI',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,HEAD_ROLLCI,CDUMMY,STATUS)

* DEGREES PER PIXEL
      CALL RAT_FITSKEY('RDF',VERSION,'PIXEL',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,DVAL,CDUMMY,STATUS)
      HEAD_PIXEL = DVAL * 3600.0

      HEAD_XDSTART = 1
      HEAD_YDSTART = 1

      CALL RAT_FITSKEY('RDF',VERSION,'XDEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_XDEND,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'YDEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_YDEND,CDUMMY,STATUS)
*
* Get exposure time info
      HEAD_NTRANGE=1
      HEAD_TSTART(1)=0.0D0

      CALL RAT_FITSKEY('RDF',VERSION,'TEND',KEYWORD,STATUS)
      CALL FTGKYD(IUNIT,KEYWORD,HEAD_TEND(1),CDUMMY,STATUS)

* The next parameters will not exist for image data so ignore errors
      HEAD_XSTART = 1
      HEAD_YSTART = 1

      CALL RAT_FITSKEY('RDF',VERSION,'XEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_XEND,CDUMMY,TSTAT)

      CALL RAT_FITSKEY('RDF',VERSION,'YEND',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_YEND,CDUMMY,TSTAT)

      CALL RAT_FITSKEY('RDF',VERSION,'FDSIZEX',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_IFDSZX,CDUMMY,TSTAT)

      CALL RAT_FITSKEY('RDF',VERSION,'FDSIZEY',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_IFDSZY,CDUMMY,TSTAT)
*
* Dummy up small map values
      HEAD_ISMNUX=1
      HEAD_ISMNUY=1
      HEAD_ISMTNU=1
      HEAD_XSMAP(1)=1
      HEAD_YSMAP(1)=1
*
      CALL RAT_FITSKEY('RDF',VERSION,'EVENTS',KEYWORD,STATUS)
      CALL FTGKYJ(IUNIT,KEYWORD,HEAD_IEVTNU,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'OBSERVER',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD_OBSERVER,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'OBSERVATORY',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD_OBSERVATORY,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'INSTRUMENT',KEYWORD,STATUS)
      HEAD_INSTRUMENT = 'XRT'

      CALL RAT_FITSKEY('RDF',VERSION,'DETECTOR',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD_DETECTOR,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'FILTER',KEYWORD,STATUS)
      CALL FTGKYS(IUNIT,KEYWORD,HEAD_FILTER,CDUMMY,STATUS)
      IF (HEAD_FILTER.EQ.'NONE') HEAD_FILTER = 'OFF'

      CALL RAT_FITSKEY('RDF',VERSION,'XPUNITS',KEYWORD,STATUS)
      HEAD_XPUNITS = 'ARCSECONDS'

      CALL RAT_FITSKEY('RDF',VERSION,'YPUNITS',KEYWORD,STATUS)
      HEAD_YPUNITS  = 'ARCSECONDS'

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
      CALL FTGKYE(IUNIT,KEYWORD,HEAD_SKYCX,CDUMMY,STATUS)

      CALL RAT_FITSKEY('RDF',VERSION,'SKYCY',KEYWORD,STATUS)
      CALL FTGKYE(IUNIT,KEYWORD,HEAD_SKYCY,CDUMMY,STATUS)
      HEAD_NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,STATUS)


C - READ TIME RANGE INFORMATION
      CALL FTMAHD(IUNIT,2,HTYPE,STATUS)

      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD_NTRANGE,CDUMMY,STATUS)
C - CHECK NUMBER OF TIME RANGES DOESN'T EXCEED MAXIMUM
      IF (HEAD_NTRANGE.GT.MAXRAN) HEAD_NTRANGE = MAXRAN
      CALL FTGCVD(IUNIT,1,1,1,HEAD_NTRANGE,0,HEAD_TSTART,LDUMMY,STATUS)
      CALL FTGCVD(IUNIT,2,1,1,HEAD_NTRANGE,0,HEAD_TEND,LDUMMY,STATUS)
      DO I = 1,HEAD_NTRANGE
         HEAD_TSTART(I) = HEAD_TSTART(I) - HEAD_BASE_SCTIME
         HEAD_TEND(I) = HEAD_TEND(I) - HEAD_BASE_SCTIME
      ENDDO

*   Set amplitude ranges
      IF (INDEX(HEAD_DETECTOR,'PSPC') .NE. 0) THEN
         HEAD_ASTART = 1
         HEAD_AEND = 256
*
*   US data uses channel 0 which can't be processed by the system
         IF (HEAD_CSTART .EQ. 0) HEAD_CSTART = 1
*
* NB: The US PSPC CPHA channels are defined to be one less than the MPE files.
*
      ELSEIF (INDEX(HEAD_DETECTOR,'HRI') .NE. 0) THEN
         HEAD_ASTART = HEAD_CSTART
         HEAD_AEND = HEAD_CEND
         HEAD_CSTART = 1
         HEAD_CEND = 1
      ENDIF


C -   CHECK STATUS
      IF (STATUS.NE.SAI__OK) THEN
         CALL MSG_SETC('KEY',KEYWORD)
         CALL MSG_PRNT('** ERROR, possibly for key lookup: ^KEY **')
      ENDIF

      RETURN
      END

*+RAT_RDMPEHEAD   Reads header information from a MPE FITS file
	SUBROUTINE RAT_RDMPEHEAD(IUNIT,  STATUS)
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
      INCLUDE 'XRTHEAD_CMN'
* Parameters :
      INTEGER IUNIT			! Logical unit for FITS file
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
      CALL FTGKYJ(IUNIT,'NAXIS2',HEAD_IEVTNU,DUMMY,STATUS)

***** Read header information put into FITS in an Irrational way
      CALL GHIST1I(IUNIT,'NO_SM_X',HEAD_ISMNUX,STATUS)
      CALL GHIST1I(IUNIT,'NO_SM_Y',HEAD_ISMNUY,STATUS)
      CALL GHIST1S(IUNIT,'DETECTOR_ID',HEAD_DETECTOR,STATUS)
      CALL GHIST1S(IUNIT,'FILTER_ID',HEAD_FILTER,STATUS)
      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD_TARGET,STATUS)
      CALL GHIST1R(IUNIT,'SKY_PIX_SIZE',HEAD_PIXEL,STATUS)
      CALL GHIST1S(IUNIT,'OBS_ID',HEAD_OBSERVER,STATUS)
      CALL GHIST1S(iUNIT,'MISSION_ID',HEAD_OBSERVATORY,STATUS)
      HEAD_INSTRUMENT = 'XRT'

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
      HEAD_AXIS_RA = (HRS*15.0) + (MIN*15.0/60.0) + (SEC*15.0/3600.)
      READ(DECSTRING,FMT='(A1,I2,X,I2,X,I2)')SIGN,DEG,MIN,ISEC
      HEAD_AXIS_DEC = DEG + (MIN/60.0) + (ISEC/3600.)
      IF (SIGN .EQ. '-') HEAD_AXIS_DEC = -HEAD_AXIS_DEC

***** Read roll angle in degrees
      CALL GHIST1S(IUNIT,'XPIX_TO_NORTH',STR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
      KR=INDEX(STR, 'D')
      IF (KR . EQ. 0) THEN
         READ(STR, *)HEAD_ROLLCI
      ELSE
         READ(STR(1:KR-1), *)HEAD_ROLLCI
      ENDIF
***** Convert roll angle to Asterx standard
      HEAD_ROLLCI = 90.0 - HEAD_ROLLCI

      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD_TITLE,STATUS)
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
            HEAD_BASE_MJD=DBLE(IMJD1) + MJDBIT/86400.0
*
*      Calculate the end UT time
            K2=INDEX(TIME(K+12:80), ':') + K+11
            READ(TIME(K2-2:K2-1), FMT='(I2)')HRS
            READ(TIME(K2+1:K2+2), FMT='(I2)')MIN
            READ(TIME(K2+4:K2+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            END_MJD=DBLE(IMJD2) + MJDBIT/86400.0
            HEAD_END_MJD = END_MJD
*
      CALL GHIST1S(IUNIT,'OBS_CLOCK',STR,STATUS)
      READ(STR, *)HEAD_BASE_SCTIME,SC_END
      CALL GHIST1S(IUNIT,'TLABL003',STR,STATUS)
      HEAD_XPUNITS=STR(17:27)
      CALL GHIST1S(IUNIT,'TLABL004',STR,STATUS)
      HEAD_YPUNITS=STR(17:27)
      CALL GHIST1S(IUNIT,'RAW_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD_ASTART=INT(R1)
      HEAD_AEND=INT(R2)
      CALL GHIST1S(IUNIT,'AMP_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD_CSTART=INT(R1)
      HEAD_CEND=INT(R2)
      CALL GHISTnD(IUNIT,'TIM_SEL',ONOFF,MAXRAN*2,SIZE,STATUS)
      HEAD_NTRANGE = SIZE / 2
      DO LP=1,HEAD_NTRANGE
         HEAD_TSTART(LP)=ONOFF((LP-1)*2+1) - HEAD_BASE_SCTIME
         HEAD_TEND(LP)=ONOFF(LP*2) - HEAD_BASE_SCTIME
      ENDDO
*
      CALL GHIST1S(IUNIT,'SKY_FIELD',STR,STATUS)
      READ(STR, *)HEAD_XSTART,HEAD_YSTART,HEAD_XEND,HEAD_YEND
      HEAD_IFDSZX = HEAD_XEND - HEAD_XSTART
      HEAD_IFDSZY = HEAD_YEND - HEAD_YSTART

*   Get sky pixel centre
      CALL GHIST1R(IUNIT,'SKY_CEN_X',HEAD_SKYCX,STATUS)
      CALL GHIST1R(IUNIT,'SKY_CEN_Y',HEAD_SKYCY,STATUS)
*
      CALL GHIST1S(IUNIT,'DET_FIELD',STR,STATUS)
      READ(STR, *)HEAD_XDSTART,HEAD_YDSTART,HEAD_XDEND,HEAD_YDEND

*  Read in small map pointers
      CALL GHISTnI(IUNIT,'START_SM',HEAD_EVSTART,MAXMAPS,SIZE,STATUS)
      HEAD_ISMTNU=HEAD_ISMNUX*HEAD_ISMNUY

* Calculate the conversion factor between spacecraft time and UT
      HEAD_SCCONV = (END_MJD - HEAD_BASE_MJD) * 86400. /
     &                            (SC_END - HEAD_BASE_SCTIME)
* Create array of small map start positions
      XSIZE = HEAD_IFDSZX / REAL(HEAD_ISMNUX)
      YSIZE = HEAD_IFDSZY / REAL(HEAD_ISMNUY)

      DO LP=1,HEAD_ISMTNU
        HEAD_XSMAP(LP) = HEAD_XSTART+MOD((LP-1),HEAD_ISMNUX) * XSIZE
        HEAD_YSMAP(LP) = HEAD_YSTART+INT((LP-1)/HEAD_ISMNUY) * YSIZE
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
	SUBROUTINE RAT_RDIMAGE(IUNIT,ORIGIN,STATUS)
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
*    Global variables :
*    Global parameters
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

      IF (origin.eq.'US')  CALL RAT_RDUSIMAGE(iunit,STATUS)
      IF (origin.eq.'RDF') CALL RAT_RDRDFIMAGE(iunit,STATUS)
      IF (origin.eq.'MPE') CALL RAT_RDMPEIMAGE(iunit,STATUS)

      RETURN
      END

*+ RAT_RDUSIMAGE -  Read image header information from US FITS file
	SUBROUTINE RAT_RDUSIMAGE(IUNIT,STATUS)
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
*    Global variables :
      INCLUDE 'XRTHEAD_CMN'
*    Global parameters
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

      CALL FTGKYS(IUNIT,'TITLE',HEAD_TITLE,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'OBJECT',CVAL,CDUMMY,STATUS)
      HEAD_TARGET = CVAL(1:20)
      IF (STATUS.NE.0) RETURN

      CALL FTGKYJ(IUNIT,'XS-MINCH',HEAD_CSTART,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-MAXCH',HEAD_CEND,CDUMMY,STATUS)
*
      CALL FTGKYD(IUNIT,'CRVAL1',HEAD_AXIS_RA,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CRVAL2',HEAD_AXIS_DEC,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CROTA2',HEAD_ROLLCI,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'CDELT2',DVAL,CDUMMY,STATUS)
      HEAD_PIXEL = DVAL * 3600.0

      HEAD_XDSTART = 1
      HEAD_YDSTART = 1
      CALL FTGKYJ(IUNIT,'XS-XDET',HEAD_XDEND,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-YDET',HEAD_YDEND,CDUMMY,STATUS)
*
* Get exposure time info
      HEAD_NTRANGE=1
      HEAD_TSTART(1)=0.0D0
      CALL FTGKYD(IUNIT,'XS-LIVTI',HEAD_TEND(1),CDUMMY,STATUS)

      CALL FTGKYS(IUNIT,'XS-SEQPI',CVAL,CDUMMY,STATUS)
      HEAD_OBSERVER = CVAL(1:20)
      CALL FTGKYS(IUNIT,'TELESCOP',HEAD_OBSERVATORY,CDUMMY,STATUS)
      HEAD_INSTRUMENT = 'XRT'
      CALL FTGKYS(IUNIT,'INSTRUME',HEAD_DETECTOR,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-FILTR',IVAL,CDUMMY,STATUS)
      IF (IVAL.EQ.0) HEAD_FILTER = 'OFF'
      IF (IVAL.EQ.1) HEAD_FILTER = 'BORON'
      HEAD_XPUNITS = 'ARCSECONDS'
      HEAD_YPUNITS  = 'ARCSECONDS'
      CALL FTGKYJ(IUNIT,'XS-MJDRD',XS_MJDRD,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'XS-MJDRF',XS_MJDRF,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-OBS',DATE_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-OBS',TIME_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-END',DATE_END,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-END',TIME_END,CDUMMY,STATUS)
      CALL FTGKYJ(IUNIT,'XS-EVREF',XS_EVREF,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX1',HEAD_SKYCX,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX2',HEAD_SKYCY,CDUMMY,STATUS)
      HEAD_NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,STATUS)

      RETURN
      END

*+ RAT_RDRDFIMAGE -  Read image header information from RAT FITS file
	SUBROUTINE RAT_RDRDFIMAGE(IUNIT,STATUS)
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
*    Global variables :
      INCLUDE 'XRTHEAD_CMN'
*    Global parameters
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

      CALL FTGKYS(IUNIT,'OBJECT',HEAD_TITLE,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'OBJECT',HEAD_TARGET,CDUMMY,STATUS)

      IF (STATUS.NE.0) RETURN
*
*     PI channel (not on HRI)
      CALL FTGKYS(IUNIT,'INSTRUME',HEAD_DETECTOR,CDUMMY,STATUS)

      IF (HEAD_DETECTOR.EQ.'HRI') THEN
         CALL FTGKYJ(IUNIT,'PHAMIN',HEAD_CSTART,CDUMMY,STATUS)
         CALL FTGKYJ(IUNIT,'PHAMAX',HEAD_CEND,CDUMMY,STATUS)
      ELSE
         CALL FTGKYJ(IUNIT,'PIMIN',HEAD_CSTART,CDUMMY,STATUS)
         CALL FTGKYJ(IUNIT,'PIMAX',HEAD_CEND,CDUMMY,STATUS)
      ENDIF
*
*     RA, DEC & ROLL
      CALL FTGKYD(IUNIT,'RA_NOM',HEAD_AXIS_RA,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'DEC_NOM',HEAD_AXIS_DEC,CDUMMY,STATUS)
      HEAD_ROLLCI=0.0
*
*     Degrees per pixel (use CDELT1 or CDELT2)
      CALL FTGKYD(IUNIT,'CDELT2',DVAL,CDUMMY,STATUS)
      IF (STATUS.NE.0) RETURN
      HEAD_PIXEL = DVAL * 3600.0
*
* Get exposure time info
      HEAD_NTRANGE=1
      HEAD_TSTART(1)=0.0D0
      CALL FTGKYD(IUNIT,'LIVETIME',HEAD_TEND(1),CDUMMY,STATUS)
*
      CALL FTGKYS(IUNIT,'OBSERVER',HEAD_OBSERVER,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TELESCOP',HEAD_OBSERVATORY,CDUMMY,STATUS)
      HEAD_INSTRUMENT = 'XRT'
      CALL FTGKYS(IUNIT,'FILTER',HEAD_FILTER,CDUMMY,STATUS)
      IF (HEAD_FILTER.EQ.'NONE') HEAD_FILTER = 'OFF'
      HEAD_XPUNITS = 'ARCSECONDS'
      HEAD_YPUNITS  = 'ARCSECONDS'
      CALL FTGKYJ(IUNIT,'MJDREFI',XS_MJDRD,CDUMMY,STATUS)
      CALL FTGKYD(IUNIT,'MJDREFF',XS_MJDRF,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE-OBS',DATE_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME-OBS',TIME_OBS,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'DATE_END',DATE_END,CDUMMY,STATUS)
      CALL FTGKYS(IUNIT,'TIME_END',TIME_END,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX1',HEAD_SKYCX,CDUMMY,STATUS)
      CALL FTGKYE(IUNIT,'CRPIX2',HEAD_SKYCY,CDUMMY,STATUS)
      HEAD_NSPOT = 0

C - CONVERT TIME FORMATS
      CALL RAT_TMCONV(DATE_OBS,TIME_OBS,DATE_END,TIME_END,
     &                                 XS_MJDRD,XS_MJDRF,STATUS)


*   Set amplitude ranges
      IF (INDEX(HEAD_DETECTOR,'PSPC') .NE. 0) THEN
         HEAD_ASTART = 1
         HEAD_AEND = 256
*
*   US data uses channel 0 which can't be processed by the system
         IF (HEAD_CSTART .EQ. 0) HEAD_CSTART = 1
*
* NB: The US PSPC CPHA channels are defined to be one less than the MPE files.
*
      ELSEIF (INDEX(HEAD_DETECTOR,'HRI') .NE. 0) THEN
         HEAD_ASTART = HEAD_CSTART
         HEAD_AEND = HEAD_CEND
         HEAD_CSTART = 1
         HEAD_CEND = 1
      ENDIF

      RETURN
      END

*+RAT_RDMPEIMAGE   Reads image header information from a MPE FITS file
	SUBROUTINE RAT_RDMPEIMAGE(IUNIT, STATUS)
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
      INCLUDE 'XRTHEAD_CMN'
* Parameters :
      INTEGER IUNIT			! Logical unit for FITS file
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
      CALL FTGKYS(IUNIT,'CTYPE1',HEAD_XPUNITS,DUMMY,STATUS)
      CALL FTGKYS(IUNIT,'CTYPE2',HEAD_YPUNITS,DUMMY,STATUS)

***** Read header information put into FITS in an Irrational way
      CALL GHIST1S(IUNIT,'DETECTOR_ID',HEAD_DETECTOR,STATUS)
      CALL GHIST1S(IUNIT,'FILTER_ID',HEAD_FILTER,STATUS)
      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD_TARGET,STATUS)
      CALL GHIST1R(IUNIT,'SKY_PIX_SIZE',HEAD_PIXEL,STATUS)
      CALL GHIST1S(IUNIT,'OBS_ID',HEAD_OBSERVER,STATUS)
      CALL GHIST1S(iUNIT,'MISSION_ID',HEAD_OBSERVATORY,STATUS)
      HEAD_INSTRUMENT = 'XRT'

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
      HEAD_AXIS_RA = HRS*15.0 + MIN*15.0/60.0 + SEC*15.0/3600.
      READ(DECSTRING,FMT='(A1,I2,X,I2,X,I2)')SIGN,DEG,MIN,ISEC
      HEAD_AXIS_DEC = DEG + MIN/60.0 + ISEC/3600.
      IF (SIGN .EQ. '-') HEAD_AXIS_DEC = -HEAD_AXIS_DEC

***** Read roll angle in degrees
      CALL GHIST1S(IUNIT,'XPIX_TO_NORTH',STR,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 999
      KR=INDEX(STR, 'D')
      IF (KR . EQ. 0) THEN
         READ(STR, *)HEAD_ROLLCI
      ELSE
         READ(STR(1:KR-1), *)HEAD_ROLLCI
      ENDIF
***** Convert roll angle to Asterx standard
      HEAD_ROLLCI = 90.0 - HEAD_ROLLCI

      CALL GHIST1S(IUNIT,'OBS_TITLE',HEAD_TITLE,STATUS)
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
            HEAD_BASE_MJD=DBLE(IMJD1) + MJDBIT/86400.0
*
*      Calculate the end UT time
            K2=INDEX(TIME(K+12:80), ':') + K+11
            READ(TIME(K2-2:K2-1), FMT='(I2)')HRS
            READ(TIME(K2+1:K2+2), FMT='(I2)')MIN
            READ(TIME(K2+4:K2+12), *)SEC
            MJDBIT=HRS*3600.0 + MIN*60.0 + SEC
            END_MJD=DBLE(IMJD2) + MJDBIT/86400.0
            HEAD_END_MJD = END_MJD
*
      CALL GHIST1S(IUNIT,'OBS_CLOCK',STR,STATUS)
      READ(STR, *)HEAD_BASE_SCTIME,SC_END
      CALL GHIST1S(IUNIT,'RAW_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD_ASTART=INT(R1)
      HEAD_AEND=INT(R2)
      CALL GHIST1S(IUNIT,'AMP_SEL',STR,STATUS)
      READ(STR, *)R1,R2
      HEAD_CSTART=INT(R1)
      HEAD_CEND=INT(R2)
* if no TIM_SEL then keep current values
      CALL GHISTnD(IUNIT,'TIM_SEL',ONOFF,MAXRAN*2,SIZE,STATUS)
      IF (STATUS.NE.SAI__OK) THEN
         CALL ERR_ANNUL(STATUS)
      ELSE
         HEAD_NTRANGE = SIZE / 2
         DO LP=1,HEAD_NTRANGE
            HEAD_TSTART(LP)=ONOFF((LP-1)*2+1) - HEAD_BASE_SCTIME
            HEAD_TEND(LP)=ONOFF(LP*2) - HEAD_BASE_SCTIME
         ENDDO
      ENDIF
*   Get sky pixel centre
      CALL GHIST1R(IUNIT,'SKY_CEN_X',HEAD_SKYCX,STATUS)
      CALL GHIST1R(IUNIT,'SKY_CEN_Y',HEAD_SKYCY,STATUS)
* Calculate the conversion factor between spacecraft time and UT
      HEAD_SCCONV = (END_MJD - HEAD_BASE_MJD) * 86400. /
     &                            (SC_END - HEAD_BASE_SCTIME)
*
999   CONTINUE
*
      IF (STATUS .NE. SAI__OK) THEN
           CALL ERR_REP(' ','from RAT_RDMPEHEAD',STATUS)
      ENDIF
*
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


*+RAT_GETEVE - slice and map range of data
      SUBROUTINE RAT_GETEVE(PTRA, NELEM, T1, T2, T3, T4,
     :                                T5, T6, T7, STATUS)
*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Global parameters
*    Status :
      INTEGER STATUS
*    Structures :
*    Import :
      INTEGER PTRA(7)                    ! pointers to mapped data
      INTEGER NELEM                      ! number of elements mapped
*    Export :
      INTEGER T1(*),T2(*),T3(*),T4(*),T5(*),T6(*),T7(*)
*    Local variables :

      CALL ARR_COP1I(NELEM,%val(PTRA(1)),T1,STATUS)
      CALL ARR_COP1I(NELEM,%val(PTRA(2)),T2,STATUS)
      CALL ARR_COP1I(NELEM,%val(PTRA(3)),T3,STATUS)
      CALL ARR_COP1I(NELEM,%val(PTRA(4)),T4,STATUS)
      CALL ARR_COP1I(NELEM,%val(PTRA(5)),T5,STATUS)
      CALL ARR_COP1I(NELEM,%val(PTRA(6)),T6,STATUS)
      CALL ARR_COP1I(NELEM,%val(PTRA(7)),T7,STATUS)

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
