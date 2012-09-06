#if HAVE_CONFIG_H
# include "config.h"
#endif

C------------------------------ SUBROUTINE 'EPHOPN' --------------------
*
*
*  P.T.Wallace   Starlink   18 April 1994
*
*  Tim Jenness   JACH       13 July  2004
*     Autoconf version [derived from VMS variant]
*
C
C++++++++++++++++++++++++
C
      SUBROUTINE EPHOPN(RDF)
C
C++++++++++++++++++++++++
C
C     THIS SUBROUTINE OPENS THE JPL PLANETARY EPHEMERIS FILE,
C     EXPECTED TO BE 'JPLEPH'.
C

*-----------------------------------------------------------------
*  Modification to JPL code to make sure that the BLOCK DATA
*  subprogram COMDAT is included at link time.
*
*  A.J.J.Broderick   Starlink   2 November 1992

      EXTERNAL COMDAT

*-----------------------------------------------------------------

      SAVE
C
C       COMMON AREA FOR CHARACTER DATA IN RECORD 1
C
      CHARACTER*6 TTL(14,3)
      CHARACTER*6 CNAM(400)
      COMMON/CHRHDR/TTL,CNAM
C
C       COMMON AREA FOR CONSTANTS AND POINTERS IN RECORD 1
C
      DOUBLE PRECISION SS(3)
      INTEGER NCON
      DOUBLE PRECISION CVAL(400)
      DOUBLE PRECISION AU
      DOUBLE PRECISION EMRAT
      INTEGER IPT(36)
      INTEGER DENUM
      INTEGER LPT(3)
*
*  Variables re-ordered to avoid alignment problems.
*
*  P.T.Wallace   Starlink   14 April 1994
*
      COMMON/EPHHDR/SS,CVAL,AU,EMRAT,NCON,IPT,DENUM,LPT
C
      INTEGER FILE
      COMMON/EPUNIT/FILE
C
C
C       COMMON AREA FOR EPH DATA BUFFERS
C
*
*  Modifications to do with alignment problems elsewhere.  This has
*  also necessitated the explicit 1652.
*
*  P.T.Wallace   Starlink   14 April 1994
*
      INTEGER IBSZ
      INTEGER IB(1652)
      COMMON/EPCOMM/IB,IBSZ
C
      INTEGER IRECSZ
C
      LOGICAL RDF
      LOGICAL FIRST
C
      CHARACTER*7 STAWD

C   Use autoconf without trying to do variable substitution
      INTEGER BYTEPRU

#if FC_RECL_UNIT == 1
       PARAMETER ( BYTEPRU = 1 )
#elif FC_RECL_UNIT == 2
       PARAMETER ( BYTEPRU = 2 )
#elif FC_RECL_UNIT == 4
       PARAMETER ( BYTEPRU = 4 )
#else
*      Should get a compilation error for this.
 error 'Impossible record length'
#endif

C
      DATA FIRST/.TRUE./
C
C       THE FOLLOWING STATEMENTS DEFINE THE LENGTH OF THE RECORDS
C       IN THE DIRECT-ACCESS FILE. IF THE 'RECL' SPECIFICATION IN
C       THE FORTRAN COMPILER ON YOUR MACHINE EXPECTS THE LENGTH TO
C       BE IN WORDS, LEAVE THE STATEMENTS AS THEY ARE. IF THE LENGTH
C       IS EXPECTED IN BYTES, REMOVE THE 'C' FROM COLUMN 1 IN THE
C       SECOND STATEMENT.
C
C     IBSZ is the record length in 4-byte WORDS
C     This should be converted to local record length units
C     by converting the record length to bytes and then dividing
C     by the number of bytes per record unit
      IRECSZ=IBSZ * 4 / BYTEPRU

C
C       OPEN FILE AND READ RECORD # 1 AND 2 ON FIRST ENTRY ONLY
C
      IF(FIRST) THEN
        FIRST=.FALSE.
        IF(RDF) THEN
          STAWD='OLD'
        ELSE
          STAWD='UNKNOWN'
        ENDIF
! Mods by Starlink in this area
        IF (RDF) THEN
           OPEN(UNIT=FILE,
     *          FILE='JPLEPH',
     *          ACCESS='DIRECT',
     *          FORM='UNFORMATTED',
     *          RECL=IRECSZ,
#if HAVE_FC_OPEN_ACTIONREAD
     *          ACTION='READ',
#elif HAVE_FC_OPEN_READONLY
     *          READONLY,
#endif
     *          STATUS=STAWD)
        ELSE
           OPEN(UNIT=FILE,
     *          FILE='JPLEPH',
     *          ACCESS='DIRECT',
     *          FORM='UNFORMATTED',
     *          RECL=IRECSZ,
     *          STATUS=STAWD)
        END IF
! End of Starlink mods
        IF(RDF) THEN
          READ(FILE,REC=1)TTL,CNAM,SS,NCON,AU,EMRAT,IPT,DENUM,LPT
          READ(FILE,REC=2)CVAL
        ENDIF
      ENDIF
C
      RETURN
C
      END
