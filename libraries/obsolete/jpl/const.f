C------------------------------ SUBROUTINE 'CONST' ---------------------
C
C+++++++++++++++++++++++++++++
C
      SUBROUTINE CONST(NAM,VAL,SSS,N)
C
C+++++++++++++++++++++++++++++
C
C     THIS SUBROUTINE OBTAINS THE CONSTANTS FROM THE EPHEMERIS FILE
C
C     CALLING SEQUENCE PARAMETERS (ALL OUTPUT):
C
C       NAM = CHARACTER*6 ARRAY OF CONSTANT NAMES
C
C       VAL = D.P. ARRAY OF VALUES OF CONSTANTS
C
C       SSS = D.P. JD START, JD STOP, STEP OF EPHEMERIS
C
C         N = INTEGER NUMBER OF ENTRIES IN 'NAM' AND 'VAL' ARRAYS
C
C     THE ARRAYS 'NAM' AND 'VAL' MUST HAVE SUFFICIENTLY LARGE
C     DIMENSIONS TO ACCOMMODATE ALL ENTRIES ON THE FILE. THE
C     VALUE 400 IS A SAFE UPPER LIMIT FOR THIS DIMENSION.
C
      SAVE
C
      CHARACTER*6 NAM(*)
      DOUBLE PRECISION VAL(*)
      DOUBLE PRECISION SSS(3)
      INTEGER N

*-------------------------------------------------------------------
*
*  Modification to JPL code to ensure that the BLOCK DATA subprogram
*  COMDAT gets included at link time.
*
*  A.J.J.Broderick   Starlink   2 November 1992
*
      EXTERNAL COMDAT
*
*-------------------------------------------------------------------

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
C
C       OPEN THE FILE AND READ RECORD 1
C
      CALL EPHOPN(.TRUE.)
C
C       COPY CORRECT DATA TO CALLING SEQUENCE VARIABLES
C
      N=NCON
      DO 1 I=1,3
    1 SSS(I)=SS(I)
      DO 2 I=1,NCON
      NAM(I)=CNAM(I)
    2 VAL(I)=CVAL(I)
C
      RETURN
C
      END
