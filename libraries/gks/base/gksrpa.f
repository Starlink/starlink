C# IL>=a, OL>=0
      SUBROUTINE GKSRPA (NID,IDAT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set pattern representation
*
*  MAINTENANCE LOG
*  ---------------
*     01/12/83  AS    Original version stabilized
*     29/03/84  CJW   Correct number of pixels stored (S38)
*     20/01/87  RMK   IS conversion. Additional values now passed down
*                     from frontend. Only store on the heap the portion
*                     of the array specified in GSPAR call. Changed to
*                     use error 93.
*     26/06/87  RMK   Altered check on colour indices to only look at
*                     the specified portion of IDAT.
*
*  ARGUMENTS
*  ---------
*     INP  NID     Length of array IDAT
*     INP  IDAT    Colour array
*
      INTEGER NID, IDAT(NID)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER I, J, INTA(3)
      REAL REALA(1)
*
*  ERRORS
*  ------
*     93   Colour index is invalid
*
*---------------------------------------------------------------------


* Check if colour indices valid
      DO 20 J=KWI5, KWI5+KWI7-1
        DO 10 I=KWI4, KWI4+KWI6-1
          IF (IDAT((J-1)*KWI2+I).LT.0 .OR.
     :        IDAT((J-1)*KWI2+I).GT.KPCI(KWKIX)-1) THEN
            KERROR = 93
            GOTO 999
          ENDIF
   10   CONTINUE
   20 CONTINUE


* Find out if index exists
      CALL GKDRGE(KPABPT(KWKIX),KWI1,3,0,INTA,REALA)
      IF (KERROR.EQ.0) THEN
* If it does, deallocate heap for pattern
        CALL GKHPDA(INTA(3),KINTGS)
      ELSE
* If it doesn't, directory will automatically extend
        KERROR = 0
      ENDIF

      INTA(1) = KWI6
      INTA(2) = KWI7
* Grab heap space for new pattern
      CALL GKHPAL(KWI6*KWI7,KINTGS,INTA(3))
      IF (KERROR.NE.0) GOTO 999

* Fill directory entry for this pattern
      CALL GKDRPU (KPABPT(KWKIX),KWI1,3,0,INTA,REALA)
      IF (KERROR.NE.0) GOTO 999

* Copy pattern into heap
      DO 40 I=KWI5,KWI5+KWI7-1
        CALL GKHPPI(INTA(3),(I-KWI5)*KWI6,KWI6,IDAT((I-1)*KWI2+KWI4))
   40 CONTINUE

* See if regeneration will be necessary
      IF (KDSMT(KWKIX).EQ.GNEMPT .AND. KWFAIS(KWKIX).EQ.GPATTR .AND.
     :    KWFASI(KWKIX).EQ.KWI1) THEN
        IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
          KRGN = .TRUE.
          KWRGN(KWKIX) = .TRUE.
        ELSE
          KNFAUP(KWKIX) = GYES
        ENDIF
      ENDIF


  999 CONTINUE

      END
