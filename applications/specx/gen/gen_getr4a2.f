*  History:
*     16 Nov 1993 (hme):
*        Disuse OUTERR_HANDLER error handler.
*     31 Jan 1994 (hme):
*        Remove second declaration of IERR.
*     31 July 2000 (ajc):
*        Re-write illegal concatenation
*        Change TYPE * to PRINT *
*        Unused IT
*-----------------------------------------------------------------------

      LOGICAL FUNCTION GEN_GETR4A2 (PROMPT, R4DEF, NSIZ,
     &                              FDEF, R4ARR, NIT, JDEF)

      IMPLICIT  NONE

C   Routine to put a prompt message to the terminal and receive an
C   integer*4 array of length NSIZ, returned in R4ARR. JDEF as in GETCH.
C   R4DEF is the default array for R4ARR, FDEF is a format qualifier
C   e.g.'2(I3)', describing how the default is to be presented by the
C   prompt. A null string for FDEF will suppress the presentation
C   of the default value.

*     Formal parameters

      INTEGER*4 NSIZ
      INTEGER*4 NIT
      REAL*4    R4DEF(NSIZ)
      REAL*4    R4ARR(NSIZ)
      CHARACTER PROMPT*(*)
      CHARACTER FDEF*(*)
      INTEGER*4 JDEF

*     Local variables

      INTEGER*4 J
      INTEGER*4 IDS
      INTEGER*4 IERR
      INTEGER*4 ILP
      INTEGER*4 ILS
      INTEGER*4 ITS
      INTEGER*4 ILT
      INTEGER*4 INEXT
      INTEGER*4 IST, IFIN
      INTEGER*4 LEVEL
      INTEGER*4 N
      CHARACTER DEFSTR*32
      CHARACTER STRING*64
      CHARACTER TSTRING*80
      CHARACTER PSTRING*80

*     Functions

      INTEGER*4 GEN_ILEN

      NIT         = 0
      JDEF        = 0
      GEN_GETR4A2 = .TRUE.

      DO J = 1,NSIZ
       R4ARR(J) = R4DEF(J)
      END DO

*  Decide level depending on how many parameters we want to use...

      LEVEL = MIN (2, NSIZ)

      IF (GEN_ILEN(FDEF).EQ.0)   THEN
        CALL GEN_INPUT (LEVEL, PROMPT, TSTRING, ILT, JDEF)
        IF (JDEF.NE.0)  RETURN

      ELSE
        STRING = '('//FDEF(1:GEN_ILEN(FDEF))//')'
        WRITE (DEFSTR, STRING, IOSTAT=IERR) R4DEF
        IDS = GEN_ILEN (DEFSTR)
        ILP = GEN_ILEN (PROMPT)
        PSTRING = PROMPT(:ILP)
        PSTRING(ILP+1:) = ' ['
        PSTRING(ILP+3:) = DEFSTR(:IDS)
        PSTRING(ILP+IDS+4:) = '] '
        CALL GEN_INPUT (LEVEL, PSTRING,
     &                  TSTRING, ILT, JDEF)
        IF (JDEF.NE.0)  RETURN
      END IF

      ITS = 1
      ILS = GEN_ILEN (TSTRING)

C  Then read those items that are given

      N     = 0
      IFIN  = 0
      INEXT = 1
      DO WHILE (N.LT.NSIZ .AND. INEXT.GT.IFIN)
        CALL GEN_GETIT3 (TSTRING(ITS:ILS), 1, IST, IFIN, INEXT, IERR)
CD      print *,'Parsed string: ', tstring(its:ils)
CD      print *,' n, ist, ifin, inext, ierr'
CD      print *,  n, ist, ifin, inext, ierr
        IF (IERR.EQ.0) THEN
          N = N + 1
          IF (      TSTRING(ITS+IST-1:ITS+IFIN-1).NE.'#'
     &        .AND. IFIN.GE.IST) THEN
            CALL GEN_EVAL_AE (TSTRING(ITS+IST-1:ITS+IFIN-1),
     &                        'R4', R4ARR(N), IERR)
            IF (IERR.NE.0) GO TO 10
          END IF
          ITS = ITS + INEXT - 1
        END IF
      END DO

      NIT = N
      RETURN

C   Error handling

   10 IF (IERR.NE.0)   THEN
        PRINT *,'Sorry - I couldn''t understand that'
        JDEF = -1
      END IF

      RETURN
      END
