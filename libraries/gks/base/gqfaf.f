C# IL>=a, OL>=0
      SUBROUTINE GQFAF (IWTYPE,NITH,NHTH,IER,NIS,IIS,NHS,IHS,NPFAI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE FILL AREA FACILITIES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns fill area facilities
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     30/09/83  AS    FORTRAN binding changes
*     24/10/90  KEVP  Put in correct handling of error 2002 (C55 & C29)
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE  workstation type
*     INP NITH    list element of interior styles requested
*     INP NHTH    list element of hatch styles requested
*     OUT IER     error indicator
*     OUT NIS     number of available fill area interior styles
*     OUT IIS     Nth element of list of available fill area interior styles
*     OUT NHS     number of available hatch styles
*     OUT IHS     Nth element of list of available hatch styles
*     OUT NPFAI   number of predefined fill area indices
*
      INTEGER IWTYPE, NITH, NHTH, IER, NIS, IIS, NHS, IHS, NPFAI
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     2002  List element requested for fill-style or hatch-style out of range
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        KWI1 = NITH
        KWI2 = NHTH
        CALL GKSONW(IWTYPE,KQFAF,1,KDAT,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF ((KERROR.EQ.0) .OR. (KERROR.EQ.2002)) THEN
          NIS = KWI1
          NHS = KWI3
          NPFAI = KWI5
*         Make sure reporting of error 2002 is correct
*         and set list elements
*         for fill style
          CALL GKLERR (NITH,NIS,KWI2,IIS,KERROR)
          IER = KERROR
*         and for hatch style.
          CALL GKLERR (NHTH,NHS,KWI4,IHS,KERROR)
          IF(KERROR .NE. 0) IER = KERROR
        ENDIF
      ELSE
        IER = KERROR
      ENDIF

      END
