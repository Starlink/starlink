C# IL>=a, OL>=0
      SUBROUTINE GKQXXI (IWKID,IENT,NTH,IER,IO,IELMNT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Nth element of list of XX indices, where XX =polyline,
*     polymarker,text,fill area,pattern,colour,set of segment names on
*     workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*     10/01/83  AS    Check for valid NTH
*     17/01/84  JRG   Allow number of entries to return even if error 902
*     02/02/84  JRG   Another comment change
*     22/01/87  JCS   IS conversion. Error number changes.
*     27/09/90  KEVP  Changed error checking to conform to GKS FORTRAN
*                     BINDING standard (C41). See comments below.
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IENT    entrypoint
*     INP NTH     list element requested
*     OUT IER     error indicator
*     OUT IO      number of XX bundle table entries
*     OUT IELMNT  Nth element of list of defined XX indices
*
      INTEGER IWKID, IENT, NTH, IER, IO, IELMNT
*
*  COMMON BLOCK USAGE
*  ------------------
*
*      Modify /WCA/ Set up before entering wkstn
*      Read   /ERR/ KERROR inspected
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     2002  List or set element invalid
*
*  COMMENTS
*  --------
*     The number of list elements is returned even if error 2002 occurs.
*     Error 2002 is not returned if the list element requested is 0 or
*     there are 0 list elements.
*     This is required by the GKS FORTRAN BINDING.
*
*     The checking of error 2002 is done completely here and
*     need no longer be done by the workstation.
*
*  NOTE
*  ----
*     This routine may only be used for inquiries about an
*     open workstation. Hence it is not used for GQEGDP.
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        KWI1 = NTH
        CALL GKSONW(IWKID,IENT,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR .EQ. 0 .OR. KERROR .EQ. 2002) THEN
            IO = KWI1
            IELMNT = KNIL
            IF((NTH.EQ.0).OR.(IO.EQ.0))THEN
               KERROR = 0
            ELSEIF((NTH.LT.0).OR.(IO.LT.NTH))THEN
               KERROR = 2002
            ELSEIF(KERROR.EQ.0)THEN
               IELMNT = KWI2
            ENDIF
        ENDIF
      ENDIF

      IER = KERROR

      END
