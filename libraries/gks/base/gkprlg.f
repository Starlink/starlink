C# IL>=a, OL>=0
      SUBROUTINE GKPRLG ( KNAME, MINST, MAXST)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONT END
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Every User Level routine must perform common functions on entry. These
*     are done by GKPRLG.
*
*  MAINTENANCE LOG
*  ---------------
*     11/11/82  CJW   Original version stabilized
*     10/02/83  CJW   Converted to subroutine - now called by all USER LEVEL
*                     routines. Does initialization of GKS. Returns error.
*     20/05/83  CJW   Errors >999 reported by GKBUG
*     27/06/83  CJW   Implement revised error handling precedure
*     27/06/83  CJW   Initialize IO system by call to GKIOOP
*     26/07/83  CJW   Routine name becomes integer
*     11/08/83  CJW   Change GKIOOP to GKIOBG
*     14/12/83  CJW   Call Internal EM-CL-KS instead of of user routine
*     22/01/87  JCS   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP   KNAME  Name of calling routine  ( Blank if inquiry )
*     INP   MINST  Minimum GKS Operating state
*     INP   MAXST  Maximum GKS Operating state
*
      INTEGER  KNAME, MINST, MAXST
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKOPS/    KOPS,KERRS
*     Modify /GKYERR/   Set error status
*     Read   /GKERR/    KERRFL
*     Modify /GKERR/    CRTNM
*     Modify /GKFLS/    KEMFLS
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
*
*  LOCALS
*  ------
*     INIGKS  True if GKS initialized (S)
*
      LOGICAL INIGKS
      SAVE INIGKS
      DATA INIGKS/.FALSE./
*
*  ERRORS
*  ------
*   -1006    Non-Inquiry routine called while GKS is in the error state
*   -2004    Documented condition to be satisfied by parameter(s) of
*            internal routine is not satisfied - MINST, MAXST must correspond
*            to one of the pairs of states listed in GKS error numbers 1-8
*
*  ALGORITHM
*  ---------
*  Prologue - The following tasks are required on entry to any user callable
*  routine -
*
*  1) Initialize GKS if not already done.
*
*  2) Checks that the GKS operating state lies in the range MINST -> MAXST.
*
*  For non inquiry routines ONLY -
*
*  3) Checks error state - performs emergency close GKS if on and returns
*
*  4) Saves the routine name ( KNAME ) in the error state list.
*
*  COMMENTS
*  --------
*  SAVEd variable INIGKS used to detect if GKS has been initialized.
*  This could be done using COMMON and BLOCK DATA - but the whole point of
*  the variable is to avoid BLOCK DATA! It would be simpler to put KERRS and
*  KOPS in a BLOCK DATA instead.
*
*  All user callable routine should start -
*
*      CALL GKPRLG(KNAME,MINST,MAXST)
*      IF (KERROR .NE. 0) GOTO 990
*       ....
*  990 RETURN
*
*  If GKS is overlayed care must be taked to ensure INIGKS is not
*  re-initialised. This should happen automatically (because of the SAVE) but
*  on some systems this routine may have to reside in the ROOT OVERLAY.
*  Inquires should use name KNIL
*
*                        SYSTEM DEPENDENT
*
*---------------------------------------------------------------------


*     Initialize GKS

      IF (.NOT. INIGKS) THEN
         KERRS = KOFF
         KOPS = GGKCL
         CALL GKIOBG
         INIGKS = .TRUE.
      END IF

*     Check GKS operating state

      IF ((KOPS .LT. MINST) .OR. (KOPS .GT. MAXST)) THEN

*        Sort out error number

         IF ((MINST .EQ. GGKCL) .AND. (MAXST .EQ. GGKCL)) THEN
            KERROR = 1
         ELSE IF ((MINST .EQ. GGKOP) .AND. (MAXST .EQ. GGKOP)) THEN
            KERROR = 2
         ELSE IF ((MINST .EQ. GWSAC) .AND. (MAXST .EQ. GWSAC)) THEN
            KERROR = 3
         ELSE IF ((MINST .EQ. GSGOP) .AND. (MAXST .EQ. GSGOP)) THEN
            KERROR = 4
         ELSE IF ((MINST .EQ. GWSAC) .AND. (MAXST .EQ. GSGOP)) THEN
            KERROR = 5
         ELSE IF ((MINST .EQ. GWSOP) .AND. (MAXST .EQ. GWSAC)) THEN
            KERROR = 6
         ELSE IF ((MINST .EQ. GWSOP) .AND. (MAXST .EQ. GSGOP)) THEN
            KERROR = 7
         ELSE IF ((MINST .EQ. GGKOP) .AND. (MAXST .EQ. GSGOP)) THEN
            KERROR = 8
         ELSE
         CALL GKBUG(-2004, 'GKPRLG')
         ENDIF

      ELSE

*        All OK

         KERROR = 0

      ENDIF

*     Non Inquiry Functions only

      IF (KNAME .NE. KNIL) THEN

         IF (KERRS .EQ. KON) THEN

*           Called while in error state
            CALL GERLOG(-1006, KNAME, KERRFL)
            CALL GERLOG(1006,KNAME,KERRFL)
            CALL GKECKS
            KERROR = -1006

         ELSE

*           Save name of calling routine

            KRTNM = KNAME

         END IF

      END IF


      END
