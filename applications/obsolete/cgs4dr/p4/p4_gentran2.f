*+  P4_GENTRAN2 - calculate the TR matrix for PGGRAY (2-D data)
      SUBROUTINE P4_GENTRAN2 (I_START, I_END, J_START, J_END,
     :   AXIS1, AXIS2, TRAN, STATUS)
*    Description :
*     This calculates the transformation matrix, TRAN, required for
*     the PGGRAY and PGCONT routines. This version of the routine
*     is called when 2-D data are to be displayed.
*    Invocation :
*     CALL P4_GENTRAN2 (I_START,I_END, J_START, J_END, AXIS1, AXIS2,
*    :   TRAN, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     It would probably be safer if the size of the axis arrays
*     were passed to this routine.
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     September 1989: Original version        (JFL)
*     30-Oct-1989: History added. Array dimensioning bugs fixed.  (SMB)
*     31-Oct-1989: Renamed to P4_GENTRAN2. Arguments rearranged.
*                  Description added.         (SMB)
*     27-Mar-1990: TRANSIZE parameter added.  (SMB)
*     20-Apr-1990: Bug fix. TRAN elements were calculated
*                  in the wrong order.        (SMB)
*      3-Aug-1994: Port to Unix               (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'P4COM.INC'
*    Import :
      INTEGER I_START, I_END              ! limits of array on axis1
      INTEGER J_START, J_END              ! limits of array on axis2
      REAL AXIS1 (*)                      ! array containing axis1 values
      REAL AXIS2 (*)                      ! array containing axis2 values
*    Export :
      REAL TRAN( TRANSIZE )               ! TR array for PGGRAY
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER DIFF
*-

      IF (STATUS .NE. SAI__OK) RETURN

*    axis1
      DIFF = I_END - I_START
      IF (DIFF .NE. 0) THEN
         TRAN (2) = (AXIS1(I_END) - AXIS1(I_START)) / FLOAT (DIFF)
         TRAN (3) = 0.0
         TRAN (1) = AXIS1(I_START) - TRAN(2) * FLOAT(I_START)
      ELSE
         TRAN (1) = 0.0
         TRAN (2) = 1.0
         TRAN (3) = 0.0
      ENDIF

*    axis2
      DIFF = J_END - J_START
      IF (DIFF .NE. 0) THEN
         TRAN (5) = 0.0
         TRAN (6) = (AXIS2(J_END) - AXIS2(J_START)) / FLOAT (DIFF)
         TRAN (4) = AXIS2(J_START) - TRAN(6) * FLOAT(J_START)
      ELSE
         TRAN (4) = 0.0
         TRAN (5) = 0.0
         TRAN (6) = 1.0
      ENDIF

      END
