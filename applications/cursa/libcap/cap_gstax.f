      SUBROUTINE CAP_GSTAX (GRIDID, AXIS, LABEL, UNITS, START, INCR,
     :  STATUS)
*+
*  Name:
*     CAP_GSTAX
*  Purpose:
*     Set one of axis of a grid.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_GSTAX (GRIDID, AXIS, LABEL, UNITS, START, INCR; STATUS)
*  Description:
*     Set one of axis of a grid.
*  Arguments:
*     GRIDID  =  INTEGER (Given)
*        Identifier for the grid.
*     AXIS  =  INTEGER (Given)
*        Number of the axis.
*     LABEL  =  CHARACTER*(*) (Given)
*        Axis label.
*     UNITS  =  CHARACTER*(*) (Given)
*        Axis units.
*     START  =  REAL (Given)
*        Axis value corresponding to the first element in the grid.
*     INCR  =  REAL (Given)
*        Axis range corresponding to the increment between successive
*        grid elements.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Map the axis.
*     Compute the axis array.
*     Unmap the axis.
*     Set the label.
*     Set the units.
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     25/6/99 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
      INCLUDE 'CNF_PAR'           ! CNF functions
*  Arguments Given:
      INTEGER
     :  GRIDID,
     :  AXIS
      CHARACTER
     :  LABEL*(*),
     :  UNITS*(*)
      REAL
     :  START,
     :  INCR
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
*  Local Variables:
      INTEGER
     :  AXPTR,   ! Pointer to axis array.
     :  ELEM,    ! Number of elements in the axis array.
     :  LLABEL,  ! Length of LABEL (excl. trail. blanks).
     :  LUNITS   !   "    "  UNITS ( "  .   "  .   "   ).
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Map the axis.

         CALL NDF_AMAP (GRIDID, 'CENTRE', AXIS, '_REAL', 'WRITE',
     :     AXPTR, ELEM, STATUS)
C        print2000, axptr, elem, status
C2000    format(1x, 'axptr, elem, status: ', i20, i5, i10)

*
*       Compute the axis array.

         CALL CAP_GCPAX (START, INCR, ELEM, %VAL(CNF_PVAL(AXPTR)),
     :                   STATUS)

*
*       Unmap the axis.

         CALL NDF_AUNMP (GRIDID, 'CENTRE', AXIS, STATUS)

*
*       Set the label.

         IF (LABEL .NE. ' ') THEN
            LLABEL = CHR_LEN(LABEL)
         ELSE
            LLABEL = 1
         END IF

         CALL NDF_ACPUT (LABEL(1 : LLABEL), GRIDID, 'LABEL', AXIS,
     :     STATUS)

*
*       Set the units.

         IF (UNITS .NE. ' ') THEN
            LUNITS = CHR_LEN(UNITS)
         ELSE
            LUNITS = 1
         END IF

         CALL NDF_ACPUT (UNITS(1 : LUNITS), GRIDID, 'UNITS', AXIS,
     :     STATUS)

      END IF

      END
