*+  P4_SLICE - copy a slice from a 2-d array into a 1-d
      SUBROUTINE P4_SLICE (XDIM, YDIM, IN, CUT, SLICE, OUT, STATUS)
*    Description :
*     This routine extracts a 1-D slice (i.e. either a row or a
*     column) from a 2-D REAL array.
*    Invocation :
*     CALL P4_SLICE (XDIM, YDIM, IN, CUT, SLICE, OUT, STATUS)
*    Parameters :
*    Method :
*    Deficiencies :
*     The size of the OUT array should really be passed in,
*     since it cannot be checked here.
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*          1989 ?: Original version.                           (JFL)
*      1-Nov-1989: History added. Dummy array size for OUT
*                  changed from (1) to (*) and "deficiencies"
*                  added.                                      (SMB)
*      2-Nov-1989: Order of arguments rearranged.              (SMB)
*     28-Aug-1990: Description added. Code spaced out and more
*                  comments added.                             (SMB)
*     18-Feb-1993: Tidy code                                   (PND)
*      4-Aug-1994: Convert to I-task for Unix port             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER XDIM, YDIM                  ! dimensions of data array
      REAL IN (XDIM, YDIM)                ! 2-D data array
      CHARACTER*1 CUT                     ! direction of slice, X or Y
      INTEGER SLICE                       ! coordinate of slice
*    Export :
      REAL OUT (*)                        ! output data array
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER I
*-

*   Return if status on entry is bad
      IF (STATUS .NE. SAI__OK) RETURN

*   Check whether a cut in X or Y is required
      IF (CUT .EQ. 'X') THEN

*      The cut is in X - a row is to be extracted.
*      Check that the slice is not outside the bounds of the array.
         IF ((SLICE.GE.1) .AND. (SLICE.LE.YDIM)) THEN

*         Copy the row to the 1-D array.
            DO I = 1, XDIM

               OUT (I) = IN (I,SLICE)
            END DO
         ENDIF
      ELSE IF (CUT .EQ. 'Y') THEN

*      The cut is in Y - a column is to be extracted.
*      Check that the slice is not outside the bounds of the array.
         IF ((SLICE.GE.1) .AND. (SLICE.LE.XDIM)) THEN

*         Copy the column to the 1-D array.
            DO I = 1, YDIM

               OUT (I) = IN (SLICE,I)
            END DO
         ENDIF
      ENDIF

      END
