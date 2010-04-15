*+  P4_BYTESLICE - copy a slice from a 2-d byte array into a 1-d
      SUBROUTINE P4_BYTESLICE( XDIM, YDIM, IN, CUT, SLICE, OUT, STATUS )
*    Description :
*     This routine extracts a 1-D slice (i.e. a row or column)
*     from a 2-D BYTE array.
*    Invocation :
*     CALL P4_BYTESLICE( XDIM, YDIM, IN, CUT, SLICE, OUT, STATUS )
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
*     18-Feb-1993: Tidy up of code                             (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER XDIM, YDIM                  ! dimensions of data array
      BYTE IN (XDIM, YDIM)                ! 2-D byte array
      CHARACTER*1 CUT                     ! direction of slice, X or Y
      INTEGER SLICE                       ! coordinate of slice
*    Import-Export :
*    Export :
      BYTE OUT (*)                        ! output byte array
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I
*    Internal References :
*    Local data :
*-

*   Return if status on entry is bad
      IF (STATUS .NE. SAI__OK) RETURN

*   Check whether a cut in X or Y is required.
      IF (CUT .EQ. 'X') THEN

*      The cut is in X - a row is to be extracted.
*      Check the slice required lies within the bounds of the array.
         IF ((SLICE.GE.1) .AND. (SLICE.LE.YDIM)) THEN

*         Copy the row to the 1-D array.
            DO I = 1, XDIM

               OUT (I) = IN (I,SLICE)
            END DO
         ENDIF

      ELSE IF (CUT .EQ. 'Y') THEN

*      The cut is in Y - a column is to be extracted.
*      Check the slice required lies within the bounds of the array.
         IF ((SLICE.GE.1) .AND. (SLICE.LE.XDIM)) THEN

*         Copy the column to the 1-D array.
            DO I = 1, YDIM

               OUT (I) = IN (SLICE,I)
            END DO
         ENDIF
      ENDIF

      END
