      SUBROUTINE ECH_ZERO_INIT( COUNT, SIZE, ARRAY )
*+
*  Name:
*     ECHOMOP - ECH_ZERO_INIT

*  Purpose:
*     Zero area of memory.

*  Description:
*     This routine clears memory areas to zeroes.

*  Invocation:
*     CALL ECH_ZERO_INIT( COUNT, SIZE, ARRAY )

*  Arguments:
*     COUNT = INTEGER (Given)
*        Number of elements in array.
*     SIZE = INTEGER (Given)
*        Size in bytes of each element.
*     ARRAY = BYTE (Returned)
*        Array to zero-out.

*  Authors:
*     DMILLS: Dave Mills (UCL, Starlink)
*     MJC: Martin Clayton (Starlink, UCL)
*     {enter_new_authors_here}

*  History:
*     01-SEP-1992 (DMILLS):
*       Initial release.
*     23-JUL-1996 (MJC):
*       Switch on size of element - use unrolled loops.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'ECH_REPORT.INC'

*  Arguments Given:
      INTEGER SIZE
      INTEGER COUNT

*  Arguments Returned:
      BYTE ARRAY( * )
*.

*  Report routine entry if enabled.
      IF ( IAND( REPORT_MODE, RPM_FULL + RPM_CALLS ) .GT. 0 )
     :   CALL ECH_REPORT( REPORT_MODE, ECH__MOD_ENTRY )

*  Call worker subroutine.
      IF ( SIZE .EQ. 1 ) THEN
         CALL ECH_ZERO_BYTE( COUNT, ARRAY )

      ELSE IF ( SIZE .EQ. 2 ) THEN
         CALL ECH_ZERO_WORD( COUNT, ARRAY )

      ELSE IF ( SIZE .EQ. 4 ) THEN
         CALL ECH_ZERO_REAL( COUNT, ARRAY )

      ELSE IF ( SIZE .EQ. 8 ) THEN
         CALL ECH_ZERO_DBLE( COUNT, ARRAY )

*  Hanlde unknown size.
      ELSE
         CALL ECH_ZERO_BYTE( SIZE * COUNT, ARRAY )
      END IF

      END

      SUBROUTINE ECH_ZERO_REAL( COUNT, ARRAY )

      IMPLICIT NONE
      INTEGER COUNT
      INTEGER ARRAY( COUNT )

      INTEGER I

      DO I = COUNT, 1, -1
         ARRAY( I ) = 0
      END DO

      END


      SUBROUTINE ECH_ZERO_BYTE( COUNT, ARRAY )

      IMPLICIT NONE
      INTEGER COUNT
      BYTE ARRAY( COUNT )

      INTEGER I

      I = COUNT
      GO TO ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ),
     :       MOD( COUNT, 16 )
      DO WHILE ( I .GT. 0 )
         ARRAY( I ) = 0
         I = I - 1
   15    ARRAY( I ) = 0
         I = I - 1
   14    ARRAY( I ) = 0
         I = I - 1
   13    ARRAY( I ) = 0
         I = I - 1
   12    ARRAY( I ) = 0
         I = I - 1
   11    ARRAY( I ) = 0
         I = I - 1
   10    ARRAY( I ) = 0
         I = I - 1
    9    ARRAY( I ) = 0
         I = I - 1
    8    ARRAY( I ) = 0
         I = I - 1
    7    ARRAY( I ) = 0
         I = I - 1
    6    ARRAY( I ) = 0
         I = I - 1
    5    ARRAY( I ) = 0
         I = I - 1
    4    ARRAY( I ) = 0
         I = I - 1
    3    ARRAY( I ) = 0
         I = I - 1
    2    ARRAY( I ) = 0
         I = I - 1
    1    ARRAY( I ) = 0
         I = I - 1
      END DO

      END

      SUBROUTINE ECH_ZERO_WORD( COUNT, ARRAY )

      IMPLICIT NONE
      INTEGER COUNT
      INTEGER*2 ARRAY( COUNT )

      INTEGER I

      I = COUNT
      GO TO ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15 ),
     :       MOD( COUNT, 16 )
      DO WHILE ( I .GT. 0 )
         ARRAY( I ) = 0
         I = I - 1
   15    ARRAY( I ) = 0
         I = I - 1
   14    ARRAY( I ) = 0
         I = I - 1
   13    ARRAY( I ) = 0
         I = I - 1
   12    ARRAY( I ) = 0
         I = I - 1
   11    ARRAY( I ) = 0
         I = I - 1
   10    ARRAY( I ) = 0
         I = I - 1
    9    ARRAY( I ) = 0
         I = I - 1
    8    ARRAY( I ) = 0
         I = I - 1
    7    ARRAY( I ) = 0
         I = I - 1
    6    ARRAY( I ) = 0
         I = I - 1
    5    ARRAY( I ) = 0
         I = I - 1
    4    ARRAY( I ) = 0
         I = I - 1
    3    ARRAY( I ) = 0
         I = I - 1
    2    ARRAY( I ) = 0
         I = I - 1
    1    ARRAY( I ) = 0
         I = I - 1
      END DO

      END
