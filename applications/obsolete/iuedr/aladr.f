      SUBROUTINE ALADR( TYPE, NWORD, ADR, STATUS )
*+
*  Name:
*     SUBROUTINE ALADR

*  Purpose:
*     Allocate memory block.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ALADR( TYPE, NWORD, ADR, STATUS )

*  Arguments:
*     TYPE = BYTE ( MAXTYPE ) (Given)
*        Data type to be allocated for.
*     NWORD = INTEGER (Given)
*        Number of items of given TYPE to be allocated for.
*     ADR = INTEGER (Returned)
*        Pointer to base address of allocated memory.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Get an address pointer to NBYTE bytes of virtual memory.
*     Keep the name and address range in a table so that
*     freeing this memory is easy.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     31-DEC-81 (JRG):
*       AT4 version.
*     20-OCT-88 (PCTR):
*       IUEDR Vn. 2.0.  Conversion to FORTRAN.
*     11-MAY-89 (PCTR):
*       IUEDR Vn. 2.1.
*       Some restructuring and final conversion to SGP/16 style.
*     30-SEP-94 (MJC):
*       IUEDR Vn. 3.1-6
*     16-DEC-94 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMDYN'

*  Local Constants:
      INTEGER MAXADR   ! Maximum number of memory blocks.
      INTEGER MAXTYPE  ! Maximum length of data type string.
      PARAMETER ( MAXADR = 32, MAXTYPE = 16 )

*  Status:
      INTEGER STATUS   ! Global status.

*  Arguments Given:
      BYTE TYPE( MAXTYPE ) ! Data type (Fortran only).

      INTEGER NWORD    ! Size of array in words of given TYPE.

*  Arguments Returned:
      INTEGER ADR      ! Pointer to base address.

*  Local Variables:
      INTEGER BYTES    ! Number of bytes per word of TYPE.
      INTEGER ISTAT    ! Local status.
      INTEGER I        ! Loop index.
      INTEGER ISLOT    ! Slot number in table.
      INTEGER NBYTE    ! Number of bytes.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find an empty slot.
      ISLOT = 0
      DO I = 1, MAXADR
         IF ( DFREE( I ) ) THEN
            ISLOT = I
            GO TO 100
         END IF
      END DO
 100  CONTINUE

*  Check that there is room in address table.
      IF ( ISLOT .EQ. 0 ) THEN
         CALL ERROUT( 'Error: no memory slots available\\', STATUS )
         GO TO 999
      END IF


*  Get size of TYPE and hence number of bytes required for array.
      CALL SZTYPE( TYPE, BYTES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: storage type unknown\\', STATUS )
      ELSE
         NBYTE = BYTES * NWORD

*     Fill it.
         ISTAT = SAI__OK
         CALL PSX_MALLOC( NBYTE, DBASE( ISLOT ), ISTAT )
         IF ( ISTAT .EQ. SAI__OK ) THEN
            DSIZE( ISLOT ) = NBYTE
            DFREE( ISLOT ) = .FALSE.
            ADR = DBASE( ISLOT )

         ELSE
            ADR = 0
            CALL ERROUT( 'Error: allocating memory\\', STATUS )
         END IF
      END IF

 999  CONTINUE

      END
