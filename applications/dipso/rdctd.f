      SUBROUTINE RDCTD( COMM, CI, GX, GY, WORV, NAME, STATUS )
*+
*  Name:
*     RDCTD

*  Purpose:
*     Read data from a catalogue into the current arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RDCTD( COMM, CI, GX, GY, WORV, NAME, STATUS )

*  Description:
*     If the catalogue is too large to fit into the common data arrays,
*     a warning is given and as much as there is room for is copied to
*     the X and Y arrays.
*
*     The BREAKS array is assumed to contain a single value equal to the
*     number of rows in the catalogue, and WORV is assumed to be 1.0.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The DIPSO command which invoked this routine.
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue.
*     GX = INTEGER (Given)
*        The CAT identifier for the X column.
*     GY = INTEGER (Given)
*        The CAT identifier for the Y column.
*     WORV = REAL (Returned)
*        The WORV value.
*     NAME = CHARACTER * ( * ) (Returned)
*        The NAME obtained from the catalogue, or a blank if the catalogue
*        has no title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The X and Y labels stored in common are set to the associated
*     column names.
*     -  The size of the current arrays is set to zero if an error occurs.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CAT_PAR'          ! CAT__ constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
*        ASIZE1 = INTEGER (Read)
*           The declared size of the X and Y current arrays.

      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        MAXBRK = INTEGER (Read)
*           The declared size of the break current array.
*        BREAK( MAXBRK ) = INTEGER (Write)
*           The pixel indices at which breaks occur in the X and Y
*           arrays.
*        FLUX( ASIZE1 ) = REAL (Write)
*           The data value at each element.
*        NBREAK = INTEGER (Write)
*           The number of breaks stored in the common BREAK array. The
*           first is stored in BREAK(1) and the last in BREAK(NBREAK).
*        NPOINT = INTEGER (Write)
*           The number of data elements used in FLUX and WAVE, starting
*           at element 1.
*        WAVE( ASIZE1 ) = REAL (Write)
*           The X value (usually wavelength or velocity) at the
*           corresponding element in the FLUX array.

*  Arguments Given:
      CHARACTER COMM*(*)
      INTEGER CI
      INTEGER GX
      INTEGER GY

*  Arguments Returned:
      REAL WORV
      CHARACTER NAME*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        ROW,               ! Row count
     :        WORKI( MAXBRK )    ! Work array

      LOGICAL
     :        NULL               ! Was no value available?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of rows in the catalogue.
      CALL CAT_TROWS( CI, NPOINT, STATUS )

*  Issue a warning if the catalogue is too big, and restrict the
*  number of elements transferred.
      IF( NPOINT .GT. ASIZE1 ) THEN
         CALL MSG_SETI( 'NP', NPOINT )
         CALL MSG_SETI( 'AS', ASIZE1)
         CALL MSGOUT( COMM, 'The catalogue has ^NP rows. '//
     :                'The current arrays can only hold ^AS elements.',
     :                .TRUE., STATUS )
         NPOINT = ASIZE1
      END IF

*  Assume no breaks, and WORV = 1.0
      NBREAK = 1
      BREAK( 1 ) = NPOINT
      WORV = 1.0

*  Copy the data into the common array, storing VAL__BADR for missing
*  values.
      IF( STATUS .EQ. SAI__OK ) THEN
         DO ROW = 1, NPOINT
            CALL CAT_RGET( CI, ROW, STATUS )

            CALL CAT_EGT0R( GX, WAVE( ROW ), NULL, STATUS )
            IF( NULL ) WAVE( ROW ) = VAL__BADR

            CALL CAT_EGT0R( GY, FLUX( ROW ), NULL, STATUS )
            IF( NULL ) FLUX( ROW ) = VAL__BADR

         END DO
      END IF

*  Return the catalogue name, ensuring that a blank string gets returned if
*  the title is undefined.
      NAME = ' '
      CALL CAT_TIQAC( CI, 'NAME', NAME, STATUS )

*  Tell the user how many elements were transferred.
      CALL MSG_SETI( 'NP', NPOINT )
      CALL MSGOUT( COMM, '^NP data values read.', .FALSE., STATUS )

*  Call a routine which will remove any "missing" (i.e. bad) values,
*  storing suitable breaks.
      CALL BADCHK( COMM, VAL__BADR, ASIZE1, MAXBRK, NPOINT, WAVE, FLUX,
     :             NBREAK, BREAK, WORKI, STATUS )

*  Jump to here if an error occurs.
 999  CONTINUE

*  If an error has occurred, set the current array size to zero.
      IF( STATUS .NE. SAI__OK ) NPOINT = 0

      END
