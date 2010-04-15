      SUBROUTINE SP0RD( COMM, PARAMS, WORV, TITLE, STATUS )
*+
*  Name:
*     SP0RD

*  Purpose:
*     Implements the DIPSO command SP0RD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SP0RD( COMM, PARAMS, WORV, TITLE, STATUS )

*  Description:
*     The SP0RD command reads the contents of an IUE SPECTRUM format 0
*     NDF into the current arrays.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "SP0RD".
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     WORV = REAL (Returned)
*        The "wavelength or velocity" value for the NDF, usually returned
*        set to 1.
*     TITLE = CHARACTER * ( * ) (Returned)
*        The NDF title, or blank if the NDF has no title.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-AUG-1994 (DSB):
*        Original version.
*     13-DEC-1995 (DSB):
*        Remove LOC argument from GETNDF call. This argument returned a
*        primary locator to keep the file open, but the NDF library now
*        looks after it all.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
*        ASIZE1 = INTEGER (Read)
*           The declared size of the X and Y current arrays.

      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        MAXBRK = INTEGER (Read)
*           The declared size of the break current array.
*        BREAK( MAXBRK ) = INTEGER (Write)
*           The pixel indices at which breaks occur in the X and Y arrays.
*        FLUX( ASIZE1 ) = REAL (Write)
*           The data value at each element.
*        NBREAK = INTEGER (Write)
*           The number of breaks stored in the common BREAK array. The
*           first is stored in BREAK(1) and the last in BREAK(NBREAK).
*        NPOINT = INTEGER (Write)
*           The number of data elements used in FLUX and WAVE, starting at
*           element 1.
*        WAVE( ASIZE1 ) = REAL (Write)
*           The X value (usually wavelength or velocity) at the corresponding
*           element in the FLUX array.

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER PARAMS*(*)

*  Arguments Returned:
      REAL WORV
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        INDF,              ! NDF identifier
     :        WORKI( MAXBRK )    ! Work array

      LOGICAL
     :        BADZER             ! Should zeros be treated as bad?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a warning if three or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 2, STATUS )

*  See if zeros in the input data are to be treated as bad values.
      CALL GET0L( PARAMS, 2, .TRUE., COMM, 'Treat zeros as bad values',
     :            .TRUE., BADZER, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get an identifier for the NDF.
      CALL GETNDF( PARAMS, 1, .FALSE., COMM, 'The NDF to be read',
     :             ' ', 'READ', ' ', INDF, STATUS )

*  Tell the user what is about to happen.
      CALL NDF_MSG( 'NDFNAM', INDF )
      CALL MSGOUT( COMM, 'Reading SPECTRUM format 0 data from NDF '//
     :             '''^NDFNAM'' into the current arrays.', .FALSE.,
     :             STATUS )

*  Read the data from the NDF into the current arrays.
      CALL RDNDF( COMM, INDF, ' ', ' ', WORV, TITLE, STATUS )

*  If required remove any pixels with value zero, and add an entry to
*  the break array for each block of contiguous zeros.
      IF( BADZER ) CALL BADCHK( COMM, 0.0, ASIZE1, MAXBRK, NPOINT,
     :                          WAVE, FLUX, NBREAK, BREAK, WORKI,
     :                          STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error has occurred, re-report it with less information if the
*  current message filtering level is not verbose, and set the size of
*  the current array to zero.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL REREP( COMM, 'An error occurred while reading '//
     :                'SPECTRUM format 0 data into the current arrays.',
     :                STATUS )
         NPOINT = 0
      END IF

      END
