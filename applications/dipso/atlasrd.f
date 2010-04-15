      SUBROUTINE ATLASRD( COMM, SPECDAT, PARAMS, WORV, TITLE, STATUS )
*+
*  Name:
*     ATLASRD

*  Purpose:
*     Implements the DIPSO command ATLASRD.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATLASRD( COMM, SPECDAT, PARAMS, WORV, TITLE, STATUS )

*  Description:
*     The ATLASRD command reads the contents of an NDF holding Karucz
*     model atmosphere fluxes into the current arrays. The NDF name is
*     derived from the supplied parameters, and is expected to be in
*     the $SPECDAT directory. If the NDF cannot be found there it is
*     looked for in the current directory. The flux values stored in
*     the NDFs are multiplied by PI.

*  Arguments:
*     COMM = CHARACTER * ( * ) (Given)
*        The name of the command which invoked this routine. This will
*        usually be "ATLASRD".
*     SPECDAT = CHARACTER * ( * ) (Given)
*        Path to SPECDAT directory.
*     PARAMS = CHARACTER * ( * ) (Given)
*        Any text supplied by the user on the command line following the
*        command name.
*     WORV = REAL (Returned)
*        The WORV value from the NDF (usually 1).
*     TITLE = CHARACTER * ( * ) (Returned)
*        The NDF title, or blank if the title is undefined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If an error occurs, then the size of the current arrays is set
*     to zero.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     23-AUG-1994 (DSB):
*        Original version.
*     13-DEC-1995 (DSB):
*        Modified to use NDF_OPEN instead of HDS_OPEN
*     2-APR-1997 (DSB):
*        SPECDAT argument added.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_ERR'          ! DAT__ error constants
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Global Variables:
      INCLUDE 'DECLARE_STKS'     ! DIPSO array sizes, etc.
*        ASIZE1 = INTEGER (Read)
*           The declared size of the X and Y current arrays.

      INCLUDE 'DECLARE_DATA'     ! DIPSO current arrays
*        FLUX( ASIZE1 ) = REAL (Write)
*           The data value at each element.
*        NPOINT = INTEGER (Write)
*           The number of data elements used in FLUX and WAVE, starting at
*           element 1.

*  Arguments Given:
      CHARACTER COMM*(*)
      CHARACTER SPECDAT*(*)
      CHARACTER PARAMS*(*)

*  Arguments Returned:
      REAL WORV
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :        FILE*255,          ! File name
     :        NAME*255,          ! Full NDF name
     :        NDFNM*255          ! Root NDF name

      INTEGER
     :        I,                 ! Element count
     :        INDF,              ! NDF identifier
     :        ILOGG,             ! Value of "LogG" parameter * 100
     :        MODE,              ! Value of "Mode" parameter
     :        NLEN,              ! Index of last non-blank character
     :        PLACE,             ! A dummy place holder (NDF__NOPL)
     :        TEFF               ! Value of "Teff" parameter

      LOGICAL
     :        THERE              ! Has a suitable file been found?

      REAL
     :        LOGG               ! Supplied value of "LogG" parameter

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Give a warning if four or more parameters were supplied.
      CALL EXPAR( COMM, PARAMS, 3, STATUS )

*  Obtain the MODE value, and set up some default values.
      CALL GET0I( PARAMS, 3, .TRUE., COMM, 'Mode', 0.0, MODE, STATUS )
      IF( MODE .EQ. 0 ) THEN
         TEFF = 3500
         LOGG = 5.0
      ELSE
         TEFF = 10000
         LOGG = 3.5
      END IF

*  Obtain the Teff value.
      CALL GET0I( PARAMS, 1, .FALSE., COMM, 'Teff', TEFF, TEFF,
     :            STATUS )

*  Obtain the LogG value. Multiple it by 100 to get the integer value
*  used in the NDF name.
      CALL GET0R( PARAMS, 2, .FALSE., COMM, 'LogG', LOGG, LOGG,
     :            STATUS )
      ILOGG = NINT( 100.0*LOGG )

*  Construct the NDF name; "f" or "klo" depending on MODE, followed by
*  the TEFF value, an underscore, and the LogG value.
      IF( MODE .EQ. 0 ) THEN
         NDFNM = 'f'
         NLEN = 1
      ELSE
         NDFNM = 'klo'
         NLEN = 3
      END IF

      CALL CHR_PUTI( TEFF, NDFNM, NLEN )
      CALL CHR_APPND( '_', NDFNM, NLEN )
      WRITE( NDFNM( NLEN + 1 : ),'(I3.3)') ILOGG
      NLEN = NLEN + 3

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Indicate that a suitable file has not yet been found.
      THERE = .FALSE.

*  If a translation was obtained, see if the file exists in $SPECDAT.
      IF( SPECDAT .NE. ' ' ) THEN
         FILE = SPECDAT//NDFNM( : NLEN )//'.sdf'
         INQUIRE( FILE=FILE, EXIST=THERE )

*  If the file could not be found, and if mode 0 is being used, try to
*  find an alternative NDF which starts with the letter K instead of F
*  (again in the $SPECDAT directory).
         IF( .NOT. THERE .AND. MODE .EQ. 0 ) THEN
            NDFNM( 1 : 1 ) = 'k'
            FILE = SPECDAT//NDFNM( : NLEN )//'.sdf'
            INQUIRE( FILE=FILE, EXIST=THERE )
            NDFNM( 1 : 1 ) = 'f'
         END IF

      END IF

*  If the NDF could not be found in $SPECDAT, look for it in the current
*  directory.
      IF( .NOT. THERE ) THEN

         FILE = NDFNM( : NLEN )//'.sdf'
         INQUIRE( FILE=FILE, EXIST=THERE )

*  If the file could not be found, and if mode 0 is being used, try to
*  find an alternative NDF which starts with the letter K instead of F.
         IF( .NOT. THERE .AND. MODE .EQ. 0 ) THEN
            NDFNM( 1 : 1 ) = 'k'
            FILE = NDFNM( : NLEN )//'.sdf'
            INQUIRE( FILE=FILE, EXIST=THERE )
            NDFNM( 1 : 1 ) = 'f'
         END IF

      END IF

*  If the file could not be found, report an error, and abort.
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'NDF', NDFNM )
         CALL MSG_SETI( 'TEFF', TEFF )
         CALL MSG_SETR( 'LOGG', LOGG )
         CALL ERR_REP( 'ATLASRD_ERR1', 'No NDF (''^NDF'') could'//
     :                 ' be found for Teff=^TEFF and LogG=^LOGG',
     :                 STATUS )
         GO TO 999
      END IF

*  Strip the file type from the file name.
      CALL NDFNAM( FILE, ' ', NAME, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Get an identifier for the NDF.
      CALL NDF_OPEN( DAT__ROOT, NAME, 'READ', 'OLD', INDF, PLACE,
     :               STATUS )

*  Tell the user what is about to happen.
      CALL NDF_MSG( 'NDFNAM', INDF )
      CALL MSGOUT( COMM, 'Reading Karucz model atmosphere fluxes from'//
     :             ' NDF ''^NDFNAM'' into the current arrays.', .FALSE.,
     :             STATUS )

*  Read the data from the NDF into the current arrays.
      CALL RDNDF( COMM, INDF, ' ', ' ', WORV, TITLE, STATUS )

*  If succesful, multiply all the flux values by PI.
      IF( STATUS .EQ. SAI__OK ) THEN

         DO I = 1, NPOINT
            FLUX( I ) = FLUX( I )*3.141593
         END DO

      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Jump to here if an error occurs.
 999  CONTINUE

*  If an error has occurred, re-report it with less information unless
*  the current message filtering level is verbose. Also, set the size of
*  the current arrays to zero.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL REREP( COMM, 'An error occurred while reading Karucz '//
     :               'model atmosphere fluxes into the current '//
     :               'arrays.', STATUS )
         NPOINT = 0
      END IF

      END
