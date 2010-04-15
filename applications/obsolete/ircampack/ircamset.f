      SUBROUTINE IRCAMSET( STATUS )
*+
*  Name:
*     IRCAMSET

*  Purpose:
*     Set or display the IRCAMPACK global parameters

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRCAMSET( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays the current values of the IRCAMPACK globals
*     parameters, and optionally sets up new values. It is intended for
*     use within procedures. The value of each parameter is displayed
*     on a separate line without any extra text, in the order listed
*     under "Usage". If the parameter does not currently have a valid
*     value, the string "undefined" is displayed. The display may be
*     logged to a text file.

*  Usage:
*     IRCAMSET IN1 IN2 IN3 IN4 I P THETA FWHM BOX ANGROT
*              MAXPERR MAXTERR

*  ADAM Parameters:
*     IN1 = NDF (Read)
*        The NDF holding the current 0 degrees waveplate position
*        intensity image.  [!]
*     IN2 = NDF (Read)
*        The NDF holding the current 45 degrees waveplate position
*        intensity image.  [!]
*     IN3 = NDF (Read)
*        The NDF holding the current 22.5 degrees waveplate position
*        intensity image.  [!]
*     IN4 = NDF (Read)
*        The NDF holding the current 67.5 degrees waveplate position
*        intensity image.  [!]
*     I = NDF (Read)
*        The NDF holding the current total intensity image. [!]
*     P = NDF (Read)
*        The NDF holding the current percentage polarisation image. [!]
*     THETA = NDF (Read)
*        The NDF holding the polarisation angle image. [!]
*     FWHM = LITERAL (Read)
*        The current FWHM of the PSF used by procedure POLSMOOTH. [!]
*     BOX = LITERAL (Read)
*        The current box size used by procedure POLSMOOTH. [!]
*     ANGROT = LITERAL (Read)
*        The current value of the rotation to be apply to vectors before
*        being displayed by procedures POLMAPD and POLMAPC. [!]
*     MAXPERR = LITERAL (Read)
*        The current value of the maximum acceptable error in percentage
*        polarisation used by procedure POLCAL. [!]
*     MAXTERR = LITERAL (Read)
*        The current value of the maximum acceptable error in
*        polarisation angle used by procedure POLCAL. [!]
*     LOGFILE = FILENAME (Write)
*        The name of a file to which the current global parameter values
*        will be written. [!]

*  Notes:
*     -  If a null value is supplied for any parameter (except LOGFILE)
*     the current value of the corresponding global parameter is
*     displayed but no new value is assigned.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER FD                 ! Log file descriptor
      LOGICAL LOG                ! Is a log file required?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Defer error reporting.
      CALL ERR_MARK

*  Open a log file if necessary.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'NONE', 0, FD, STATUS )

*  Annul the error if a null value was given.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         LOG = .FALSE.
      ELSE
         LOG = .TRUE.
      END IF

*  Start an NDF context.
      CALL NDF_BEGIN

*  Get the NDFs for the four waveplate positions.
      CALL ISTNDF( 'IN1', LOG, FD, STATUS )
      CALL ISTNDF( 'IN2', LOG, FD, STATUS )
      CALL ISTNDF( 'IN3', LOG, FD, STATUS )
      CALL ISTNDF( 'IN4', LOG, FD, STATUS )

*  Now get the total intensity, percentage polarisation and polarisation
*  angle NDFs.
      CALL ISTNDF( 'I', LOG, FD, STATUS )
      CALL ISTNDF( 'P', LOG, FD, STATUS )
      CALL ISTNDF( 'THETA', LOG, FD, STATUS )

*  Now get the parameters for the Gaussian smoothing performed by
*  POLSMOOTH.
      CALL ISTSTR( 'FWHM', LOG, FD, STATUS )
      CALL ISTSTR( 'BOX', LOG, FD, STATUS )

*  Now get the angle to rotate vectors by when displaying a vector map.
      CALL ISTSTR( 'ANGROT', LOG, FD, STATUS )

*  Now get error limits used by POLCAL
      CALL ISTSTR( 'MAXPERR', LOG, FD, STATUS )
      CALL ISTSTR( 'MAXTERR', LOG, FD, STATUS )

*  If necessary, close the log file.
      IF( LOG ) CALL FIO_CLOSE( FD, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRCAMSET_ERR', 'IRCAMSET: Error setting '//
     :                 'global IRCAM parameters', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
