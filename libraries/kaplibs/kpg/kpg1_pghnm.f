      SUBROUTINE KPG1_PGHNM( NAME, STATUS )
*+
*  Name:
*     KPG1_PGHNM

*  Purpose:
*     Return an HDS name describing the current PGPLOT graphics device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGHNM( NAME, STATUS )

*  Description:
*     This routine returns a string which can be used as an HDS component
*     name. The string describes the current PGPLOT graphics device. It is 
*     intended for use in identifying resources related to the graphics 
*     device (such as palette and colour table), stored within HDS files.
*     
*     For most devices, the returned name is simply the PGPLOT device type.
*     For GWM windows, the returned string includes the name of the gwm
*     window (so that each window can have separate resources).

*  Arguments:
*     NAME = CHARACTER * ( DAT__SZNAM ) (Returned)
*        The returned string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A graphics device must previously have been opened using PGPLOT.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-OCT-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Returned:
      CHARACTER NAME*(DAT__SZNAM)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DEV*(DAT__SZNAM) ! Device name
      INTEGER LENGTH             ! Length of string returned by PGQINF
      INTEGER NC                 ! Number of characters in the buffer

*.

*  Initialize.
      NAME = ' '

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the workstation type, remove any blanks, convert to upper case.
      CALL PGQINF( 'TYPE', NAME, NC )
      CALL CHR_RMBLK( NAME )
      CALL CHR_UCASE( NAME )

*  If the device is a GWM window add the window name.
      IF( NAME .EQ. 'GWM' .OR. NAME .EQ. '3800' ) THEN  
         CALL PGQINF( 'DEVICE', DEV, LENGTH )
         CALL CHR_RMBLK( DEV )
         CALL CHR_UCASE( DEV )
         CALL CHR_APPND( '_', NAME, NC )
         CALL CHR_APPND( DEV, NAME, NC )
      END IF

*  Clear any grot added by PGPLOT.
      CALL CHR_CLEAN( NAME )

      END
