      SUBROUTINE PON_SHODATA( I, NLINES, STATUS )
*+
*    Name:
*       PON_SHODATA

*    Purpose:
*       Display a line of data from PONGO.

*    Language:
*       Starlink Fortran 77

*    Invocation:
*       CALL PON_SHODATA( I, NLINES, STATUS )

*    Description:
*       This routine will display the Ith line of data for the PONGO
*       package. The text is buile using internal write statements and
*       delivered via MSG_OUT.

*    Arguments:
*       I = INTEGER (Given)
*          Display item I in the list.
*       NLINES = INTEGER (Returned)
*          Number of lines output by the routine.
*       STATUS = INTEGER (Given and Returned)
*          The global status.

*    Authors:
*       JBVAD::PAH: Paul Harrison (STARLINK)
*       PCTR: P.C.T. Rees (STARLINK)
*       {enter_new_authors_here}

*    History:
*       6-APR-1990 (JBVAD::PAH):
*          Original version.
*       24-JUN-1992 (PCTR):
*          Code tidy and prologue changes.
*       {enter_further_changes_here}

*    Bugs:
*       {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Arguments Given:
      INTEGER  I

*  Arguments Returned:
      INTEGER NLINES

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER OUTBUF * ( 81 )  ! Output buffer

      INTEGER IOERR              ! Fortran I/O status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the header if I < 1.
      IF ( I .LE. 0 ) THEN
         CALL MSG_SETI( 'NDAT', NDAT )
         CALL MSG_OUT( ' ', '^NDAT data points.', STATUS )
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', ' Number     X          X error        Y' //
     :                 '      Y error      Label       Symbol', STATUS )
         CALL MSG_BLANK( STATUS )

*     Number of header lines output.
         NLINES = 4
      ELSE

*     Write information on the Ith line of data.
         WRITE( OUTBUF, '( X, I4, 2X, 2( E12.5, 2X, E8.2, 3X ), ' //
     :          'A15, 2X, I2 )', IOSTAT=IOERR ) I, XDATA( I ),
     :                                          ERRX( I ), YDATA( I ),
     :                                          ERRY( I ),
     :                                          CLABELS( I )( :15 ),
     :                                          ISYMBS( I )
         CALL MSG_OUT( ' ', OUTBUF, STATUS )
         NLINES = 1
      END IF

      END
* $Id$
