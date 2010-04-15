      SUBROUTINE ASCOUT( STATUS )
*+
*  Name:
*     ASCOUT

*  Purpose:
*     Write an NDF to an ASCII table.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASCOUT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine takes an NDF (section) and writes it to an ASCII
*     table. The first part of the output file is a header giving
*     textual information and a head for the table. These lines start
*     with a blank carriage return control character followed by an
*     exclamation mark as the first printed character. The table itself
*     has to the left all the axis values and optionally the pixel
*     widths, and to the right the data value and its error if known.
*     The spectroscopic axis is written with higher precision (12
*     significant digits instead of 7) if its storage type is _DOUBLE.
*     The total number of table columns can be 8 at most. All pixel
*     widths are written if and only if requested, regardless of whether
*     there is explicit information in the input file. Each width
*     occupies the column to the right of the corresponding centre
*     value.

*  Usage:
*     ascout in out

*  ADAM Parameters:
*     WIDTH = _LOGICAL (Read)
*        True if pixel widths are to be written, too. [NO]
*     BAD = _REAL (Read)
*        The alternative bad value. Where the data or variance array has
*        bad values, BAD is written to the ASCII table.
*     IN = NDF (Read)
*        The input NDF.
*     OUT = FILENAME (Read)
*        The ASCII output file.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Examples:
*     ascout in(1.5:2.5) out
*        This expects a 1-D data set in IN and will write to the ASCII
*        file OUT the information for axis values between 1.5 and 2.5.
*        Should IN be more than 1-D, the first hyper-row would be used.
*     ascout in(1.5:2.5,10:15) out
*        This will accept a 2-D data set in IN and write to OUT the
*        information for 1st axis coordinate values between 1.5 and 2.5
*        and for 2nd axis pixel number between 10 and 15. Note that
*        integers in the section specification are interpreted as pixel
*        numbers.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     10 Apr 1991 (hme):
*        Original version.
*     05 Jul 1991 (hme):
*        Use SSETDO instead of own subsetting.
*     19 Sep 1991 (hme):
*        Avoid use of NDF.
*     28 Oct 1991 (hme):
*        Bug fix in ASCWRD: No longer assume variance to exist.
*     20 Nov 1991 (hme):
*        N-D output.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     27 May 1992 (hme):
*        Port to NDF and Unix. Consider the Extension.
*        Initialise labels and units to blank.
*     22 Feb 1994 (hme):
*        Apart from getting the parameters, delegate the action to a
*        linear procdure routine. Also remove the INFO parameter, since
*        it had no effect anyway.
*     23 Feb 1994 (hme):
*        Open file with list format. Remove unnecessary include files.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL LWIDTH
      INTEGER IWIDTH
      INTEGER NDF
      REAL BADVAL
      INTEGER FD
      INTEGER FILENO

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Get parameters.
      CALL PAR_GET0L( 'WIDTH', LWIDTH, STATUS )
      CALL PAR_GET0R( 'BAD',   BADVAL, STATUS )
      CALL NDF_ASSOC( 'IN', 'READ', NDF, STATUS )
      CALL FIO_ASSOC( 'OUT', 'WRITE', 'LIST', 0, FD, STATUS )
      CALL FIO_UNIT( FD, FILENO, STATUS )

*  Call the linear procedure.
      IF ( LWIDTH ) THEN
         IWIDTH = 1
      ELSE
         IWIDTH = 0
      END IF
      CALL SPD_CZYA( IWIDTH, BADVAL, NDF, FILENO, STATUS )

*  Close down.
 500  CONTINUE
      CALL NDF_ANNUL( NDF, STATUS )
      CALL NDF_END( STATUS )
      CALL FIO_CANCL( 'OUT', STATUS )
      CALL FIO_DEACT( STATUS )

      END
