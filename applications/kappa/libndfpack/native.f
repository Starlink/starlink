      SUBROUTINE NATIVE( STATUS )
*+
*  Name:
*     NATIVE

*  Purpose:
*     Converts an HDS object to native machine data representation.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL NATIVE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application converts an HDS object (or structure) so that
*     all primitive data values within it are represented using the
*     appropriate native data representation for the machine in use
*     (this includes the appropriate number format and byte ordering).
*     This may typically be required after moving HDS files from
*     another machine which uses a different number format and/or byte
*     order, and will minimise the subsequent access time on the new
*     machine.  Conversion is performed by modifying the data in situ.
*     No separate output file is produced.
*
*     This application can also be used to replace any IEEE floating point 
*     NaN or Inf values in an HDS object with the appropriate Starlink 
*     bad value. This conversion is performed even if the data values
*     within the object are already represented using the appropriate
*     native data representation for the machine in use.

*  Usage:
*     native object

*  ADAM Parameters:
*     OBJECT = UNIVERSAL (Read and Write)
*        The HDS structure to be converted; either an entire container
*        file or a particular object or structure within the file may
*        be specified.  If a structure is given, all components (and
*        sub-components, etc.) within it will also be converted.

*  Examples:
*     native myfile
*        Converts all the primitive data in the HDS container file
*        myfile to be held using the appropriate native machine
*        representation for faster subsequent access.
*     native yourfile.data_array
*        Converts just the DATA_ARRAY component (and its contents, if a
*        structure) in the container file yourfile to the appropriate
*        native machine data representation.  Other file contents remain
*        unchanged.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-MAY-1992 (RFWS):
*        Original version.
*     1995 April 24 (MJC):
*        Made usage and examples lowercase.  Sorted the variable
*        declarations.
*     12-OCT-1998 (DSB):
*        Added prologue comment about conversion of NaN and Inf values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) LOC ! Structure locator

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the HDS structure to be converted.
      CALL DAT_ASSOC( 'OBJECT', 'UPDATE', LOC, STATUS )

*  Perform the conversion and annul the locator.
      CALL KPG1_NACVT( LOC, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NATIVE_ERR',
     :     'NATIVE: Error converting an HDS object to native machine '//
     :     'data representation.', STATUS )
      END IF

      END
