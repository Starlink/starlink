      SUBROUTINE POL1_CLCAT( IWCS, CI, STATUS )
*+
*  Name:
*     POL1_MKCAT

*  Purpose:
*     Write coordinate information to a catalogue and close it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CLCAT( IWCS, CI, STATUS )

*  Description:
*     This routine write the supplied AST FrameSet in the textual 
*     information associated with the supplied catalogue, and then closes
*     the catalogue. NB, at the moment, the CAT library reports errors if
*     you try to store textual information in a FITS file before any rows 
*     have been written to the catalogue. For this reason, the WCS
*     information is stored after all rows have been written, just before
*     the catalogue is closed.

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST pointer to a FrameSet. This should have been obtained using 
*        the NDF_GTWCS subroutine.
*     CI = INTEGER (Given and Returned)
*        A CAT identifier for the catalogue. Returned equal to CAT__NOID.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-JAN-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER IWCS

*  Arguments Given and Returned:
      INTEGER CI

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL POL1_SINK

*  Global Variables:
      INTEGER CCI                ! Catalogue identifier passed to POL1_SINK
      COMMON /POLSINK/ CCI      

*  Local Variables:
      INTEGER CHAN               ! Pointer to an AST Channel
*.

*  Copy the supplied FrameSet into the textual information associated
*  with the catalogue (if supplied), and if no error has already occurred ...
      IF( STATUS .EQ. SAI__OK .AND. IWCS .NE. AST__NULL ) THEN

*  Add a header to the textual information.
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '>>>>> Coordinate system '//
     :                   'information follows, stored ', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '>>>>> as an AST FrameSet '//
     :                   '(see Starlink User Note 210).', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '>>>>> The coordinates '//
     :                   'stored in columns X and Y of the', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', '>>>>> table refer to the '//
     :                   'PIXEL Frame within this FrameSet.', STATUS )
         CALL CAT_PUTXT( CI, 'COMMENT', ' ', STATUS )

*  Create an AST Channel. AST provides the POL1_SINK function with strings
*  to be written out, and POL1_SINK stores these strings in the textual
*  information associated with the catalogue. POL1_SINK is attached to
*  the end of this file.
         CHAN = AST_CHANNEL( AST_NULL, POL1_SINK, 'FULL=-1,COMMENT=0',
     :                       STATUS )

*  Pass the catalogue identifier to POL1_SINK using the common block
*  /POLSINK/.
         CCI = CI 

*  Write the supplied FrameSet to the Channel. If the FrameSet cannot
*  be written (which shouldn't happen), report an error and immediately
*  flush it so that we can carry on.
         IF( AST_WRITE( CHAN, IWCS, STATUS ) .EQ. 0 ) THEN
            IF( STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ', 'Failed to write World Coordinate '//
     :                    'System information to the output catalogue.',
     :                    STATUS )
               CALL ERR_FLUSH( STATUS )
            END IF
         END IF

*  Annul the AST Channel.
         CALL AST_ANNUL( CHAN, STATUS )

      END IF

*  Release the catalogue.
      CALL CAT_TRLSE( CI, STATUS )

      END


      SUBROUTINE POL1_SINK( STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR' 
      INTEGER STATUS, L, CI
      CHARACTER LINE*80
      COMMON /POLSINK/ CI      

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL AST_GETLINE( LINE, L, STATUS ) 
      CALL CAT_PUTXT( CI, 'COMMENT', LINE( : L ), STATUS )

      END 
