      SUBROUTINE KPG1_RDLS2( IGRP, NPOS, NAX, POS, ID, STATUS )
*+
*  Name:
*     KPG1_RDLS2

*  Purpose:
*     Reads a set of positions from a GRP group.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_RDLS2( IGRP, NPOS, NAX, POS, ID, STATUS )

*  Description:
*     This routine reads a set of positions from a GRP group. Each
*     element in the group should consist of a set of numerical fields
*     separated by white space. The first field is assumed to contain 
*     an integer position identifier which is returned in ID. NAX axis 
*     values should follow this position identifier.

*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group.
*     NPOS = INTEGER (Given)
*        The maximum number of positions to be returned.
*     NAX = INTEGER (Given)
*        The number of axis values required for each position.
*     POS( NPOS, NAX ) = DOUBLE PRECISION (Returned)
*        The positions read from the group.
*     ID( NPOS ) = INTEGER (Returned)
*        The position identifiers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1998 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER IGRP
      INTEGER NPOS
      INTEGER NAX
    
*  Arguments Returned:
      DOUBLE PRECISION POS( NPOS, NAX )
      INTEGER ID( NPOS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TEXT*( GRP__SZNAM )   ! Text of current element
      CHARACTER WORDS( NDF__MXDIM )*30! Words extracted from current element
      INTEGER I                  ! Element index
      INTEGER ICOL               ! Index of last column read
      INTEGER J                  ! Axis index
      INTEGER LSTAT              ! CHR status
      INTEGER NCOL               ! No. of fields required in each element
      INTEGER NWRD               ! No. of words in current element
      INTEGER SIZE               ! No. of elements in group
      INTEGER WSTART( NDF__MXDIM )    ! Index of start of each word
      INTEGER WSTOP( NDF__MXDIM )! Index of end of each word
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the number of columns required in the group.
      NCOL = NAX + 1

*  Get the size of the group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )

*  Loop round each element in the group.
      DO I = 1, MIN( SIZE, NPOS )
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS ) 

*  Remove leading spaces, and convert to upper case.
         CALL CHR_LDBLK( TEXT )          
         CALL CHR_UCASE( TEXT )          

*  Skip blank lines, and comment lines.
         IF( TEXT .NE. ' ' .AND. TEXT( 1 : 1 ) .NE. '#' ) THEN

*  Split the element up into words delimited by spaces.
            LSTAT = STATUS
            CALL CHR_DCWRD( TEXT, NDF__MXDIM, NWRD, WSTART, WSTOP, 
     :                      WORDS, LSTAT ) 

*  Read the position identifier from the first field.
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL CHR_CTOI( WORDS( 1 ), ID( I ), STATUS )
               ICOL = 1

*  Report an error if the field was not an integer.
               IF( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'W', WORDS( 1 ) )
                  CALL MSG_SETC( 'LINE', TEXT )
                  CALL ERR_REP( 'KPG1_RDLS2_ERR1', 'The first field '//
     :                          'in the following line (^W) is not an'//
     :                          ' integer position identifier: ^LINE', 
     :                          STATUS )
                  GO TO 999
               END IF

*  Use the element index as the position identifier if no identifiers are
*  included in the group.
            ELSE
               ID( I ) = I
               ICOL = 0
            END IF

*  Report an error if this element has too many or too few fields. 
            IF( NWRD .NE. NCOL .AND. STATUS .EQ. SAI__OK ) THEN
               STATUS = SAI__ERROR

               CALL MSG_SETI( 'I', ID( I ) )
               CALL MSG_SETI( 'NW', NWRD )
               CALL MSG_SETI( 'NC', NCOL )
               CALL MSG_SETC( 'LINE', TEXT )

               CALL ERR_REP( 'KPG1_RDLS2_ERR2', 'Incorrect number of '//
     :                    'axis values (^NW) found for position #^I '//
     :                    '(^NC are required): ^LINE', STATUS )

               GO TO 999

            END IF

*  Loop round each axis value.
            DO J = 1, NAX

*  Find the corresponding column  index.
               ICOL = ICOL + 1

*  If this word contains the word "BAD" return a bad value.
               IF( INDEX( WORDS( ICOL ), 'BAD' ) .NE. 0 ) THEN
                  POS( I, J ) = AST__BAD

*  Otherwise, attempt to read an double precision value from the current word.
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  CALL CHR_CTOD( WORDS( ICOL ), POS( I, J ), STATUS ) 

*  If the word could not be decoded, report an error.
                  IF( STATUS .NE. SAI__OK ) THEN
                     CALL MSG_SETI( 'I', ID )
                     CALL MSG_SETI( 'J', J )
                     CALL MSG_SETC( 'W', WORDS( ICOL ) )
                     CALL MSG_SETC( 'LINE', TEXT )

                     CALL ERR_REP( 'KPG1_RDLS2_ERR3', 'Cannot read a '//
     :                    'numerical value from string ''^W'' (axis '//
     :                    '^J of position #^I): ^LINE', STATUS )

                     GO TO 999

                  END IF

               END IF

            END DO

         END IF

      END DO

 999  CONTINUE

      END
