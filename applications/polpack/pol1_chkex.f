      SUBROUTINE POL1_CHKEX( INDF, LOC, IGRP, STATUS )
*+
*  Name:
*     POL1_CHKEX

*  Purpose:
*     Checks that the POLPACK extension in an NDF contains 
*     usable values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CHKEX( INDF, LOC, IGRP, STATUS )

*  Description:
*     The routine does the following:
*     1) Checks that the POLPACK extension contains a WPLATE component
*     with a legal value. An error is reported if not.
*     2) Sets up a default IMGID component in the POLPACK extension if
*     the extension does not currently contain an IMGID value, or if the
*     existiung value is blank. The default IMGID value used is the
*     basename of the NDF.
*     3) Checks that the IMGID value is unique amongst the NDFs being
*     processed. If not, a warning (not an error) is given.
*     4) Sets up a default ANGROT component in the POLPACK extension if
*     the extension does not currently contain an ANGROT value. The default 
*     value used is zero.
*     5) Appends the WPLATE value to the FILTER value. If there is no
*     FILTER value then one is created equal to WPLATE.
*     6) Copies the FILTER value into the CCDPACK extension (an extension
*     is created if necessary).
*     
*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to POLPACK extension of NDF.
*     IGRP = INTEGER (Given and Returned)
*        Identifier for a GRP group holding the user IMGID values. If
*        this is supplied equal to GRP__NOID, then a new group is created
*        and its identifier is returned.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1997 (DSB):
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
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) LOC

*  Arguments Given and Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER * ( DAT__SZLOC ) CCDLOC
      CHARACTER FILTER*256
      CHARACTER WPLATE*10
      CHARACTER IMGID*256
      CHARACTER NDFNAM*256
      INTEGER I
      INTEGER IAT
      INTEGER INDX
      INTEGER LC
      LOGICAL THERE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the WPLATE extension item does not exist.
      CALL DAT_THERE( LOC, 'WPLATE', THERE, STATUS )
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLIMP_NOWPL', 'Mandatory extension item '//
     :                 'WPLATE has no value.', STATUS )
      END IF

*  Report an error if the WPLATE extension item now has an illegal value.
      CALL CMP_GET0C( LOC, 'WPLATE', WPLATE, STATUS )
      IF( WPLATE .NE. '0.0' .AND. WPLATE .NE. '45.0' .AND.
     :    WPLATE .NE. '22.5' .AND. WPLATE .NE. '67.5' .AND.
     :    STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'WP', WPLATE )
         CALL ERR_REP( 'POLIMP_BADWPL', 'Extension item WPLATE has '//
     :                 'the illegal value ''^WP''.', STATUS )
      END IF

*  Get the value of the IMGID component, creating it with a blank value if  
*  it does not currently exist in the extension.
      CALL DAT_THERE( LOC, 'IMGID', THERE, STATUS )
      IF( .NOT. THERE ) THEN
         CALL DAT_NEW0C( LOC, 'IMGID', 1, STATUS ) 
         CALL CMP_PUT0C( LOC, 'IMGID', ' ', STATUS ) 
         IMGID = ' '
      ELSE
         CALL CMP_GET0C( LOC, 'IMGID', IMGID, STATUS )
      END IF

*  If the current IMGID value is blank, delete it and create a new one
*  with a value equal to the basename of the NDF.
      IF( IMGID .EQ. ' ' ) THEN

*  Erase the existing component.
         CALL DAT_ERASE( LOC, 'IMGID', STATUS )

*  Find the full name of the NDF.
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_LOAD( ' ', '^NDF', NDFNAM, LC, STATUS ) 

*  Find the position of the last "/" character marking the end of the
*  directory path.
         IF( STATUS .EQ. SAI__OK ) THEN
            LC = CHR_LEN( NDFNAM )
            I = LC
            DO WHILE( NDFNAM( I : I ) .NE. '/' .AND. I .GT. 0 )
               I = I - 1
            END DO

*  We use the string following the last "/".
            I = I + 1

*  Tell the user what is happening.
            IMGID = NDFNAM( I : LC ) 
            CALL MSG_SETC( 'IMGID', IMGID )
            CALL MSG_OUT( ' ', '     Setting IMGID to ''^IMGID''', 
     :                    STATUS )

*  Create ther IMGID component and store the NDF basename as its value.
            CALL DAT_NEW0C( LOC, 'IMGID', LC - I + 1, STATUS ) 
            CALL CMP_PUT0C( LOC, 'IMGID', IMGID, STATUS ) 

         END IF
      END IF

*  If necessary, create a GRP group to hold the used IMGID values.
      IF( IGRP .EQ. GRP__NOID ) THEN
         CALL GRP_NEW( 'Used IMGIDs', IGRP, STATUS )
      ENDIF

*  See if the IMGID value has already been used. If so, issue a warning.
*  If not, add it to the group of used IMGID values.
      LC = CHR_LEN( IMGID ) 
      IF( LC .GT. 0 ) THEN
         CALL GRP_INDEX( IMGID( : LC ), IGRP, 1, INDX, STATUS )
      ELSE
         CALL GRP_INDEX( ' ', IGRP, 1, INDX, STATUS )
      END IF

      IF( INDX .GT. 0 ) THEN
         CALL MSG_SETC( 'IMGID', IMGID )
         CALL MSG_OUT( ' ', '     WARNING - The IMGID value ''^IMGID'' '
     :                 //'has already been used!', STATUS )
      ELSE      
         CALL GRP_PUT( IGRP, 1, IMGID, 0, STATUS ) 
      END IF

*  If the extension does not currently contain an ANGROT value,
*  create one with the default value of zero.
      CALL DAT_THERE( LOC, 'ANGROT', THERE, STATUS )
      IF( .NOT. THERE ) THEN
         CALL DAT_NEW0R( LOC, 'ANGROT', STATUS ) 
         CALL CMP_PUT0R( LOC, 'ANGROT', 0.0, STATUS ) 
         CALL MSG_SETC( 'IMGID', IMGID )
         CALL MSG_OUT( ' ', '     Setting ANGROT to 0.0 degrees.',
     :                 STATUS )
      END IF

*  If the extension does not currently contain a FILTER value,
*  create a blank one. Otherwise, get the existing one.
      CALL DAT_THERE( LOC, 'FILTER', THERE, STATUS )
      IF( .NOT. THERE ) THEN
         CALL DAT_NEW0C( LOC, 'FILTER', 1, STATUS ) 
         CALL CMP_PUT0C( LOC, 'FILTER', ' ', STATUS ) 
         FILTER = ' '
      ELSE
         CALL CMP_GET0C( LOC, 'FILTER', FILTER, STATUS )
      END IF

*  Append WPLATE to the FILTER value unless the FILTER value already
*  contains the WPLATE string.
      IAT = CHR_LEN( FILTER )
      IF( INDEX( FILTER, WPLATE ) .EQ. 0 ) THEN
         CALL CHR_APPND( '_', FILTER, IAT )
         CALL CHR_APPND( WPLATE, FILTER, IAT )
      END IF

*  Store the new FILTER value.
      CALL MSG_SETC( 'VL', FILTER( : IAT ) )
      CALL MSG_OUT( ' ', '     Setting FILTER to ''^VL''', STATUS )
      CALL DAT_ERASE( LOC, 'FILTER', STATUS )
      CALL DAT_NEW0C( LOC, 'FILTER', IAT, STATUS ) 
      CALL CMP_PUT0C( LOC, 'FILTER', FILTER( : IAT ), STATUS ) 

*  See if there is a CCDPACK extension. If not create one.
      CALL NDF_XSTAT( INDF, 'CCDPACK', THERE, STATUS )       
      IF ( .NOT. THERE ) THEN
         CALL NDF_XNEW( INDF, 'CCDPACK', 'CCDPACK_EXT', 0, 0, CCDLOC, 
     :                  STATUS ) 

*  Erase any FILTER component in the existing CCDPACK extension.
      ELSE
         CALL NDF_XLOC( INDF, 'CCDPACK', 'UPDATE', CCDLOC, STATUS ) 
         CALL DAT_THERE( CCDLOC, 'FILTER', THERE, STATUS )
         IF( THERE ) CALL DAT_ERASE( CCDLOC, 'FILTER', STATUS )         
      END IF

*  Store the new FILTER value in the CCDPACK extension.
      CALL DAT_NEW0C( CCDLOC, 'FILTER', IAT, STATUS ) 
      CALL CMP_PUT0C( CCDLOC, 'FILTER', FILTER( : IAT ), STATUS ) 

*  Annul the locator to the CCDPACK extension.
      CALL DAT_ANNUL( CCDLOC, STATUS )

      END
