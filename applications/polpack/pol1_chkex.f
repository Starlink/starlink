      SUBROUTINE POL1_CHKEX( INDF, LOC, IGRP, QUIET, STATUS )
*+
*  Name:
*     POL1_CHKEX

*  Purpose:
*     Check the values in the POLPACK extension and WCS component of
*     an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CHKEX( INDF, LOC, IGRP, QUIET, STATUS )

*  Description:
*     The routine does the following:
*     1) Checks that the POLPACK extension contains one and only one of 
*     WPLATE or STOKES. An error is reported if not.
*     2) If STOKES is found, no further checks are made.
*     3) If WPLATE is found...
*     4) Check the WPLATE value.
*     5) Sets up a default IMGID component in the POLPACK extension if
*     the extension does not currently contain an IMGID value, or if the
*     existiung value is blank. The default IMGID value used is the
*     basename of the NDF.
*     6) Checks that the IMGID value is unique amongst the NDFs being
*     processed. If not, a warning (not an error) is given.
*     7) Sets up a default ANGROT component in the POLPACK extension if
*     the extension does not currently contain an ANGROT value. The default 
*     value used is zero.
*     8) Appends the WPLATE value to the FILTER value. If there is no
*     FILTER value then one is created equal to WPLATE.
*     9) Copies the FILTER value into the CCDPACK extension (an extension
*     is created if necessary).
*     10) Adds a Frame into the NDF's WCS component representing a 2D
*     cartesian coordinate system with origin at pixel coordinates (0,0), 
*     with its first axis parallel to the analyser for WPLATE = 0.0
*     degrees.
*     
*  Arguments:
*     INDF = INTEGER (Given)
*        Identifier for the NDF.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator to POLPACK extension of NDF.
*     IGRP = INTEGER (Given and Returned)
*        Identifier for a GRP group holding the used IMGID values. If
*        this is supplied equal to GRP__NOID, then a new group is created
*        and its identifier is returned.
*     QUIET = LOGICAL (Given)
*        Supress screen output - except for warnings and errors?
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-DEC-1997 (DSB):
*        Original version.
*     2-JUL-1998 (DSB):
*        Modified to handle Stokes cubes as well as intensity images.
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
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) LOC
      LOGICAL QUIET

*  Arguments Given and Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Constants:
      DOUBLE PRECISION DTOR
      PARAMETER ( DTOR = 0.01745329251994329577 )

*  Local Variables:
      CHARACTER CCDLOC*(DAT__SZLOC) 
      CHARACTER COMP*(GRP__SZNAM)
      CHARACTER FDIRN*(GRP__SZFNM)
      CHARACTER FILTER*256
      CHARACTER FTYPE*(GRP__SZFNM)
      CHARACTER IMGID*256
      CHARACTER NDFNAM*256
      CHARACTER SLICE*(GRP__SZNAM)
      CHARACTER WPLATE*10
      DOUBLE PRECISION ANGCOS  
      DOUBLE PRECISION ANGSIN
      DOUBLE PRECISION MAT( NDF__MXDIM*NDF__MXDIM )
      INTEGER DIM( NDF__MXDIM )
      INTEGER FORM                      
      INTEGER FRAME                  
      INTEGER I
      INTEGER IAT
      INTEGER ICURR
      INTEGER INDX
      INTEGER IPIXEL
      INTEGER IPOLAN
      INTEGER IWCS
      INTEGER LC
      INTEGER MAP
      INTEGER NDIM
      LOGICAL WTHERE
      LOGICAL STHERE
      LOGICAL THERE
      REAL ANGROT
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the extension contains the WPLATE item.
      CALL DAT_THERE( LOC, 'WPLATE', WTHERE, STATUS )

*  See if the extension contains the STOKES item.
      CALL DAT_THERE( LOC, 'STOKES', STHERE, STATUS )

*  Report an error if neither item was found.
      IF( .NOT. WTHERE .AND. .NOT. STHERE .AND. STATUS .EQ. SAI__OK 
     :    .AND. .NOT. QUIET ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLIMP_NOWPL', 'No value found for either of '//
     :                 'the POLPACK extension items WPLATE or STOKES.', 
     :                 STATUS )
      END IF

*  Report an error if both were found.
      IF( WTHERE .AND. STHERE .AND. STATUS .EQ. SAI__OK 
     :    .AND. .NOT. QUIET ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'POLIMP_STWPL', 'Values found for both of the '//
     :                 'mutually exclusive POLPACK extension items '//
     :                 'WPLATE and STOKES.', STATUS )
      END IF

*  First check intensity images (identified by having a WPLATE value and
*  no STOKES value)...
*  =======================================================================
      IF( WTHERE ) THEN

*  Check the WPLATE extension item.
         CALL CMP_GET0C( LOC, 'WPLATE', WPLATE, STATUS )

*  If the value is "45" or "0", use "45.0" and "0.0". The existing
*  WPLATE component will not be long enough to hold the trailing ".0"
*  so erase it and create a new, longer, WPLATE component.
         IF( WPLATE .EQ. '0' ) THEN
            CALL DAT_ERASE( LOC, 'WPLATE', STATUS )
            CALL DAT_NEW0C( LOC, 'WPLATE', 3, STATUS ) 
            CALL CMP_PUT0C( LOC, 'WPLATE', '0.0', STATUS ) 

         ELSE IF( WPLATE .EQ. '45' ) THEN
            CALL DAT_ERASE( LOC, 'WPLATE', STATUS )
            CALL DAT_NEW0C( LOC, 'WPLATE', 4, STATUS ) 
            CALL CMP_PUT0C( LOC, 'WPLATE', '45.0', STATUS ) 

*  Report an error if the WPLATE extension item has an illegal value.
         ELSE IF( WPLATE .NE. '0.0' .AND. WPLATE .NE. '45.0' .AND.
     :            WPLATE .NE. '22.5' .AND. WPLATE .NE. '67.5' .AND.
     :            STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'WP', WPLATE )
            CALL ERR_REP( 'POLIMP_BADWPL', 'Extension item WPLATE has'//
     :                    ' the illegal value ''^WP''.', STATUS )
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

*  Extract the file basename, and use it as the IMGID value.
            CALL NDG1_HSPEC( NDFNAM, ' ', .FALSE., FDIRN, IMGID, FTYPE,
     :                       COMP, SLICE, FORM, STATUS )

*  Tell the user what is happening.
            IF( .NOT. QUIET ) THEN
               CALL MSG_SETC( 'IMGID', IMGID )
               CALL MSG_OUT( ' ', '     Setting IMGID to ''^IMGID''', 
     :                       STATUS )
            END IF

*  Create ther IMGID component and store the NDF basename as its value.
            CALL DAT_NEW0C( LOC, 'IMGID', MAX( 1, CHR_LEN( IMGID ) ), 
     :                      STATUS ) 
            CALL CMP_PUT0C( LOC, 'IMGID', IMGID, STATUS ) 

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
            CALL MSG_OUT( ' ', '     WARNING - The IMGID value  '
     :                   //'''^IMGID'' has already been used!', STATUS )
         ELSE      
            CALL GRP_PUT( IGRP, 1, IMGID, 0, STATUS ) 
         END IF

*  If the extension does not currently contain an ANGROT value,
*  create one with the default value of zero.
         CALL DAT_THERE( LOC, 'ANGROT', THERE, STATUS )
         IF( .NOT. THERE ) THEN
            CALL DAT_NEW0R( LOC, 'ANGROT', STATUS ) 
            CALL CMP_PUT0R( LOC, 'ANGROT', 0.0, STATUS ) 
            IF( .NOT. QUIET ) THEN
               CALL MSG_SETC( 'IMGID', IMGID )
               CALL MSG_OUT( ' ', '     Setting ANGROT to 0.0 degrees.',
     :                       STATUS )
            END IF
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
         IF( .NOT. QUIET ) THEN
            CALL MSG_SETC( 'VL', FILTER( : IAT ) )
            CALL MSG_OUT( ' ', '     Setting FILTER to ''^VL''', 
     :                    STATUS )
         END IF
         CALL DAT_ERASE( LOC, 'FILTER', STATUS )
         CALL DAT_NEW0C( LOC, 'FILTER', MAX( 1, IAT ), STATUS ) 
         CALL CMP_PUT0C( LOC, 'FILTER', FILTER( : IAT ), STATUS ) 

*  See if there is a CCDPACK extension. If not create one.
         CALL NDF_XSTAT( INDF, 'CCDPACK', THERE, STATUS )       
         IF ( .NOT. THERE ) THEN
            CALL NDF_XNEW( INDF, 'CCDPACK', 'CCDPACK_EXT', 0, 0, CCDLOC, 
     :                     STATUS ) 

*  Erase any FILTER component in the existing CCDPACK extension.
         ELSE
            CALL NDF_XLOC( INDF, 'CCDPACK', 'UPDATE', CCDLOC, STATUS ) 
            CALL DAT_THERE( CCDLOC, 'FILTER', THERE, STATUS )
            IF( THERE ) CALL DAT_ERASE( CCDLOC, 'FILTER', STATUS )         
         END IF

*  Store the new FILTER value in the CCDPACK extension.
         CALL DAT_NEW0C( CCDLOC, 'FILTER', MAX( 1, IAT ), STATUS ) 
         CALL CMP_PUT0C( CCDLOC, 'FILTER', FILTER( : IAT ), STATUS ) 

*  Annul the locator to the CCDPACK extension.
         CALL DAT_ANNUL( CCDLOC, STATUS )

*  Add a Frame to the NDFs WCS component in which the first axis
*  corresponds to the analyser WPLATE=0.0 axis. 
*  ============================================================
*  Get the number of axes in the NDF.
         CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )

*  Start an AST context.
         CALL AST_BEGIN( STATUS )

*  Get an AST pointer for the FrameSet stored in the WCS component of
*  the NDF (or equivalent info from the FITS or IRAS90 extension).
         CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Note the original Current frame.
         ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Remove any existing POLANAL Frame.
         IF( AST_FINDFRAME( IWCS, AST_FRAME( NDIM, 'MINAXES=1, '//
     :                      'MAXAXES=20', STATUS ), 'POLANAL', 
     :                      STATUS ) .NE. AST__NULL ) THEN

            IPOLAN = AST_GETI( IWCS, 'CURRENT', STATUS )
            CALL AST_REMOVEFRAME( IWCS, AST__CURRENT, STATUS )

*  Correct the original index of the Current Frame to take account of the
*  removed Frame.
            IF( ICURR .GT. IPOLAN ) THEN
               ICURR = ICURR - 1
            ELSE IF( ICURR .EQ. IPOLAN ) THEN
               ICURR = AST__NOFRAME
            END IF
   
         END IF

*  Create a mapping from pixel coordinates (given by the PIXEL Frame in the
*  FrameSet), to a 2D cartesian coordinate system in which the X axis is
*  parallel to the analyser WPLATE = 0.0 axis. First get the
*  anti-clockwise angle in degrees fro mthe pixel X axis to the analyser X
*  axis.
         CALL CMP_GET0R( LOC, 'ANGROT', ANGROT, STATUS )

*  If the rotation is zero, this is just a unit mapping.
         IF( ANGROT .EQ. 0.0 ) THEN
            MAP = AST_UNITMAP( NDIM, ' ', STATUS )

*  Otherwise, create a MatrixMap describing the rotation from pixel
*  coordinates to analyser coordinates. 
         ELSE

*  Set the entire matrix to zero.
            DO I = 1, NDIM*NDIM
               MAT( I ) = 0.0
            END DO

*  Set the diagonal elements to 1.0.
            DO I = 0, NDIM - 1
               MAT( 1 + ( NDIM + 1 )*I ) = 1.0
            END DO

*  Store the  trig terms describing the rotation of the first 2 axes.
            ANGCOS = COS( DTOR*DBLE( ANGROT ) )
            ANGSIN = SIN( DTOR*DBLE( ANGROT ) )

            MAT( 1 ) = ANGCOS
            MAT( 2 ) = ANGSIN
            MAT( 1 + NDIM ) = -ANGSIN
            MAT( 2 + NDIM ) = ANGCOS

*  Create the MatrixMap.
            MAP = AST_MATRIXMAP( NDIM, NDIM, 0, MAT, ' ', STATUS )
         END IF

*  Now try to find the PIXEL Frame in the NDF's FrameSet. The PIXEL
*  Frame becomes the current Frame if found.
         IF( AST_FINDFRAME( IWCS, AST_FRAME( NDIM, ' ', STATUS ), 
     :                      'PIXEL', STATUS ) .NE. AST__NULL ) THEN

*  Get the index of the current Frame (i.e. the PIXEL Frame) within the
*  FrameSet.
            IPIXEL = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Create the Frame describing analyser coordinates. Use the domain
*  POLANAL to identify it.
            FRAME = AST_FRAME( NDIM, 'Domain=POLANAL, Title=Polpack '//
     :                         'analyser frame', STATUS )

*  Now add the analyser Frame into the FrameSet, using the Mapping created
*  above to connect it to the pixel coordinate Frame.
            CALL AST_ADDFRAME( IWCS, IPIXEL, MAP, FRAME, STATUS )

*  Reinstate the original Current Frame (if it still exists).
            IF( ICURR .NE. AST__NOFRAME ) THEN
               CALL AST_SETI( IWCS, 'Current', ICURR, STATUS )
            END IF

*  Store the new FrameSet back in the NDF.
            CALL NDF_PTWCS( IWCS, INDF, STATUS )

         END IF

*  End the AST context.
         CALL AST_END( STATUS )

      END IF

      END
