      SUBROUTINE WCSADD( STATUS )
*+
*  Name:
*     WCSADD

*  Purpose:
*     Adds a new co-ordinate Frame into the WCS component of an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL WCSADD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application adds a new co-ordinate Frame into the WCS component 
*     of an NDF. The new Frame must be linearly connected to an existing
*     Frame (called the "basis" Frame) in the WCS component. The linear 
*     transformation is specified by parameter TR. The new Frame becomes the 
*     current co-ordinate Frame in the NDF.
*
*     The new Frame is a copy of the basis Frame, with the new Domain 
*     attribute specified by parameter DOMAIN. If necessary, other attributes 
*     of the Frame (Title, Label, Format, etc) can be changed using 
*     application WCSATTRIB.

*  Usage:
*     wcsadd ndf frame domain [tr] 

*  ADAM Parameters:
*     DOMAIN = LITERAL (Read)
*        The value for the Domain attribute for the new Frame. Care should be
*        taken to ensure that domain names are used consistently. This
*        will usually mean avoiding any domain names which are already in
*        use within the WCS component, particularly the standard domain names
*        such as GRID, PIXEL, AXIS and GRAPHICS. The supplied value is 
*        stripped of spaces, and converted to upper case before being used.
*     EPOCH = _DOUBLE (Read)
*        If the basis Frame is specified using a "Sky Co-ordinate System" 
*        specification for a celestial co-ordinate system (see parameter 
*        FRAME), then an epoch value is needed to qualify it. This is the 
*        epoch at which the supplied sky positions were determined. It should 
*        be given as a decimal years value, with or without decimal places  
*        ("1996.8" for example). Such values are interpreted as a Besselian 
*        epoch if less than 1984.0 and as a Julian epoch otherwise. The 
*        suggested default is the value stored in the basis Frame.
*     FRAME = LITERAL (Read)
*        A string specifying the basis Frame. If a null value is supplied
*        the current co-ordinate Frame in the NDF is used. The string can 
*        be one of the following:
*
*        - A domain name such as SKY, AXIS, PIXEL, etc. The two
*        "pseudo-domains" WORLD and DATA may be supplied and will be
*        translated into PIXEL and AXIS respectively, so long as the WCS
*        component of the NDF does not contain Frames with these domains.
*
*        - An integer value giving the index of the required Frame within
*        the WCS component.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see 
*        section "Sky Co-ordinate Systems" in SUN/95).
*
*     NDF = NDF (Read and Write)
*        The NDF in which the new Frame is to be stored.
*     TR( ) = _DOUBLE (Read)
*        The values of this parameter are the coefficients of a linear 
*        transformation from the basis Frame specified by parameter FRAME to 
*        the new Frame. For instance, if a feature has co-ordinates 
*        (X,Y,Z,...) in the basis Frame, and co-ordinates (U,V,W,...) in 
*        the new Frame, then the following transformations would be used, 
*        depending on how many axes the two Frames have:
*
*        - 1-dimensional:
*
*              U = TR(1) + TR(2)*X 
*
*        - 2-dimensional:
*
*              U = TR(1) + TR(2)*X + TR(3)*Y
*
*              V = TR(4) + TR(5)*X + TR(6)*Y
*
*        - 3-dimensional:
*
*              U = TR(1) + TR(2)*X + TR(3)*Y + TR(4)*Z
*
*              V = TR(5) + TR(6)*X + TR(7)*Y + TR(8)*Z
*
*              W = TR(9) + TR(10)*X + TR(11)*Y + TR(12)*Z
*
*        The correct number of values must be supplied (that is, N*(N+1)
*        where N is the number of axes in the new and old Frames). If a 
*        null value (!) is given it is assumed that the new Frame and the
*        basis Frame are connected using a unit mapping (i.e. corresponding
*        axis values are identical in the two Frames). [!]

*  Examples:
*     wcsadd ngc5128 pixel old_pixel
*        This adds a new co-ordinate Frame into the WCS component of the
*        NDF called ngc5128. The new Frame is given the domain OLD_PIXEL
*        and is a copy of the existing PIXEL Frame. This OLD_PIXEL Frame 
*        will be retained through further processing and can be used as a 
*        record of the original pixel co-ordinate Frame.
*     wcsadd my_data dist-lum dist(au)-lum [0,2.0628E5,0,0,0,1]
*        This adds a new co-ordinate Frame into the WCS component of the
*        NDF called my_data. The new Frame is given the domain DIST(AU)-LUM
*        and is a copy of an existing Frame with domain DIST-LUM. The first
*        axis in the new Frame is derived from the first axis in the basis
*        Frame but is in different units (AU instead of parsecs). This
*        change of units is achieved by multiplying the old Frame axis 1
*        values by 2.0628E5. The values on the second axis are copied 
*        without change. You could then use application WCSATTRIB to set
*        the "Unit" attribute for axis 1 of the new Frame to "AU".

*  Notes:
*     -  The new Frame has the same number of axes as the basis Frame.
*     -  An error is reported if the transformation supplied using parameter 
*     TR is singular. 

*  Related Applications:
*     KAPPA: NDFTRACE, WCSFRAME, WCSREMOVE, WCSATTRIB

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1998 (DSB):
*        Original version.
*     25-AUG-1999 (DSB):
*        Add TOKEN arg in call to KPG1_ASFRM
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants 
      INCLUDE 'PAR_ERR'          ! PAR error constants 
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN            ! Used length of a string

*  Local Variables:
      CHARACTER DOM*40           ! Domain for new Frame
      DOUBLE PRECISION DET       ! Matrix determinant
      DOUBLE PRECISION INA( NDF__MXDIM ) ! Corner "A" of window in input Frame
      DOUBLE PRECISION INB( NDF__MXDIM ) ! Corner "B" of window in input Frame
      DOUBLE PRECISION MATRIX( NDF__MXDIM*NDF__MXDIM ) ! Pure matrix (no offset)
      DOUBLE PRECISION MTEST( NDF__MXDIM*NDF__MXDIM ) ! Pure matrix (no offset)
      DOUBLE PRECISION OFFSET( NDF__MXDIM ) ! Pixel offset vector
      DOUBLE PRECISION OTEST( NDF__MXDIM )  ! Pixel offset vector
      DOUBLE PRECISION OUTA( NDF__MXDIM )! Corner "A" of window in output Frame
      DOUBLE PRECISION OUTB( NDF__MXDIM )! Corner "B" of window in output Frame
      DOUBLE PRECISION TR( NDF__MXDIM*( NDF__MXDIM + 1 ) ) ! Mapping co-effs
      INTEGER ACTVAL             ! No. of transformation co-efficients supplied
      INTEGER AXES( NDF__MXDIM ) ! Axis selection array
      INTEGER FRMB               ! Pointer to basis Frame
      INTEGER FRMN               ! Pointer to new Frame
      INTEGER I                  ! General loop count
      INTEGER IBASIS             ! Index of basis Frame
      INTEGER INDF               ! NDF identifier for NDF being modified
      INTEGER IWCS               ! Pointer to WCS FrameSet
      INTEGER J                  ! Column index
      INTEGER K                  ! Index within supplied list of co-efficients
      INTEGER L                  ! Index within vectorised matrix array
      INTEGER MAP                ! Pointer to old->new Mapping
      INTEGER MTRMAP             ! MatrixMap implied by given co-efficients
      INTEGER NAXB               ! No. of axes in basis Frame
      INTEGER NCOEF              ! Required no. of transformation co-efficients 
      INTEGER SING               ! Non-zero if matrix is singular
      INTEGER WINMAP             ! WinMap implied by given co-efficients
      INTEGER WORK( NDF__MXDIM ) ! Work space
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Obtain an identifier for the NDF to be modified.
      CALL NDF_ASSOC( 'NDF', 'UPDATE', INDF, STATUS )

*  Get the WCS FrameSet associated with the NDF.
      CALL KPG1_GTWCS( INDF, IWCS, STATUS )

*  Get the existing Frame which is to be used as the basis for the new Frame. 
*  The selected Frame becomes the Current Frame.
      CALL NDF_MSG( 'NDF', INDF )
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IWCS, 'PIXEL', 'AXIS', .TRUE.,
     :                 '^NDF', STATUS )

*  Get its index, get a pointer to it, and save the number of axes in it.
      IBASIS = AST_GETI( IWCS, 'CURRENT', STATUS )
      FRMB = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )
      NAXB = AST_GETI( FRMB, 'NAXES', STATUS )

*  Copy the basis Frame to create the new Frame.
      FRMN = AST_COPY( FRMB, STATUS ) 

*  Get the Domain for the new Frame. 
      CALL PAR_GET0C( 'DOMAIN', DOM, STATUS )

*  Remove spaces, and convert to upper case. 
      CALL CHR_RMBLK( DOM )
      CALL CHR_UCASE( DOM )

*  Store it in the new Frame.
      CALL AST_SETC( FRMN, 'DOMAIN', DOM( : MAX( 1, CHR_LEN( DOM ) ) ), 
     :               STATUS )

*  Get the co-efficients of the linear transformation from the basis
*  Frame to the new Frame. Ensure the exact required number are supplied.
      ACTVAL = 0
      NCOEF = ( NAXB + 1 )*NAXB
      DO WHILE( ACTVAL .NE. NCOEF .AND. STATUS .EQ. SAI__OK ) 
         CALL PAR_GET1D( 'TR', NCOEF, TR, ACTVAL, STATUS )
         IF( ACTVAL .NE. NCOEF .AND. STATUS .EQ. SAI__OK ) THEN
            CALL MSG_SETI( 'N', NCOEF )
            CALL MSG_OUT( 'WCSADD_MSG1', 'Please supply exactly ^N '//
     :                    'co-efficient values (or a null value) for '//
     :                    ' parameter %TR.', STATUS )
            CALL PAR_CANCL( 'TR', STATUS )
         END IF
      END DO

*  If a null value was given, annul the error and create a unit MatrixMap.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         MAP = AST_MATRIXMAP( NAXB, NAXB, 2, 0.0D0, ' ', STATUS ) 

*  Otherwise, if no error has occurred, extract the offset and matrix
*  from the supplied list of co-efficients.         
      ELSE

*  Extract the offset into a separate vector, making two copies.
         DO I = 1, NAXB
            OFFSET( I ) = TR( 1 + ( I - 1 )*( NAXB + 1 ) )
            OTEST( I ) = OFFSET( I )
         END DO

*  Extract the matrix into a separate vector, making two copies.
         K = 1
         L = 1
         DO I = 1, NAXB
            K = K + 1
   
            DO J = 1, NAXB
               MATRIX( L ) = TR( K )
               MTEST( L ) = TR( K )
               L = L + 1
               K = K + 1
            END DO
   
         END DO

*  See if the matrix is singular. The MTEST and OTEST arrays 
*  are changed by this call, This is why we took two copies above.
         CALL SLA_DMAT( NAXB, MTEST, OTEST, DET, SING, WORK )

*  Report an error if the matrix is singular.
         IF( SING .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'WCSADD_ERR1', 'The supplied '//
     :                 'transformation matrix is singular, and '//
     :                 'therefore cannot be inverted.', STATUS )
         END IF

*  Create a MatrixMap from the supplied MATRIX array.
         MTRMAP = AST_MATRIXMAP( NAXB, NAXB, 0, MATRIX, ' ', STATUS )

*  Create a WinMap which gives the required shift of pixel origin. 
         DO I = 1, NAXB
            INA( I ) = 0.0D0 
            INB( I ) = MAX( ABS( OFFSET( I ) ), 1.0D0 )
            OUTA( I ) = INA( I ) + OFFSET( I )
            OUTB( I ) = INB( I ) + OFFSET( I )
         END DO

         WINMAP = AST_WINMAP( NAXB, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Concatenate these two mappings in series to get the mapping from the
*  basis Frame to the new Frame.
         MAP = AST_CMPMAP( MTRMAP, WINMAP, .TRUE., ' ', STATUS )

      END IF

*  Add the new Frame into the FrameSet. It becomes the Current Frame.
      CALL AST_ADDFRAME( IWCS, IBASIS, MAP, FRMN, STATUS ) 

*  Save the FrameSet in the NDF.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  Tidy up.
*  ========
 999  CONTINUE

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'WCSADD_ERR2', 'WCSADD: Failed to add a new '//
     :                 'co-ordinate Frame into an NDF WCS component.',
     :                 STATUS )
      END IF

      END
