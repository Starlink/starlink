      SUBROUTINE KPG1_WCFAX( INDF, FS, AXIS, EL, CENTRE, STATUS )
*+
*  Name:
*     KPG1_WCAXC

*  Purpose:
*     Fills co-ordinates for an axis from a WCS component FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_WCFAX( INDF, FS, AXIS, EL, CENTRE, STATUS )

*  Description:
*     This routine returns an array of world co-ordinates along a
*     nominated axis for all elements of an NDF.  The co-ordinates are
*     derived from the Current Frame in the supplied FrameSet. 

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier.
*     FS = INTEGER (Given)
*        An AST pointer for a FrameSet.
*     AXIS = INTEGER (Given)
*        The number of the axis whose co-ordinates are to be
*        returned.
*     EL = INTEGER (Returned)
*        The number of elements in the co-ordinate array.
*     CENTRE( EL ) = DOUBLE PRECISION (Returned)
*        The axis centres.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2006 January 25 (MJC):
*        Original version based upon DSB's FTS1_WCSAX.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'AST_PAR'          ! AST_ public constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Arguments Given:
      INTEGER INDF
      INTEGER FS
      INTEGER AXIS

*  Arguments Returned:
      INTEGER EL
      DOUBLE PRECISION CENTRE( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIMS( NDF__MXDIM ) ! Dimensions of the NDF
      INTEGER IAX                ! Axes loop counter
      INTEGER ICURR              ! Index of Current Frame 
      INTEGER IERR               ! Position of first numerical error
      INTEGER IGRID              ! Index of the GRID Frame
      INTEGER IPCOIN             ! Pointer to mapped grid co-ordinates
      INTEGER IPCOUT             ! Pointer to mapped current-Frame
                                 ! co-ordinates
      INTEGER MAP1               ! n-D GRID to n-D Current mapping
      INTEGER MAP2               ! n-D to 1-D mapping
      INTEGER MAP3               ! n-D GRID to 1-D Current mapping
      INTEGER NDIM               ! Number of dimensions
      INTEGER NERR               ! Number of numerical errors
      INTEGER OUTPRM( NDF__MXDIM ) ! Indices of corresponding axes

*.

      EL = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate the axis.
      CALL NDF_DIM( INDF, NDF__MXDIM, DIMS, NDIM, STATUS )
      IF ( AXIS .LT. 1 .OR. AXIS .GT. NDIM ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL MSG_SETI( 'AX', AXIS )
         CALL MSG_SETI( 'N', NDIM )
         CALL ERR_REP( 'KPG1_WCFAX',
     :     'The chosen axis ^AX is not valid for ^NDF, which has ^N '/
     :     /'dimensions (probable programming error).', STATUS )
         GOTO 999
      END IF

*  Find the number of elements in the NDF.
      EL = 1
      DO IAX = 1, NDIM
         EL = EL * DIMS( IAX )
      END DO

*  This is a brute-force method, as it may demand considerable memory
*  and ignores insignificant dimensions.

*  Obtain workspace to hold the input GRID co-ordinates.
      CALL PSX_CALLOC( EL * NDIM, '_DOUBLE', IPCOIN, STATUS )

*  Check we can safely use %VAL on the pointer returned by NDF_AMAP.
      IF ( STATUS .NE. SAI__OK ) GOTO 999

*  Fill the input co-ordinate array
      CALL KPG1_FIGRD( NDIM, DIMS, EL, %VAL( CNF_PVAL( IPCOIN ) ),
     :                 STATUS )

*  Save the index of the original current Frame in the FrameSet, so 
*  that it can be re-instated later.
      ICURR = AST_GETI( FS, 'CURRENT', STATUS )

*  We wish to map from the GRID Frame to the current Frame.  Thus
*  we can substitute the FramSet for the mapping.

*  Search for a GRID Frame in the FrameSet.  If one is found, it
*  becomes the current Frame. If one is not found, do nothing.
      IF ( AST_FINDFRAME( FS, AST_FRAME( NDIM, ' ', STATUS ), 
     :                    'GRID', STATUS ) .NE. AST__NULL ) THEN

*  If found, get the index of the GRID Frame.
         IGRID = AST_GETI( FS, 'CURRENT', STATUS )

*  Get the mapping from the GRID Frame to the Current Frame.
         MAP1 = AST_GETMAPPING( FS, IGRID, ICURR, STATUS )

*  Initialise the indices of the axes in the one-dimensional Frame
*  corresponding to each axis in the n-dimensional Frame.  Since the
*  axes are presumed to be independent of each other, it does not matter
*  what values we use so long as the axis being processed is assigned
*  the value 1.  We use AST PermMaps to pick the axis to process.
         DO IAX = 1, NDIM
            OUTPRM( IAX ) = 0
         END DO
         OUTPRM( AXIS ) = 1

*  Process the axis in a separate AST context.
         CALL AST_BEGIN( STATUS )

*  MAP1 goes from n-dimensional GRID co-ordinates to n-dimensional 
*  current co-ordinates.  We need a mapping that transforms
*  n-dimensional GRID co-ordinates to a one-dimensional co-ordinates
*  along the specified axis.  So we want to modify MAP1 such that it
*  maps only a single axis.  To do this we add a PermMap to the input
*  and output of MAP1, that selects only the required axis.  Create a 
*  PermMaps to extract axis AXIS from an n-dimensional Frame; MAP2 goes
*  from n-dimensional to one-dimensional.
         MAP2 = AST_PERMMAP( NDIM, OUTPRM, 1, AXIS, 0.0D0, ' ', 
     :                       STATUS )

*  Concatenate the Mappings together, to get a mapping between axis
*  AXIS in the GRID Frame to the same axis in the current Frame.
         MAP3 = AST_CMPMAP( MAP1, MAP2, .TRUE., ' ', STATUS )

*  Transform the GRID co-ordinates to current-Frame co-ordinates at
*  each pixel of the NDF.
         CALL AST_TRANN( MAP3, EL, NDIM, EL, %VAL( CNF_PVAL( IPCOIN ) ),
     :                   .TRUE., 1, EL, CENTRE, STATUS )

*  End the AST context and do the next axis.
         CALL AST_END( STATUS )

*  There should always be a GRID Frame, but just in case something odd
*  has been done to the NDF, copy the grid co-ordinates to the output
*  array.  A case could be made for looking or an AXIS or PIXEL, however,
*  for now the aim is to avoid messy crashes.
      ELSE
         CALL VEC_DTOD( .TRUE., EL, %VAL( CNF_PVAL( IPCOIN ) ),
     :                  CENTRE, IERR, NERR, STATUS )

      END IF

*  Re-instate the original current Frame.
      CALL AST_SETI( FS, 'CURRENT', ICURR, STATUS )

  999 CONTINUE

      END
