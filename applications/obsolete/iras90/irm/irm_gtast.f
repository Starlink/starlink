      SUBROUTINE IRM_GTAST( PARAM, PICID, LBND, UBND, IDA, STATUS )
*+
*  Name:
*     IRM_GTAST

*  Purpose:
*     Get an IRA astrometry structure associated with an AGI picture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRM_GTAST( PARAM, PICID, LBND, UBND, IDA, STATUS )

*  Description:
*     An attempt is made to get an NDF from the environment using the
*     supplied parameter. If this is successful, astrometry information
*     is read from the NDF. If a null value is obtained, then an attempt
*     is made to get astrometry information from the MORE structure
*     stored with the picture in the AGI database. If this fails, an
*     attempt is made to get astrometry information from the reference
*     object stored with the picture in the AGI database. If this fails,
*     the supplied parameter value is cancelled and another attempt is
*     made to get an NDF from the environment.
*
*     If the conditional message filter level is not set to MSG__QUIET
*     then an informational message is displayed if the astrometry
*     information is obtained from the MORE structure or from the
*     reference object.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter to use.
*     PICID = INTEGER (Given)
*        The AGI identifier for the picture for which the associated
*        astrometry structure is required.
*     LBND( 2 ) = INTEGER (Returned)
*        The lower pixel bounds of the NDF given by parameter PARAM or
*        stored as the reference object. If the astrometry structure is
*        obtained from the MORE structure of the picture, then LBND is
*        returned holding the pixel indices corresponding to the lower
*        bounds of the picture (assuming the pictures world coordinates
*        are pixel coordinates).
*     UBND( 2 ) = INTEGER (Returned)
*        The upper pixel bounds of the NDF given by parameter PARAM or
*        stored as the reference object. If the astrometry structure is
*        obtained from the MORE structure of the picture, then UBND is
*        returned holding the pixel indices corresponding to the upper
*        bounds of the picture (assuming the pictures world coordinates
*        are pixel coordinates).
*     IDA = INTEGER (Returned)
*        The IRA identifier for the astrometry information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.
      INCLUDE 'PAR_ERR'          ! PAR_ error constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER PICID

*  Arguments Returned:
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER IDA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to component of MORE.
      CHARACTER MLOC*(DAT__SZLOC)! Locator to MORE.
      CHARACTER RLOC*(DAT__SZLOC)! Locator to reference object.
      CHARACTER TYPE*(DAT__SZTYP)! Component type.

      INTEGER I                  ! Component index.
      INTEGER INDF               ! NDF identifier
      INTEGER NCOMP              ! No. of components in MORE.
      INTEGER NDIM               ! No. of dimensions in the NDF.

      LOGICAL AGAIN              ! True if more components remain to be
                                 ! checked.
      LOGICAL THERE              ! True if an object exists.

      REAL WX1                   ! X world coordinate at left hand edge
      REAL WX2                   ! X world coordinate at right hand edge
      REAL WY1                   ! Y world coordinate at left hand edge
      REAL WY2                   ! Y world coordinate at right hand edge

*.

*  Initialise the returned IRA identifier to the bad value.
      IDA = IRA__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an NDF context.
      CALL NDF_BEGIN

*  See if the user has specified an NDF to use.
      CALL NDF_ASSOC( PARAM, 'READ', INDF, STATUS )

*  If a good NDF was obtained...
      IF( STATUS .EQ. SAI__OK ) THEN

*  Get an IRA identifier for the astrometry information stored within
*  it.
         CALL IRA_IMPRT( INDF, IDA, STATUS )

*  Get the bounds of the NDF.
         CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Annul the NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

*  Otherwise, if a null value was supplied, try to get an IRA identifier
*  from the MORE structure stored in the AGI database.
      ELSE IF( STATUS .EQ. PAR__NULL ) THEN

*  Annul the error.
         CALL ERR_ANNUL( STATUS )

*  See if the picture has an associated MORE structure.
         CALL AGI_IMORE( PICID, THERE, STATUS )

*  If so, get a locator to it.
         IF( THERE ) THEN
            CALL AGI_MORE( PICID, 'READ', MLOC, STATUS )

*  Find the number of components stored in the MORE structure.
            CALL DAT_NCOMP( MLOC, NCOMP, STATUS )

*  Loop round the list of components until a component with HDS type
*  "IRAS_ASTROMETRY" is found.
            I = 1
            AGAIN = .TRUE.
            DO WHILE( AGAIN .AND. STATUS .EQ. SAI__OK )

               CALL DAT_INDEX( MLOC, I, CLOC, STATUS )
               CALL DAT_TYPE( CLOC, TYPE, STATUS )

*  Once found, attempt to read the astrometry information.
               IF( TYPE .EQ. IRA__HDSTY ) THEN
                  CALL IRA_READ( CLOC, IDA, STATUS )
                  AGAIN = .FALSE.

*  Set the bounds of the area covered to the world coordinate bounds
*  of the picture (converted to pixel indices).
                  CALL AGI_BEGIN
                  CALL AGI_SELP( PICID, STATUS )
                  CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )
                  CALL AGI_END( -1, STATUS )

                  LBND( 1 ) = NINT( MIN( WX1, WX2 ) + 0.5 )
                  LBND( 2 ) = NINT( MIN( WY1, WY2 ) + 0.5 )
                  UBND( 1 ) = NINT( MAX( WX1, WX2 ) + 0.5 )
                  UBND( 2 ) = NINT( MAX( WY1, WY2 ) + 0.5 )

*  Report it.
                  CALL MSG_OUTIF( MSG__NORM, 'IRM_GTAST_MSG1',
     : '  Using astrometry information stored with the picture in '//
     : 'the AGI database', STATUS )

*  If not found, increment to the next component if any components are
*  left.
               ELSE

                  IF( I .LT. NCOMP ) then
                     I = I + 1
                  ELSE
                     AGAIN = .FALSE.
                  END IF

               END IF

*  Annul the locator to the component.
               CALL DAT_ANNUL( CLOC, STATUS )

            END DO

*  Annul the component to the MORE structure.
            CALL DAT_ANNUL( MLOC, STATUS )

         END IF

      END IF

*  If a good IRA identifier has not yet been obtained, try to get one
*  from the reference object stored in the AGI database.
      IF( IDA .EQ. IRA__NOID .AND. STATUS .EQ. SAI__OK ) THEN

*  See if the picture has a reference object associated with it.
         CALL IRM_AGREF( PICID, 'READ', THERE, RLOC, STATUS )

*  Report the object associated with the picture if it exists.
         IF ( THERE .AND. STATUS .EQ. SAI__OK ) THEN

*  Import it to the NDF system.
            CALL NDF_IMPRT( RLOC, INDF, STATUS )

*  Get an IRA identifier for the astrometry information stored within
*  it.
            CALL IRA_IMPRT( INDF, IDA, STATUS )

*  Report it.
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_OUTIF( MSG__NORM, 'IRM_GTAST_MSG2',
     :                  '  Using astrometry information stored in ^NDF',
     :                     STATUS )

*  Get the bounds of the NDF.
            CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )

*  Annul the NDF identifier.
            CALL NDF_ANNUL( INDF, STATUS )

*  Assign the name of the reference object to a message token.
            CALL IRM_HMSG( 'OBJNM', RLOC )

*  Annul the locator to the reference object.
            CALL REF_ANNUL( RLOC, STATUS )

*  If an error occurred trying to use the reference object, add a
*  context message, annul the astrometry identifier and flush the error.
            IF( STATUS .NE. SAI__OK ) THEN
               CALL ERR_REP( 'IRM_GTAST_ERR1',
     :    'IRM_GTAST: Cannot use reference object (^OBJNM) to define '//
     :    'astrometry', STATUS )
               CALL IRA_ANNUL( IDA, STATUS )
               CALL ERR_FLUSH( STATUS )
            END IF

         END IF

      END IF

*  If a good IRA identifier has still not been obtained, prompt the user
*  for an NDF to get one from.
      IF( IDA .EQ. IRA__NOID .AND. STATUS .EQ. SAI__OK ) THEN

*  Cancel the parameter and get a new NDF from the user.
         CALL PAR_CANCL( PARAM, STATUS )
         CALL NDF_ASSOC( PARAM, 'READ', INDF, STATUS )

*  Get an IRA identifier for the astrometry information stored within
*  it.
         CALL IRA_IMPRT( INDF, IDA, STATUS )

*  Get the bounds of the NDF and annul the identifier.
         CALL NDF_BOUND( INDF, 2, LBND, UBND, NDIM, STATUS )
         CALL NDF_ANNUL( INDF, STATUS )

      END IF

*  End the NDF context.
      CALL NDF_END( STATUS )

      END
