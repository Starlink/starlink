      SUBROUTINE IRI_NEW( INDF, INSTRM, BAND, TYPE, UNITS, LOC, STATUS )
*+
*  Name:
*     IRI_NEW

*  Purpose:
*     Create an IMAGE_INFO structure within an existing NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRI_NEW( INDF, INSTRM, BAND, TYPE, UNITS, LOC, STATUS )

*  Description:
*     This routine creates structures describing an IRAS image within a
*     specified NDF. If the NDF does not contain an IRAS extension, one
*     is created. Any IMAGE_INFO structure which currently exists is
*     deleted and a new one created with an HDS locator to it being
*     returned to the calling application. Values are stored for the
*     mandatory IMAGE_INFO components BAND, INSTRUMENT and TYPE, and the
*     NDF UNITS component. Note, no ASTROMETRY structure is created by
*     this routine. The IRA package must be used to create such a
*     structure before the image can be used by other IRAS90
*     applications.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the NDF holding the image.
*     INSTRM = CHARACTER * ( * ) (Given)
*        The instrument from which the data originated; CPC or
*        SURVEY.
*     BAND = INTEGER (Given)
*        The IRAS waveband no of the data in the image. This must be
*        in the range 1 - 4 for data from the survey array and 1 - 2
*        for data from the CPC.
*     TYPE = CHARACTER * ( * ) (Given)
*        The image type. Legal values are listed in ID/12. An error is
*        reported if an unknown value is supplied.
*     UNITS = CHARACTER * ( * ) (Given)
*        A string describing the system of units in which the DATA array
*        values are stored. This string is stored as the NDF UNITS
*        component. The supplied value must be on of the standard values
*        listed in ID12, otherwise an error is reported. Note, in this
*        case the error is reported after the units have been stored,
*        so that annulling the error will enable an application to
*        proceed without needing to store a new value for UNITS.
*     LOC = CHARACTER * ( * ) (Returned)
*        An HDS locator to the created IMAGE_INFO structure residing
*        within the IRAS extension of the NDF. This locator should be
*        annulled using DAT_ANNUL when it is no longer needed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-JUN-1992 (DSB):
*        Original version.
*     4-DEC-1992 (DSB):
*        Argument TYPE added. Check for standard UNITS moved to the end.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'IRI_PAR'          ! IRI constants.
      INCLUDE 'IRI_ERR'          ! IRI error values.

*  Arguments Given:
      INTEGER INDF
      CHARACTER INSTRM*(*)
      INTEGER BAND
      CHARACTER TYPE*(*)
      CHARACTER UNITS*(*)

*  Arguments Returned:
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( NDF__MXDIM )  ! NDF diemnsion sizes.
      INTEGER NDIM               ! No. of NDF dimension.
      LOGICAL THERE              ! True if an object exists.
      CHARACTER XLOC*(DAT__SZLOC)! Locator to the IRAS extension.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Validate argument BAND, INSTRM.
      IF( INSTRM .EQ. 'CPC' ) THEN

         IF( BAND .LT. 1 .OR. BAND .GT. 2 ) THEN
            STATUS = IRI__BADBN
            CALL MSG_SETC( 'B', BAND )
            CALL ERR_REP( 'IRI_NEW_ERR1',
     :              'IRI_NEW: Illegal IRAS waveband number (^B) given',
     :                    STATUS )
            GO TO 999
         END IF

      ELSE IF( INSTRM .EQ. 'SURVEY' ) THEN

         IF( BAND .LT. 1 .OR. BAND .GT. 4 ) THEN
            STATUS = IRI__BADBN
            CALL MSG_SETC( 'B', BAND )
            CALL ERR_REP( 'IRI_NEW_ERR2',
     :              'IRI_NEW: Illegal IRAS waveband number (^B) given',
     :                    STATUS )
            GO TO 999
         END IF

      ELSE
         STATUS = IRI__BADIN
         CALL MSG_SETC( 'I', INSTRM )
         CALL ERR_REP( 'IRI_NEW_ERR3',
     :                 'IRI_NEW: Illegal IRAS instrument (^I) given',
     :                 STATUS )
         GO TO 999

      END IF

*  Validate the TYPE argument.
      IF( TYPE .NE. IRI__CPC .AND.
     :    TYPE .NE. IRI__SKYFL .AND.
     :    TYPE .NE. IRI__GALPL .AND.
     :    TYPE .NE. IRI__ALLSK .AND.
     :    TYPE .NE. IRI__DSCO .AND.
     :    TYPE .NE. IRI__ISSA .AND.
     :    TYPE .NE. IRI__YORIC .AND.
     :    TYPE .NE. IRI__MAPCR .AND.
     :    TYPE .NE. IRI__COLC .AND.
     :    TYPE .NE. IRI__NONAM ) THEN

         STATUS = IRI__BADTY
         CALL MSG_SETC( 'T', TYPE )
         CALL ERR_REP( 'IRI_NEW_ERR4',
     :                 'IRI_NEW: Illegal image type (^T) given',
     :                 STATUS )

      END IF

*  Check that the NDF is two dimensional.
      CALL NDF_DIM( INDF, NDF__MXDIM, DIM, NDIM, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. NDIM .NE. 2 ) THEN
         STATUS = IRI__NOT2D
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'IRI_NEW_ERR5',
     :               'IRI_NEW: NDF has ^ND dimensions (should have 2)',
     :                 STATUS )
      END IF

*  See if the NDF already has an IRAS extension.
      CALL NDF_XSTAT( INDF, 'IRAS', THERE, STATUS )

*  If it does...
      IF( THERE ) THEN

*  Get a locator to it.
         CALL NDF_XLOC( INDF, 'IRAS', 'UPDATE', XLOC, STATUS )

*  See if an IMAGE_INFO structure exists Within the IRAS extension.
         CALL DAT_THERE( XLOC, 'IMAGE_INFO', THERE, STATUS )

*  If it does, delete it.
         IF( THERE ) CALL DAT_ERASE( XLOC, 'IMAGE_INFO', STATUS )

*  If no IRAS extension exists, create one.
      ELSE
         CALL NDF_XNEW( INDF, 'IRAS', 'IRAS', 0, 0, XLOC, STATUS )

      END IF

*  Create a structure called IMAGE_INFO within the IRAS extension and
*  get a locator to it.
      CALL DAT_NEW( XLOC, 'IMAGE_INFO', 'IMAGE_INFO', 0, 0, STATUS )
      CALL DAT_FIND( XLOC, 'IMAGE_INFO', LOC, STATUS )

*  Annull the locator to the IRAS extension.
      CALL DAT_ANNUL( XLOC, STATUS )

*  Store the value of IMAGE_INFO component INSTRUMENT.
      CALL DAT_NEW0C( LOC, 'INSTRUMENT', IRI__SZINS, STATUS )
      CALL CMP_PUT0C( LOC, 'INSTRUMENT', INSTRM, STATUS )

*  Store the value of the IMAGE_INFO component BAND.
      CALL DAT_NEW0I( LOC, 'BAND', STATUS )
      CALL CMP_PUT0I( LOC, 'BAND', BAND, STATUS )

*  Store the value of the IMAGE_INFO component TYPE.
      CALL DAT_NEW0C( LOC, 'TYPE', IRI__SZTYP, STATUS )
      CALL CMP_PUT0C( LOC, 'TYPE', TYPE, STATUS )

*  Store the NDF UNITS component.
      CALL NDF_CPUT( UNITS, INDF, 'UNITS', STATUS )

*  Validate argument UNITS.
      IF( UNITS .NE. IRI__JPS .AND.
     :    UNITS .NE. IRI__MJPS .AND.
     :    UNITS .NE. IRI__JPP .AND.
     :    UNITS .NE. IRI__FPS .AND.
     :    UNITS .NE. IRI__FPP ) THEN

         STATUS = IRI__BADUN
         CALL MSG_SETC( 'U', UNITS )
         CALL ERR_REP( 'IRI_NEW_ERR6',
     :                 'IRI_NEW: Non-standard units (^U) given',
     :                 STATUS )

      END IF

*  If an error has been reported, give a context message.
 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRI_NEW_ERR7',
     : 'IRI_NEW: Error adding IRAS90 image information to ^NDF',
     :                 STATUS )
      END IF

      END
