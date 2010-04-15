      SUBROUTINE PREPC3( INDF, II, NITEM, SIZE, WORK, NNOISE, STATUS )
*+
*  Name:
*     PREPC3

*  Purpose:
*     Store information describing an NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPC3( INDF, II, NITEM, SIZE, WORK, NNOISE, STATUS )

*  Description:
*     Stores information describing an NDF in the supplied workspace.
*     The workspace is a 2D array, consisting of one row of NITEM
*     values for each of the NDFs in the input group. The information
*     describing the NDF is stored in the workspace in the row with the
*     same index as the NDF within the input group.  The first two items
*     of information are the same for both PO and YORIC images:
*
*     1) "NOISE" or "DATA", depending on whether the image is a noise or
*     data grid.
*     2) The type of image, PO or YORIC.
*
*     The other itsm of information stored depends on the type of
*     image;
*
*     For PO images:
*        3) Grid number
*        4) Wave band index.
*        5) Input units.
*        6) (blank)
*        7) (blank)
*
*     For YORIC images:
*        3) Value of FITS keyword OBJECT.
*        4) Value of FITS keyword DATE.
*        5) Value of FITS keyword ITERNO.
*        6) Wave band index.
*        7) Input units.

*  Arguments:
*     INDF = INTEGER (Given)
*        An identifier for the input NDF.
*     II = INTEGER (Given)
*        The index at which this NDF is stored in the input group.
*     NITEM = INTEGER (Given)
*        The maximum number of items of information stored about each
*        NDF.
*     SIZE = INTEGER (Given)
*        The total number of NDFs in the input group.
*     WORK( NITEM, SIZE ) = CHARACTER * ( * ) (Given and Returned)
*        The workspace in which to store the items of information which
*        describe the current NDF.
*     NNOISE = INTEGER (Given and Returned)
*        The number of noise grids found so far.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
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
      INCLUDE 'IRI_PAR'          ! IRI_ constants

*  Arguments Given:
      INTEGER INDF
      INTEGER II
      INTEGER NITEM
      INTEGER SIZE

*  Arguments Given and Returned:
      CHARACTER WORK( NITEM, SIZE )*(*)
      INTEGER NNOISE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER FTSLOC*(DAT__SZLOC)! Locator to the FITS extension.
      CHARACTER YORTYP*(IRI__SZYOR)! Type of YORIC image.


      INTEGER BAND               ! Waveband index.
      INTEGER CARD               ! Fits header card number.
      INTEGER CLEN               ! No. of characters per FITS header
                                 ! card.
      INTEGER FTSPNT             ! Pointer to the mapped array of FITS
                                 ! header cards.
      INTEGER I                  ! LOOP COUNT.
      INTEGER NCARD              ! No. of mapped FITS header cards.
      INTEGER NCHAR              ! No. of characters used.


      LOGICAL FLUX               ! True if PO image is a flux map.
      LOGICAL NOISE              ! True if PO image is a noise map.
      LOGICAL THERE              ! True if an object or keyword was
                                 ! found.


      REAL RITER                 ! YORIC iteration number.
      REAL RSKGRD                ! PO grid number.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise all returned information to null.
      DO I = 1, NITEM
         WORK( I, II ) = ' '
      END DO

*  See if there is a FITS extension in this NDF.
      CALL NDF_XSTAT( INDF, 'FITS', THERE, STATUS )

*  Report an error if the FITS extension does not exist.
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'PREPC3_ERR1',
     :              'PREPC3: ^NDF does not contain any FITS keywords',
     :                  STATUS       )
         GO TO 999
      END IF

*  Obtain a locator to the FITS extension.
      CALL NDF_XLOC( INDF, 'FITS', 'READ', FTSLOC, STATUS )

*  Map the FITS card images stored within the extension, and get the
*  number of characters in each one.
      CALL DAT_MAPV( FTSLOC, '_CHAR', 'READ', FTSPNT, NCARD, STATUS )
      CALL DAT_CLEN( FTSLOC, CLEN, STATUS )

*  See what type of input image we are dealing with. This is derived
*  from the FITS keyword INSTRUME. Note, the final argument is required
*  because of the way that UNIX handles character arrays. There is no
*  corresponding dummy argument in routine PREPA4. Character arrays
*  passed using %VAL must always come earlier in the argument list than
*  any other character strings.
      CALL PREPA4( NCARD, %VAL( FTSPNT ), WORK( 2, II ),  STATUS,
     :             %VAL( CLEN ) )

*  If this is a PO image,
      IF( WORK( 2, II ) .EQ. IRI__DSCO ) THEN

*  See what type of PO image it is.
         CALL PREPC7( NCARD, %VAL( FTSPNT ), FLUX, NOISE, STATUS,
     :                %VAL( CLEN ) )

*  Store the type of image.
         IF( NOISE ) THEN
            WORK( 1, II ) = 'NOISE'
            NNOISE = NNOISE + 1
         ELSE
            WORK( 1, II ) = 'DATA'
         END IF

*  Get the value of keyword DSKYGRID.
         CALL IRM_GKEYR( NCARD, %VAL( FTSPNT ), 1, 'DSKYGRID', THERE,
     :                   RSKGRD, CARD, STATUS, %VAL( CLEN ) )

*  If not found, report an error.
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'PREPC3_ERR2',
     :             'PREPC3: Cannot find FITS keyword DSKYGRID in ^NDF.',
     :                   STATUS )
            GO TO 999
         END IF

*  Store the grid number.
         CALL CHR_ITOC( NINT( RSKGRD ), WORK( 3, II ), NCHAR )

*  Store the waveband index.
         CALL PREPA8( NCARD, %VAL( FTSPNT ), BAND, STATUS,
     :                %VAL( CLEN ) )
         CALL CHR_ITOC( BAND, WORK( 4, II ), NCHAR )

*  Get the value of the FITS keyword BUNIT.
         CALL IRM_GKEYC( NCARD, %VAL( FTSPNT ), 1, 'BUNIT', THERE,
     :                   WORK( 5, II ), CARD, STATUS, %VAL( CLEN ) )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'PREPC3_ERR3',
     :             'PREPC3: Cannot find FITS keyword BUNIT in ^NDF.',
     :                   STATUS )
            GO TO 999
         END IF

*  If this is a YORIC image...
      ELSE IF( WORK( 2, II ) .EQ. IRI__YORIC ) THEN

*  See what type of YORIC image it is.
         CALL PREPC6( NCARD, %VAL( FTSPNT ), YORTYP, STATUS,
     :                %VAL( CLEN ) )

*  Store the type of image.
         IF( YORTYP .EQ. IRI__YOIMG ) THEN
            WORK( 1, II ) = 'DATA'
         ELSE IF( YORTYP .EQ. IRI__YOPHN ) THEN
            WORK( 1, II ) = 'NOISE'
            NNOISE = NNOISE + 1
         END IF

*  Get the value of the FITS keyword OBJECT.
         CALL IRM_GKEYC( NCARD, %VAL( FTSPNT ), 1, 'OBJECT', THERE,
     :                   WORK( 3, II ), CARD, STATUS, %VAL( CLEN ) )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'PREPC3_ERR4',
     :             'PREPC3: Cannot find FITS keyword OBJECT in ^NDF.',
     :                   STATUS )
            GO TO 999
         END IF

*  Get the value of the FITS keyword DATE.
         CALL IRM_GKEYC( NCARD, %VAL( FTSPNT ), 1, 'DATE', THERE,
     :                   WORK( 4, II ), CARD, STATUS, %VAL( CLEN ) )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'PREPC3_ERR5',
     :             'PREPC3: Cannot find FITS keyword DATE in ^NDF.',
     :                   STATUS )
            GO TO 999
         END IF

*  Get the values of the FITS keyword ITERNO
         CALL IRM_GKEYR( NCARD, %VAL( FTSPNT ), 1, 'ITERNO', THERE,
     :                   RITER, CARD, STATUS, %VAL( CLEN ) )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'PREPC3_ERR6',
     :             'PREPC3: Cannot find FITS keyword ITERNO in ^NDF.',
     :                   STATUS )
            GO TO 999
         END IF

*  Store it.
         CALL CHR_ITOC( NINT( RITER ), WORK( 5, II ), NCHAR )

*  Get the waveband index and store it.
         CALL PREPA8( NCARD, %VAL( FTSPNT ), BAND, STATUS,
     :                %VAL( CLEN ) )
         CALL CHR_ITOC( BAND, WORK( 6, II ), NCHAR )

*  Get the value of the FITS keyword BUNIT.
         CALL IRM_GKEYC( NCARD, %VAL( FTSPNT ), 1, 'BUNIT', THERE,
     :                   WORK( 7, II ), CARD, STATUS, %VAL( CLEN ) )
         IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'PREPC3_ERR7',
     :             'PREPC3: Cannot find FITS keyword BUNIT in ^NDF.',
     :                   STATUS )
            GO TO 999
         END IF

      END IF

*  Annul the locator to the FITS extension.
 999  CALL DAT_ANNUL( FTSLOC, STATUS )

      END
