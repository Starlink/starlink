      SUBROUTINE PREPC6( NCARD, FITS, IMGTYP, STATUS )
*+
*  Name:
*     PREPC6

*  Purpose:
*     See what type of YORIC image is being handled.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPC6( NCARD, FITS, IMGTYP, STATUS )

*  Description:
*     The IPAC YORIC/HIRES processor produces files of several different
*     types holding different quantities. This routine identifies the
*     particular type of YORIC image being handled by looking at the
*     comments held at the end of the FITS header.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     IMGTYP = CHARACTER * ( * ) (Returned)
*        The type of YORIC image, returned equal to one of the
*        constants:
*           IRI__YOCFV: Correction Factor Variance maps
*           IRI__YOCVG: Coverage maps
*           IRI__YORES: Resolution maps
*           IRI__YOPHN: Photometric noise maps
*           IRI__YOIMG: Surface brightness maps
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
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)

*  Arguments Returned:
      CHARACTER IMGTYP*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      CHARACTER CFV*26           ! String identifying a correction
                                 ! factor variance map.
      CHARACTER CVG*8            ! String identifying a coverage map.
      CHARACTER PHN*17           ! String identifying a photometric
                                 ! noise map.
      CHARACTER RES*10           ! String identifying a resolution
                                 ! map.
      PARAMETER ( CFV = 'CORRECTION FACTOR VARIANCE' )
      PARAMETER ( CVG = 'COVERAGE' )
      PARAMETER ( PHN = 'PHOTOMETRIC NOISE' )
      PARAMETER ( RES = 'RESOLUTION' )


*  Local Variables:
      CHARACTER TEXT*80          ! Text from a FITS header card.

      INTEGER CARD               ! Card number at which a comment was
                                 ! found.
      INTEGER START              ! Card number at which to start
                                 ! searching for a comment.

      LOGICAL OK                 ! True if units are recognised.
      LOGICAL THERE              ! True if a comment was found.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned image type as unknown.
      IMGTYP = ' '

*  Find the FITS keyword VERSION.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'VERSION', THERE, TEXT, CARD,
     :                STATUS )

*  If the VERSION keyword was not found, report an error.
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PREPC6_ERR1',
     :                 'PREPC6: Input is not a YORIC image.',
     :                 STATUS )
         GO TO 999
      END IF

*  Search through the comment cards occurring after the VERSION keyword
*  for each of the strings used to identify the different sort of HIRES
*  images.
      START = CARD + 1
      DO WHILE( THERE )
         CALL IRM_COMNT( NCARD, FITS, START, THERE, TEXT, CARD, STATUS )
         START = CARD + 1

         IF( INDEX( TEXT, CFV ) .NE. 0 ) THEN
            IMGTYP = IRI__YOCFV
            THERE = .FALSE.

         ELSE IF( INDEX( TEXT, CVG ) .NE. 0 ) THEN
            IMGTYP = IRI__YOCVG
            THERE = .FALSE.

         ELSE IF( INDEX( TEXT, RES ) .NE. 0 ) THEN
            IMGTYP = IRI__YORES
            THERE = .FALSE.

         ELSE IF( INDEX( TEXT, PHN ) .NE. 0 ) THEN
            IMGTYP = IRI__YOPHN
            THERE = .FALSE.

         END IF

      END DO

*  Get the value of FITS keyword BUNIT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'BUNIT', THERE, TEXT, CARD,
     :                STATUS )

*  Remove blanks and convert to upper case.
      CALL CHR_RMBLK( TEXT )
      CALL CHR_UCASE( TEXT )

*  See if the units are one of the IRI standard system of units.
      CALL IRI_CHECK( TEXT, OK, STATUS )

*  If it is, and if no image type was specified by the comments
*  following the VERSION keyword, assume the image is an intensity
*  (IMG) map.
      IF( OK .AND. IMGTYP .EQ. ' ' ) IMGTYP = IRI__YOIMG

*  Finish.
 999  CONTINUE

      END
