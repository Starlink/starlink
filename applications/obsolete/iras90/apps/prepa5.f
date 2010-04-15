      SUBROUTINE PREPA5( NCARD, FITS, TYPE, LABEL, TITLE, STATUS )
*+
*  Name:
*     PREPA5

*  Purpose:
*     Get the title and label to store in the output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA5( NCARD, FITS, TYPE, LABEL, TITLE, STATUS )

*  Description:
*     If TITLE and/or LABEL hold non-blank values on entry then those
*     values are returned as the TITLE and/or label to be stored in the
*     output NDF. If either of them are supplied with blank values,
*     then new values are created and returned in place of the blank
*     values; the title is made up from the image type and the value of
*     the OBJECT keyword, and the label is set to a value which
*     describes the quantity measured by the data array.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     TYPE = CHARACTER * ( * ) (Given)
*        The image type. This should be equal to one of the symbolic
*        constants defined within the IRI subsystem.
*     LABEL = CHARACTER * ( * ) (Given and Returned)
*        The label to be stored in the output NDF. If a blank value is
*        supplied then a new, non-blank value is returned.
*     TITLE = CHARACTER * ( * ) (Given and Returned)
*        The title to be stored in the output NDF. If a blank value is
*        supplied then a new, non-blank value is returned.
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
      INCLUDE 'IRI_PAR'          ! IRI_ constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER TYPE*(*)

*  Arguments Given and Returned:
      CHARACTER LABEL*(*)
      CHARACTER TITLE*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER SUBTYP*(IRI__SZYOR)! Type of YORIC image.
      CHARACTER TEXT*80          ! Value of FITS keyword.

      INTEGER CARD               ! Index of the FITS header card which
                                 ! contained the requested keyword.
      INTEGER IAT                ! Position of last non-blank character.

      LOGICAL FLUX               ! True if image is a PO flux map.
      LOGICAL NOISE              ! True if image is a PO noise map.
      LOGICAL THERE              ! True if the keyword was found.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If a title was supplied, don't replace it.
      IF( TITLE .EQ. ' ' ) THEN

*  Otherwise, get the 'OBJECT' keyword and remove leading blanks.
         CALL IRM_GKEYC( NCARD, FITS, 1, 'OBJECT', THERE, TEXT, CARD,
     :                STATUS )
         CALL CHR_LDBLK( TEXT )
         IF( .NOT. THERE ) TEXT = ' '

*  Contruct the title from the image type and object name.
         IF( TYPE .NE. IRI__NONAM ) THEN
            IAT = 0
            CALL CHR_APPND( TYPE, TITLE, IAT )
            CALL CHR_APPND( ' image', TITLE, IAT )
            IF( TEXT .NE. ' ' ) THEN
               CALL CHR_APPND( ':', TITLE, IAT )
               CALL CHR_APPND( ' '//TEXT, TITLE, IAT )
            END IF

*  If the image type is unknown, just use the object text.
         ELSE
            TITLE = TEXT
         END IF

      END IF

*  If a label was supplied, don't replace it.
      IF( LABEL .EQ. ' ' ) THEN

*  If the input NDF is a PO map, see if it is a flux, intensity or
*  noise map.
         IF( TYPE .EQ. IRI__DSCO ) THEN
            CALL PREPC7( NCARD, FITS, FLUX, NOISE, STATUS )

*  If it is a flux map, use the label "Point source brightness". If it
*  is a intensity map, use "Surface brightness".
            IF( FLUX ) THEN
               LABEL = 'Point source brightness'
               IAT = 23
            ELSE
               LABEL = 'Surface brightness'
               IAT = 18
            END IF

*  If it is a noise map, add "error" to the end of the label.
            IF( NOISE ) CALL CHR_APPND( ' error', LABEL, IAT )

*  If the input NDF is a SKYFLUX weight image, use the label
*  "Statistical weight".
         ELSE IF( TYPE .EQ. IRI__SKYFL .AND. TEXT .EQ. ' ' ) THEN
            LABEL = 'Statistical weight'

*  If the input NDF is a YORIC image, see what type of YORIC image it
*  is.
         ELSE IF( TYPE .EQ. IRI__YORIC ) THEN
            CALL PREPC6( NCARD, FITS, SUBTYP, STATUS )

*  Photometric noise maps are given the label "photometric noise".
            IF( SUBTYP .EQ. IRI__YOPHN ) THEN
               LABEL = 'Photometric noise'

*  Coverage maps are given the label "Pixel coverage".
            ELSE IF( SUBTYP .EQ. IRI__YOCVG ) THEN
               LABEL = 'Pixel coverage'

*  Intensity maps are given the label "Surface brightness".
            ELSE IF( SUBTYP .EQ. IRI__YOIMG ) THEN
               LABEL = 'Surface brightness'

*  Resolution maps are given the label "Resolution".
            ELSE IF( SUBTYP .EQ. IRI__YORES ) THEN
               LABEL = 'Resolution'

*  Correction factor variance maps are given the label "Correction
*  factor variance".
            ELSE IF( SUBTYP .EQ. IRI__YOCFV ) THEN
               LABEL = 'Correction factor variance'

            END IF

*  If any other sort of known input image is supplied, use a label
*  "Surface brightness". If the input NDF is of an unknown type,
*  don't give it any label.
         ELSE IF( TYPE .NE. IRI__NONAM ) THEN
            LABEL = 'Surface brightness'

         END IF

      END IF

      END
