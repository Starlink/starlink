      SUBROUTINE PREPA3( PFACT, PHIST, INDF1, INDF2, NDFOUT, TITLE,
     :                   LABEL, FLDLON, FLDLAT, SCS, PROJ, UNITS, IGRP,
     :                   STATUS )
*+
*  Name:
*     PREPA3

*  Purpose:
*     Process an input NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA3( PFACT, PHIST, INDF1, INDF2, NDFOUT, TITLE, LABEL,
*                  FLDLON, FLDLAT, SCS, PROJ, UNITS, IGRP, STATUS )

*  Description:
*     This routine process a single input data array and an optional
*     noise grid, producing one or two output NDFs depending on whether
*     the input is a CPC image or not. It can also handle

*  Arguments:
*     PFACT = CHARACTER * ( * ) (Given)
*        The parameter to use for getting factors for converting input
*        units to output units in the case of either system of units
*        being unknown.
*     PHIST = CHARACTER * ( * ) (Given)
*        The parameter to use for deciding if history is to be included
*        in the output NDF.
*     INDF1 = INTEGER (Given)
*        The identifier for the input NDF holding the data which is to
*        be stored in the output data arrays.
*     INDF2 = INTEGER (Given)
*        The identifier for the input NDF holding the data which is to
*        be stored in the output variance arrays. If this is supplied
*        equal to NDF__NOID then no variance array will be included in
*        the output NDFs.
*     NDFOUT = CHARACTER * ( * ) (Given)
*        The name of the output NDF. If the input is a CPC image, the
*        prefixes _50 (for the the 50um data) and _100 (for the 100um
*        data) are appended to the string given by NDFOUT to generate
*        the names of the two output NDFs.
*     TITLE = CHARACTER * ( * ) (Given)
*        The title for the output NDF. If a blank string is supplied
*        than a default title is created from the image type and the
*        value of the FITS keyword "OBJECT". If the input is a CPC
*        image, the prefixes " (50um)" (for the the 50um data) and "
*        (100um)" (for the 100um data) are appended to the string to
*        generate the titles for the two output NDFs.
*     LABEL = CHARACTER * ( * ) (Given)
*        The label for the output NDF. If a blank string is supplied
*        than a default label is used descriing the content of the data
*        array. If the input is a CPC image, the prefixes " (50um)"
*        (for the the 50um data) and " (100um)" (for the 100um data)
*        are appended to the string to generate the labels for the two
*        output NDFs.
*     FLDLON = CHARACTER * ( * ) (Given)
*        The longitude value to be stored as the field position (as a
*        formatted string). If a blank is supplied, then the value of
*        FITS keyword CRVAL1 is used.
*     FLDLAT = CHARACTER * ( * ) (Given)
*        The latitude value to be stored as the field position (as a
*        formatted string). If a blank is supplied, then the value of
*        FITS keyword CRVAL2 is used.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system to which FLDLON and FLDLAT refer.
*        This is also the sky coordinate system assumed for the CRVAL1
*        and CRVAL2 keywords if the input image type is unrecognised.
*     PROJ = CHARACTER * ( * ) (Given)
*        The projection type to assume if the input image type is
*        unrecognised.
*     UNITS = CHARACTER * ( * ) (Given)
*        The system of units in which the output data array is to be
*        produced. If a blank value is supplied, then the output is in
*        the same units as the input, whatever they may be.
*     IGRP = INTEGER (Given)
*        An identifier for a group into which the names of succesfully
*        created output NDFs are put.
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
      INCLUDE 'DAT_PAR'          ! DAT_ constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Arguments Given:
      CHARACTER PHIST*(*)
      CHARACTER PFACT*(*)
      INTEGER INDF1
      INTEGER INDF2
      CHARACTER NDFOUT*(*)
      CHARACTER TITLE*(*)
      CHARACTER LABEL*(*)
      CHARACTER FLDLON*(*)
      CHARACTER FLDLAT*(*)
      CHARACTER SCS*(*)
      CHARACTER PROJ*(*)
      CHARACTER UNITS*(*)
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  External Routines:
      INTEGER CHR_LEN            ! Used length of a string.

*  Local Variables:
      CHARACTER FTSLOC*(DAT__SZLOC) ! Locator to FITS extension.
      CHARACTER L*(GRP__SZNAM+8) ! Label for output NDF.
      CHARACTER N*(GRP__SZNAM+3) ! Name of output NDF.
      CHARACTER T*(GRP__SZNAM+8) ! Title for output NDF.
      CHARACTER TYPE*(IRI__SZTYP)! Type of input image.


      INTEGER BAND               ! Survey array waveband index.
      INTEGER CLEN               ! No. of characters per FITS header
                                 ! card.
      INTEGER FTSPNT             ! Pointer to mapped array of FITS
                                 ! header cards.
      INTEGER INDF3              ! Identifier for NDF section holding
                                 ! CPC 50um data.
      INTEGER INDF4              ! Identifier for NDF section holding
                                 ! CPC 100um data.
      INTEGER LLEN               ! Used length of variable L.
      INTEGER NCARD               ! Number of mapped FITS header cards.
      INTEGER NLEN               ! Used length of variable N.
      INTEGER TLEN               ! Used length of variable T.


      LOGICAL THERE              ! True if the input NDF contains a FITS
                                 ! extension.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if there is a FITS extension in this NDF.
      CALL NDF_XSTAT( INDF1, 'FITS', THERE, STATUS )

*  Report an error if the FITS extension does not exist.
      IF( .NOT. THERE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PREPA3_ERR1',
     :      'PREPA3: The input NDF does not contain any FITS keywords',
     :                  STATUS       )
         GO TO 999
      END IF

*  Obtain a locator to the FITS extension.
      CALL NDF_XLOC( INDF1, 'FITS', 'READ', FTSLOC, STATUS )

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
      CALL PREPA4( NCARD, %VAL( FTSPNT ), TYPE, STATUS, %VAL( CLEN ) )

*  Get the label and title for outputs associated with this input NDF.
      CALL PREPA5( NCARD, %VAL( FTSPNT ), TYPE, LABEL, TITLE, STATUS,
     :             %VAL( CLEN ) )

*  If this is a CPC image, the single input NDF creates two output NDFs,
*  one for the 50um band and one for the 100um band.
      IF( TYPE .EQ. IRI__CPC ) THEN

*  Create identifiers for NDF sections holding the two planes of the
*  input NDF.
         CALL PREPA6( INDF1, INDF3, INDF4, STATUS )

*  Save the used lengths of the output NDF name, title and label.
         NLEN = CHR_LEN( NDFOUT )
         TLEN = CHR_LEN( TITLE )
         LLEN = CHR_LEN( LABEL )

*  Create 50um output NDF name, title and label.
         N = NDFOUT( : NLEN )//'_50'
         T = TITLE( : TLEN )//' (50 um)'
         L = LABEL( : LLEN )//' (50 um)'

*  Create the 50um (band 1) output NDF.
         CALL PREPA7( NCARD, %VAL( FTSPNT ), PFACT, TYPE, 1, INDF3,
     :                INDF2, N, T, L, FLDLON, FLDLAT, SCS, PROJ, UNITS,
     :                IGRP, PHIST, STATUS, %VAL( CLEN ) )

*  Create 100um output NDF name, title and label.
         N = NDFOUT( : NLEN )//'_100'
         T = TITLE( : TLEN )//' (100 um)'
         L = LABEL( : LLEN )//' (100 um)'

*  Create the 100um (band 2) output NDF.
         CALL PREPA7( NCARD, %VAL( FTSPNT ), PFACT, TYPE, 2, INDF4,
     :                INDF2, N, T, L, FLDLON, FLDLAT, SCS, PROJ, UNITS,
     :                IGRP, PHIST, STATUS, %VAL( CLEN ) )

*  Annull the two NDF section identifiers.
         CALL NDF_ANNUL( INDF3, STATUS )
         CALL NDF_ANNUL( INDF4, STATUS )

*  If this is a survey array image, there will only be one output NDF.
      ELSE

*  Get the waveband index.
         CALL PREPA8( NCARD, %VAL( FTSPNT ), BAND, STATUS, %VAL( CLEN ))

*  Create the output NDF.
         CALL PREPA7( NCARD, %VAL( FTSPNT ), PFACT, TYPE, BAND, INDF1,
     :                INDF2, NDFOUT, TITLE, LABEL, FLDLON, FLDLAT, SCS,
     :                PROJ, UNITS, IGRP, PHIST, STATUS, %VAL( CLEN ) )

      END IF

*  Annul the locator to the FITS extension.
 999  CONTINUE
      CALL DAT_ANNUL( FTSLOC, STATUS )

      END
