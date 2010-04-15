      SUBROUTINE IRC_IMPRT( INDF, IDC, STATUS )
*+
*  Name:
*     IRC_IMPRT

*  Purpose:
*     Import an existing NDF containing CRDD into the IRC_ system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_IMPRT( INDF, IDC, STATUS )

*  Description:
*     This routine checks that the given NDF is two dimensional and
*     contains an IRAS extension, which in turn contains a CRDD_INFO
*     component. If it doesn't, an error is reported. If it does, a
*     check is made that the NDF contains CRDD of a recognised type.  A
*     record is then made of the CRDD file in the IRC_ common blocks
*     and an IRC identifier returned for this record. This identifier
*     can be passed to other IRC routines to access the CRDD
*     descriptive information. The bulk data values (contained in the
*     NDF DATA component) should be accessed using the normal NDF_
*     routines (see SUN/33).

*  Arguments:
*     INDF = INTEGER (Given)
*        The NDF identifier for the CRDD file. This can be an NDF
*        section so long as the bounds of the second dimension do not
*        extend beyond the bounds of the second dimension of the base
*        NDF.
*     IDC  = INTEGER (Returned)
*        The IRC identifier for the CRDD information.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JAN-1991 (DSB):
*        Original version.
*     9-MAY-1991 (DSB):
*        Updated for IRA version 2.
*     11-FEB-1992 (DSB):
*        CRDD_INFO component DET_ORIGIN added.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Write)
*           True if the corresponding IRC identifier is valid.
*        CCM_IRASL( IRC__MAX ) = CHARACTER (Write)
*           HDS locator for IRAS extension.
*        CCM_CRDDL( IRC__MAX ) = CHARACTER (Write)
*           HDS locator for CRDD_INFO component.
*        CCM_POINT( IRC__MAX ) = LOGICAL (Write)
*           True if pointing information has been generated.
*        CCM_BAND( IRC__MAX ) = INTEGER (Write)
*           IRAS band number (NOT wavelength).
*        CCM_REFRA( IRC__MAX ) = DOUBLE PRECISION (Write)
*           RA (B1950) of the reference point.
*        CCM_REFDE( IRC__MAX ) = DOUBLE PRECISION (Write)
*           DEC (B1950) of the reference point.
*        CCM_SOP( IRC__MAX ) = INTEGER (Write)
*           SOP number.
*        CCM_OBS( IRC__MAX ) = INTEGER (Write)
*           OBS number.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Write)
*           HDS type of the DETAILS component.
*        CCM_NOMSP( IRC__MAX ) = REAL (Write)
*           Nominal scan speed.
*        CCM_SLOW( IRC__MAX ) = INTEGER (Write)
*           Lowest sample number in the DATA array.
*        CCM_SHIGH( IRC__MAX ) = INTEGER (Write)
*           Highest sample number in the DATA array.
*        CCM_DLOW( IRC__MAX ) = INTEGER (Write)
*           Lowest detector index in the DATA array.
*        CCM_DHIGH( IRC__MAX ) = INTEGER (Write)
*           Highest detector index in the DATA array.
*        CCM_DETNO( IRC__MXD2S, IRC__MAX ) = INTEGER (Write)
*           The detector number corresponding to each detector index.
*        CCM_DETOR( IRC__MAX ) = INTEGER (Write)
*           The index within CCM_DETNO corresponding to row zero of the
*           NDF.

*  Arguments Given:
      INTEGER INDF

*  Arguments Returned:
      INTEGER IDC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BAND               ! The IRAS band number.
      CHARACTER CLOC*(DAT__SZLOC)! Locator for the CRDD_INFO object.
      INTEGER DETNO              ! Detector number.
      INTEGER DNLBND             ! NDF 2nd dimension corresponding to
                                 ! the start of the DET_NUMBERS array.
      INTEGER DNUBND             ! NDF 2nd dimension corresponding to
                                 ! the end of the DET_NUMBERS array.
      INTEGER DSIZE              ! Size of the DET_NUMBERS array.
      INTEGER INDEX              ! Index into the DET_NUMBERS array.
      INTEGER LBND( NDF__MXDIM ) ! Lower bound of each NDF dimension.
      INTEGER NDIM               ! No. of dimensions in the object.
      INTEGER SIZE2              ! The size of the 2nd NDF dimension.
      LOGICAL THERE              ! True if the object exists.
      INTEGER UBND( NDF__MXDIM ) ! Upper bound of each NDF dimension.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the next free IRC identifier.
      CALL IRC1_GETID( IDC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If the IRAS extension doesn't exist in the specified NDF, give a
*  message and quit.
      CALL NDF_XSTAT( INDF, IRC__XNAME, THERE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF( .NOT. THERE ) THEN
         STATUS = IRC__NOEXT
         CALL ERR_REP( 'IRC_IMPRT_ERR1',
     :              'IRC_IMPRT: NDF does not contain an IRAS extension',
     :                 STATUS )
         GO TO 999

      END IF

*  If the extension does exist, get a locator to it.
      CALL NDF_XLOC( INDF, IRC__XNAME, 'READ', CCM_IRASL( IDC ),
     :               STATUS )

*  See if the "CRDD_INFO" component of the IRAS extension exists. (The
*  actual component name is stored in the symbolic constant IRC__CNAME.)
      CALL DAT_THERE( CCM_IRASL( IDC ), IRC__CNAME, THERE, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF( .NOT. THERE ) THEN
         STATUS = IRC__NOCCM
         CALL ERR_REP( 'IRC_IMPRT_ERR2',
     :           'IRC_IMPRT: No CRDD information found in NDF', STATUS )
         GO TO 999

      END IF

*  If it does exist get a locator to it and store it in common.
      CALL DAT_FIND( CCM_IRASL( IDC ), IRC__CNAME, CLOC, STATUS )
      CCM_CRDDL( IDC ) = CLOC

*  Check that the NDF does not have more than two dimensions.
      CALL NDF_BOUND( INDF, NDF__MXDIM, LBND, UBND, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF( NDIM .GT. 2 ) THEN
         STATUS = IRC__NOT2D
         CALL ERR_REP( 'IRC_IMPRT_ERR3',
     :                 'IRC_IMPRT: NDF has more than two dimensions',
     :                 STATUS )
         GO TO 999

      END IF

*  Check that the second dimension is not too big.
      SIZE2 = UBND( 2 ) - LBND( 2 ) + 1
      IF( SIZE2 .GT. IRC__MXD2S ) THEN
         STATUS = IRC__D2BIG
         CALL MSG_SETI( 'DET', SIZE2 )
         CALL MSG_SETI( 'MAX', IRC__MXD2S )
         CALL ERR_REP( 'IRC_IMPRT_ERR4',
     :     'IRC_IMPRT: NDF contains ^DET rows. Maximum allowed for a '//
     :     'CRDD file is ^MAX', STATUS )
         GO TO 999

      END IF

*  Store the NDF bounds in common.
      CCM_SLOW( IDC ) = LBND( 1 )
      CCM_SHIGH( IDC ) = UBND( 1 )
      CCM_DLOW( IDC ) = LBND( 2 )
      CCM_DHIGH( IDC ) = UBND( 2 )

*  Check that NDF component UNITS has a legal value.
      CALL IRC1_CHKUN( INDF, STATUS )

*  Check that NDF component LABEL has a legal value.
      CALL IRC1_CHKLA( INDF, STATUS )

*  Get the HDS type of the DETAILS component and store in common. This
*  identifies the type of pointing information. The actual name is
*  stored in symbolic constant IRC__DNAME.
      CALL CMP_TYPE( CLOC, IRC__DNAME, CCM_TYPE( IDC ), STATUS )

*  Check that the CRDD type is recognised.
      IF( CCM_TYPE( IDC ) .NE. 'SURVEY_BSIGHT'
     :    .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC_IMPRT_ERR5',
     :                 'IRC_IMPRT: CRDD type "^T" is unrecognised',
     :                 STATUS )
         GO TO 999
      END IF

*  Get the IRAS band number and check it is valid.
      CALL CMP_GET0I( CLOC, 'BAND', BAND, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      IF( BAND .LE. 0 .OR. BAND .GT. I90__BANDS ) THEN
         STATUS = IRC__BADBN
         CALL MSG_SETI( 'B', BAND )
         CALL ERR_REP( 'IRC_IMPRT_ERR6',
     :                 'IRC_IMPRT: Invalid IRAS band number found: ^B',
     :                 STATUS )
         GO TO 999

*  If it is, store it in common.
      ELSE
         CCM_BAND( IDC ) = BAND

      END IF

*  Get the value of the DET_ORIGIN component and store in common.
      CALL CMP_GET0I( CLOC, 'DET_ORIGIN', CCM_DETOR( IDC ), STATUS )

*  Get the size of the DET_NUMBERS component and convert to the
*  corresponding NDF bounds.
      CALL CMP_SHAPE( CLOC, 'DET_NUMBERS', 1, DSIZE, NDIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

      DNLBND = 1 - CCM_DETOR( IDC )
      DNUBND = DSIZE - CCM_DETOR( IDC )

*  Report an error if there are any rows in the NDF for which there is
*  no corresponding detector number.
      IF( DNLBND .GT. LBND( 2 ) .OR. DNUBND .LT. UBND( 2 ) ) THEN
         STATUS = IRC__BADDT
         CALL ERR_REP( 'IRC_IMPRT_ERR7',
     :     'IRC_IMPRT: CRDD file contains data for which there is no '//
     :     'corresponding detector.', STATUS )
         GO TO 999
      END IF

*  Copy the detector numbers from the DET_NUMBER component to common.
      CALL CMP_GET1I( CLOC, 'DET_NUMBERS', DSIZE, CCM_DETNO( 1, IDC ),
     :                DSIZE, STATUS )

*  Store the detector index for each detector number, or a bad value if
*  the detector number isn't present in the CRDD file. Give an error if
*  any detector number is found more than once, or if any detector does
*  not belong to the correct band. Detector index values start from the
*  lower bound of the 2nd dimension of the NDF, whereas the values
*  stored in CCM_DETNO start at 1. Therefore the CCM_DETNO index must be
*  shifted before being stored as the detector index.
      DO DETNO = 1, I90__DETS
         CCM_DETIN( DETNO, IDC ) = VAL__BADI
      END DO

      DO INDEX = 1, DSIZE
         DETNO = CCM_DETNO( INDEX, IDC )

         IF( CCM_DETIN( DETNO, IDC ) .EQ. VAL__BADI ) THEN
            IF( I90__DBAND( DETNO ) .EQ. BAND ) THEN
               CCM_DETIN( DETNO, IDC ) = INDEX - CCM_DETOR( IDC )

            ELSE
               STATUS = IRC__BADDB
               CALL MSG_SETI( 'DET', DETNO )
               CALL MSG_SETI( 'WL', I90__WAVEL( BAND ) )
               CALL ERR_REP( 'IRC_IMPRT_ERR8',
     :             'IRC_IMPRT: Detector #^DET is not a ^WL um detector',
     :                       STATUS )
               GO TO 999

            END IF

         ELSE
            STATUS = IRC__MULDE
            CALL MSG_SETI( 'DET', DETNO )
            CALL ERR_REP( 'IRC_IMPRT_ERR9',
     :                 'IRC_IMPRT: Detector #^DET found more than once',
     :                    STATUS )

            GO TO 999

         END IF

      END DO

*  Get the other CRDD_INFO components.
      CALL CMP_GET0D( CLOC, 'REF_RA', CCM_REFRA( IDC ), STATUS )
      CALL CMP_GET0D( CLOC, 'REF_DEC', CCM_REFDE( IDC ), STATUS )
      CALL CMP_GET0I( CLOC, 'SOP', CCM_SOP( IDC ), STATUS )
      CALL CMP_GET0I( CLOC, 'OBS', CCM_OBS( IDC ), STATUS )
      CALL CMP_GET0R( CLOC, 'NOM_SPEED', CCM_NOMSP( IDC ), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Check that the scan speed is not zero.
      IF( CCM_NOMSP( IDC ) .EQ. 0.0 ) THEN
         STATUS = IRC__ZEROS
         CALL ERR_REP( 'IRC_IMPRT_ERR10',
     :            'IRC_IMPRT: CRDD file claims to have zero scan speed',
     :                  STATUS )

         GO TO 999
      END IF

*  Set a flag in common to indicate that no pointing info has yet been
*  requested from the CRDD file.
      CCM_POINT( IDC ) = .FALSE.

*  Finally, mark the IRC identifier as valid.
      CCM_VALID( IDC ) = .TRUE.

*  If an error occured, annul the IRC identifier and give a contextual
*  message.
 999  CONTINUE
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL IRC_ANNUL( IDC, STATUS )
         CALL NDF_MSG( 'NDF', INDF )
         CALL ERR_REP( 'IRC_IMPRT_ERR11',
     :                 'IRC_IMPRT: Not a valid CRDD file: ^NDF',
     :                  STATUS )
      END IF

      END
