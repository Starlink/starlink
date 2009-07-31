      SUBROUTINE COLCORR( STATUS )
*+
*  Name:
*     COLCORR

*  Purpose:
*     Calculate colour corrected surface brightness images.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL COLCORR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine takes two aligned images, one holding colour
*     temperature values and the other holding optical depth values,
*     and creates an output image holding the corresponding surface
*     brightness values, colour corrected for the central wavelength of
*     any of the four IRAS wavebands. The temperature and optical depth
*     images can be created by COLTEMP.
*
*     The calculation of colour corrected surface brightness values is
*     based on the assumption that all sources have greybody spectra
*     with a constant emissivity spectral index, BETA. The value of BETA
*     used is the same as was used when the optical depth image given
*     for parameter TAU was created. It is assumed that all sources are
*     optically thin.
*     
*     The input NDFs should be aligned pixel-for-pixel. If the bounds
*     of the two NDFs do not match, the output image covers just the
*     overlap area. Any QUALITY component present in the temperature
*     NDF is propagated to the output NDF. All extensions are
*     propagated from the temperature NDF.
*
*     Variances for the calculated surface brightness values are
*     created if both input NDFs have VARIANCE components.

*  Usage:
*     COLCORR TEMP TAU BAND OUT

*  ADAM Parameters:
*     BAND = _INTEGER (Read)
*        The wavelength (in microns) of the IRAS survey band for which
*        colour corrected data is to be generated (12, 25, 60 or 100).
*        Note, the value specified need not necessarily be the same as
*        that supplied for the WAVEL parameter of application COLTEMP.
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output NDF. See help on "History_in_IRAS90" for more
*        information on history. 
*                                              [current history setting]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     OUT = NDF (Write)
*        The colour corrected output image, in units of MJy/sr.
*     TAU = NDF (Read)
*        An optical depth image, in units of 1.0E-16 as created by
*        COLTEMP. Note, it is assumed that these values are much less
*        than unity (i.e. all sources are optically thin). A warning is
*        issued if this is not the case.
*     TEMP = NDF (Read)
*        A colour temperature image, as created by COLTEMP.

*  Examples:
*     COLCORR M51_TEMP M51_TAU 12 M51_CC12
*         This example generates a 12 micron colour corrected image
*         called M51_CC12 from the temperature map M51_TEMP and the
*         optical depth map M51_TAU. COLTEMP would previously have been
*         run to create the temperature and optical depth images.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1993 (DSB):
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
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'I90_DAT'          ! IRAS90 data.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER
     :      COLCOR*40,           ! Comment for IMAGE_INFO.
     :      LOC*(DAT__SZLOC),    ! Locator to IMAGE_INFO.
     :      UNITS*(IRI__SZUNI),  ! Units from temperature NDF.
     :      XLOC*(DAT__SZLOC)    ! Locator to IRAS extension.

      DOUBLE PRECISION
     :      CC,                  ! Constant used to get optical depth.
     :      NU                   ! Central frequency of selected band.

      INTEGER
     :      BAND,                ! Output waveband index.
     :      EL,                  ! No. of elements in a mapped array.
     :      INDF1,               ! Identifier for input temp NDF.
     :      INDF2,               ! Identifier for input opt. depth NDF.
     :      INDF3,               ! Identifier for output NDF.
     :      IPO,                 ! Pointer to mapped data array from
                                 ! optical depth input NDF.
     :      IPOUT,               ! Pointer to mapped data array from
                                 ! output NDF.
     :      IPT,                 ! Pointer to mapped data array from
                                 ! temperature input NDF.
     :      IPVO,                ! Pointer to mapped variance array from
                                 ! optical depth input NDF.
     :      IPVOUT,              ! Pointer to mapped varaince array from
                                 ! output NDF.
     :      IPVT                 ! Pointer to mapped variance array from
                                 ! temperature input NDF.

      LOGICAL
     :      BAD,                 ! True if there are any bad values in
                                 ! the output.
     :      VAR                  ! True if output variances are to be
                                 ! created.

      REAL
     :      BETA,                ! Emisivity spectral index.
     :      WAVEL0               ! Wavelength at which optical depth was
                                 ! created, in microns.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the filter level for conditional message output.
      CALL MSG_IFGET( STATUS )

*  Start a new NDF context.
      CALL NDF_BEGIN

*  Get the temperature image.
      CALL NDF_ASSOC( 'TEMP', 'READ', INDF1, STATUS )

*  Get the data units.
      CALL NDF_CGET( INDF1, 'UNITS', UNITS, STATUS )

*  Abort if they are not "K" (Kelvin).
      IF( UNITS .NE. 'K' .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'U', UNITS )
         CALL ERR_REP( 'COLCORR_ERR1',
     :'COLCORR: Temperature map has unknown units "^U". Should be "K".',
     :                 STATUS )      
         GO TO 999
      END IF

*  Get the optical depth image.
      CALL NDF_ASSOC( 'TAU', 'READ', INDF2, STATUS )

*  Extract the wavelength at which the optical depth was calculated from
*  the IRAS extension.
      CALL NDF_XGT0R( INDF2, 'IRAS', 'TAU_WAVELEN', WAVEL0, STATUS )

*  Extract the emissivity spectral index from the IRAS extension.
      CALL NDF_XGT0R( INDF2, 'IRAS', 'TAU_', BETA, STATUS )

*  Tell the user.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      CALL MSG_SETR( 'B', BETA )
      CALL MSG_OUTIF( MSG__NORM, 'COLCORR_MSG1',
     :   '  Using emissivity spectral index, BETA = ^B', STATUS )
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Now create sections from the two input images which cover the region
*  of overlap between the two images. This ensure that the two data
*  arrays from which output flux densities are derived have the same
*  shape and size.
      CALL NDF_MBND( 'TRIM', INDF1, INDF2, STATUS )

*  Set a flag if both input NDFs have defined variances.
      CALL NDF_STATE( INDF1, 'VAR', VAR, STATUS )
      IF( VAR ) CALL NDF_STATE( INDF2, 'VAR', VAR, STATUS )

*  Map the DATA components of both input NDFs.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IPT, EL, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'READ', IPO, EL, STATUS )

*  If required, map the VARIANCE components of both input NDFs.
      IF( VAR ) THEN
         CALL NDF_MAP( INDF1, 'VAR', '_REAL', 'READ', IPVT, EL,
     :                 STATUS )
         CALL NDF_MAP( INDF2, 'VAR', '_REAL', 'READ', IPVO, EL,
     :                 STATUS )
      END IF

*  Obtain the wavelength at which the flux density is to be calculated.
      CALL IRM_GTBND( 'BAND', 0, .FALSE., BAND, STATUS )

*  Create the output by propagation from the temperature input.  The
*  QUALITY component is the only component propagated (together with
*  all extensions).
      CALL NDF_PROP( INDF1, 'NOHISTORY,NOLABEL,NOTITLE,QUALITY', 'OUT',
     :               INDF3, STATUS )

*  Create an IMAGE_INFO structure within the output IRAS extension.
      CALL IRI_NEW( INDF3, 'SURVEY', BAND, IRI__COLC, IRI__MJPS, LOC,
     :              STATUS )
      WRITE(COLCOR,*) 'Greybody, BETA = ',BETA
      CALL DAT_NEW0C( LOC, 'COLCOR', 40, STATUS )
      CALL CMP_PUT0C( LOC, 'COLCOR', COLCOR, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

*  Store label and title in the output temperature NDF.
      CALL NDF_CPUT( 'Colour corrected surface brightness', INDF3,
     :               'LABEL', STATUS )
      CALL NDF_CPUT( 'Output from IRAS90:COLCORR', INDF3, 'TITLE',
     :               STATUS )
      
*  Map the DATA component of the output map.
      CALL NDF_MAP( INDF3, 'DATA', '_REAL', 'WRITE', IPOUT, EL, STATUS )

*  If required, map the VARIANCE component of the output map.
      IF( VAR ) CALL NDF_MAP( INDF3, 'VAR', '_REAL', 'WRITE', IPVOUT,
     :                        EL, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set up a constant needed by CCORA0 and CCORA1.
      CC = 1.0D-4*
     :     ( DBLE( WAVEL0 )/DBLE( I90__WAVEL( BAND ) ) )**DBLE( BETA )

*  Store central frequency of output waveband.
      NU = 299.79D0/DBLE( I90__WAVEL( BAND ) )

*  Store values in the output DATA array. There are separate routines
*  to handle the two cases of output variances being required or not
*  required.
      IF( VAR ) THEN
         CALL CCORA0( EL, %VAL( IPT ), %VAL( IPO ), %VAL( IPVT ),
     :                %VAL( IPVO ), CC, NU, %VAL( IPOUT ),
     :                %VAL( IPVOUT ), BAD, STATUS )
      ELSE
         CALL CCORA1( EL, %VAL( IPT ), %VAL( IPO ), CC, NU,
     :                %VAL( IPOUT ), BAD, STATUS )
      END IF

*  Set the bad value flag for the output NDF.
      CALL NDF_SBAD( BAD, INDF3, 'DATA', STATUS )
      IF( VAR ) CALL NDF_SBAD( BAD, INDF3, 'VAR', STATUS )

*  Store history in the output.
      CALL CCORA2( 'HISTORY', INDF1, INDF2, INDF3, BAND, STATUS )

 999  CONTINUE

*  If an error occured, delete the output NDF.
      IF( STATUS .NE. SAI__OK ) CALL NDF_DELET( INDF3, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'COLCORR_ERR2',
     :   'COLCORR: Error generating a colour corrected surface '//
     :   'brightness map.', STATUS )
      END IF

      END
