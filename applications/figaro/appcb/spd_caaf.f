      SUBROUTINE SPD_CAAF( DIALOG, VARUSE, COVRSX,
     :   NELM, NDF, PNTR, IN, PLABEL, STATUS )
*+
*  Name:
*     SPD_CAAF

*  Purpose:
*     Data and workspace access for FITGAUSS.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CAAF( DIALOG, VARUSE, COVRSX,
*        NELM, NDF, PNTR, IN, PLABEL, STATUS )

*  Description:
*     This routine performs all the access to input data and to
*     workspaces for the FITGAUSS application. This routine returns six
*     NDF identifiers. All of these must be annulled by the calling
*     routine after use.

*  Arguments:
*     DIALOG = LOGICAL (Given)
*        True if the user is available for consultation.
*     VARUSE = LOGICAL (Given and Returned)
*        On entry: false if variances are to be ignored.
*        On exit: true if variances are to be used and are available.
*     COVRSX = LOGICAL (Returned)
*        True if covariance row sums are found and mapped. If VARUSE is
*        returned false, covariance row sums are not looked for and
*        COVRSX is returned false as well.
*     NELM = INTEGER (Returned)
*        The size of the given NDF (section). This is the size of most
*        mapped arrays.
*     NDF( 6 ) = INTEGER (Returned)
*        This is a vector of six NDF identifiers. The NDFs are:
*        -  1: The input NDF (section).
*        -  2: The input's axis centre NDF from the Specdre Extension.
*              If NDF(1) has no such information, this identifier will
*              be NDF__NOID.
*        -  3: A work space NDF with one _REAL array of 4*NELM size.
*              FITGAUSS uses this for the packed masked arrays.
*        -  4: A work space NDF with two _REAL arrays of NELM size.
*              FITGAUSS uses these for masked abscissa values and
*              residuals.
*        -  5: A work space NDF with two _REAL arrays of NELM size.
*              FITGAUSS uses these while evaluating the fit and plotting
*              error bars.
*        -  6: The input's covariance row sums. If the Specdre Extension
*              to NDF(1) has no such information, this identifier will
*              be NDF__NOID.
*     PNTR( 10 ) = INTEGER (Returned)
*        This is a vector of ten pointers to mapped arrays.
*        -  1: Input centres (abscissa values, NDF(1) or NDF(2), _REAL,
*              NELM)
*        -  2: Input data (NDF(1), _REAL, NELM)
*        -  3: Input variances (if present and used, NDF(1), _REAL,
*              NELM)
*        -  4: Work space (NDF(3), _REAL, 4*NELM)
*        -  5: Unused
*        -  6: Work space (NDF(4), _REAL, NELM)
*        -  7: Work space (NDF(4), _REAL, NELM)
*        -  8: Work space (NDF(5), _REAL, NELM)
*        -  9: Work space (NDF(5), _REAL, NELM)
*        - 10: Input covariance row sums (NDF(6), _REAL, NELM)
*     IN = CHARACTER * ( * ) (Returned)
*        The name of the input NDF as tokenised by NDF_MSG.
*     PLABEL( 3 ) = CHARACTER * ( * ) (Returned)
*        Suitable plot labels. The first two labels are derived from the
*        label and unit of the axis and data respectively. The third
*        label is (the trailing part of) the NDF name.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This is set to SAI__ERROR if the given NDF
*        (section) is not one-dimensional, or if the given NDF (section)
*        does not extend along the spectroscopic axis of its base NDF.

*  Notes:
*     This routine recognises the Specdre Extension v. 0.7.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     09 Apr 1992 (hme):
*        Original version adapted from SPABU.
*     08 Jul 1992 (hme):
*        Use SPE-routines.
*     23 Jun 1993 (hme):
*        Immediate error reporting, disuse MESSAG.
*     27 Jan 1995 (hme):
*        Renamed from SPABV.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard DAT constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants

*  Arguments Given:
      LOGICAL DIALOG

*  Arguments Given and Returned:
      LOGICAL VARUSE

*  Arguments Returned:
      LOGICAL COVRSX
      INTEGER NELM
      INTEGER NDF( 6 )
      INTEGER PNTR( 10 )
      CHARACTER * ( * ) IN
      CHARACTER * ( * ) PLABEL( 3 )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      LOGICAL REPLY
      LOGICAL VEXIST             ! True if variances available
      LOGICAL EXTXST             ! True if Extension exists
      INTEGER I                  ! Unused
      INTEGER PLACE              ! NDF placeholder
      INTEGER DIM( NDF__MXDIM )  ! NDF dimensions
      INTEGER NDIM( 2 )          ! NDF dimensionality
      INTEGER AXIS( 2 )          ! Axis numbers
      CHARACTER * ( 64 ) ALABEL  ! Axis label
      CHARACTER * ( 64 ) AUNITS  ! Axis unit
      CHARACTER * ( 64 ) DLABEL  ! Data label
      CHARACTER * ( 64 ) DUNITS  ! Data unit
      CHARACTER * ( DAT__SZLOC ) XLOC ! Locator to Extension

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      ALABEL = ' '
      AUNITS = ' '
      DLABEL = ' '
      DUNITS = ' '

*  Access the given NDF and get its name.
      CALL NDF_ASSOC( 'IN', 'UPDATE', NDF(1), STATUS )
      CALL NDF_MSG(  'SPD_CAAF_T01', NDF(1) )
      CALL MSG_LOAD( 'SPD_CAAF_M01', '^SPD_CAAF_T01', IN, I, STATUS )

*  Find out NDF's dimensionality and which is the (last) non-degenerate
*  axis.
      CALL NDF_DIM( NDF(1), NDF__MXDIM, DIM, NDIM(1), STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      NDIM(2) = 0
      DO 1 I = 1, NDIM(1)
         IF ( DIM(I) .GT. 1 ) THEN
            NDIM(2) = NDIM(2) + 1
            AXIS(1) = I
         END IF
 1    CONTINUE

*  Check that the dimensionality is 1.
      IF ( NDIM(2) .NE. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_CAAF_EXX',
     :      'SPD_CAAF: Error: The given data ' //
     :      'set is not one-dimensional.', STATUS )
         GO TO 500
      END IF

*  See if variances exist. There might be covariance row sums in the
*  NDF's Specdre Extension. This routine is not interested in those:
*  This routine does not calculate variances for fitted parameters and
*  so does not need the curvature of psi-squared. In fact, it uses
*  reciprocal errors as weights. Reciprocal variances would be used in
*  minimising chi-squared.
      CALL NDF_STATE( NDF(1), 'VARIANCE', VEXIST, STATUS )
      VARUSE = VARUSE .AND. VEXIST

*  See if the Extension exists.
*  Find out the spectroscopic axis.
      CALL SPD_EAAA( NDF(1), 'READ', EXTXST, XLOC, STATUS )
      CALL SPD_EABA( NDF(1), EXTXST, AXIS(2), STATUS )

*  Check that the non-degenerate axis is the spectroscopic axis.
      IF ( AXIS(1) .NE. AXIS(2) ) THEN

*     If we can chat with the user, then we might recover from this
*     problem.
         IF ( DIALOG ) THEN
            CALL MSG_OUT( 'SPD_CAAF_M02',
     :         'The axis you are using is not the ' //
     :         'spectroscopic axis. You can either abort the ' //
     :         'application, or choose to change the ' //
     :         'Specdre Extension to identify the used axis ' //
     :         'as the future spectroscopic axis. ',
     :         STATUS )
            CALL MSG_OUT( 'SPD_CAAF_M03',
     :         'NOTE however, that information in the ' //
     :         'Extension may be lost if you change ' //
     :         'the spectroscopic axis. ',
     :         STATUS )
            CALL PAR_GET0L( 'REPAIR', REPLY, STATUS )

*        If the user wants to update the Extension.
            IF ( REPLY ) THEN

*           Locate the Extension (create if necessary).
*           Set the spectroscopic axis.
               IF ( EXTXST ) CALL DAT_ANNUL( XLOC, STATUS )
               CALL SPD_EAAA( NDF(1), 'UPDATE', EXTXST, XLOC, STATUS )
               CALL SPD_EABB( NDF(1), XLOC, AXIS(1), STATUS )

*        Else (user wants to abort).
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'SPD_CAAF_EXX',
     :            'SPD_CAAF: Error: The used ' //
     :            'axis is not the spectroscopic axis.', STATUS )
               GO TO 500
            END IF

*     Else (cannot chat with user), must abort.
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SPD_CAAF_EXX',
     :         'SPD_CAAF: Error: The used ' //
     :         'axis is not the spectroscopic axis.', STATUS )
            GO TO 500
         END IF
      END IF

*  Access and map pixel centres, wherever they are (Extension or axis
*  structure). We get the labels at the same time. They will be needed
*  for the plots.
      CALL SPD_EAEA( NDF(1), XLOC, AXIS(1), 'READ', '_REAL',
     :   ALABEL, AUNITS, PNTR(1), NDF(2), NELM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL DAT_ANNUL( XLOC, STATUS )
         GO TO 500
      END IF

*  Try to access the covariance row sums. But recover from failure.
      IF ( EXTXST .AND. VARUSE ) THEN
         CALL ERR_MARK
            CALL SPD_EAGD( NDF(1), XLOC, 'READ', '_REAL',
     :         PNTR(10), NDF(6), I, STATUS )
            COVRSX = ( STATUS .EQ. SAI__OK )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
      ELSE
         COVRSX = .FALSE.
      END IF
      IF ( .NOT. COVRSX ) NDF(6) = NDF__NOID

*  Release the Extension locator.
      IF ( EXTXST ) CALL DAT_ANNUL( XLOC, STATUS )

*  If variances to be used.
      IF ( VARUSE ) THEN

*     Map input data values and variances.
         CALL NDF_MAP( NDF(1), 'DATA,VARIANCE', '_REAL', 'READ',
     :      PNTR(2), NELM, STATUS )

*  Else (don't use variances), map data only and indicate that no
*  covariance row sums are used.
      ELSE
         CALL NDF_MAP( NDF(1), 'DATA', '_REAL', 'READ', PNTR(2),
     :      NELM, STATUS )
      END IF

*  Get data label and unit.
      CALL NDF_CGET( NDF(1), 'LABEL', DLABEL, STATUS )
      CALL NDF_CGET( NDF(1), 'UNITS', DUNITS, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get a work space NDF for masking.
*  The data component is used for masked centres, data, weights.
*  which are packed into one array. This array is _REAL.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_REAL', 1, 1, 4*NELM, PLACE, NDF(3), STATUS )
      CALL NDF_MAP( NDF(3), 'DATA', '_REAL', 'WRITE',
     :   PNTR(4), I, STATUS )

*  Get work space for residuals, centre and data array.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_REAL', 1, 1, NELM, PLACE, NDF(4), STATUS )
      CALL NDF_MAP( NDF(4), 'VARIANCE,DATA', '_REAL', 'WRITE', PNTR(6),
     :   I, STATUS )

*  Get two work spaces of same type and size as input data, and thus
*  larger than masked arrays. These are used for error bars while
*  plotting and for single precision masked arrays while finding
*  residuals.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_REAL', 1, 1, NELM, PLACE, NDF(5), STATUS )
      CALL NDF_MAP( NDF(5), 'DATA,VARIANCE', '_REAL', 'WRITE', PNTR(8),
     :   I, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get plot labels.
      PLABEL(1) = ALABEL(:CHR_LEN(ALABEL)) //
     :    ' [' // AUNITS(:CHR_LEN(AUNITS)) // ']'
      PLABEL(2) = DLABEL(:CHR_LEN(DLABEL)) //
     :    ' [' // DUNITS(:CHR_LEN(DUNITS)) // ']'
      I = CHR_LEN(IN)
      IF ( I .LE. LEN(IN) ) THEN
         PLABEL(3) = IN(:I)
      ELSE
         PLABEL(3) = '...' // IN(I+4-LEN(IN):I)
      END IF
      IF ( PLABEL(1) .EQ. ' []' ) PLABEL(1) = ' '
      IF ( PLABEL(2) .EQ. ' []' ) PLABEL(2) = ' '

*  Tidy up.
 500  CONTINUE
      END
