      SUBROUTINE SPD_CZFD( DIALOG, VARUSE, MAXPAR,
     :   NELM, NDF, PNTR, IN, PLABEL, STATUS )
*+
*  Name:
*     SPD_CZFD

*  Purpose:
*     Data and workspace access for FITPOLY.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZFD( DIALOG, VARUSE, MAXPAR, NELM, NDF, PNTR, IN,
*        PLABEL, STATUS )

*  Description:
*     This routine performs all the access to input data and to
*     workspaces for the FITPOLY application. This routine returns five
*     NDF identifiers. Insofar as these are valid (.NE. NDF__NOID) must
*     they be annulled by the calling routine after use.

*  Arguments:
*     DIALOG = LOGICAL (Given)
*        True if the user is available for consultation.
*     VARUSE = LOGICAL (Given and Returned)
*        On entry: false if variances are to be ignored.
*        On exit: true if variances are to be used and are available.
*     MAXPAR = INTEGER (Given)
*        The maximum possible order plus 1. This is used for the size of
*        one of the work spaces.
*     NELM = INTEGER (Returned)
*        The size of the NDFs (sections). This is the size of most
*        mapped arrays.
*     NDF( 6 ) = INTEGER (Returned)
*        This is a vector of five NDF identifiers. The NDFs are:
*        -  1: The input NDF (section).
*        -  2: The input's axis centre NDF from the Specdre Extension.
*              If NDF(1) has no such information, this identifier will
*              be NDF__NOID.
*        -  3: A work space NDF with one _DOUBLE array of 3*NELM size.
*              FITPOLY uses this for the packed masked arrays.
*        -  4: A work space NDF with two _REAL arrays of NELM size.
*              FITPOLY uses these for masked abscissa values and
*              residuals.
*        -  5: A work space NDF with two _REAL arrays of NELM size.
*              FITPOLY uses these while evaluating the fit and plotting
*              error bars.
*        -  6: A work space NDF with one _DOUBLE array of
*              NELM + 3*(NELM+MAXPAR).
*     PNTR( 9 ) = INTEGER (Returned)
*        This is a vector of nine pointers to mapped arrays.
*        -  1: Input centres (abscissa values, NDF(1) or NDF(2), _REAL,
*              NELM)
*        -  2: Input data (NDF(1), _REAL, NELM)
*        -  3: Input variances (if present and used, NDF(1), _REAL,
*              NELM)
*        -  4: Work space (NDF(3), _DOUBLE, 3*NELM)
*        -  5: Work space (NDF(6), _DOUBLE, NELM+3*(NELM+MAXPAR))
*        -  6: Work space (NDF(4), _REAL, NELM)
*        -  7: Work space (NDF(4), _REAL, NELM)
*        -  8: Work space (NDF(5), _REAL, NELM)
*        -  9: Work space (NDF(5), _REAL, NELM)
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
*     This routine enquires the ADAM parameters IN and REPAIR.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     03 Apr 1992 (hme):
*        Original version.
*     03 Jul 1992 (hme):
*        Use SPE-routines and include file.
*     23 Jun 1993 (hme):
*        Immediate error reporting, disuse MESSAG.
*     27 Jan 1995 (hme):
*        Renamed from SPABU.
*     21 Nov 1995 (hme):
*        Change from mapping input errors to input variances. Due to the
*        move from NAG to PDA one wants 1/variance as weight, not 1/error.
*        The reciprocal is not up to this routine but to the masking.
*        Also, the work space is no longer just 3*NELM. Introduce a sixth
*        NDF for this one.
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
      INTEGER MAXPAR

*  Arguments Given and Returned:
      LOGICAL VARUSE

*  Arguments Returned:
      INTEGER NELM
      INTEGER NDF( 6 )
      INTEGER PNTR( 9 )
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
      CALL NDF_MSG( 'SPD_CZFD_T01', NDF(1) )
      CALL MSG_LOAD( 'SPD_CZFD_M01', '^SPD_CZFD_T01', IN, I, STATUS )

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
         CALL ERR_REP( 'FITPOLY_EXX', 'FITPOLY: Error: The given ' //
     :      'data set is not one-dimensional.', STATUS )
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
            CALL MSG_OUT( 'SPD_CZFD_M02',
     :         'The axis you are using is not the ' //
     :         'spectroscopic axis. You can either abort the ' //
     :         'application, or choose to change the ' //
     :         'Specdre Extension to identify the used axis ' //
     :         'as the future spectroscopic axis. ',
     :         STATUS )
            CALL MSG_OUT( 'SPD_CZFD_M03',
     :         'NOTE however, that information in the ' //
     :         'Extension may be lost if you change ' //
     :         'the spectroscopic axis. ',
     :         STATUS )
            CALL PAR_GET0L( 'REPAIR', REPLY, STATUS )

*        If the user wants to update the Extension.
            IF ( REPLY ) THEN

*           Locate the Extension (create if necessary).
*           Set the spectroscopic axis.
*           Annul the locator.
               IF ( EXTXST ) CALL DAT_ANNUL( XLOC, STATUS )
               CALL SPD_EAAA( NDF(1), 'UPDATE', EXTXST, XLOC, STATUS )
               CALL SPD_EABB( NDF(1), XLOC, AXIS(1), STATUS )

*        Else (user wants to abort).
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'FITPOLY_EXX', 'FITPOLY: Error: The ' //
     :            'used axis is not the spectroscopic axis.', STATUS )
               GO TO 500
            END IF

*     Else (cannot chat with user), must abort.
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_REP( 'FITPOLY_EXX', 'FITPOLY: Error: The ' //
     :         'used axis is not the spectroscopic axis.', STATUS )
            GO TO 500
         END IF
      END IF

*  Access and map pixel centres, wherever they are (Extension or axis
*  structure). We get the labels at the same time. They will be needed
*  for the plots.
      CALL SPD_EAEA( NDF(1), XLOC, AXIS(1), 'READ', '_REAL',
     :   ALABEL, AUNITS, PNTR(1), NDF(2), NELM, STATUS )
      IF ( EXTXST ) CALL DAT_ANNUL( XLOC, STATUS )

*  Map the data values and variances. We get the labels at the same time.
*  They will be needed for the plots.
      IF ( VARUSE ) THEN
         CALL NDF_MAP( NDF(1), 'DATA,VARIANCE', '_REAL', 'READ',
     :      PNTR(2), NELM, STATUS )
      ELSE
         CALL NDF_MAP( NDF(1), 'DATA', '_REAL', 'READ', PNTR(2),
     :      NELM, STATUS )
      END IF
      CALL NDF_CGET( NDF(1), 'LABEL', DLABEL, STATUS )
      CALL NDF_CGET( NDF(1), 'UNITS', DUNITS, STATUS )

*  Check status.
      IF ( STATUS .NE. SAI__OK ) GO TO 500

*  Get a work space NDF for masking.
*  This is used for masked centres, data, weights
*  (=1/variance), which are packed into one array. This array is _DOUBLE so
*  it can be passed on directly to PDA.
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_DOUBLE', 1, 1, 3*NELM, PLACE, NDF(3), STATUS )
      CALL NDF_MAP( NDF(3), 'DATA', '_DOUBLE', 'WRITE',
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

*  Get work space for PDA_DPOLFT. It needs really two, which are here
*  concatenated into one
      CALL NDF_TEMP( PLACE, STATUS )
      CALL NDF_NEW( '_DOUBLE', 1, 1, 4*NELM+3*MAXPAR,
     :   PLACE, NDF(6), STATUS )
      CALL NDF_MAP( NDF(6), 'DATA', '_DOUBLE', 'WRITE', PNTR(5),
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
