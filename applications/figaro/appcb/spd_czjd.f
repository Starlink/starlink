      SUBROUTINE SPD_CZJD( INFO, MODE, NPIX, OPIX, NCOMP, ROWNUM, FWHM,
     :   THRESH, XVAL, DATA, MXDWC, NFEAT, RESULT, RESVAR, STATUS )
*+
*  Name:
*     SPD_CZJD

*  Purpose:
*     Auto-locate isolated line features in an array row.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZJD( INFO, MODE, NPIX, OPIX, NCOMP, ROWNUM, FWHM,
*        THRESH, XVAL, DATA, MXDWC, NFEAT, RESULT, RESVAR, STATUS )

*  Description:
*     This routine uses a filtering algorithm to find narrow isolated
*     features in a one-dimensional data set. The features must be
*     postive, that is above the background, like emission lines in a
*     spectrum. The 1-D data set is in general the N-th row of a 2-D
*     data set.
*
*     Given the approximate width of line features (FWHM expressed in
*     pixel coordinates) this routine looks for pixels whose value
*     exceeds that of the average of 5 baseline pixels on either side by
*     at least a given threshold. When such a pixel is found an attempt
*     is made to fit a Gauss or triangle function to the suspected
*     feature. For this purpose a local constant baseline level is
*     subtracted. If the fit converges and the fitted peak exceeds the
*     threshold then the feature's centre and peak are stored in the
*     array provided. Their variances are stored in a separate array.

*  Arguments:
*     INFO = LOGICAL (Given)
*        True if informational messages are to be issued.
*     MODE = CHARACTER * ( * ) (Given)
*        The kind of line to be fitted to a feature candidate. 'G' for
*        Gauss, 'T' for triangle. This string must be exact (one upper
*        case letter).
*     NPIX = INTEGER (Given)
*        The length of the 1-D data set, length of a row in DATA.
*     OPIX = INTEGER (Given)
*        This integer should be 1.5 times FWHM. It is used for pixel
*        offsets and for declaring the array size of MXDWC.
*     NCOMP = INTEGER (Given)
*        The maximum number of features that can be stored. This is used
*        for declaring the array size of RESULT and RESVAR.
*     ROWNUM = INTEGER (Given)
*        The row number in DATA to be searched for narrow features.
*     FWHM = REAL (Given)
*        The approximate full width at half maximum of the features to
*        be looked for. This is used for incrementing the pixel count
*        during search, and as a parameter guess for the line fit. The
*        width is a free parameter in the fit, so FWHM need not be
*        precise.
*     THRESH = REAL (Given)
*        The threshold value. A candidate feature must seem to exceed
*        the local background by at least this value. An accepted
*        feature must have a fitted peak exceeding this value.
*     XVAL( NPIX ) = REAL (Given)
*        The pixel coordinates for the row in question. These should run
*        from 0.5 to NPIX-0.5 in steps of 1.0.
*     DATA( NPIX, ROWNUM ) = REAL (Given)
*        The data array. Narrow features are looked for in the ROWNUM-th
*        row.
*     MXDWC( 4 * ( 2 * OPIX + 9 ) ) = REAL (Given and Returned)
*        This is a workspace needed to pack the local profile before
*        fitting.
*     NFEAT = INTEGER (Returned)
*        The number of features located.
*     RESULT( 2 * NCOMP, ROWNUM ) = REAL (Returned)
*        The array to store the centres and peaks of located features
*        in. Each feature takes up two consecutive elements for the
*        centre and the peak resp. Only the first NFEAT elements in the
*        ROWNUM-th row are set.
*     RESVAR( 2 * NCOMP, ROWNUM ) = REAL (Returned)
*        The array to store the variances of centres and peaks of
*        located features in. Each feature takes up two consecutive
*        elements for the centre and the peak resp. Only the first NFEAT
*        elements in the ROWNUM-th row are set.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This may be set if more features are found
*        than can be stored.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     08 Jun 1993 (hme):
*        Original version.
*     16 Mar 1994 (hme):
*        Add logging plot for the row. This is inefficient since the
*        graphics device is opened and closed for each call to this
*        routine.
*     25 Nov 1994 (hme):
*        Renamed from SPADU.
*     30 Jan 1995 (hme):
*        SPD_WAAAx needs integer switches, not logical.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants
      INCLUDE 'PAR_ERR'          ! Standard PAR status values

*  Arguments Given:
      LOGICAL INFO
      CHARACTER * ( * ) MODE
      INTEGER NPIX
      INTEGER OPIX
      INTEGER NCOMP
      INTEGER ROWNUM
      REAL FWHM
      REAL THRESH
      REAL XVAL( NPIX )
      REAL DATA( NPIX, ROWNUM )

*  Arguments Given and Returned:
      REAL MXDWC( 4 * ( 2 * OPIX + 9 ) )

*  Arguments Returned:
      INTEGER NFEAT
      REAL RESULT( 2 * NCOMP, ROWNUM )
      REAL RESVAR( 2 * NCOMP, ROWNUM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXGAU             ! Maximum no. of Gauss components
      PARAMETER ( MAXGAU = 6 )
      REAL RT8LN2                ! Square root of 8 ln(2)
      PARAMETER ( RT8LN2 = 2.354820 )

*  Local Variables:
      LOGICAL FITTED             ! True if line fit succeeded
      INTEGER I                  ! Temporary integer
      INTEGER MPIX               ! Slightly smaller than NPIX
      INTEGER LPIX, RPIX         ! Count good baseline pixels
      INTEGER PIXNO              ! Current pixel to check
      INTEGER MSKELM             ! Number of pixels for line fit
      INTEGER CF( MAXGAU )       ! Full array of centre fit flags
      INTEGER PF( MAXGAU )       ! Full array of peak fit flags
      INTEGER WF( MAXGAU )       ! Full array of width fit flags
      INTEGER ZONID              ! SGS zone identifier
      REAL XLEFT, XRIGHT         ! Baseline locations left and right
      REAL MEANL, MEANR          ! Baseline levels left and right
      REAL MASK( 2 )             ! XVAL mask for fit
      REAL CENTRE( MAXGAU )      ! Full array of centres
      REAL PEAK(   MAXGAU )      ! Full array of peaks
      REAL WIDTH(  MAXGAU )      ! Full array of FWHM or sigma
      REAL CHISQR                ! Chi squared resulting from fit
      DOUBLE PRECISION COVAR( 3, 3 ) ! Fit covariance
      REAL LEFT, RIGHT, BOTTOM, TOP ! PGPLOT window
      REAL DLOW, DHIGH           ! Min/max data in row
      REAL DELTA                 ! Plotted range

*  Local Data:
      DATA CF / 0,0,0,0,0,0 /
      DATA PF / 0,0,0,0,0,0 /
      DATA WF / 0,0,0,0,0,0 /
      DATA CENTRE / 0.,0.,0.,0.,0.,0. /
      DATA PEAK   / 0.,0.,0.,0.,0.,0. /
      DATA WIDTH  / 1.,1.,1.,1.,1.,1. /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report new row to user.
      IF ( INFO ) THEN
         CALL MSG_SETI( 'ARCLOCAT_T04', ROWNUM )
         CALL MSG_OUT( 'ARCLOCAT_M17', 'Scanning through ' //
     :      'row #^ARCLOCAT_T04 ...', STATUS )
      END IF

*  Set pixel to first to be checked.
*  Set feature count to zero.
      PIXNO = 6 + OPIX
      NFEAT = 0

*  While pixel less than nx-5-1.5w.
      MPIX = NPIX - 5 - OPIX
 1    CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( PIXNO .LT. MPIX .AND. NFEAT .LT. NCOMP ) THEN

*     Average of the 10 baseline pixels.
         MEANL = 0.
         MEANR = 0.
         LPIX = 0
         RPIX = 0
         DO 2 I = 0, 4
            IF ( DATA(PIXNO-OPIX-I,ROWNUM) .NE. VAL__BADR ) THEN
               LPIX = LPIX + 1
               MEANL = MEANL + DATA(PIXNO-OPIX-I,ROWNUM)
            END IF
            IF ( DATA(PIXNO+OPIX+I,ROWNUM) .NE. VAL__BADR ) THEN
               RPIX = RPIX + 1
               MEANR = MEANR + DATA(PIXNO+OPIX+I,ROWNUM)
            END IF
 2       CONTINUE

*     If baseline not determined or pixel itself bad, then
*     increment pixel to be checked by 1 (careful near bad pixels).
         IF ( LPIX .LE. 0 .OR. RPIX .LE. 0 .OR.
     :         DATA(PIXNO,ROWNUM) .EQ. VAL__BADR ) THEN
            PIXNO = PIXNO + 1

*     Else (baseline could be determined and pixel is not bad itself).
         ELSE

*        Work out the left and right mean baseline levels.
            MEANL = MEANL / FLOAT(LPIX)
            MEANR = MEANR / FLOAT(RPIX)

*        If the excess is below threshold, then
*        increment pixel to be checked by half the width.
            IF ( DATA(PIXNO,ROWNUM)-(MEANL+MEANR)/2. .LE. THRESH ) THEN
               PIXNO = PIXNO + INT( 0.5 * FWHM ) + 1

*        Else (excess is above threshold).
            ELSE

*           Any error conditions and reports occuring during masking/
*           packing the local profile or fitting it, are supressed.
               IF ( STATUS .NE. SAI__OK ) GOTO 500
               CALL ERR_MARK

*              Mask and pack the XVAL and DATA into MXDWC, also create a
*              weight array therein. The local profile extends from
*              PIXNO-OPIX-4 to PIXNO+OPIX+4, thus has 2*OPIX+9 pixels.
                  MASK(1) = XVAL(PIXNO-OPIX-4)
                  MASK(2) = XVAL(PIXNO+OPIX+4)
                  CALL SPD_WAAAR( 0,
     :               0, 2*OPIX+9, 2, XVAL(PIXNO-OPIX-4),
     :               DATA(PIXNO-OPIX-4,ROWNUM), 0., 0., 1, MASK,
     :               MSKELM, MXDWC, STATUS )

*              Subtract a linear baseline from the local profile.
                  XLEFT  = XVAL(PIXNO-OPIX-2)
                  XRIGHT = XVAL(PIXNO+OPIX+2)
                  DO 3 I = 1, MSKELM
                     MXDWC(MSKELM+I) = MXDWC(MSKELM+I) - MEANL -
     :                  ( MXDWC(I) - XLEFT ) / ( XRIGHT - XLEFT ) *
     :                  ( MEANR - MEANL )
 3                CONTINUE

*              Fit a single Gauss or triangle. SPD_WAA[EH] fit in
*              general 6 lines, thus need arrays of length 6.
                  CENTRE(1) = XVAL(PIXNO)
                  PEAK(1)   = DATA(PIXNO,ROWNUM) - (MEANL+MEANR) / 2.
                  IF ( MODE .EQ. 'G' ) THEN
                     WIDTH(1) = FWHM / RT8LN2
                     CALL SPD_WFGA( .FALSE., .FALSE., MSKELM, 1, 3, 3,
     :                  0., MXDWC, CF, PF, WF, CENTRE, PEAK, WIDTH,
     :                  CHISQR, COVAR, FITTED, STATUS )
                  ELSE IF ( MODE .EQ. 'T' ) THEN
                     WIDTH(1) = FWHM
                     CALL SPD_WFTA( .FALSE., .FALSE., MSKELM, 1, 3, 3,
     :                  0., MXDWC, CF, PF, WF, CENTRE, PEAK, WIDTH,
     :                  CHISQR, COVAR, FITTED, STATUS )
                  END IF
               IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
               CALL ERR_RLSE

*           If the fit failed.
*           We regard the fit as a failure also if the peak is less than
*           the threshold or the left and right background levels differ
*           by more than 100% of the peak.
*           We could also see if the width has deviated too much from
*           the guess, but do not do so.
               IF ( .NOT. FITTED .OR. PEAK(1) .LE. THRESH .OR.
     :               PEAK(1) .LE. ABS( MEANL - MEANR ) ) THEN

*              Report to user.
                  IF ( INFO ) THEN
                     CALL MSG_SETR( 'ARCLOCAT_T05', XVAL(PIXNO) )
                     CALL MSG_OUT( 'ARCLOCAT_M18', '   rejected ' //
     :                  'candidate feature near ^ARCLOCAT_T05,',
     :                  STATUS )
                  END IF

*              Increment pixel to be checked by half the width.
                  PIXNO = PIXNO + INT( 0.5 * FWHM ) + 1

*           Else (fit converged).
               ELSE

*              Report to user.
                  IF ( INFO ) THEN
                     CALL MSG_SETR( 'ARCLOCAT_T06', CENTRE(1) )
                     CALL MSG_OUT( 'ARCLOCAT_M19', '   accepted ' //
     :                  'feature at ^ARCLOCAT_T06,', STATUS )
                  END IF

*              Increment feature count.
                  NFEAT = NFEAT + 1
                  IF ( NFEAT .GT. NCOMP ) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP( 'ARCLOCAT_E14', 'ARCLOCAT: ' //
     :                  'Error storing feature location: More ' //
     :                  'features were located than can be stored.',
     :                  STATUS )
                     GO TO 500
                  END IF

*              Store centre, peak and their variances.
*              The COVAR matrix returned by the fit routines needs
*              multiplication with rms squared, due to ignoring input
*              variances.
                  RESULT(2*NFEAT-1,ROWNUM) = CENTRE(1)
                  RESULT(2*NFEAT,  ROWNUM) = PEAK(1)
                  RESVAR(2*NFEAT-1,ROWNUM) =
     :               CHISQR * SNGL(COVAR(1,1)) / FLOAT( MSKELM - 3 )
                  RESVAR(2*NFEAT,  ROWNUM) =
     :               CHISQR * SNGL(COVAR(2,2)) / FLOAT( MSKELM - 3 )

*              Increment pixel to be checked by twice the width plus 5.
                  PIXNO = PIXNO + INT( 2. * FWHM ) + 5
               END IF
            END IF
         END IF
         GO TO 1
      END IF

*  Report new row to user.
      IF ( INFO ) THEN
         CALL MSG_SETI( 'ARCLOCAT_T07', NFEAT )
         CALL MSG_OUT( 'ARCLOCAT_M20', 'Finished scan through ' //
     :      'row with ^ARCLOCAT_T07 features found.', STATUS )
      END IF

*  Access graphics device.
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      CALL SPD_UGAA( 'DEVICE', 'WRITE', ' ', I, ZONID, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         GO TO 500
      END IF

*  Set up window.
*  After finding the exact y range, add 5 % on either side.
      LEFT  = XVAL(1)
      RIGHT = XVAL(NPIX)
      CALL SPD_UAAAR( .TRUE., NPIX, DATA(1,ROWNUM),
     :   DLOW, DHIGH, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 400
      DELTA  = DHIGH - DLOW
      BOTTOM = DLOW  - DELTA / 20.
      TOP    = DHIGH + DELTA / 20.
      CALL SPD_UGAC( ZONID, STATUS )
      CALL PGSWIN( LEFT, RIGHT, BOTTOM, TOP )

*  Plot (row in bin-style, centre from feature list as
*  green dashed lines).
      CALL PGBOX( 'BCTSN', 0., 0, 'BCTSN', 0., 0 )
      CALL SPD_WAAD( .FALSE., .FALSE., .FALSE., .TRUE., .FALSE., 1,
     :   0, NPIX, XVAL, DATA(1,ROWNUM), 0., 0., 0., 0., STATUS )
      CALL PGSLS( 2 )
      CALL PGSCI( 3 )
      I = 1
 4    CONTINUE             ! Start of 'DO WHILE' loop
      IF ( I .LT. NCOMP ) THEN
         IF ( RESULT(2*I-1,ROWNUM) .NE. VAL__BADR ) THEN
            CALL PGMOVE( RESULT(2*I-1,ROWNUM), BOTTOM )
            CALL PGDRAW( RESULT(2*I-1,ROWNUM), TOP )
            I = I + 1
            GO TO 4
         END IF
      END IF
      CALL PGSLS( 1 )
      CALL PGSCI( 1 )

*  Before leaving, save the current view port as an AGI picture.
      CALL SPD_UGAD( 'DATA', 'SPECDRE_ARCLOCAT', I, STATUS )

*  Return.
 400  CONTINUE
      CALL SPD_UGAB( 'DEVICE', .FALSE., STATUS )
 500  CONTINUE
      END
