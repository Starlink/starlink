      SUBROUTINE SPD_WZJC( MODE, NPIX, OPIX, PIXNO, FWHM, XVAL, DATA,
     :   MXDWC, FITTED, LOCAT, LOCATV, STREN, STRENV, STATUS )
*+
*  Name:
*     SPD_WZJC

*  Purpose:
*     Perform local Gauss or triangle fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZJC( MODE, NPIX, OPIX, PIXNO, FWHM, XVAL, DATA,
*        MXDWC, FITTED, LOCAT, LOCATV, STREN, STRENV, STATUS )

*  Description:
*     This routine performs a Gauss or triangle fit on part of a
*     one-dimensional data set. The local profile is centred on PIXNO
*     and extends OPIX+4 pixels on either side. (If this exceeds the
*     extent of the data array, then no fit is performed.) The outer 5
*     pixels on either side are used to work out local left and right
*     background levels, which in turn are used to subtract a linear
*     baseline before the line fit.

*  Arguments:
*     MODE = CHARACTER * ( * )  (Given)
*        Can be 'G' or 'T' for Gauss or triangle fit. This is
*        case-sensitive.
*     NPIX = INTEGER (Given)
*        Size of the XVAL and DATA arrays.
*     OPIX = INTEGER (Given)
*        This is 1.5 times FWHM.
*     PIXNO = INTEGER (Given)
*        The pixel on which the local profile should be centred.
*     FWHM = REAL (Given)
*        The guessed width for the line fit.
*     XVAL( NPIX ) = REAL (Given)
*        The array of x values. XVAL(PIXNO) will be the guessed centre.
*     DATA( NPIX ) = REAL (Given)
*        The array of data values. The guessed peak will be the excess
*        of DATA(PIXNO) over the local background level. This array may
*        contain bad values.
*     MXDWC( 4 * ( 2 * OPIX + 9 ) ) = REAL (Given and Returned)
*        A workspace for the packed local profile.
*     FITTED = LOGICAL (Returned)
*        True if the fit succeeded, false otherwise.
*     LOCAT = REAL (Returned)
*        The fitted centre.
*     LOCATV = REAL (Returned)
*        The variance of LOCAT.
*     STREN = REAL (Returned)
*        The fitted peak.
*     STRENV = REAL (Returned)
*        The variance of STREN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     09 Jun 1993 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPADX.
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

*  Arguments Given:
      CHARACTER * ( * ) MODE
      INTEGER NPIX
      INTEGER OPIX
      INTEGER PIXNO
      REAL FWHM
      REAL XVAL( NPIX )
      REAL DATA( NPIX )

*  Arguments Given and Returned:
      REAL MXDWC( 4 * ( 2 * OPIX + 9 ) )

*  Arguments Returned:
      LOGICAL FITTED
      REAL LOCAT, LOCATV
      REAL STREN, STRENV

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXGAU             ! Maximum no. of Gauss components
      PARAMETER ( MAXGAU = 6 )
      REAL RT8LN2                ! Square root of 8 ln(2)
      PARAMETER ( RT8LN2 = 2.354820 )

*  Local Variables:
      INTEGER I                  ! Temporary integer
      INTEGER MSKELM             ! Number of pixels for line fit
      INTEGER LPIX, RPIX         ! Count good baseline pixels
      INTEGER CF( MAXGAU )       ! Full array of centre fit flags
      INTEGER PF( MAXGAU )       ! Full array of peak fit flags
      INTEGER WF( MAXGAU )       ! Full array of width fit flags
      REAL MEANL, MEANR          ! Baseline levels left and right
      REAL XLEFT, XRIGHT         ! Baseline locations left and right
      REAL MASK( 2 )             ! XVAL mask for fit
      REAL CENTRE( MAXGAU )      ! Full array of centres
      REAL PEAK(   MAXGAU )      ! Full array of peaks
      REAL WIDTH(  MAXGAU )      ! Full array of FWHM or sigma
      REAL CHISQR                ! Chi squared resulting from fit
      DOUBLE PRECISION COVAR( 3, 3 ) ! Fit covariance

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

*  Default success switch.
      FITTED = .FALSE.

*  Get average of left and right base line range.
      MEANL = 0.
      MEANR = 0.
      LPIX = 0
      RPIX = 0
      IF ( PIXNO-OPIX-4 .GE. 1 .AND. PIXNO+OPIX+4 .LE. NPIX ) THEN
         DO 1001 I = 0, 4
            IF ( DATA(PIXNO-OPIX-I) .NE. VAL__BADR ) THEN
               LPIX = LPIX + 1
               MEANL = MEANL + DATA(PIXNO-OPIX-I)
            END IF
            IF ( DATA(PIXNO+OPIX+I) .NE. VAL__BADR ) THEN
               RPIX = RPIX + 1
               MEANR = MEANR + DATA(PIXNO+OPIX+I)
            END IF
 1001    CONTINUE
      END IF

*  If base line could be determined.
      IF ( LPIX * RPIX .GT. 0 ) THEN

*     Work out the left and right mean baseline levels.
         MEANL = MEANL / FLOAT(LPIX)
         MEANR = MEANR / FLOAT(RPIX)

*     Any error conditions and reports occuring during masking/
*     packing the local profile or fitting it, are supressed.
         CALL ERR_MARK

*        Mask and pack the XVAL and DATA into MXDWC, also create a
*        weight array therein. The local profile
*        extends from PIXNO-OPIX-4 to PIXNO+OPIX+4, thus has 2*OPIX+9
*        pixels.
            MASK(1) = XVAL(PIXNO-OPIX-4)
            MASK(2) = XVAL(PIXNO+OPIX+4)
            CALL SPD_WAAAR( 0, 0,
     :         2*OPIX+9, 2, XVAL(PIXNO-OPIX-4),
     :         DATA(PIXNO-OPIX-4), 0., 0., 1, MASK,
     :         MSKELM, MXDWC, STATUS )

*        Subtract a linear baseline from the local profile.
            XLEFT  = XVAL(PIXNO-OPIX-2)
            XRIGHT = XVAL(PIXNO+OPIX+2)
            DO 1003 I = 1, MSKELM
               MXDWC(MSKELM+I) = MXDWC(MSKELM+I) - MEANL -
     :            ( MXDWC(I) - XLEFT ) / ( XRIGHT - XLEFT ) *
     :            ( MEANR - MEANL )
 1003       CONTINUE

*        Fit a single Gauss or triangle. SPFDFT/SPAAW fit in general
*        6 lines, thus need arrays of length 6.
            CENTRE(1) = XVAL(PIXNO)
            PEAK(1)   = DATA(PIXNO) - (MEANL+MEANR) / 2.
            IF ( MODE .EQ. 'G' ) THEN
               WIDTH(1) = FWHM / RT8LN2
               CALL SPD_WFGA( .FALSE., .FALSE., MSKELM, 1, 3, 3,
     :            0., MXDWC, CF, PF, WF, CENTRE, PEAK, WIDTH,
     :            CHISQR, COVAR, FITTED, STATUS )
            ELSE IF ( MODE .EQ. 'T' ) THEN
               WIDTH(1) = FWHM
               CALL SPD_WFTA( .FALSE., .FALSE., MSKELM, 1, 3, 3,
     :            0., MXDWC, CF, PF, WF, CENTRE, PEAK, WIDTH,
     :            CHISQR, COVAR, FITTED, STATUS )
            END IF
            LOCAT  = CENTRE(1)
            LOCATV = CHISQR * SNGL(COVAR(1,1)) / FLOAT( MSKELM - 3 )
            STREN  = PEAK(1)
            STRENV = CHISQR * SNGL(COVAR(2,2)) / FLOAT( MSKELM - 3 )
         IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         CALL ERR_RLSE
      END IF

*  Return.
      END
