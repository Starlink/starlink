      SUBROUTINE WFWCS0 (FTUNIT,
     :     RAPCX,
     :     DCPCX,
     :     DCPCG,
     :     XZPIX,
     :     YZPIX,
     :     CD11,
     :     CD12,
     :     CD21,
     :     CD22,
     :     KPROJ,
     :     DISTOR,
     :     FPAR,
     :     FTSTAT)

*+
*
*     - - - - - - -
*      W F W C S 0
*     - - - - - - -
*
*   Write out plate distortion parameters as a FITS WCS header.
*
* XXX BLAH
*-

      IMPLICIT NONE

*   Parameters:
*   Unit number to which we write FITS headers
      INTEGER FTUNIT
*   Location of the projection pole, in pixels
      DOUBLE PRECISION XZPIX,YZPIX
*   Transformation from pixel coordinates (px, py) to plate coordinates (ax, ay),
*   in radians, is
*       ax = cd11 * px + cd12 * py
*       ay = cd21 * px + cd22 * py
      DOUBLE PRECISION CD11,CD21,CD12,CD22
*   See astrml.f for following parameters
      DOUBLE PRECISION RAPCX,DCPCX,DCPCG,DISTOR
      CHARACTER KPROJ*40
*   Prefix to FITSWCS parameter string, can be PV or QV
      CHARACTER FPAR*2
*   FITS inherited status
      INTEGER FTSTAT

*   Local variables:
*   For formatting
      CHARACTER SIGN
      INTEGER IXMSF(4),N
      CHARACTER*(100) WRKSTR

*   Distortion coefficient in degrees**-2
      DOUBLE PRECISION DISTORD2

*  Degrees to radians
      DOUBLE PRECISION D2R
      PARAMETER (D2R=1.745329251994329576923691D-2)

*  Functions
      DOUBLE PRECISION sla_DRANGE,sla_DRANRM




      CALL FTPKYS (FTUNIT, 'CTYPE1', 'RA---TAN',
     :     'RA -- tangent-plane projection', FTSTAT)
      CALL FTPKYS (FTUNIT, 'CTYPE2', 'DEC--TAN',
     :     'DEC -- tangent-plane projection', FTSTAT)
      CALL FTPKYS (FTUNIT, 'RADESYS', 'FK5',
     :     'Celestial coordinates in FK5 system', FTSTAT)
*   Default equinox J2000
      CALL FTPKYG (FTUNIT, 'EQUINOX', 2000.0D0, 1,
     :     'Equator and equinox of J2000.0', FTSTAT)
*   Native longitude of celestial pole.  This is the
*   default, as specified in C&G
      IF (DCPCG/D2R.LT.90D0) THEN
         CALL FTPKYG (FTUNIT, 'LONPOLE', 180D0, 0,
     :        'Native longitude of celestial pole',
     :        FTSTAT)
      ELSE
         CALL FTPKYG (FTUNIT, 'LONPOLE', 0D0, 0,
     :        'Native longitude of celestial pole',
     :        FTSTAT)
      ENDIF

*   Coordinates of reference point.
*   We have to deal only with zenithal coordinates.
*   The estimated and predicted coordinates here in ASTROM
*   are in the projected plane, and are the `intermediate
*   world coordinates' of the FITS-WCS proposals.  Note that we
*   are outputting the `report' coordinates, RAPCX and DCPCX.
      CALL sla_DR2TF(1,sla_DRANRM(RAPCX),SIGN,IXMSF)
*   Because of the dranrm, SIGN is always '+'
      WRITE (WRKSTR, '("Projection pole -- RA =  ",
     :     I3,":",I2.2,":",I2.2,".",I1)')
     :     IXMSF
      CALL FTPKYG (FTUNIT, 'CRVAL1', RAPCX/D2R, 7,
     :     WRKSTR, FTSTAT)
      CALL sla_DR2AF(0,sla_DRANGE(DCPCX),SIGN,IXMSF)
      WRITE (WRKSTR, '("Projection pole -- Dec = ",
     :     A,I2.2,":",I2.2,":",I2.2)')
     :     SIGN, (IXMSF(N),N=1,3)
      CALL FTPKYG (FTUNIT, 'CRVAL2', DCPCX/D2R, 7,
     :     WRKSTR, FTSTAT)

      CALL FTPKYG (FTUNIT, 'CRPIX1',
     :     XZPIX, 7,
     :     'Projection pole -- x-pixels', FTSTAT)
      CALL FTPKYG (FTUNIT, 'CRPIX2',
     :     YZPIX, 7,
     :     'Projection pole -- y-pixels', FTSTAT)
*   Note that the coordinates, and the
*   transformation matrix, are required to be in degrees
*   rather than radians.
      CALL FTPKYS (FTUNIT, 'CUNIT1', 'deg',
     :     'RA always given in degrees', FTSTAT)
      CALL FTPKYS (FTUNIT, 'CUNIT2', 'deg',
     :     'Dec always given in degrees', FTSTAT)
*   Transformation matrix, again in units of degrees, not radians
      CALL FTPKYD (FTUNIT, 'CD1_1',
     :     CD11/D2R, 7,
     :     'Transformation to intermed. world coords',
     :     FTSTAT)
      CALL FTPKYD (FTUNIT, 'CD2_1',
     :     CD21/D2R, 7, ' ', FTSTAT)
      CALL FTPKYD (FTUNIT, 'CD1_2',
     :     CD12/D2R, 7, ' ', FTSTAT)
      CALL FTPKYD (FTUNIT, 'CD2_2',
     :     CD22/D2R, 7, ' ', FTSTAT)
*   Include the relevant coefficients of the TAN distortion.
*   If we write the undistorted gnomonic projection
*   coordinates as (\xi, \eta), following C&G, and the distorted
*   coordinates (ie, the plate coordinates) as (x,y), then
*   ASTROM handles the relationship between these as
*   x=\xi(1+q\rho^2) and y=\eta(1+q\rho^2), where
*   \rho^2=\xi^2+\eta^2.  We need to invert this, to give
*   (\xi,\eta) as a function of (x,y).  Write r^2=x^2+y^2
*   and Q=1/(1+q\rho^2), so that \rho^2=Qr^2.  Substitute
*   \rho^2 into Q, then Q into itself, and expand, keeping
*   terms to O(r^6), to obtain Q=1 - qr^2 + 3q^2r^4 -
*   12q^3r^6 + O(r^8), and the distortion functions become
*   \xi=Q(r)x, \eta=Q(r)y.  The parameter q in this program
*   has units rad^{-2} (I'm pretty sure), but since the FITS
*   files work in degrees, we need to convert it to deg^{-2}.
      WRITE (WRKSTR,'("ASTROM projection: ",A)') KPROJ
      CALL FTPCOM (FTUNIT, WRKSTR, FTSTAT)
      DISTORD2 = DISTOR*(D2R**2)
      WRITE (WRKSTR, '("ASTROM q=",F10.4,"rad**-2 = ",
     :     F10.4,"deg**-2")') DISTOR, DISTORD2
      CALL FTPCOM (FTUNIT, WRKSTR, FTSTAT)

      CALL FTPKYD (FTUNIT, FPAR//'1_0', 0D0, 7,
     :     'Distortion function for x: a_0=0', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'1_1', 1D0, 7,
     :     'a_1 x     = 1', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'1_7', -DISTORD2, 7,
     :     'a_7 x^3   = -q', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'1_9', -DISTORD2, 7,
     :     'a_9 xy^2  = -q', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'1_17', 3*DISTORD2**2, 7,
     :     'a_17 x^5  = 3q^2', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'1_21', 3*DISTORD2**2, 7,
     :     'a_21 xy^4 = 3q^2', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'1_31', -12*DISTORD2**3, 7,
     :     'a_31 x^7  = -12q^3', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'1_37', -12*DISTORD2**3, 7,
     :     'a_37 xy^6 = -12q^3', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_0', 0D0, 7,
     :     'Distortion function for y: b_0=0', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_1', 1D0, 7,
     :     'b_1 y     = 1', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_7', -DISTORD2, 7,
     :     'b_7 y^3   = -q', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_9', -DISTORD2, 7,
     :     'b_9 yx^2  = -q', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_17', 3*DISTORD2**2, 7,
     :     'b_17 y^5  = 3q^2', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_21', 3*DISTORD2**2, 7,
     :     'b_21 yx^4 = 3q^2', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_31', -12*DISTORD2**3, 7,
     :     'b_31 y^7  = -12q^3', FTSTAT)
      CALL FTPKYD (FTUNIT, FPAR//'2_37', -12*DISTORD2**3, 7,
     :     'b_37 yx^6 = -12q^3', FTSTAT)

      END
