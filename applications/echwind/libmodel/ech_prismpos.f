*+  ECH_PRISMPOS  -  Determine the optimal prism rotation and position
*                    at a specified wavelength

      subroutine ech_prismpos (wc, gam, pos, status)
*
*   Description :
*
*     Determine the prism rotation and position by direct calculation. The
*     rotation is such that a ray of the specified wavelength will fall on
*     the centre of the detector. The position is such that the echelle is
*     fully illuminated by rays of the specified wavelength.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_PRISMPOS (WC, GAMMA, POS, STATUS)
*
*   Arguments :
*
*     WC      =  REAL (READ)           Desired central wavelength (Angstroms)
*     GAMMA   =  REAL (WRITE)          Optimal prism rotation (radians)
*     POS     =  REAL (WRITE)          Optimal prism position (mm)
*     STATUS  =  INTEGER (READ, WRITE) Global status value
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     C.J. Hirst  UCL  (ZUVAD::CJH)
*
*   History :
*
*     ?? ??? 1988  : Original version (ZUVAD::CJH)
*     01 Aug 1989  : Added comments (AAOEPP::WFL)
*     12 Dec 1989  : Changed name to ECH_PRISMPOS, reworked arguments and
*                    included code from ECH_FORMAT_CENTRE (AAOEPP::WFL)
*     23 Sep 1994  : Added common variables ech_collxe, ech_prface and
*                    ech_prapex (MPF/RGO)
*
*   Type definitions :
*
      implicit none             ! no default typing allowed
*
*   Global constants :
*
      include 'SAE_PAR'        ! ADAM error codes
*
*   Import :
*
      real wc                   ! central wavelength
*
*   Export :
*
      real gam                  ! prism rotation
      real pos                  ! prism position
*
*   Status :
*
      integer status
*
*   Global Variables :
*
      include 'ech_common'      ! current echelle parameters
*
*   Local Constants :
*
      double precision rad      ! degrees per radian
      parameter (rad=57.29577951)
*
*   Local variables :
*
      double precision wave0    ! copy of echelle field central wavelength
      double precision angle    ! copy of prism angle
      double precision xe       ! collimated axis to echelle distance
      double precision face     ! prism face length, just glass
      double precision apal     ! prism apex along face
      double precision wavec    ! copy of desired central wavelength
      double precision a        ! (another) copy of prism angle
      double precision refr0    ! refractive index at wave0
      double precision refrc    ! refractive index at wavec
      double precision iwave0   ! incidence angle at wave0
      double precision iwavec   ! incidence angle at wavec
      double precision dev      ! single prism deviation at wave0
      double precision i2wavec  ! incidence angle on second prism at wavec
      double precision gamma    ! rotation of 3 prism system
      double precision fac      ! distance from vertex to centre of prism face
      double precision z1       ! first prism apex coordinates
      double precision x1       !   "      "    "     "    "
      double precision al2      ! distance between apices of adjacent prisms
      double precision z2       ! second prism apex coordinates
      double precision x2       !   "      "     "     "    "
      double precision al3      ! distance between apices of first and third ps
      double precision z3       ! third prism apex coordinates
      double precision x3       !   "     "     "     "    "
      double precision phi1     ! prism face angles
      double precision phi2     !   "     "     "
      double precision phi3     !   "     "     "
      double precision phi4     !   "     "     "
      double precision phi5     !   "     "     "
      double precision phi6     !   "     "     "
      double precision facet    ! total length of prism face
      double precision z4       ! fourth pole vertex base last prism
      double precision x4       !    "     "     "     "    "    "
      double precision l        ! distance between first and third face centres
      double precision xpe      ! X distance from third prism to echelle
      double precision pe       ! actual distance from third prism to echelle
      double precision m        ! prism system displacement wrt collimator
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Copy COMMON variables to DOUBLE PRECISION equivalents
*
      wave0=ech_wave0           ! wavelength at field centre
      angle=ech_angle           ! prism angle
      xe=ech_collxe             ! collimated axis to echelle distance
      face=ech_prface           ! prism face length, just glass
      apal=ech_prapex           ! prism apex along face
*
*   Copy the central wavelength to DOUBLE PRECISION equivalent and take a copy
*   of the prism angle (done so that its name coincides with the name used in
*   Francisco Diego's thesis).
*
      wavec=wc
      a=angle
*
*   Calculate prism refractive indices and the reference wavelength and at the
*   required wavelength.
*
      call ech_refindex(wave0, refr0, status)
      call ech_refindex(wavec, refrc, status)
*
*   Calculate incidence angle at minimum deviation for both wavelengths.
*
      iwave0=asin(refr0*sin(a/2.0))
      iwavec=asin(refrc*sin(a/2.0))
*
*   Calculate a single prism deviation at the reference wavelength.
*
      dev=2.0*iwave0 - a
*
*   Calculate the rotation of the 3 prism system.
*
      i2wavec=2.0*iwave0 - iwavec
      gamma=asin(refrc*sin(a-asin(sin(i2wavec)/refrc)))-iwave0
*
*   All coordinates are referred to the intersection of the optical axis with
*   the first prism face. All are in metres.
*
*     Z is as required for GRT for each pole.
*     Z is horizontal, positive to the left.
*     X is positive upwards.
*     Y is positive coming out from the paper.
*
      fac=face/2.0 + apal
*
*   Calculate the first prism apex coordinates. Z1 is referred to the previous
*   surface, placed at the intersection with the Z axis.
*
      z1=-fac*sin(iwave0+gamma)
      x1=fac*cos(iwave0+gamma)
*
*   Calculate the second prism apex coordinates.
*
      al2=(face+apal)*cos(90.0/rad-iwave0)*2.0
      z2=-al2*cos(2.0*iwave0-a+gamma)
      x2=x1 - al2*sin(2.0*iwave0-a+gamma)
*
*   Calculate the third prism apex coordinates.
*
      al3=2.0*al2*cos(iwave0-a/2.0)
      z3=-al2*cos(4.0*iwave0-2.0*a+gamma)
      x3=x2 - al2*sin(4.0*iwave0-2.0*a+gamma)
*
*   Calculate the prism face angles.
*
      phi1=iwave0 + gamma
      phi2=phi1 - a
      phi3=phi2 + 2.0*iwave0
      phi4=phi3 - a
      phi5=phi4 + 2.0*iwave0
      phi6=phi5 - a
*
*   Calculate the 4th pole vertex base for the last prism.
*
      facet=face+apal
      z4=facet*sin(phi6)
      x4=x3 - facet*cos(phi6)
*
*   Calculate the prism system displacement with respect to the collimator.
*   (The 3.0 below is presumably ech_npr - the number of prisms.)
*
      l=al3 - 2.0*fac*sin(2.0*iwave0-1.5*a)
      xpe=abs(xe-x3) - fac*cos(phi6)
      pe=xpe/cos(90.0/rad-3.0*dev-2.0*gamma)
      m=-(pe*sin(2.0*gamma)+l*sin(gamma)*cos(1.5*dev))/sin(3.0*dev)
*
*   Return the single precision prism system rotation.
*
      gam=gamma
*
*   Convert the prism position to millimetres.
*
      pos=m*1000.0

      end
