*+  ECH_REFINDEX  -  Determine the refractive index of fused silica
*                    at a specified wavelength

      subroutine ech_refindex (wave, ref, status)
*
*   Description :
*
*     Use the empirical expression derived by I.H. Malitson, J.O.S.A 55, 1205,
*     Oct 1965. This expression is:
*     \begin{verbatim}
*                  0.6961663 . w**2     0.4079426 . w**2      0.8974794 . w**2
*     ref**2 = 1 + ----------------- + ------------------- + ------------------
*                 w**2 - 0.0684043**2  w**2 - 0.0062414**2   w**2 - 9.896161**2
*     \end{verbatim}
*     where wavelength is measured in microns. This routine uses wavelength in
*     Angstroms and converts to microns internally.
*
*   Invocation :
*
*     CALL ECH_REFINDEX (WAVE, REF, STATUS)
*
*   Arguments :
*
*     WAVE    =  DOUBLE (READ)           Wavelength (Angstroms)
*     REF     =  DOUBLE (WRITE)          Refractive index
*     STATUS  =  INTEGER (READ, WRITE)   Global status value
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     W.F. Lupton  AAO  (AAOEPP::WFL)
*
*   History :
*
*     12 Dec 1989  : Original version (AAOEPP::WFL)
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
      double precision wave     ! wavelength in Angstroms
*
*   Export :
*
      double precision ref      ! refractive index
*
*   Status :
*
      integer status            ! global status value
*
*   Local variables :
*
      double precision wm2      ! square of wavelength in microns
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   Convert wavelength to microns and square it.
*
      wm2=(wave*wave)/(10000.0*10000.0)
*
*   Calculate refractive index according to the above formula.
*
      ref=sqrt(1.0+wm2*(0.6961663/(wm2-0.0684043**2.0)
     :                 +0.4079426/(wm2-0.1162414**2.0)
     :                 +0.8974794/(wm2-9.8961610**2.0)))

      end

