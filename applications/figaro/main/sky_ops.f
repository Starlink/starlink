      subroutine sky_ops()
*+
* Name:
*    SKY_OPS

* Invocation:
*    CALL SKY_OPS()
* Purpose:
*   Sky operations
*
*
* Description:
*  Allows the use of SKY lines for a variety of operations
*  [1] SKY VIG
*      Use the fluxes variations of a SKY line as a function of position
*      to create a insitu VIG type correction.
*       To Accomplish this the SKY line FLUXES are formed from the
*      Results cube and Normalized to unity around some central point in
*      the data.
*      A Chebyschev Poly is fitted to the data.
*
*      the resulting 1D correction curve is applied to the data.
*      Note that the data must then be refitted using the COPY option.
*  [2] ZERO POINT CORRECTIONS.
*      Use the lines identified in the SKY group to determine if there
*      any ZERO point velocity corrections to be applied to the measured
*      velocities determined from the actual OBJECT lines.  In addtion
*      this option also yields the FINAL instrumental resolution of the
*      data.
*  [3] SKY SUBTRACTION
*      This option allow the use of a sky TEMPLATE or an actual BLOCK in
*      the data as a SKY which can be removed from the data.  In the
*      second case of uisng data from the current image to do this we
*      extract the FLUXES of all the LINES in the SKY GROUP from the
*      results cube. We then perform a linear LSQ fit to the differences
*      between the Sky fluxes at each BLOCK in the form Current_block(i
*      = 1, nsky_lines) = SKY_block(...) * CONSTANT The advantage of
*      this technique is that the SKY is automatically matched to the
*      current summation taking into account any residual VIG or
*      FLATFIELD uncertainties.  The TEMPLATE option works in a similar
*      way however a TEMPLATE spectrum is used. Each line in the
*      TEMPALTE is fitted with a GAUSSIAN and the fluxes used as above.
*      SYNTHETIC provides faclities for creating such TEMPLATES from the
*      data.
*
*   [4] SYNTHETIC
*      This creates a SYNTHETIC SKY spectrum from a data BLOCK. The main
*      purspose of this is to enable the removal of contaminating object
*      lines.  This uses either the ALL code to simultaneously fit the
*      lines and continuum or a SPLINE interpolation over the OBJECT
*      lines. It is possible to achieve the same reuslt using the more
*      general SYNTHETIC option in LONGSLIT, but the facility is
*      provided here for convenience.
*
* History:
*  ACD: 28/9/00 Remove local unused variables.

*-
      implicit none
      integer istat,iopt
      logical LOOP

* General Symbolic Constants

      integer STATUS_OK
      integer ZERO
      integer ONE


* Symbolic constants for menu control

      integer INSTRUMENT
      integer REMOVE
      integer VIG
      integer EXIT
      integer SYNTHETIC

      Parameter ( INSTRUMENT = 1)
      Parameter ( REMOVE = 2)
      Parameter ( VIG     = 3)
      Parameter ( SYNTHETIC = 4)
      Parameter ( EXIT      = 5)

      Parameter ( STATUS_OK = 0)
      Parameter ( ZERO      = 0)
      Parameter ( ONE       = 1)

      istat    = ZERO
      LOOP = .TRUE.

      do while (LOOP)


* Dummy routine to invoke sky related operations  for LONGSLIT

*     Get option required by user


         call sky_opts(iopt,istat)

         if( istat .ne. STATUS_OK) then

*      Abort request or error

            goto 500
         else if( iopt .eq. INSTRUMENT ) then

*     Sky velocity/Width measuremnts

            call sky_vel(istat)

         else if( iopt .eq. REMOVE) then

*     Remove the Sky

            call remove_Sky(istat)

         else if(iopt .eq. VIG) then

* SKY Vig correction

            call sky_vig(istat)

* SYNTHETIC
         else if(iopt.eq.SYNTHETIC) then

            call synthetic_sky(istat)
*     Exit

         else if(iopt .eq. EXIT) then
            LOOP = .false.
         end if
      end do

 500  continue
      end


