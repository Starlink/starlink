*+  ECH_FINDORDER  -  Determine the order number of a specified wavelength
*                     at a specified configuration

      subroutine ech_findorder(wave, order, status)
*
*   Description :
*
*     Use ECH_ORDERNUM to find the order number (which may be out by 1).
*     Derive the free spectral range of this order, and if the given wavelength
*     is outside this range, adjust the order number appropriately.
*
*     ECH_INIT must be called before calling this routine.
*
*   Invocation :
*
*     CALL ECH_FINDORDER (WAVE, ORDER, STATUS)
*
*   Arguments :
*
*     WAVE    =  REAL (READ)           Wavelength whose order number is to be
*                                      determined (ie the wavelength will lie
*                                      within the free spectral range of the
*                                      order). (Angstroms)
*     ORDER   =  INTEGER (WRITE)       Order number corresponding to wavelength
*     STATUS  =  INTEGER (READ, WRITE) Global status value
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     M.P. Fisher RGO  (GXSEG0::MPF)
*
*   History :
*
*     13 Jan 1994 : Initial version, ideas from F. Diego's ECHGAM.FOR
*                   (GXSEG0::MPF)
*     23 Sep 1994 : Renamed ech_findorder for Unix version because archive
*                   truncates filenames of more tham 15 characters. (MPF/RGO)
*
*   Type definitions :
*
      implicit none              ! no default typing allowed
*
*   Global constants :
*
      include 'SAE_PAR'         ! ADAM error codes
*
*   Import :
*
      real wave                  ! central wavelength
*
*   Export :
*
      integer order              ! order number
*
*   Status :
*
      integer status
*
*   Global Variables :
*
      include 'ech_common'       ! current echelle parameters
*
*   Local variables :
*
      real free                  ! free spectral range
      real gamma                 ! approximate echelle gamma of wavelength
      real pos                   ! prism position
      real thetaoff              ! echelle theta that gives centre of FSR
      real wave1                 ! approximate central wavelength of order
      real wavec                 ! accurate central wavelength of order
      real wavemax               ! max. wavelength in FSR of order
      real wavemin               ! min. wavelength in FSR of order
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status .ne. sai__ok) return

      thetaoff = ech_thetab - ech_blaze0
*
*   Find order in which the wavelength will lie within the FSR (may be out by 1)
*
      call ech_ordernum(wave, 0.0, 0.0, order, status)
*
*   Find approximate central wavelength of calculated order
*
      call ech_wcentral(order, thetaoff, 0.0, wave1, status)
*
*   Find the echelle gamma angle near centre of calculated order
*
      call ech_prismpos(wave1, gamma, pos, status)
*
*   Find accurate central wavelength of calculated order
*
      call ech_wcentral(order, thetaoff, gamma, wavec, status)
*
*   Find FSR of order and its wavelength range
*
      call ech_frspr(order, thetaoff, gamma, free, status)
      wavemin = wavec - free/2.0
      wavemax = wavemin + free
*
*   Adjust order number if wavelength is outside the FSR
*
      if (wave .lt. wavemin) then
        order = order + 1
      else if (wave .gt. wavemax) then
        order  = order - 1
      endif

      end


