      subroutine init_tols(tol,ntols,ifarc,sdata,wavdim)
*+
* Name:
*    INIT_TOLS

* Invocation:
*    CALL INIT_TOLS(TOL,NTOLS,IFARC,SDATA,WAVDIM)

* Purpose:
*  To initialise the tolerance array TOL.

* Description:
*  To initialise the tolerance array TOL.

* Arguments:
*    TOL(NTOLS) = REAL ARRAY (Given)
*        Tolerances
*    NTOLS = INTEGER (Given)
*        Number of tolerances
*    IFARC = LOGICAL (Given)
*        If the image is an arc
*    SDATA(WAVDIM) = REAL ARRAY (Given)
*        X array (only used if image an arc)
*    WAVDIM = INTEGER (Given)
*        Number of channels

* History:
*  TNW 4/10/90 Modified so that default values are read from file (these
*           are multiplied by the dispersion and such like if required).
*
*-
      implicit none
      integer ntols
      real tol(ntols)

* Physical units/channel

      real dispersion
      logical ifarc
      integer wavdim
      real sdata(wavdim)

* Get value for dispersion

      call get_dispers(sdata,wavdim,dispersion)

* tolernaces on line centres

      tol(1)   = tol(1) * dispersion
      if(ifarc) then
        tol(3) = sdata(1)
        tol(2) = sdata(wavdim)
      end if

* tolerances on line width

      tol(4)   = tol(4) * dispersion
      tol(5)   = tol(5) * dispersion
      tol(6)   = tol(6) * dispersion
      end
