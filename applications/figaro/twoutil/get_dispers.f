      subroutine get_dispers(sdata,in,dispersion)
*+
* Name:
*    GET_DISPERS

* Invocation:
*    CALL GET_DISPERS(SDATA,IN,DISPERSION)

* Purpose:
*   Get dispersion

* Description:
*      This subroutine uses the wavelength data supplied in sdata
*     to evaluate the dispersion of the data. It assumes that
*     the minimum dispersion found should be used, and thus
*     will work even if the data are not wavelength calibrated.
*     Because of the way that the sdata array is used in FIGARO
*     the value of dispersion will automatically be set to UNITY
*     for the case of non-wavelength calibrated data.

* Arguments:
*      IN = INTEGER (Given)
*        Number of channels in Spectrum
*      SDATA(IN) = REAL ARRAY (Given)
*        The X data structure from figaro giving channels
*                     scale
*      DISPERSION = REAL (Returned)
*        The calculated dispersion
* External references:
*            NONE
* Libraries:
*            NONE
* Method:
*        obvious
* Authors :
*     Dave Axon (MAN::DJA)
* History:
*      11_FEB 1985:    Original. (MAN:DJA)
*-
*    Type Definitions :

* Must declare everything

      implicit none
*    Import
      integer in
      real sdata(in)
*    Global constants :
*    Export :
       Real dispersion
*    Status :
*    Global variables :
*    Local constants :
*    Local variables :

*  do loop

       Integer i
* -----------------------------------------------------------

* set dispersion to some big value

      dispersion = sdata(in)-sdata(1)

* find minimum dispersion

      do i = 1,(in-1)
        dispersion = min(dispersion,(sdata(i+1)-sdata(i)))
      end do
      end
