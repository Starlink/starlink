      subroutine scale_data(data,dens)
*+
* Name:
*    SCALE_DATA

* Invocation:
*    CALL SCALE_DATA(DATA,DENS)

* Purpose:
*   Scale data for optimisation

* Description:
*   Scale data held in ADATA and ADENS to the range
*   0 to 1,for optimization ,and resulting scaled
*   data in DATA and DENS ,and the scaleing factors
*   DENSC and DATSC
*
* Arguments:
*    Altered
*      DENS(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        Y data (scaled on output)
*      DATA(MPTS) = DOUBLE PRECISION ARRAY (Given)
*        X data (scaled on output)
* Global variables:
*      MPTS = INTEGER (Given)
*        Number of data elements (include file opt_cmn)
*      DATSC = DOUBLE PRECISION (Returned)
*        X scaling factor (include file opt_cmn)
*      DATAZERO = DOUBLE PRECISION (Returned)
*        X scaling zero point (include file opt_cmn)
*      DENSC = DOUBLE PRECISION (Returned)
*        Y scaling factor (include file opt_cmn)
*      DENSZERO = DOUBLE PRECISION (Returned)
*        Y scaling zero point (include file opt_cmn)

* Authors:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW 13/3/91 DENSZERO added
*   TNW 21/10/91 Made to scale "in place"
*-
      implicit none

* Export

      include 'opt_cmn'
      double precision data(mpts),dens(mpts)

* local

      double precision denmax,dentst,invdensc,invdatsc
      integer i
*
* calculate scaleing factors for position and density
*
      denmax    = dens(1)
      denszero  =  denmax
      do i = 2, mpts
        dentst = dens(i)
        denmax  = max(denmax,dentst)
        denszero  = min(denszero,dentst)
      end do
      datazero  = data(1)
      datsc     = data(mpts)-datazero
      densc     = denmax-denszero
      invdatsc = 1.0d0/datsc
      invdensc = 1.0d0/densc
      do i = 1, mpts
        data(i) = (data(i)-datazero) * invdatsc
        dens(i) = (dens(i)-denszero) * invdensc
      end do
      end
