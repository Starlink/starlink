      subroutine gr_range(data,is,ie,pmin,pmax,status)
*+
* Name:
*    GR_RANGE

* Invocation:
*    CALL GR_RANGE(DATA,IS,IE,PMIN,PMAX,STATUS)

* Purpose:
*  To determine the range of a plot, on the basis of a data array.

* Description:
*  To determine the range of a plot, on the basis of a data array.
*  This traps the case of all the data being of 1 value, and sets the
*  range a little to either side.
*
* Arguments:
*   DATA(IE) = REAL ARRAY (Given)
*     Data array
*   IS = INTEGER (Given)
*     Start element of data array to consider
*   IE = INTEGER (Given)
*     End element of data array to consider
*   STATUS = INTEGER (Given and returned)
*     Error status. 0=ok
*   PMIN = REAL (Returned)
*     Minimum value for plot range
*   PMAX = REAL (Returned)
*     Maximum value for plot range
*   GR_RANGE = LOGICAL (Returned)
*     If data present (i.e. not bad values).
* Subroutines/functions referenced:
*      GEN_RANGEF, GEN_SIMILAR, PGRNGE

* Author:
*   T.N.Wilkins, Cambridge,  1-JUN-1990
*-
      implicit none
      include 'SAE_PAR'
      integer is
      integer ie
      real data(ie)
      real pmin
      real pmax
      integer status
      include 'PRM_PAR'

*

      integer pstat
      logical gen_similar
      real vmin,vmax,delta

      call gen_rangef(data,is,ie,vmax,vmin)
      if(gen_similar(vmin,vmax)) then
        if(gen_similar(vmin,val__badr)) then
          call par_wruser('No data for this plot',pstat)
          status = SAI__ERROR
        else
          delta = max(1.0,abs(vmin*1.0e-6))
          vmin = vmin - delta
        end if
        vmax = vmax + delta
      end if
      call pgrnge(vmin,vmax,pmin,pmax)
      end
