      subroutine modify_params(params,oldpar,newpar)
*+
* Name:
*    MODIFY_PARAMS

* Invocation:
*    CALL MODIFY_PARAMS(PARAMS,OLDPAR,NEWPAR)

* Purpose:
*  To change the occurance of OLDPAR to NEWPAR.

* Description:
*  To change the occurance of OLDPAR to NEWPAR. Multiple occurances of
*  OLDPAR are not checked for.
*
* Arguments:
*    OLDPAR = CHARACTER*(*) (Given)
*      Old name of parameter
*    NEWPAR = CHARACTER*(*) (Given)
*      New name of parameter
*    PARAMS = CHARACTER*(*) (Given and returned)
*      Parameter names array
* History:
*   T.N.Wilkins, Cambridge, 15-AUG-1990
*-
      implicit none
      character*(*) params
      character*(*) oldpar
      character*(*) newpar
      integer ind,test,status

*

      ind = index(params,oldpar)
      if ( ind.ne.0 ) then

*   We have to make sure that the reference picked up is a true
*   parameter name, i.e. in the correct place.

        test = ind/10
        if ( (test*10+1).eq.ind ) then
          params(ind:ind+9) = newpar
        else
          call par_wruser('Error, parameter name not in correct place'
     :            ,status)
        end if
      else
        call par_wruser('Error, parameter name not found',status)
      end if
      end
