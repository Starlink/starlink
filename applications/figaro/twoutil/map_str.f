      subroutine map_str(name,mode,arrind,mdim,ndim,dims,status)
*+
* Name:
*    MAP_STR

* Invocation:
*    CALL MAP_STR(NAME,MODE,ARRIND,MDIM,NDIM,DIMS,STATUS)
*
* Description:
*    To map an array. The whole array is mapped and the
*    dimensions are returned.
*
* Purpose:
*    To map an array. The whole array is mapped and the
*    dimensions are returned.
*
* Arguments:
*      NAME = CHARACTER*(*) (Given)
*        DTA name of array, excluding
*                         .MORE.FIGARO.RESULTS or whatever
*      MODE = CHARACTER*(*) (Given)
*        Mode or mapping for ACCRES
*      MDIM = INTEGER (Given)
*        Dimension of DIMS
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*      ARRIND = INTEGER (Returned)
*        Array index in DYNAMIC_MEM to start of array
*      NDIM = INTEGER (Returned)
*        Number of dimensions of array
*      DIMS(MDIM) = INTEGER ARRAY (Returned)
*        Dimensions of array
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge,  1-4-JUL-1991
* History:
*-
      implicit none
      character*(*) name,mode
      integer arrind
      integer status,mdim
      integer ndim,dims(mdim),i,nelm
      include 'PRM_PAR'

* Get size of data

      ndim = mdim
      call accres(' ',name,'si',ndim,dims,' ',status)

      nelm = 1
      do i = 1,ndim
        nelm = nelm*dims(i)
      end do

* Map data for update

      call accres(' ',name,mode,nelm,arrind,' ',status)

      end





