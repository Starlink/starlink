      subroutine get_substr(envir,maxsub,substructs,nsub,status)
*+
* Name:
*    GET_SUBSTR

* Invocation:
*    CALL GET_SUBSTR(ENVIR,MAXSUB,SUBSTRUCTS,NSUB,STATUS)

* Purpose:
*  To search for sub-structures in an HDS file-uses DTA.

* Description:
*  To search for sub-structures in an HDS file-uses DTA.

* Arguments:
*      ENVIR = CHARACTER*(*) (Given)
*        Environment
*      MAXSUB = INTEGER (Given)
*        Size of SUBSTRUCTS array
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*      SUBSTRUCTS(MAXSUB) = CHARACTER*(*) ARRAY (Returned)
*        Sub-structures
*      NSUB = INTEGER (Returned)
*        Number of sub-structures found

* Subroutines/functions referenced:

* History:
*   T.N.Wilkins, Cambridge, 19-OCT-1990
*-
      implicit none
      include 'SAE_PAR'
      character*(*) envir
      integer maxsub
      character*(*) substructs(maxsub)
      integer nsub
      integer status

*
      if(status.ne.SAI__OK) return
      nsub = 0
      do while(status.eq.SAI__OK)
        nsub = nsub + 1
        call dta_nmvar(envir,nsub,substructs(nsub),status)
        if(nsub.eq.maxsub) status = 1
      end do
      nsub = nsub - 1
      status = 0
      end
