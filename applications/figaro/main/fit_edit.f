      subroutine fit_edit(results,resvar,status)
*+
* Name:
*    FIT_EDIT

* Invocation:
*    CALL FIT_EDIT(RESULTS,RESVAR,STATUS)

* Purpose:
*   Edit fits

* Description:
*    To edit fits in the results block. This allows components to be
*    moved or deleted as required. This version doesn't alter the
*    fit status.
*
* Arguments:
*      RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given and returned)
*        Results block
*      RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given and returned)
*        Results block variance
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*  Subroutines/functions referenced:
*    GET_PARNUM, PAR_QNUM, PAR_QSTR
* Author:
*    T.N.Wilkins, Cambridge,  9-APR-1991
* History:
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer status

*

      logical more,qstat,par_qnum
      integer lines,parnums,lined,parnumd,get_parnum,i,j,nopts,key
      parameter (NOPTS = 3)
      integer OPT_COPY,OPT_DELETE,OPT_EXIT
      parameter (OPT_COPY = 1, OPT_DELETE = 2, OPT_EXIT = 3)
      real rlines,rlined,value
      include 'PRM_PAR'
      integer dumi
      real dumr
      character dumc
      character*10 parnam
      character*20 dict(NOPTS)
      data dict/
     :     'COPY   : Copy fits',
     :     'DELETE : Delete fits',
     :     'EXIT   : Exit'/

      rlines = 1.0
      rlined = 1.0
      more = .true.
      do while(more)
        call qmenu('Edit Menu',dict,NOPTS,opt_exit,dumr,dumc,key,dumi
     :       ,status)

        if(key.eq.OPT_COPY) then

*     Copy fits

*      Get source line/parameter

          qstat = par_qnum('Source line?',1.0,real(line_count),rlines,
     :            .true.,' ',value)
          rlines = value
          lines = nint(rlines)
          call par_qstr('Source parameter?',' ',.false.,.false.,parnam)
          parnums = get_parnum(parnam)

*      Get destination line/parameter

          qstat = par_qnum('Destination line?',1.0,real(line_count),
     :            rlined,.true.,' ',value)
          rlined = value
          lined = nint(rlined)
          call par_qstr('Destination parameter?',' ',.false.,.false.,
     :            parnam)
          parnumd = get_parnum(parnam)

*      Perform the copy

          do j = 1, spdim2
            do i = 1, spdim1
              results(parnumd,lined,i,j) = results(parnums,lines,i,j)
              resvar(parnumd,lined,i,j) = resvar(parnums,lines,i,j)
            end do
          end do
        else if(key.eq.OPT_DELETE) then

*     Delete fits

*      Get line/parameter

          qstat = par_qnum('Line?',1.0,real(line_count),rlines,.true.,
     :                  ' ',value)
          rlines = value
          lines = nint(rlines)
          call par_qstr('Parameter?',' ',.false.,.false.,parnam)
          parnums = get_parnum(parnam)

*       Delete by setting to bad value

          do j = 1, spdim2
            do i = 1, spdim1
              results(parnums,lines,i,j) = VAL__BADR
              resvar(parnums,lines,i,j) = VAL__BADR
            end do
          end do

        else

*     Exit

          more = .false.
        end if
      end do
      end
