      integer function oget_parnum(param,parr)
*+
* Name:
*    OGET_PARNUM

* Invocation:
*   (INTEGER) = OGET_PARNUM(PARAM,PARR)

* Purpose:
*     To locate a parameter in the old format results structure

* Description:
*     To locate a parameter in the old format results structure using the
*    parameter identification array. Note that PARAMS and PARAM are
*    case-sensitive.
*
* Arguments:
*      PARAM = CHARACTER*(*) (Given)
*        Parameter to find (letters which can be missed
*                         out enclosed by (...).
*      PARR = CHARACTER*(*) (Given)
*        Parameter names array
*      OGET_PARNUM = INTEGER (Returned)
*        Location of parameter in "cube"
* Subroutines/functions referenced:
* History:
*   T.N.Wilkins, Cambridge,  7-8-JUN-1989
*        "           "       17-18-JUL-1989 Bug fixes, also to update
*                            .params structure if required
*        "           "       15-SEP-1989 New (more portable) method of
*                            referencing params array
*        "           "       30-NOV-1989 Bug fix to prevent writing
*                            beyond array limits.
*        "           "       14-MAY-1990 Checking of array moved to
*                            MAP_RES
*        "           "       30-MAY-1991 DSA_WRUSER replaced PAR_WRUSER
*-
      implicit none
      include 'arc_dims'
      character*(*) param,parr
      integer ilen,ind,ind1,tmpstart
      character*10 tmpp*20,tmppa
      integer jcnt,j,jcnta
      logical miss
      character*2 bss,bsn
      data bss/'\\'/
      bsn = bss(1:1)//'n'

*
      tmpp = ' '
      ilen = min(len(param),len(tmpp))
      tmpp(:ilen) = param(:ilen)
      oget_parnum = 0

* If a slash is present, then the abbreviated name is to be used. So
* work this out in advance. Also remove brackets from full name

      jcnt = 0
      jcnta = 0
      miss = .false.
      do j = 1, ilen
        if(tmpp(j:j).eq.' ') then
          goto 1
        else if(tmpp(j:j).eq.'(') then
          miss = .true.
        else if(tmpp(j:j).eq.')') then
          miss = .false.
        else
          jcnt = jcnt + 1
          if(.not.miss) then
            jcnta = jcnta + 1
            tmppa(jcnta:jcnta) = tmpp(j:j)
          end if
          tmpp(jcnt:jcnt) = tmpp(j:j)
        end if
      end do
  1   continue
      do j = jcnt+1, len(tmpp)
        tmpp(j:j) = ' '
      end do
      do j = jcnta+1, 10
        tmppa(j:j) = ' '
      end do

* Now search through PARAMS

      ind = index(parr,tmpp(:10))
      if(ind.ne.0) then
        oget_parnum = ind/10 + 1
      else

*   Abbreviated form of name to be used

        ind = index(parr,tmppa(:jcnta))
        if(ind.ne.0) then
          ind1 = (ind/10)*10

*     should be preceded by a slash

          if((ind - ind1).gt.1) then
            tmpstart = ind - 1
            if(parr(tmpstart:tmpstart).eq.'/')
     :         oget_parnum = ind/10 + 1
          else
            oget_parnum = ind/10 + 1
          end if
        end if
        if(oget_parnum.eq.0) then
          call dsa_wruser('Error, parameter ')
          call dsa_wruser(param)
          call dsa_wruser(' not found in old structure')
          call dsa_wruser(bsn)
        end if
      end if
      end
