      subroutine correct(data,coeffi,coeffl,ni,nl,mord,kp1i,kp1l,xin,
     :               icor,lcor,nli)
*+
* Name:
*    CORRECT

* Invocation:
*    CALL CORRECT(DATA,COEFFI,COEFFL,NI,NL,MORD,KP1I,KP1L,XIN,
*                    ICOR,LCOR,NLI)

* Purpose:
*    To correct for vignetting in two dimensions using already
*  determined coefficients.
*
* Description:
*    To correct for vignetting in two dimensions using already
*  determined coefficients.
*
* Arguments:
*    COEFFI(MORD) = DOUBLE PRECISION ARRAY (Given)
*        Coefficients channel direction
*    COEFFL(MORD) = DOUBLE PRECISION ARRAY (Given)
*              "      xsect       "
*    NI = INTEGER (Given)
*
*    NL = INTEGER (Given)
*
*    MORD = INTEGER (Given)
*
*    KP1I = INTEGER (Given)
*
*    KP1L = INTEGER (Given)
*
*    NLI = INTEGER (Given)
*
*    DATA(NL,NI) = REAL ARRAY (Given and returned)
*
*    XIN(NLI) = DOUBLE PRECISION ARRAY (Workspace)
*
*    ICOR(NI) = DOUBLE PRECISION ARRAY (Workspace)
*
*    LCOR(NL) = DOUBLE PRECISION ARRAY (Workspace)
*

* History:
* TMP variable introduced, TNW 8/6/92
*-
      implicit none
      integer ni,nl,nli
      real data(nl,ni)
      integer mord
      double precision coeffl(mord),coeffi(mord)
      integer kp1i,kp1l
*
*  Local
*
      double precision xin(nli)
      double precision icor(ni),lcor(nl)
      integer hni,hnl
      real mult,tmp
      integer i,l

      call fill_map2(icor,ni,coeffi,kp1i,mord,xin)
      call fill_map2(lcor,nl,coeffl,kp1l,mord,xin)

      hni=ni/2
      hnl=nl/2

      mult=icor(hni)*lcor(hnl)
      do i=1,ni
        tmp = mult/real(icor(i))
        do l=1,nl
          data(l,i) = data(l,i)*tmp/real(lcor(l))
        end do
      end do
      end
