      subroutine correct2(data,coeffl,nl,ni,mord,kp1,i,lcor,xin,add)
*+
* Name:
*    CORRECT2

* Invocation:
*    CALL CORRECT2(DATA,COEFFL,NL,NI,MORD,KP1,I,LCOR,XIN,ADD)

* Purpose:
*    To subtract a continuum from data.

* Description:
*    To subtract a continuum from data, using coefficients e.t.c.
*     which have already been found         - 2 D version
* The option is also provied to add it back.
*
* Arguments:
*    COEFFL(MORD,NI) = DOUBLE PRECISION ARRAY (Given)
*        Coefficients
*    NL = INTEGER (Given)
*        Number of channels
*    NI = INTEGER (Given)
*          "    "  xsects
*    MORD = INTEGER (Given)
*        Maximum order (for dimensions)
*    KP1 = INTEGER (Given)
*        Order+1
*    I = INTEGER (Given)
*
*    ADD = LOGICAL (Given)
*
*    DATA(NL,NI) = REAL ARRAY (Given and returned)
*        The data
*    LCOR(NL) = DOUBLE PRECISION ARRAY (Workspace)
*
*    XIN(NL) = DOUBLE PRECISION ARRAY (Workspace)
*
* History:
*   TNW/CAVAD 17/8/90 ADD argument added
*-
      implicit none
      integer nl,ni,i,j
      real data(nl,ni)
      integer mord
      double precision coeffl(mord,ni)
      integer kp1
      logical add
*
*  Local
*
      double precision xin(nl)
      double precision lcor(nl)

      call fill_map2(lcor,nl,coeffl(1,i),kp1,mord,xin)

      if(add) then
        do j=1,nl
          data(j,i) = data(j,i)+real(lcor(j))
        end do
      else
        do j=1,nl
          data(j,i) = data(j,i)-real(lcor(j))
        end do
      end if

      end
