      subroutine copy2work(out,nw,in,add)
*+
* Name:
*    COPY2WORK

* Invocation:
*    CALL COPY2WORK(OUT,NW,IN,ADD)
* Description:
*   To either set out=in or out=out+in, for each element, depending
*   upon the value of add.
* Purpose:
*   To either set out=in or out=out+in, for each element, depending
*   upon the value of add.
* Arguments:
*    NW = INTEGER (Given)
*      Dimensions of arrays
*    IN(NW) = REAL ARRAY (Given)
*      Input array
*    ADD = LOGICAL (Given)
*      If to add (rather than set out=in)
*    OUT(NW) = REAL ARRAY (Given and returned)
*      Output array

* History:
*    T.N.Wilkins / Manchester 6/88
*    Altered to use gen_move, TNW 6/7/88
*    Altered to use copr2r, TNW 16/12/88
*-
      implicit none
      integer nw
      real in(nw),out(nw)
      logical add
      if(add) then
        call gen_addaf(nw,out,in,out)
      else
        call copr2r(nw,in,out)
      end if
      end
