      subroutine fibslcop(data,nw,nx,ny,xpts,ypts,npt,out)
*+
* Name:
*    FIBSLCOP

* Invocation:
*    CALL FIBSLCOP(DATA,NW,NX,NY,XPTS,YPTS,NPT,OUT)
* Purpose:
*  To extract data from a cube into a long-slit type spectrum.
*
* Description:
*  To extract data from a cube into a long-slit type spectrum.
*
* Arguments:
*    NW = INTEGER (Given)
*        Wavelength dimension of main data array
*    NX = INTEGER (Given)
*        X spatial dimension of main data array
*    NY = INTEGER (Given)
*        Y spatial dimension of main data array
*    DATA(NW,NX,NY) = REAL ARRAY (Given)
*        Main 3-d data array
*    NPT = INTEGER (Given)
*        Number of points to copy to output
*    XPTS(NPT) = INTEGER ARRAY (Given)
*        X coordinates of points required for output
*    YPTS(NPT) = INTEGER ARRAY (Given)
*        Y coordinates of points required for output
*    OUT(NW,NPT) = REAL ARRAY (Returned)
*        Output 2-d array

* History:
*  T.N.Wilkins Manchester 1988
*  Altered to use copr2r TNW 16/12/88
*-
      implicit none
      integer nw,nx,ny,npt,xpts(npt),ypts(npt),j
      real data(nw,nx,ny),out(nw,npt)

      do j = 1,npt
        call copr2r(nw,data(1,xpts(j),ypts(j)),out(1,j))
      end do
      end
