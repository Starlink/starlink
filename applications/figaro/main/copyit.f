      subroutine copyit(nz,nx,ny,z,zn,pixlen,npixel,angle,xs,ys,
     :   xref,xpts,yref,ypts,temp)
*+
* Name:
*    COPYIT

* Invocation:
*    CALL COPYIT(NZ,NX,NY,Z,ZN,PIXLEN,NPIXEL,ANGLE,XS,YS,
*        XREF,XPTS,YREF,YPTS,TEMP)

* Purpose:
*  To copy a cube into a longslit-type spectrum

* Description:
*  To copy a cube into a longslit-type spectrum

* Arguments:
*    nz            (i) : Dimension of cube (wavelength direction)
*    nx            (i) :    "      "    "
*    ny            (i) :    "      "    "
*    z(nz,nx,ny)   (r) : Input cube
*    zn(nz,npixel) (r) : Output array
*    pixlen        (r) : Pixel length
*    npixel        (i) : Number of pixels
*    angle         (r) : Angle
*    xs            (r) : X start position
*    ys            (r) : Y start position
*  Workspace = REAL (Given)
*
*    xref(nx),xpts(npixel),yref(ny),ypts(npixel),temp(npixel)

* Author:
*   T.N.Wilkins 30/3/88
*-
      implicit none
      integer nz,nx,ny,npixel
      real z(nx,ny,nz),zn(nz,npixel),pixlen,angle,xs,ys,sinang,cosang
      integer i,k
      real xref(nx),xpts(npixel),yref(ny),ypts(npixel),temp(npixel)

      cosang = cos(angle)
      sinang = sin(angle)
      do i = 1, nx
        xref(i)=real(i)
      end do
      do i = 1, ny
        yref(i)=real(i)
      end do
      do i = 1, npixel
        xpts(i) = (xs + pixlen * real(i - 1)*sinang)
        ypts(i) = (ys + pixlen * real(i - 1)*cosang)
      end do
      do k = 1, nz
        call itplbv2(z(1,1,k),22,nx,ny,xref,yref,npixel,xpts,
     :      ypts,temp)
        do i = 1, npixel
          zn(k,i) = temp(i)
        end do
      end do
      end
