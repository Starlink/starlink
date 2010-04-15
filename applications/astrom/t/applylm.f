      program applylm

*   Test understanding of ASTROM and dcmpf.
*
*   The plan here is to generate a linear fit from known zero points,
*   scales, non-perpendicularity and orientation, then use slalib to
*   decompose them.
*
*   The test works, in the sense that in the ASTROM output file I do get
*   the same signs for non-perp and orientation, and roughly the same
*   magnitudes, as I put in.  I don't get exactly the same numbers back
*   out, though.  I think this is just because, below, the _zeros_ I
*   give to makecpts are zero, and I apply the (xzero, yzero)
*   translation afterwards.  It's fairly clear that this is wrong, but I
*   can't quite see why.  This is unlikely to be a problem with ASTROM,
*   however, and this program was originally intended only to test signs
*   with an explicit set of input values.
*
*   Running astrom generates sla_SVD warning 5 (which is just a harmless
*   nullity warning, I think)

      implicit none
      doubleprecision xzero, yzero, xscale, yscale, perp, orient
      doubleprecision lm(6)
      doubleprecision r2d
      parameter (r2d=57.295779513082320876)
      integer npixpos,i,j
      parameter(npixpos=9)
      doubleprecision pixpos(2*npixpos)
      doubleprecision xrad, yrad
      integer ihmsf(4), idmsf(4)
      character xsign,ysign
      integer status

      doubleprecision sla_dranrm, sla_drange

*   The test parameters
      call sla_dtf2r(17, 0, 0, xzero, status)
      call sla_daf2r(74, 0, 0, yzero, status)
      xscale = -1d0/3600/r2d    ! 1 arcsec per pixel
      yscale = +1d0/3600/r2d    ! 1 arcsec per pixel
      perp = -0.5d0/r2d     ! non-perp = -0.25 deg
      orient = -90d0/r2d            ! orientation = -85 deg
*      perp = -0.25d0/r2d            ! non-perp = -0.25 deg
*      orient = -85d0/r2d            ! orientation = -85 deg

**   On the equator
*      call sla_dtf2r(0, 0, 0, xzero, status)
*      call sla_daf2r(0, 0, 0, yzero, status)
*      xscale = -1d0/3600/r2d    ! 1 arcsec per pixel
*      yscale = +1d0/3600/r2d    ! 1 arcsec per pixel
*      perp = 0d0               ! square
*      orient = 0d0             ! upright

**   The parameters in t1.sh
*      call sla_dtf2r(17, 27, 9.1d0, xzero, status)
*      call sla_daf2r(74, 20, 8d0, yzero, status)
*      xscale = -0.3313d0/3600/r2d
*      yscale = +0.3315d0/3600/r2d
*      perp = -0.345d0/r2d
*      orient = -88.828d0/r2d

      call makecpts(lm, 0d0, 0d0, xscale, yscale, perp, orient)
      write(*,'("*** Components: ",6e11.3)') (lm(j),j=1,6)

      pixpos(1)  = -100
      pixpos(2)  = +100
      pixpos(3)  = 0
      pixpos(4)  = +100
      pixpos(5)  = +100
      pixpos(6)  = +100

      pixpos(7)  = -100
      pixpos(8)  = 0
      pixpos(9)  = 0
      pixpos(10) = 0
      pixpos(11) = +100
      pixpos(12) = 0

      pixpos(13) = -100
      pixpos(14) = -100
      pixpos(15) = 0
      pixpos(16) = -100
      pixpos(17) = +100
      pixpos(18) = -100

      write(*,'("J2000")')
      write(*,'("ASTR")')
      call sla_dr2tf(3, xzero, xsign, ihmsf)
      call sla_dr2af(2, yzero, ysign, idmsf)
      write(*,'(i2,2i3.2,".",i3.3,2x,a,i3.3,2i3.2,".",i2.2,
     :     "  J2000 2000.0")')
     :     (ihmsf(j),j=1,4), ysign, (idmsf(j),j=1,4)

      do i=1,npixpos
         call sla_xy2xy(pixpos(2*i-1),pixpos(2*i),lm,xrad,yrad)
         xrad = xrad + xzero
         yrad = yrad + yzero
         call sla_dr2tf(3, sla_dranrm(xrad), xsign, ihmsf)
         call sla_dr2af(2, sla_drange(yrad), ysign, idmsf)
         write(*,'(i2,2i3.2,".",i3.3,2x,a,i3.3,2i3.2,".",i2.2,
     :        "  0.0  0.0  2000.0")')
     :        (ihmsf(j),j=1,4), ysign, (idmsf(j),j=1,4)
         write(*,'(2f10.2)') pixpos(2*i-1), pixpos(2*i)
*         write(*,'("x,y=", 2f7.0, "->", 2e10.2, "=",a,4i4,1x,a,4i4)')
*     :        pixpos(2*i-1), pixpos(2*i),
*     :        xrad, yrad,
*     :        xsign, ihmsf, ysign, idmsf
      enddo

      write(*,'("END")')

      stop
      end

      subroutine makecpts (mat, xz, yz, xs, ys, perp, orient)
* Do the forward transformation that slaDcmpf inverts.
*
* Args are xz, yz, xs, ys, perp/rad, orient/rad
* Write a, b, c, d, e, f to mat()
*
      implicit none
      doubleprecision xz, yz, xs, ys, perp, orient
      doubleprecision mat(6)
      doubleprecision c1, s1, c2, s2, rk11, rk12, rk21, rk22

      c1 = cos (orient)
      s1 = sin (orient)
      c2 = cos (perp/2.0)
      s2 = sin (perp/2.0)
      rk11 = +c1*c2+s1*s2
      rk12 = +c1*s2+s1*c2
      rk21 = -s1*c2+c1*s2
      rk22 = -s1*s2+c1*c2

      mat(1) = rk11*xs*xz + rk12*ys*yz
      mat(2) = rk11*xs
      mat(3) = rk12*ys
      mat(4) = rk21*xs*xz + rk22*ys*yz
      mat(5) = rk21*xs
      mat(6) = rk22*ys

      end
