      subroutine check_masking(line,istartx,iendx,istarty,iendy,masked,
     :              mask)
*+
* Name:
*    CHECK_MASKING

* Invocation:
*    CALL CHECK_MASKING(LINE,ISTARTX,IENDX,ISTARTY,IENDY,MASKED,
*                   MASK)

* Purpose:
*    Check if point masked out

* Description:
*   Check if any point inside the current window is masked out
*   if it is assume whole window is masked out amd return masked
*   = .TRUE.
*
* Arguments:
*     LINE = INTEGER (Given)
*        Current line
*     ISTARTX = INTEGER (Given)
*        Start cross-section
*     IENDX = INTEGER (Given)
*        End cross-section
*     ISTARTY = INTEGER (Given)
*        Start in 2nd spatial dimension
*     IENDY = INTEGER (Given)
*        End in 2nd spatial dimension
*     MASK(NYP,NXP,SPDIM2) = INTEGER*2 ARRAY (Given)
*        Mask
*     MASKED = LOGICAL (Given and returned)
*        If masked out
*
      implicit none
      include 'arc_dims'
*-
      integer status
      integer istartx,iendx,line,istarty,iendy
      logical masked
      integer*2 mask(nyp,nxp,spdim2)
* local
      character*60 chars
      integer istore,jstore,len1

      istore = istartx
      jstore = istarty

*
*  Check mask value against user's threshold iteration value
*
*       NOTE:     Fit not considered <=  iteration <  Fit considered
*
      do while ( (.not.masked) .and. (jstore.le.iendy) )
        if( (mask(line,istore,jstore).ne.0).and.(
     :      mask(line,istore,jstore).lt.iteration)) then
          masked = .true.

*     Tell user that point is masked out

          len1 = 0
          call chr_fill(' ',chars)
          call chr_putc('Point',chars,len1)
          if(nyp.gt.1) then
            call chr_putc(', Line : ',chars,len1)
            call chr_puti(line,chars,len1)
          end if
          call chr_putc(', Position : ',chars,len1)
          call chr_puti(istore,chars,len1)
          if(spdim2.gt.1) then
            call chr_putc(', ',chars,len1)
            call chr_puti(jstore,chars,len1)
          end if
          call chr_putc(' MASKED OUT',chars,len1)
          call opt_wruser(chars(:len1),status)
        end if
        istore=istore+1
        if(istore.gt.iendx) then
          istore = istartx
          jstore = jstore + 1
        end if
      end do
      end
