      subroutine gtprof(ref,data,xa,ya,xadj,total,hex,status)
*+
* Name:
*    GTPROF

* Invocation:
*    CALL GTPROF(REF,DATA,XA,YA,XADJ,TOTAL,HEX,STATUS)

* Purpose:
*    To locate profile(s) using a cursor and display them.

* Description
*    To locate profile(s) using a cursor and display them.
*
* Arguments:
*   DATA(NW,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        The data
*   TOTAL(SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Total values array
*   XADJ(SPDIM2) = REAL ARRAY (Given)
*        X position adjustment
*   XA(SPDIM1) = REAL ARRAY (Given)
*        X (spatial) array
*   YA(SPDIM2) = REAL ARRAY (Given)
*        Y (spatial) array
*   HEX = LOGICAL (Given)
*        If hexagonal array
*   WAVELENGTH = REAL (Given)
*        Rest-wavelength of line
*   REF(SPDIM1,SPDIM2) = INTEGER*2 (Workspace)
* Global variables:
*   XUNITS = CHARACTER*(*) (Given)
*        Wavelength array units
*   NYP = INTEGER (Given)
*        Number of line slots
*   ITERATION = INTEGER (Given and returned)
*        Iteration number
*   STATUS = INTEGER (Given and returned)
*        Non-zero if an error occured
*
* Subroutines/functions referenced:
*   CNF_PVAL    : Full pointer to dynamically allocated memory
*   COPY2WORK   : Copy data to work array
*   EXAMPROF    : Exaamine line profile
*   RX2CHN      : Convert value to pixel number in array
*   ZERO_SHORT  : Zero integer*2 array
*
*   PAR_WRUSER  : Write string to user
* History:
*   T.N.Wilkins Manchester
*   TNW 29/11/88 Changed to use getwork
*   TNW 20/2/90 d_vsptr used instead
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer status
      include 'arc_dims'
      integer rx2chn,ix,iy
      integer*2 ref(spdim1,spdim2)
      real total(spdim1,spdim2)
      real data(wavdim,spdim1,spdim2),xadj(spdim2),xa(spdim1)
      real ya(spdim2),x,y
      logical add,go,hex
      integer npts,pstat,len1,pgcurse
      character*44 string
      character ch,chr_upper

      call pgqinf('cursor',string,len1)
      if(string(:2).eq.'NO') then
        call par_wruser('No cursor available',pstat)
        status = SAI__ERROR
      end if
      call zero_short(ref,spdim1*spdim2)
      if(status.eq.SAI__OK) then
        npts = 0
        call par_wruser('E to end',pstat)
        add = .false.
        go = .true.
        do while(go)

*       Get cursor position

          call sync
          if(pgcurse(x,y,ch).ne.1) return
          ch = chr_upper(ch)
          go = ch.ne.'E'
          if(hex) then
            ix = rx2chn(xa,spdim1,x-xadj(iy))
            iy = rx2chn(ya,spdim2,y)
          else
            ix = nint(x)
            iy = nint(y)
          end if

*        Check in range

          if((ix.gt.spdim1).or.(iy.gt.spdim2).or.(ix.lt.1).or.(iy.lt.1))
     :                 then
            call par_wruser('Point outside range',pstat)
          else if(ref(ix,iy).eq.1) then
            call par_wruser('Point already included-ignored!',pstat)

*        Is there data at this position?

          else if(total(ix,iy).eq.VAL__BADR) then
            call par_wruser('No data at this point',pstat)
          else

*        All seems ok

            write(string,'(''Profile for x='',i3,'',y='',i3)')ix,iy
            call par_wruser(string,pstat)
            npts = npts + 1
            ref(ix,iy) = 1
            call copy2work(%VAL(CNF_PVAL(d_vsptr)),wavdim,data(1,ix,iy),
     :                     add)
            add = .true.
          end if
        end do

*         Plot and examine line profile

        if(add) then
          call examprof(ref,ix,iy,npts,status)
        end if
      end if
      end
