       subroutine dja_combine(nlines,nsects,vel,velerr,xmean,vmeanerr)
*+
* Name:
*    DJA_COMBINE

* Invocation:
*    CALL DJA_COMBINE(NLINES,NSECTS,VEL,VELERR,XMEAN,VMEANERR)
*
* Purpose:
*   Compute mean velocity of the various spectral lines at
*   each crossection in the data.
*
*
* Description:
*   Compute mean velocity of the various spectral lines at
*   each crossection in the data.
*
*
* Arguments:
*    NLINES = INTEGER (Given)
*        Number spectral lines for which
*                               velocities are determined
*    NSECTS = INTEGER (Given)
*        number of crossection in data
*    VEL(NLINES,NSECTS) = REAL ARRAY (Given)
*        velocity of ith line in jth xsect.
*    VELERR(NLINES,NSECTS) = REAL ARRAY (Given)
*        asssociated errror
*    XMEAN(NSECTS) = REAL ARRAY (Returned)
*
*    VMEANERR(NSECTS) = REAL ARRAY (Returned)
*
*
*- ---------------------------------------------------------------------
      implicit none
      integer nlines,nsects
      real vel(nlines,nsects),velerr(nlines,nsects)
      integer i,j,kk
      real xmean(nsects)
      real vmeanerr(nsects)
      real wtsum
      real wt

* zero out average arrays

      do i=1,nsects
        vmeanerr(i)=  0.0
        xmean(i) =  0.0
        wtsum    =  0.0
        kk       =  0
        do j = 1,nlines
          if(velerr(j,i).ne.0.0) then
            wt       =1.0/(velerr(j,i)*velerr(j,i))
            xmean(i)=xmean(i)+vel(j,i)*wt
            wtsum   =wtsum+wt
            kk      =kk+1
          end if
        end do
        if(kk.ge.1) then
          xmean(i)=xmean(i)/wtsum
          vmeanerr(i)=sqrt(1.0/wtsum)
        end if
      end do
      end
