      subroutine getrange(z,nx,ny,left,right,x)
*+
* Name:
*    GETRANGE

* Invocation:
*    CALL GETRANGE(Z,NX,NY,LEFT,RIGHT,X)
* Purpose:
*   To get the range (in wavelength) of the data for consideration

* Description:
*   To get the range (in wavelength) of the data for consideration

* Arguments:
*    NX = INTEGER (Given)
*        X (spatial) dimension of data
*    NY = INTEGER (Given)
*        Y (spatial) dimension of data
*    Z(NX,NY) = REAL ARRAY (Given)
*        Data array
*    X(NX) = REAL ARRAY (Given)
*        Wavelengths array
*    LEFT = REAL (Returned)
*        Left trams
*    RIGHT = REAL (Returned)
*        Right trams
*
*   Subroutines/functions referenced:
*    FIND_LINES      : Locate lines in 1-d spectrum
*    GETWORK         : Get virtual memory
*
*    DSA_FREE_WORKSPACE : Free virtual memory
*    FIG_XTRACT = INTEGER (Returned)
*        Extract 1-d spectrum from 2-d
*    PAR_WRUSER = INTEGER (Returned)
*        Write character string to user

* History:
*   T.N.Wilkins Manchester 5/88
*   TNW 29/11/88 Changed to use getwork
*-
      implicit none
      include 'SAE_PAR'
      integer nx,ny,vsptr,status,line_count,slot,pstat
      real z(nx,ny),left,right,x(nx)
      logical loop
      character*10 xunits,yunits,xlabel
      include 'DYNAMIC_MEMORY'

* Extract 1-d spectrum

      status = SAI__OK
      call getwork(nx,'float',vsptr,slot,status)
      if(status.ne.SAI__OK) return
      call fig_xtract(z,nx,ny,1,ny,dynamic_mem(vsptr))

* Find the line

      loop = .true.
      do while(loop)
        call find_lines(x,dynamic_mem(vsptr),nx,left,right,line_count,
     :       1,.false.,xlabel,xunits,yunits,status)
        loop = line_count.lt.1
        if(loop) then
          call par_wruser('Not enough lines located',pstat)
        end if
      end do
      call dsa_free_workspace(slot,status)
      end
