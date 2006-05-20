      subroutine fibslfil(xpts,ypts,maxnpt,xa,ya,xadj,ifxcut,hex,size)
*+
* Name:
*    FIBSLFIL

* Invocation:
*    CALL FIBSLFIL(XPTS,YPTS,MAXNPT,XA,YA,XADJ,IFXCUT,HEX,SIZE)

* Purpose:
*   To take a slice through a 3-d data array and write it to a file

* Description:
*   To take a slice through a 3-d data array, using the nearest points
*   to the line selected. This outputs the resultant slice to a file.
*
* Arguments:
*     XPTS(MAXNPT) = INTEGER ARRAY (Given)
*        X point work array
*     YPTS(MAXNPT) = INTEGER ARRAY (Given)
*        Y point work array
*     MAXNPT = INTEGER (Given)
*        Dimension of work arrays
*     XA(SPDIM1) = REAL ARRAY (Given)
*        X array
*     YA(SPDIM2) = REAL ARRAY (Given)
*        Y array
*     XADJ(SPDIM2) = REAL ARRAY (Given)
*        X adjustment array
*     IFXCUT = LOGICAL (Given)
*        If a cut in the X direction is required
*                          -otherwise Y
*     HEX = LOGICAL (Given)
*        If hexagonal array
*  Given in ARC_DIMS:
*     D_SPTR = INTEGER (Given)
*        Pointer to data array
*     SPDIM1 = INTEGER (Given)
*        X dimension of data
*     SPDIM2 = INTEGER (Given)
*        Y dimension of data
*     WAVDIM = INTEGER (Given)
*        wavelength dimension of data
*
*   Subroutines/functions referenced:
*     CNF_PVAL       : Full pointer to dynamically allocated memory
*     FIBSLCOP       : Copy data to output file
*     HEXMARK        : Draw hexagon
*     RX2CHN         : Convert value to pixel number in array
*
*     DSA_NAMED_OUTPUT : Create output file
*     DSA_RESHAPE_DATA : Create data arrays for output file-based
*                        on similar arrays with different dimensions
*     DSA_RESHAPE_AXIS : Same as reshape_data, but for axis
*     DSA_MAP_DATA    : Map main data array
*     DSA_UNMAP       : Unmap array
*     DSA_CLOSE_STRUCTURE : Close data file
*     PAR_RDCHAR      : Get character parameter from user
*
* Author:
*    T.N.Wilkins Manchester 24/6/88
* History:
*    T.N.Wilkins Cambridge 3/9/91 Made to use arc_dims, also accres to
*                                 delete structure from output file.
*    A.J.Holloway Manchester 6/4/98 Replace dsa_named_output with fda
*                            6/10/98 Replace dsa_map 'w' with 'WRITE'
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      integer maxnpt,xpts(maxnpt),ypts(maxnpt)
      real xa(spdim1),ya(spdim2),xadj(spdim2),x,y
      real size
      integer status,slot
      integer i,ix,iy,npt,rx2chn,dims(2),out,pgcurse
      logical ifxcut,hex
      character*72 ch*1

*  Find cursor position

      call sync
      if(pgcurse(x,y,ch).ne.1) return
      npt = 0
      call pgscr(3,1.0,0.0,0.0)
      call gr_spen(3)
      call pgsfs(2)

*  X direction cut

      if(ifxcut) then
        if(hex) then
          iy = rx2chn(ya,spdim2,y)
        else
          iy = nint(y)
        end if
        do i = 1, spdim1
          npt = npt + 1
          xpts(npt) = i
          ypts(npt) = iy
          if(hex) then
            call hexmark((xa(i)+xadj(iy)),ya(iy),size)
          else
            call rectmark(real(i),real(iy),0.5)
          end if
        end do
      else

*  Y direction cut

        do i = 1, spdim2
          if(hex) then
            ix = rx2chn(xa,spdim2,x-xadj(i))
            call hexmark((xa(ix)+xadj(i)),ya(i),size)
          else
            ix = nint(x)
            call rectmark(real(ix),real(i),0.5)
          end if
          npt = npt + 1
          xpts(npt) = ix
          ypts(npt) = i
        end do
      end if

* Output file

      if(npt.ge.1) then
        dims(1) = wavdim
        dims(2) = npt

* Replace with fda calls below
*
*        call par_rdchar('output',' ',output)
*        call dsa_named_output('output',output,'data',1,1,status)

        call dsa_output('output','output','data',0,0,status)


        call dsa_reshape_data('output','data',2,dims,status)
        call dsa_reshape_axis('output',1,'data',1,1,dims,status)

*     Delete results structure from output file

        call accres('output','results','FI',0,0,' ',status)
        call accres(' ',' ','DE',0,0,' ',status)

*     Return ACCRES reference to main data file

        call accres('data','results','FI',0,0,' ',status)

        call dsa_map_data('output','WRITE','float',out,slot,status)
        if(status.ne.SAI__OK) return
        call fibslcop(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,spdim2,xpts,
     :                ypts,npt,%VAL(CNF_PVAL(out)))
        call dsa_unmap(slot,status)
        call dsa_close_structure('output',status)
      end if
      end
