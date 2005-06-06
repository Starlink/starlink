      subroutine crigauss( STATUS )
*+
* Name:
*    CRIGAUSS

* Invocation:
*    CALL CRIGAUSS( STATUS )
* Purpose:
*   To create a file with a profile of 1 to 5 gaussians.

* Description:
*   The profiles are evaluated and copied to each cross-section.
*   This is really intended for testing software.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image to be created
*    YDIM = INTEGER (Read)
*        Y dimension of data
*    XDIM = INTEGER (Read)
*        X dimension of data"
*    XSTART = REAL (Read)
*        First X value
*    XEND = REAL (Read)
*        Last X value
*    WIDTH1 = REAL (Read)
*        WIDTH1 of gaussian (fwhm)
*    CENTRE1 = REAL (Read)
*        CENTRE1 of gaussian
*    HEIGHT1 = REAL (Read)
*        HEIGHT1 of gaussian
*    WIDTH2 = REAL (Read)
*        WIDTH2 of gaussian (fwhm)
*    CENTRE2 = REAL (Read)
*        CENTRE2 of gaussian
*    HEIGHT2 = REAL (Read)
*        HEIGHT2 of gaussian
*    WIDTH3 = REAL (Read)
*        WIDTH3 of gaussian (fwhm)
*    CENTRE3 = REAL (Read)
*        CENTRE3 of gaussian
*    HEIGHT3 = REAL (Read)
*        HEIGHT3 of gaussian
*    WIDTH4 = REAL (Read)
*        WIDTH4 of gaussian (fwhm)
*    CENTRE4 = REAL (Read)
*        CENTRE4 of gaussian
*    HEIGHT4 = REAL (Read)
*        HEIGHT4 of gaussian
*    WIDTH5 = REAL (Read)
*        WIDTH5 of gaussian (fwhm)
*    CENTRE5 = REAL (Read)
*        CENTRE5 of gaussian
*    HEIGHT5 = REAL (Read)
*        HEIGHT5 of gaussian
*    BASE = REAL (Read)
*        BASE for gaussian
*    NCOMP = INTEGER (Read)
*        NCOMP Number of componants

* Author:
* T.N.Wilkins Manchester May 1987
* Some adaptation to DSA routines 25/10/88
* AJH Changed dsa_map modes from 'w' to 'WRITE' (FDA)
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer status,nx,ny,iptr,xptr,dims(2)
      real value
      integer wptr,slot
      status = SAI__OK
      call dsa_open(status)
      call dsa_output('data','image',' ',0,0,status)

* Open new file and creat X and Z structures with .DATA arrays,
* then map them

      call par_rdval('xdim',5.0,1.0e4,100.0,' ',value)
      nx = nint(value)
      call par_rdval('ydim',1.0,1.0e4,100.0,' ',value)
      ny = nint(value)
      dims(1) = nx
      dims(2) = ny
      call dsa_simple_output('data','d,a1','float',2,dims,status)
      call dsa_map_data('data','WRITE','float',iptr,slot,status)
      call dsa_map_axis_data('data',1,'WRITE','float',xptr,slot,status)

* Fill in values of arrays

      call dsa_get_work_array(nx*10,'int',wptr,slot,status)
      if(status.eq.SAI__OK) then
        call fillim(%VAL(CNF_PVAL(iptr)),%VAL(CNF_PVAL(xptr)),nx,ny,
     :              %VAL(CNF_PVAL(wptr)))
      end if
      call dsa_close(status)
      end
