      subroutine cube2long( STATUS )
*+
* Name:
*    CUBE2LONG

* Invocation:
*    CALL CUBE2LONG( STATUS )
* Purpose:
*   To take a longslit spectrum from a non-sorted TAURUS cube.

* Description:
*   This uses cubic spline interpolation to create a 2-d file from a 3-d
*   file, given a location, angle and length.

* Parameters:
*  CUBE = FILE (Read)
*        Sorted TAURUS cube
*  XPOINT = REAL (Read)
*        X point anywhere on slit
*  YPOINT = REAL (Read)
*        Y point anywhere on slit
*  ANGLE = REAL (Read)
*        Position angle (degrees)
*  OUTPUT = FILE (Write)
*        Output longslit spectrum
* History:
*   T.N.Wilkins Manchester 30/3/1988
*   TNW Changed to use DSA routines 17/10/88
*   TNW Changed to use GETWORK 29/11/88
*   A.J.Holloway Manchester 6/4/98
*   AJH Removed call to create_file to remove dsa_named_output
*   AJH Changed mode inb dsa_map.. from 'r' to 'READ'
*-
      implicit none

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer zptr,znptr,nz,ny,nx,npixel,status,dims(3),ndim
      integer ptr1,ptr2,ptr3,ptr4,ptr5
      integer slot,slot1,slot2,slot3,slot4,slot5,nelm
      real xd,yd,xe,xs,ys,ye,xintersect1,xintersect2,yintersect1
      real yintersect2,angle,x,y,pixlen,slitlen
      character*72 output

      status = SAI__OK
      call dsa_open(status)

* Open input file

      call dsa_input('cube','cube',status)
      call dsa_data_size('cube',3,ndim,dims,nelm,status)
      if(status.ne.SAI__OK) goto 500
      if(ndim.ne.3) then
        call par_wruser('File is not a cube',status)
        goto 500
      end if
      nz = dims(3)
      nx = dims(1)
      ny = dims(2)

* Get slit position etc. from user

      call par_rdval('xpoint',1.0,real(nx),1.0,'pixels',x)
      call par_rdval('ypoint',1.0,real(ny),1.0,'pixels',y)
      call par_rdval('angle',-360.0,360.0,45.0,'degrees',angle)
      call par_rdval('pixlen',1.0,real(nx),1.0,'pixels',pixlen)

* Convert angle to radians

      angle = angle * 1.74533e-2

* Get axis intersects

      xintersect1 = x - y * tan(angle)
      xintersect2 = x + (ny - y) * tan(angle)
      yintersect1 = y - x / tan(angle)
      yintersect2 = y + (nx - x) / tan(angle)

* Get start and end points

      if(yintersect1.gt.ny) then
        ys = ny
        xs = xintersect2
      else if(yintersect1.lt.1.0) then
        ys = 1
        xs = xintersect1
      else
        ys = yintersect1
        xs = 1
      end if
      if(yintersect2.gt.ny) then
        ye = ny
        xe = xintersect2
      else if(yintersect2.lt.1.0) then
        ye = 1
        xe = xintersect1
      else
        ye = yintersect2
        xe = nx
      end if

* Slit length

      xd = xe - xs
      yd = ye - ys
      slitlen = sqrt(yd * yd + xd * xd)
      npixel = int(slitlen/pixlen)

* Check orientation of slit is as requested by user

      if(sin(angle).lt.0.0) then
        xs = xe
        ys = ye
      end if

* Create new output file and map it

      dims(1) = nz
      dims(2) = npixel

* Replacing dsa_named_output with dsa_output (fda)
*
      call dsa_output('output','output',' ',1,1,status)
      call dsa_simple_output('output','D','FLOAT',2,dims,status)

* Returns to previous code

      call dsa_map_data('cube','READ','float',zptr,slot,status)
      call dsa_map_data('output','UPDATE','float',znptr,slot,status)

* Get virtual memory:
*  PTR1  NX     (r)
*  PTR2  NPIXEL (r)
*  PTR3  NY     (r)
*  PTR4  NPIXEL (r)
*  PTR5  NPIXEL (r)

       call dsa_get_work_array(nx,'float',ptr1,slot1,status)
       call dsa_get_work_array(npixel,'float',ptr2,slot2,status)
       call dsa_get_work_array(ny,'float',ptr3,slot3,status)
       call dsa_get_work_array(npixel,'float',ptr4,slot4,status)
       call dsa_get_work_array(npixel,'float',ptr5,slot5,status)
      if(status.ne.SAI__OK) goto 500

* Copy data

      call copyit(nz,nx,ny,%VAL(CNF_PVAL(zptr)),%VAL(CNF_PVAL(znptr)),
     :            pixlen,npixel,angle,xs,ys,%VAL(CNF_PVAL(ptr1)),
     :            %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :            %VAL(CNF_PVAL(ptr4)),%VAL(CNF_PVAL(ptr5)))

* Close files

 500  continue
      call dsa_close(status)
      end





