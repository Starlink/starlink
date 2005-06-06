      subroutine cscan( STATUS )
*+
* Name:
*    CSCAN

* Invocation:
*    CALL CSCAN( STATUS )

* Purpose:
*    To display cuts through an CUBE.

* Description:
*   This displays line profiles
*   from a sorted data cube (i.e. the first dimension is that of
*   wavelength).

* Parameters:
*    CUBE = FILE (Read)
*        Name of CUBE for input
*    YSTART = REAL (Read)
*        display lower limit
*    YEND = REAL (Read)
*        display upper limit
*    TSTART = INTEGER (Read)
*        display lower limit
*    TEND = INTEGER (Read)
*        display upper limit
*    HARDCOPY = LOGICAL (Read)
*        use hard graphics device for display

* Subroutines/functions referenced:
*   CANAXLIM           : Reset parameters for axis limits
*   CLGRAP             : Close graphics
*   CNF_PVAL           : Full pointer to dynamically allocated memory
*   DRAWPOLY           : Draw a polyline
*   GR_SELCT           : Select/open a graphics device
*
*   DSA_AXIS_RANGE     : Get range to use in a given axis direction
*   DSA_CLOSE          : Close DSA
*   DSA_INPUT          : Open file for input
*   DSA_DATA_SIZE      : Get size of data
*   DSA_GET_RANGE      : Get range of values of main data array
*   DSA_MAP_DATA       : Map main data array
*   DSA_MAP_AXIS_DATA  : Map axis data array
*   DSA_OPEN           : Open DSA
*   GEN_RANGEF         : Get range of array
*   PAR_BATCH = LOGICAL
*        Find out if running in batch mode
*   PAR_RDKEY          : Get value of keyword
*   PAR_WRUSER         : Write character string to user
*
* Authors:
*   TNW: T.N.Wilkins Manchester, Cambridge from 2/89

* History:
*  TNW 12/87-1/88
*  TNW 3-4/90 PGPLOT version
*  TNW 25/1/91 Made just to make 1 array, then exit.
*  AJH 11/98  CHnaged dsa_map_data to READ
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer status
      integer nl
      integer xptr
      integer dims(3),ndim,nelm
      integer i,iptr
      real datmin,datmax,dummy1,dummy2
      integer j,nz,ixend
      integer iyend,iystart,startpos
      real xstart,xend,ystart,yend,xpts(2),ypts(2)
      real xps,xpe,yps,ype,xws,xwe,yws,ywe,xvs,xve,yvs,yve
      integer slot,dyn_element
      logical hard,par_batch,batch
      integer ixstart
*  ---------------------------------------------------------------------
*
*   Get name of input file
*
      status = SAI__OK
      call dsa_open(status)
      call dsa_input('cube','cube',status)
      batch = par_batch()
*
*     Get dimensions of input data
*
      call dsa_data_size('cube',3,ndim,dims,nelm,status)
      if (ndim.ne.3) then
        call par_wruser('CSCAN is for 3-d data',status)
        status = SAI__ERROR
      end if
      nl=dims(2)
      nz=dims(1)
*
      call dsa_map_data('cube','READ','float',iptr,slot,status)
      iptr = dyn_element(iptr)
*
      call dsa_map_axis_data('cube',1,'READ','float',xptr,slot,status)
*
*   Find data.
*
      if(batch) then
        hard = .true.
      else
        call par_rdkey('hardcopy',.false.,hard)
      end if

* Attempt to read max and min values from file, if not evaluate them
* and write them to the file for future use

      call dsa_get_range('cube',datmin,datmax,status)

* Create plots

      call dsa_axis_range('cube',2,' ',.false.,dummy1,dummy2,ixstart,
     :                    ixend,status)
      call dsa_axis_range('cube',3,' ',.false.,dummy1,dummy2,iystart,
     :                     iyend,status)
      if(status.ne.SAI__OK) goto 500

*   Open graphics

      call gr_selct((.not.hard),status)
      if(status.ne.SAI__OK) then
        goto 500
      end if

*   Draw numbers for axis

      call pgvport(0.05,0.99,0.05,0.99)
      xstart = real(ixstart) - 0.5
      xend = real(ixend) + 0.5
      ystart = real(iystart) - 0.5
      yend = real(iyend) + 0.5

      call pgwnad(xstart,xend,ystart,yend)
      call pgbox('BCN',0.0,0,'BCN',0.0,0)

*   Draw grid (this is at integer+/-0.5)

      ypts(1) = ystart
      ypts(2) = yend
      do i = ixstart, ixend - 1
        xpts(1) = real(i) + 0.5
        xpts(2) = xpts(1)
        call pgline(2,xpts,ypts)
      end do
      xpts(1) = xstart
      xpts(2) = xend
      do j = iystart, iyend - 1
        ypts(1) = real(j) + 0.5
        ypts(2) = ypts(1)
        call pgline(2,xpts,ypts)
      end do

*   Create individual profile plots

      call pgqwin(xws,xwe,yws,ywe)
      call pgqvp(0,xvs,xve,yvs,yve)
      do j = iystart,iyend
        yps = ((real(j)-0.5-yws)/(ywe-yws))*(yve-yvs) + yvs
        ype = ((real(j)+0.5-yws)/(ywe-yws))*(yve-yvs) + yvs
        do i = ixstart,ixend
          startpos = iptr + 4 * nz * ((j-1)*nl+(i-1))
          xps = ((real(i)-0.5-xws)/(xwe-xws))*(xve-xvs) + xvs
          xpe = ((real(i)+0.5-xws)/(xwe-xws))*(xve-xvs) + xvs
          call pgvport(xps,xpe,yps,ype)
          call drawpoly(%VAL(CNF_PVAL(xptr)),%VAL(CNF_PVAL(startpos)),
     :                  nz,datmin,datmax)
        end do
      end do
      call clgrap
  500 continue
      call dsa_close(status)
      end
