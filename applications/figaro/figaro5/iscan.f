      subroutine iscan( STATUS )
*+
* Name:
*    ISCAN

* Invocation:
*    CALL ISCAN( STATUS )

* Purpose:
*    To display cuts through an IMAGE.

* Description:
*   Either the user can go into a loop in which (s)he is prompted for
*   the start and end x-sects to extract, or the program will go through
*   the data, displaying successive cuts.

* Parameters
*    IMAGE = FILE (Read)
*        Image to take cuts from
*    XSTART = REAL (Read)
*        Starting wavelength (or pixel number if not calibrated)
*    XEND = REAL (Read)
*        End wavelength etc.
*    YSTART = INTEGER (Read)
*        Starting cross-section
*    YEND = INTEGER (Read)
*        End cross-section
*    YBLOCK = INTEGER (Read)
*        Width to extract from data in cross-sections (if scanning)
*    SCAN = LOGICAL (Read)
*        If to scan through data
*    HARDCOPY = LOGICAL (Read)
*        If to plot in hard-copy

* Subroutine/functions called:
*    CANAXLIM          : Reset parameters for axis limits
*    GETWORK           : Get virtual memory
*    GRCLOSE           : Close graphics
*    GR_INIT           : Initialise graphics common blocks
*    GR_SELCT          : Open graphics, selecting a device
*    PLOT_SPECT        : Plot a 1-d spectrum
*
*    DSA_CLOSE         : Close DSA
*    DSA_DATA_SIZE     : Get data size
*    DSA_GET_AXIS_INFO : Get axis units/label
*    DSA_INPUT         : Open input file
*    DSA_MAP_AXIS_DATA : Map axis array
*    DSA_MAP_DATA      : Map data array
*    DSA_OPEN          : Open DSA
*    DYN_ELEMENT       : Convert address to array element
*    FIG_XTRACT        : Take slice thru' data in X direction
*    ICH_TIDY = INTEGER (Read)
*        Remove control characters from string
*    PAR_QUEST         : Get YES/NO response from user
*    PAR_RDKEY         : Read key parameter
*    PAR_RDVAL         : Read value parameter
*    PAR_WRUSER        : Write character string to user

* Authors:
*   TNW: T.N.Wilkins Manchester, Cambridge from 2/89

* History:
*    TNW Current version 24/8/88
*    Altered TNW to use DSA routines, 10/88
*    New version using ENCODE_RANGE, TNW 1991
*    ICH_TIDY better used, TNW 6/6/91
*    AJH Change dsa_map.. 'r' to 'READ' etc
*-
      implicit none
      integer status
      integer nl,ni
      character*64 label
      integer xptr
      integer dims(2),ndim,nelm
      character*64 xinfo(2)
      integer iptr,ivalue1
      real value2,dummy1,dummy2
      integer ye,ys,yend,ystart,dy,block,nblocks
      integer wptr,slot,slot2,dyn_element
      integer ich_tidy,lenx1,lenx2
      logical loop
      logical par_quest,scan,hard,par_batch,batch
      character*80 chars
      integer ixstart,npts
      integer len1
      include 'SAE_PAR'
      include 'DYNAMIC_MEMORY'
*  ---------------------------------------------------------------------
      status = SAI__OK
      label='channel direction cut'
*
*   Get name of input file
*
      call dsa_open(status)
      call gr_init
      call dsa_input('image','image',status)
*
*     Get dimensions of input data
*
      call dsa_data_size('image',2,ndim,dims,nelm,status)
      batch = par_batch()
*
*     Get dimensions of input data
*
      dims(2) = max(dims(2),1)
      nelm=dims(1)*dims(2)
      nl=dims(1)
      ni=dims(2)
*
      call dsa_map_data('image','READ','float',iptr,slot,status)
      iptr = dyn_element(iptr)
      call dsa_map_axis_data('image',1,'READ','float',xptr,slot,status)
      xptr = dyn_element(xptr)
      call dsa_object_name('image',label,status)
      if(status.ne.SAI__OK)  then
        go to 500
      endif
      call dsa_get_axis_info('image',1,2,xinfo,0,0,status)
      len1 = ich_tidy(label)
      lenx1 = ich_tidy(xinfo(1))
      lenx2 = ich_tidy(xinfo(2))
      status=0
*
*   Find data.
*
      call getwork(nl,'float',wptr,slot2,status)
      loop=.true.
      call par_rdkey('scan',batch,scan)
      if(batch) then
        hard = .true.
      else
        call par_rdkey('hardcopy',.false.,hard)
      end if
      call dsa_axis_range('image',1,' ',.false.,dummy1,dummy2,ixstart,
     :     ivalue1,status)
      npts = ivalue1 - ixstart + 1
      if (scan) then
        call dsa_axis_range('image',2,' ',.false.,dummy1,dummy2,ys,ye
     :     ,status)
        if(status.ne.SAI__OK) goto 500
        call par_rdval('yblock',1.0,real(ni),max(1.0,real(ni/10))
     :     ,'x-sects',value2)
        dy=nint(value2)
        nblocks=(ye-ys+1)/dy
        do block=1,nblocks
          ystart=ys+(block-1)*dy
          yend=min((ys+(block)*dy-1),ye)
          len1 = 0
          call chr_fill(' ',chars)
          call chr_appnd(label,chars,len1)
          len1 = len1 + 1
          call chr_putc('(',chars,len1)
          call encode_range(' ',' ',ystart,yend,chars,len1)
          call chr_putc(')',chars,len1)
          call fig_xtract(dynamic_mem(iptr),nl,ni,ystart,yend,
     :            dynamic_mem(wptr))
          call gr_selct((.not.hard),status)
          if(status.ne.SAI__OK) then
            goto 500
          end if
          call plot_spect(npts,dynamic_mem(xptr+(ixstart-1)*4),
     :       dynamic_mem(wptr+(ixstart-1)*4),chars(:len1),
     :   xinfo(2)(:lenx2)//' '//xinfo(1)(:lenx1),' ')
          call sla_wait(0.3)
        end do
      else
        do while(loop)
          call dsa_axis_range('image',2,' ',.false.,dummy1,dummy2,ys,ye
     :     ,status)
          call gr_selct((.not.hard),status)
          if(status.ne.SAI__OK) goto 500
          call fig_xtract(dynamic_mem(iptr),nl,ni,ys,ye,
     :        dynamic_mem(wptr))
          len1 = 0
          call chr_fill(' ',chars)
          call chr_appnd(label,chars,len1)
          len1 = len1 + 1
          call chr_putc('(',chars,len1)
          call encode_range(' ',' ',ys,ye,chars,len1)
          call chr_putc(')',chars,len1)
          call plot_spect(npts,dynamic_mem(xptr+(ixstart-1)*4),
     :     dynamic_mem(wptr+(ixstart-1)*4),chars(:len1),
     :        xinfo(2)(:lenx2)//' '//xinfo(1)(:lenx1),' ')
          if(batch) then
            loop = .false.
          else
            loop=par_quest('Another x-sect',.true.)
            if(loop) then
              call canaxlim(2)
            end if
          end if
        end do
      end if
      call clgrap
  500 continue
      call dsa_close(status)
      end
