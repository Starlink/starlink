      subroutine himage( STATUS )
*+
* Name:
*    HIMAGE

* Invocation:
*    CALL HIMAGE( STATUS )

* Purpose:
*   Create greyscale plot of image

* Description:
*     Handles an 'HIMAGE' command, producing an image on any suitable
*     device. Originally based on the routine IMAGE.
*
* Parameters:
*     IMAGE = FILE (Read)
*        The name of the first image to be displayed.
*     YSTART = REAL (Read)
*        The first Y value to be displayed.
*     YEND = REAL (Read)
*        The last Y value to be displayed.
*     XSTART = REAL (Read)
*        The first X value to be displayed.
*     XEND = REAL (Read)
*        The last X value to be displayed.
*             X and Y are the horizontal and vertical directions
*             - as displayed on the Grinnell - respectively.  The
*             first value for each is 1.
*     LOW = REAL (Read)
*        The minimum count level for the display, for
*             the first image.
*     HIGH = REAL (Read)
*        The maximum count level for the display, for
*             the first image.
*     PLOTDEV = CHARACTER (Read)
*        The name of the plotting device
*     ASPECT = REAL (Read)
*         The apect ratio of the plot
*     SHRINK = LOGICAL (Read)
*        If to shrink image to leave margin all round
*     LOG = LOGICAL (Read)
*        To take logarithms of the image to display it
*
*
* Subroutines/functions referenced:
*     CNF_PVAL         : Full pointer to dynamically allocated memory
*     GR_FOTOR         : Plot greyscale image
*
*     DSA_AXIS_RANGE   : Get range along axis to use
*     DSA_CLOSE        : Close DSA
*     DSA_DATA_SIZE    : Get size of main data array
*     DSA_FREE_WORKSPACE : Free workspace
*     DSA_GET_WORK_ARRAY : Get workspace
*     DSA_INPUT        : Open input data file
*     DSA_MAP_DATA     : Map main data array
*     DSA_OBJECT_NAME  : Get name of object
*     DSA_OPEN         : Open DSA
*     CHR_LEN          : Get non-blank length of character string
*     PAR_RDCHAR       : Read character string parameter
*     PAR_RDKEY        : Read key parameter
*     PAR_RDVAL        : Read value parameter
*     PAR_WRUSER       : Write character string to user
*
*                                    KS / CIT 18th April 1984
* History:
*     26th May 1986.  KS / AAO.  Now handles errors in the usual
*                     Figaro way and releases resources properly.
*                     PAR_WRUSER used instead of WRUSER.
*     Jan 1987        TNW/Manchester To give greyscale plots using GKS
*     30/3/88         T.N.Wilkins Manchester to work quicker and to
*                     only try to print file if writing to a file.
*     24/8/88         TNW To use dynamic_mem array etc.
*     25/1/89         TNW status set to 0, had been missed off
*                     conversion to DSA
*     13/10/89        TNW/CAVAD PHO_FOTOR made independent of FIGARO
*      5/1/99         AJH change sda_map 'r' to 'READ'
*-
      implicit none

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
*
*     Functions
*
      integer chr_len
*
*     High and low limits
*
      real dummy1,dummy2
*
*     Local variables
*
      logical shrink
      integer ndim,dims(2),ixst
      integer status,start,ichst,ichen,ixen
      integer nch,nxp,nyp,nelm
      logical plog,key
      real value,low,high,asp
      integer temp,slot
      character object*64,pltdev*40
      external par_wruser
*
*     Initial values
*
      status = SAI__OK

      call dsa_open(status)
*
*     Get the object name
*     Assuming this is a file name, open it (it may not be open)
*     and assume that the actual data is the .Z.DATA component.
*
      call dsa_input('IMAGE','IMAGE',status)
*
*     We now have an object name, start to find out about it.
*
      call dsa_data_size('IMAGE',2,ndim,dims,nelm,status)
      if (status.ne.SAI__OK)  then
        go to 500
      end if
      if (ndim.ne.2) then
        call par_wruser('This is not an image',status)
        if (ndim.gt.2) then
          call par_wruser('But it has more than 2 dimensions, so',
     :                                                    status)
          call par_wruser('let''s have a go at it',status)
        else
          go to 500
        end if
      end if
*
      call dsa_map_data('IMAGE','READ','FLOAT',start,slot,status)
*
*     Get parameters for display
*
      call dsa_axis_range('IMAGE',1,' ',.false.,dummy1,dummy2,ichst,
     :     ichen,status)
      call dsa_axis_range('IMAGE',2,' ',.false.,dummy1,dummy2,ixst,
     :     ixen,status)
      if(status.ne.SAI__OK) then
        goto 500
      end if
      call par_rdval('LOW',val__minr,val__maxr,0.,' ',low)
      call par_rdval('HIGH',val__minr,val__maxr,1000.,' ',high)
      nxp=ichen-ichst+1
      nyp=ixen-ixst+1
      call par_rdkey('SHRINK',.false.,shrink)
      call par_rdkey('LOG',.false.,plog)
      asp = real(nxp)/real(nyp)
      call par_sdval('ASPECT',asp,status)
      call par_rdval('ASPECT',1.0e-3,1.0e3,asp,' ',value)
      asp = value
      call par_rdkey('KEY',.false.,key)
*
*     Look for object name and list on terminal if found
*
      call dsa_object_name('IMAGE',object,status)
      if (status.eq.SAI__OK) then
        nch=chr_len(object)
        if ((status.eq.SAI__OK).and.(nch.ge.1)) then
          call par_wruser(' ',status)
          call par_wruser(object(:nch),status)
          call par_wruser(' ',status)
        end if
      end if
      call par_rdchar('PLOTDEV','CANON_L',pltdev)
      status = SAI__OK
      call dsa_get_work_array(nxp*nyp,'INT',temp,slot,status)
      if(status.ne.SAI__OK) then
        goto 500
      end if
      call gr_fotor(nxp,nyp,high,low,object,asp,%VAL(CNF_PVAL(start)),
     :              ichst,ixst,dims(1),dims(2),%VAL(CNF_PVAL(temp)),
     :              shrink,plog,key,pltdev,par_wruser,status)
*
*     Tidy up
*
  500 continue
      call dsa_close(status)
      end
