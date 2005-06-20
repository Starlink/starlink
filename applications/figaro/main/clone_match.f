      subroutine clone_match(line_name,left,right,wavelength,dleft,
     :      dright,linposl,linposr,waves,ids,ifcomb,arc,tarc,nlinesc,
     :      status)
*+
* Name:
*    CLONE_MATCH

* Invocation:
*    CALL CLONE_MATCH(LINE_NAME,LEFT,RIGHT,WAVELENGTH,DLEFT,
*           DRIGHT,LINPOSL,LINPOSR,WAVES,IDS,IFCOMB,ARC,TARC,NLINESC,
*           STATUS)

* Purpose:
*  Match old and new spectra

* Description:
*   To match the cloning (old) spectrum to the new spectrum to be cloned
*  from it, producing new arrays left and right for line location in the
*  new spectrum.
*
* Arguments:
*    IFCOMB = LOGICAL (Given)
*        If called from COMB
*    NLINESC = INTEGER (Given)
*        Number of lines in cloning file
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    LINE_NAME(NYP) = CHARACTER*10 ARRAY (Returned)
*        Line names
*    LEFT(NYP) = REAL ARRAY (Returned)
*        Left trams
*    RIGHT(NYP) = REAL ARRAY (Returned)
*        Right trams
*    WAVELENGTH(NYP) = REAL ARRAY (Returned)
*        Rest wavelengths
*    ARC(NYP) = INTEGER*2 ARRAY (Returned)
*        Arc useage array
*    DLEFT(NLINESC) = DOUBLE PRECISION ARRAY (Workspace)
*        Cloning left tram array
*    DRIGHT(NLINESC) = DOUBLE PRECISION ARRAY (Workspace)
*        Cloning right tram array
*    LINPOSL(NLINESC) = DOUBLE PRECISION ARRAY (Workspace)
*        DLEFT interpolated to current data
*    LINPOSR(NLINESC) = DOUBLE PRECISION ARRAY (Workspace)
*        DRIGHT interpolated to current data
*    WAVES(NLINESC) = REAL ARRAY (Workspace)
*        Cloning wavelengths
*    IDS = CHARACTER*(*) (Workspace)
*        Cloning line name array
*    TARC(NLINESC) = INTEGER*2 ARRAY (Workspace)
*        Cloning arc useage array
*
*  Subroutines/functions referenced:
*     ACCRES            : Access results structure
*     ARSORT            : Sort
*     CANAXLIM          : Cancel axis limit parameters
*     CLONE_PLOT        : Plots of data
*     GETWORK           : Get work array
*     GR_SOFT           : Select softcopy graphics device
*     GR_GETWV          : Get limits of plot (data values)
*     GR_POLYL          : Draw polyline
*     PGPAGE            : Clear display surface
*     INTRPL            : Cubic spline interpolation
*
*     DSA_AXIS_RANGE    : Get range along axis
*     DSA_DATA_SIZE     : Get size of data
*     DSA_FREE_WORKSPACE: Free workspace
*     DSA_MAP_AXIS_DATA : Map axis data array
*     DSA_MAP_DATA      : Map main data array
*     DSA_UNMAP         : Unmap array
*     DYN_ELEMENT = INTEGER (Workspace)
*        Convert address to array element in dynamic_mem
*     GEN_NFILLF        : Fill array with 1-n
*     FIG_YTRACT        : Take slice through data parallel to 2nd axis
*     FIG_XTRACT        : Take slice through data parallel to 1st axis
*     PAR_WRUSER        : Write character string to user
*
*     CHR_FILL          : Fill character string with one character
*     GR_SPEN           : Select graphics pen
*
* History:
*   TNW 10/10/88 IFCOMB argument added
*   TNW 18/10/88 To use ACCRES and 19/10/88 to use IFOVERLAP
*   TNW 28/11/88 To use GETWORK
*   TNW IOA/Cambridge 8/12/89 IDS made argument (workspace)
*   TNW/CAVAD 2/2/90 ARC array now copied.
*   TNW/Cambridge 3/90 PGPLOT version
*          "      2/1/91 Workspace reduced. Also dimension of work
*                        arrays determined above, so 50 is not
*                        hard-wired in here.
*          "      8/7/91 Changes for new results structure, clone_plot
*                        output arrays double precision
*   ACD: 28/9/00 Remove character strings continued across continuation
*                lines.
*
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
*-
      integer status
      integer nlinesc
      character*10 line_name(nyp)
      real left(nyp),right(nyp),wavelength(nyp)
      double precision chan1(3),chan2(3)
      integer*2 arc(nyp)
      integer ixstart,ixend,vsptrc,x2ptr,slot2,axis1,axis2
      integer ncchannels,ncxsects,nli,nlic,nil,nilc
      integer pstat,itemp
      integer i
      logical ifcomb,ok
      integer imid,specdim,dims(3)
      integer nlines
      real waves(nlinesc)
      double precision dleft(nlinesc),dright(nlinesc),linposl(nlinesc)
      double precision linposr(nlinesc)
      integer*2 tarc(nlinesc)
      character*(*) ids
      integer count
      logical par_quest,direct
      integer nelmc
      integer xslot,dslot,clptr
      real value(2),xmin,xmax,gen_elemf

* Read in tramlines etc.

      direct = .true.
      if(batch) then
        call par_wruser(
     :     'Will copy over line locations etc. directly',pstat)
      else if(.not.par_quest('Copy over line locations directly',
     :        .false.)) then
        direct = .false.
      end if

*     Problem passing character*(*) to ACCRES, easiest to copy
*     afterwards even if lines copied over directly


      call accres(' ','more.ids','rc',nlinesc*10,0,ids,status)

      if(direct) then

*     Copying over directly

        count = min(nlinesc,nyp)
        call accres(' ','more.traml','rf',count,left,' ',status)
        call accres(' ','more.tramr','rf',count,right,' ',status)
        call accres(' ','more.twodspec.select','rs',nslct*count,arc,' '
     :                  ,status)
        call accres(' ','more.rest_wave','rf',count,wavelength,' ',
     :                  status)

*     Copy line names over

        do i = 1, count
          itemp = i*10
          line_name(i) = ids(itemp-9:itemp)
        end do
      else

*     Copying over, with shifts decided by marking with a cursor

*     Get size of cloning data

        call dsa_data_size('clone',3,specdim,dims,nelmc,status)
        ncchannels=dims(1)
        ncxsects=dims(2)

        call accres(' ','more.traml','rd',nlinesc,dleft,' ',status)
        call accres(' ','more.tramr','rd',nlinesc,dright,' ',status)
        call accres(' ','more.twodspec.select','rs',nslct*nlinesc,tarc
     :                  ,' ',status)
        call accres(' ','more.rest_wave','rf',nlinesc,waves,' ',status)

*  Get workspace:
*     VSPTRC (r) ncchannels

        call getwork(ncchannels,'float',vsptrc,slot2,status)
*
*  Map the data
*
        call dsa_map_data('clone','READ','float',clptr,dslot,status)

        call par_wruser('We find the required shifts of the lines '/
     :    /'by comparing data from the 2 files',pstat)

        if(ifcomb) then
          axis1 = 2
          axis2 = 1
          nli = wavdim
          nlic = ncchannels
          nilc = ncxsects
          nil = spdim1
        else
          axis1 = 1
          axis2 = 2
          nli = spdim1
          nilc = ncchannels
          nil = wavdim
          nlic = ncxsects
        end if
*
*  Map the clone X-array (or Y if comb)
*
        call dsa_map_axis_data('clone',axis1,'READ','float',x2ptr,xslot,
     :                         status)

*   Get limits to extract data from and extract it. Continue looping
*   until we have a satisfactory match between the 2 data sets

        ok = .false.
        do while(.not.ok)
          imid=nli/2
          ixstart=max(1,imid-10)
          ixend=min(nli,imid+10)
          call par_wruser('Current data',pstat)
          call dsa_axis_range('data',axis2,' ',.false.,value(1),
     :                        value(2),ixstart,ixend,status)
          call canaxlim(axis2)
          if(status.ne.SAI__OK)  then
            goto 500
          end if
          if(ifcomb) then
            call fig_ytract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,
     :                      ixstart,ixend,%VAL(CNF_PVAL(d_vsptr)))
          else
            call fig_xtract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,
     :                      ixstart,ixend,%VAL(CNF_PVAL(d_vsptr)))
          end if
          imid=nlic/2
          ixstart=max(1,imid-10)
          ixend=min(nlic,imid+10)
          call par_wruser('Cloning  data',pstat)
          call dsa_axis_range('clone',axis2,' ',.false.,value(1),
     :                       value(2),ixstart,ixend,status)
          call canaxlim(axis2)

          if(ifcomb) then
            call fig_ytract(%VAL(CNF_PVAL(clptr)),ncchannels,ncxsects,
     :                      ixstart,ixend,%VAL(CNF_PVAL(vsptrc)))
          else
            call fig_xtract(%VAL(CNF_PVAL(clptr)),ncchannels,ncxsects,
     :                      ixstart,ixend,%VAL(CNF_PVAL(vsptrc)))
          end if

* Get positions of 3 lines on each spectrum

          call gr_soft(status)
          call clone_plot(nil,%VAL(CNF_PVAL(d_xptr)),
     :                    %VAL(CNF_PVAL(d_vsptr)),nilc,
     :                    %VAL(CNF_PVAL(x2ptr)),%VAL(CNF_PVAL(vsptrc)),
     :                    chan1,chan2)

* Interpolate to find positions of other lines, using tram lines from
* clone file.

          do i=1,nlinesc
            if(dright(i).gt.1.0d-30)nlines=i
          end do
          if(nlines.le.0) then
            call par_wruser('No lines in input file!',pstat)
            goto 500
          end if
          call darsort(chan1,chan2,3)

          call intrpl(3,chan2,chan1,nlines,dleft,linposl,status)
          call intrpl(3,chan2,chan1,nlines,dright,linposr,status)

          call gr_spen(3)
          xmin = gen_elemf(%VAL(CNF_PVAL(d_xptr)),1)
          xmax = gen_elemf(%VAL(CNF_PVAL(d_xptr)),wavdim)
          count=0
          do i=1,nlines
            if((real(linposl(i)).gt.xmin).and.
     :                  (real(linposr(i)).lt.xmax)) then
              if(count.lt.nyp) then
                count=count+1
                left(count)=real(linposl(i))
                right(count)=real(linposr(i))
                waves(count)=waves(i)
                ids((count*10):(count*10+9))=ids((i*10):(i*10+9))
                tarc(count) = tarc(i)
                call gr_vline(left(count))
                call gr_vline(right(count))
              else
                call par_wruser('Error, no more space for lines',pstat)
              end if
            end if
          end do

*   Select graphics pen 1, and clear graphics display

          ok = par_quest('OK?',.true.)
          call gr_spen(1)
          call pgpage
        end do

        call dsa_free_workspace(slot2,status)

        call dsa_unmap(xslot,status)
        call dsa_unmap(dslot,status)

*     Copy over work arrays into results structure

        do i = 1, count
          wavelength(i) = waves(i)
          line_name(i) = ids((i*10-9):(i*10))
          arc(i) = tarc(i)
        end do

*     Fill trailing parts of tram arrays with 0s (in case less lines
*     than on a previous attempt)

        itemp = nyp - count
        if(itemp.gt.0) then
          call zero_real(left(count+1),itemp)
          call zero_real(right(count+1),itemp)
        end if
      end if

* Return to looking at current .RES structure

      call accres('data','results','fi',1,1,' ',status)
 500  continue
      end
