      subroutine gr_fotor(icol,irow,white,black,title,asp,z,ichst,
     :   ixst,nl,ni,iwork,shrink,plog,key,pltdev,typsub,status)
*+
* Name:
*    GR_FOTOR

* Invocation:
*    CALL GR_FOTOR(ICOL,IROW,WHITE,BLACK,TITLE,ASP,Z,ICHST,
*        IXST,NL,NI,IWORK,SHRINK,PLOG,KEY,PLTDEV,TYPSUB,STATUS)

* Purpose:
*   Creates a bitmap of a grey-scale plot

* Description:
*         This subroutine creates a bitmap of a grey-scale plot
*         of a REAL array. A key of sixteen grey levels is
*         provided. The user can specify which values correspond to
*         black and white on the plot. Beyond these values the plotted
*         data are black and white respectively. The array need not be
*         square.
*
*         Note that GKS or SGS should not be active when this routine is
*         called.
*
* Method:
*
*         A rectangular graphics zone is defined by SGS - a square
*         for the grey-scale data, and a rectangle to the right to
*         accommodate the key. Within the square part of the zone a
*         sub-zone is created with aspect ratio equal to that of the
*         data array or square. The data are copied to a 32-bit integer
*         array and scaled to the maximum number of grey levels times
*         the greyness (i.e. the fraction of the way from the black
*         value to the white.) A GKS cell array of the scaled data is
*         made.
*
*         The original rectangular zone is selected and a square subzone
*         to the right is created. The size of the grey markers in the
*         key can be changed by a single scaling parameter. The key
*         comprises a title and a selection of the grey levels, each
*         of which is annotated by the mean value it represents.
*
* Arguments:
*     ICOL = INTEGER (Given)
*        Number of columns in image array (>1)
*     IROW = INTEGER (Given)
*        Number of rows in image array (>1)
*     WHITE = REAL (Given)
*        Value in array corresponding to white on the plot
*     BLACK = REAL (Given)
*        Value in array corresponding to black on the plot
*     TITLE = CHARACTER*(*) (Given)
*        Title for the plot (about 15 characters maximum
*                    and the remainder are truncated
*     ASP = REAL (Given)
*        Aspect ratio of plot
*     Z = REAL (Given)
*        Array containing image to be plotted, dimension
*                   ICOL by IROW.
*     SHRINK = LOGICAL (Given)
*        If to shrink image slighly for copying
*     PLOG = LOGICAL (Given)
*        Plot logarithm of data
*     KEY = LOGICAL (Given)
*        Plot a key (if false no border is drawn either)
*     TYPSUB = SUBROUTINE (Given)
*             Routine to output character string to terminal
*     STATUS = INTEGER (Returned)
*        Non-zero if fails
*
* Graphics:
*          GKS:
*            GCA, GQCF, GQWKC, GQWKCA
*
*          SGS:
*            SGS_BOX, SGS_CLOSE, SGS_OPEN, SGS_SELZ, SGS_SW, SGS_ZSHAP
*            SGS_ZONE, SGS_ICURW
*
* Calls to other routines: GRAPHLIB (library prefix GR_)
*                                      KEYR
*                                  CHR_
*
* History:
*
*         Original -  Malcolm J. Currie  RAL  1986 June 26th
*         Altered to use ADJUST parameter
*                  -  T.N.Wilkins   Manchester Jan 1987
*         Altered to use any (?) printer
*                  -  T.N.Wilkins   Manchester June 1987
*     30/3/88         T.N.Wilkins Manchester to work quicker and to
*                     only try to print file if writing to a file.
*     22-28/6/88      TNW, to set colour table to grey, some changes
*                     to use of colour table. Key can be omitted and
*                     log option introduced. Some optimisation.
*                     ADJUST replaced with ASP as argument.
*     13/10/89        TNW/CAVAD PHO_FOTOR made independent of FIGARO
*     22/6/90         TNW/CAVAD Prefix changed to GR_ (not PHO_)
*     26/7/91            "      Version for Suns (+new GKS)
*     1/11/91            "      Use include file for differences between
*                               2 versions of GKS
*-
      implicit none
      include 'SAE_PAR'
      character*(*) title
      integer icol,irow,iwork(icol,irow),status,ichst,ixst,nl,ni
      character*(*) pltdev
      real white,black,z(nl,ni),twhite,tblack,lowlim
      real range,temp1,asp,zkey,mult
      integer len1,temp,wkcat,iwkid,icon,itype,pstat
      integer ncols,nlevs,nlevu,nlevk,i,j,izone,izone1,nlevup2
      integer itemp
      character*14 prfile,prname*72,qname*15
      logical cola,shrink,plog,key
      external typsub

      if ( icol .lt. 2 .or. irow .lt. 2 ) then
        goto 500
      end if
*
*        Open workstation
*
      call sgs_open(pltdev,izone,status)
      if(status.ne.SAI__OK) then
        call typsub('Error opening graphics',pstat)
        goto 500
      end if
      if(shrink) then
        call sgs_sw(0.0,1.0,0.0,1.0,status)
        call sgs_zone(0.02,0.98,0.02,0.98,izone,status)
      end if
*
*         Find device type in order to...
*
      call sgs_icurw(iwkid)
      call gqwkc(iwkid,status,icon,itype)

* Check if workstation is of type "OUTIN"

      call gqwkca(itype,status,wkcat)
*
*        and find out how many colour table entries there are
*
      call gqcf(itype,status,ncols,cola,nlevs)
      nlevu=max(2,nlevs-3)
      nlevk=min(16,nlevu)

*    Get name of file created, and also logical name pointing to queue

      if(wkcat.eq.0) then

        len1 = 0
        call chr_fill(' ',qname)
        call chr_putc('GKS_',qname,len1)
        call chr_puti(itype,qname,len1)
        call chr_putc('_QUEUE',qname,len1)

        call gns_idng(iwkid,prfile,len1,status)
        inquire(file=prfile,name=prname)

      end if
*
*         Copy data normalizing to range
*
      nlevup2 = nlevu + 2
      if(plog) then
        twhite = max(white,1.0e-20)
        tblack = max(black,1.0e-20)
        lowlim = min(twhite,tblack)
        twhite = log(twhite)
        tblack = log(tblack)
        range=twhite-tblack
        mult = real(nlevu)/range
        do i=1,irow
          itemp = ixst + i - 1
          do j=1,icol
            temp1 = z(j-1+ichst,itemp)
            temp1 = max(temp1,lowlim)
            temp1 = log(temp1)
            temp = nint( (temp1-tblack)*mult) + 2
            iwork(j,i) = max(2,min(nlevup2,temp))
          end do
        end do
      else
        range=white-black
        mult = real(nlevu)/range
        do i=1,irow
          itemp = ixst + i - 1
          do j=1, icol
            temp1 = z(j - 1 + ichst, itemp)
            temp = nint( (temp1 - black) * mult) + 2
            iwork(j,i) = max(2,min(nlevup2,temp))
          end do
        end do
      end if
*
*         Get a zone of the right aspect ratio allowing space for the
*         key
*
      if(key) then
        zkey = 0.2
        call sgs_zshap((1.0+zkey),'BL',izone,status)
        call sgs_zshap(1.0,'BL',izone1,status)
        call sgs_zshap(asp,'BL',izone1,status)
      else
        call sgs_zshap(asp,'CC',izone1,status)
      end if
*
*         Set world co-ordinates to pixels
*
      call sgs_sw(0.5,real(icol)-0.5,0.5,real(irow)-0.5,status)
*
*   Set colour table to grey-scale
*
      call setgrey(2,nlevs-1)
*
*         Draw the map
*
      include 'gr_fotor_gca'
*
*         Draw a frame
*
      if(key) then
        call sgs_box(0.5,real(icol)-0.5,0.5,real(irow)-0.5)
*
*         Select original zone.
*
        call sgs_selz(izone,status)
*
*         Draw key and title
*
        call gr_keyr(nlevk,nlevu,zkey,black,white,title)
      end if

      call sgs_close

*    Print the file - if present!
*    This assumes that logical names are set up for the queues of the
*  form GKS_(workstation type)_QUEUE, e.g. GKS_1200_QUEUE.

      if(wkcat.eq.0) call print_file(prname,qname,typsub)
 500  continue
      end
