      subroutine search(x,y,in,numb,chan1,chan2,nyp,peaks,hpfy,status)
*+
* Name:
*    SEARCH

* Invocation:
*    CALL SEARCH(X,Y,IN,NUMB,CHAN1,CHAN2,NYP,PEAKS,HPFY,STATUS)

* Purpose:
*   Locate (arc) lines automatically

* Description:
*  This function searches what is assumed to be an arc spectrum read
*  from the arrays and gives up to 50 arc line positions.
*  SEARCH considers a line to be strong enough to be a real line
*  if the peak level is greater than [(the average noise level nearby) +
*  SIGLEV*(sigma of the noise level)]
*
*  The parameter NWIDTH gives the approximate width of the lines (in
* channels)
*  - being the number of channels over which a search is made for a peak
* .
*  Also NWIDTH channels centred on the peak are omitted when calculating
*  the mean noise level.
*
*  The parameter SIGLEV is the significance level used in deciding what
* is
*  and is not a line, by its peak value. (See above)
*
* Variables used:
*      LINES     number of lines in the data
*      NWIDTH    the approx. width of the lines
*      SIGLEV    the significance level deciding what is a line
*
* Arguments:
*    X(IN) = REAL ARRAY (Given)
*       X axis array
*    Y(IN) = REAL ARRAY (Given)
*       Y (intensity) data
*    IN = INTEGER (Given)
*       Number of elements in X and Y
*    NYP = INTEGER (Given)
*       Number of slots for lines
*    NUMB = INTEGER (Returned)
*       Number of lines found
*    CHAN1(NYP) = REAL ARRAY (Returned)
*       Left trams
*    CHAN2(NYP) = REAL ARRAY (Returned)
*       Right trams
*    PEAKS(4000) = REAL ARRAY (Workspace)
*
*    HPFY(IN) = REAL ARRAY (Workspace)
*        High-pass filtered version of Y
*    STATUS = INTEGER (Given and Returned)
*        Global status

* History:
*  Modified to run as a command processor subroutine.
*                                       KS  17th May 1982
*  Tidied to use the WRTARC routine.   AQB / RGO  24-FEB-1984
*  Vastly modified to use different algorithm (use of NWIDTH and
*  SIGLEV)                             AQB / RGO  10-APR-1984
*  Modified for use in arcpartii T.N.W./Man APR-1985
*  Altered to receive PEAKS as workspace, TNW 12/8/88
*  Separate trams for plotting, and option to plot trams here removed,
*                              TNW/Cambridge 15-APR-1991
*  High-pass filter before searching, TNW Durham, 9/8/93
*  Use QMENU, TNW 7/2/94
*-
      implicit none
      integer in
      real x(in),y(in),hpfy(in)
      integer nyp
      integer numb
      real chan1(nyp),chan2(nyp)
* Local
*
      real lprod,lgmean,lsigg
      integer icentre,mindis
      real y_l,y_r,rchn2rx
      integer num_l,num_r,nprod
      real pkcnts
      integer ioldcen
      real gmeanl,gmeanr
      real sum1,sum2,dist,val,centre,sigma,last_centre
      real chans(11)
      integer status,nok, stchan, enchan
      real peaks(4000)
      real min_lev
      real count,mincnt,maxcnt
      character*18 chars
      logical ok
      integer i,j,l,npk,lt1,lt2,jc,lst,nl1
      real value,siglev,gmean,sigg,gmean_set,sigg_set,diff
      integer nwidth
      real cent
      integer NMENU,nnums,ivalue,iopt
      parameter (NMENU = 4)
      character*65 menu(NMENU),dumc*1
      save nwidth,siglev,min_lev
      data menu/
     :     'NWIDTH %F : Approximate width of lines (pixels, odd) = ',
     :     'SIGLEV %F : Significance level for peak ............ = ',
     :     'PEAK   %F : Minimum peak level ..................... = ',
     :     'OK        : Settings okay'/
      data nwidth,siglev,min_lev/5,100.0,100.0/
*
* If PROMPT active, explain NWIDTH and SIGLEV
*
      ok = .false.
      do while(.not.ok)
         ivalue = 55
         call chr_puti(nwidth,menu(1),ivalue)
         call chr_term(ivalue,menu(1))
         ivalue = 55
         call chr_putr(siglev,menu(2),ivalue)
         call chr_term(ivalue,menu(2))
         ivalue = 55
         call chr_putr(min_lev,menu(3),ivalue)
         call chr_term(ivalue,menu(3))
         call qmenu('Search Settings',menu,NMENU,NMENU,value,dumc,iopt
     :        ,nnums,status)
         if(status.ne.0) then
            return
         else if(iopt.eq.1) then
            ivalue = nint(value)
*
*   Check that nwidth is an odd number
*
            if (mod(ivalue,2).eq.0) then
               call par_wruser('NWIDTH has to be odd',status)
            else if((ivalue.ge.1).and.(ivalue.le.101)) then
               nwidth = ivalue
            else
               call par_wruser('Nwidth should be in the range 1 to 101'
     :              ,status)
            end if
         else if(iopt.eq.2) then
            siglev = value
         else if(iopt.eq.3) then
            min_lev = value
         else if(iopt.eq.4) then
            ok = .true.
         end if
      end do

* High-pass filter the data, peaks just used as workspace here

      call hpfil(in,y,hpfy,peaks,nwidth)
*
* Start analysis
*
      gmean = 4
      sigg = 2
      numb=0
      lst=(nwidth+1)/2
      nl1=in-lst+1
      npk = 0
      do i=lst,nl1
*
* ICHANS is an array of NWIDTH elements formed from portion of the arc
*
         do j=1,nwidth
            l=i+j-lst
            chans(j)=hpfy(l)
         end do
*
* Do analysis on this block
* NOK is returned as non-zero if a potential line is found, in which
* case CENT is the centroided line number and COUNT is the count level
* of
* the channel in which it occurs.
*
         lt1=max(i-nwidth,1)
         lt2=min(i+nwidth,in)
*
         mincnt=hpfy(lt1)
         maxcnt=mincnt
         do jc=lt1+1,lt2
            count=hpfy(jc)
            mincnt = min(mincnt,count)
            maxcnt = max(maxcnt,count)
         end do
*
         call centrd(chans,nwidth,cent,count,0,nok)
         if (nok.eq.0) then
*
*     Possible line found, store it
*
            cent=cent+real(i)
            if (npk.gt.0) then
               if (abs(cent-peaks(npk)).gt.1) then
                  npk = npk + 1
                  peaks(npk) = cent
               end if
            else
               npk = npk + 1
               peaks(npk) = cent
            end if

*   nok

         end if

* i=lst,nl1

      end do
*
*  Prepare to indicate line positions
*
      call gr_spen(3)
*
* Now centroid the lines. Centroid symmetrically around the peak,
* using all channels up to the nearest next line.
* ARC_DEFLIN defines the limits of the line. If these are less
* than NWIDTH then we cannot use the line.  Calculate mean noise level
* excluding NWIDTH channels around line, and channels with zero counts.
*
      numb = 0

* SIGG_SET is set to -1 to flag that it has not been set

      sigg_set=-1.0
      gmean_set=0.0
      do i = 1,npk
         diff=0.0
         icentre = nint(peaks(i))
         call arc_deflin(in,icentre,stchan,enchan,hpfy)
         mindis = min(abs(icentre-stchan),abs(enchan-icentre))
         if (((ioldcen.lt.enchan).and.(ioldcen.gt.stchan)).or.
     :        (mindis.le.(real(nwidth)/2.0))) go to 444
         if ((enchan-stchan).gt.nwidth) then
            pkcnts = hpfy(icentre)

*     Is the line high enough?

            if (pkcnts.lt.min_lev) go to 444
            nprod = 0
            y_l=0.0
            y_r=0.0
            lprod=0.0
            num_l=0
            num_r=0
            do j = stchan, enchan
               if((hpfy(j).gt.0).and.(j.lt.(icentre-nwidth/2).or.
     :              (j.gt.(icentre+nwidth/2)))) then
                  if ((j.lt.(icentre-nwidth)).or.
     :                 (j.gt.(icentre+nwidth))) then
                     lprod = lprod + log10(hpfy(j)/(siglev*0.3))
                  else
                     if(abs(j-icentre).le.nwidth)then
                        if (j.lt.icentre) then
                           y_l=y_l+hpfy(j)
                           num_l=num_l+1
                        else
                           y_r=y_r+hpfy(j)
                           num_r=num_r+1
                        end if
                     end if
                     lprod = lprod + log10(hpfy(j))
                  end if
                  nprod = nprod + 1
               end if
            end do
            if((num_l.gt.0).and.(num_r.gt.0))then
               gmeanl=y_l/num_l
               gmeanr=y_r/num_r
               diff=abs((gmeanl-gmeanr)/(gmeanl+gmeanr))
            end if
*
*   If no channels found that can be used for noise then use last values
*   of mean and sigma.
*
            if (sigg_set.lt.0.0) then
               sigg_set=sigg
               gmean_set=gmean
            else
               sigg_set=sigg_set*0.85+sigg*0.15
               gmean_set=gmean_set*0.85+gmean*0.15
            end if
            if (nprod.gt.0) then
               lgmean = lprod * (1./nprod)
               lsigg  = lgmean*0.5

*     nprod.gt.0

            end if
            if(lgmean.lt.38.0) then
               gmean = 10**lgmean
            end if
            if(lsigg.lt.38.0) then
               sigg = 10**lsigg
            end if
            if(gmean.gt.(gmean_set*1.4))then
               gmean=gmean_set*0.85+gmean*0.15
               sigg=sigg_set*0.85+sigg*0.15
            end if
*
*     Check that this is not just a side peak on another line.
*
            if (diff.lt.0.9)then
               if (pkcnts.ge.(gmean+siglev*sigg)) then
                  sum1 = 0.
                  sum2 = sum1
                  stchan = icentre - mindis
                  enchan = icentre + mindis
                  do j = stchan,enchan
                     dist = real(j-stchan+1)
                     val = hpfy(j)
                     sum1 = sum1+dist*val
                     sum2 = sum2+val
                  end do

*         Centre in pixels

                  centre = real(stchan-1)+sum1/sum2
                  icentre = nint(centre)

*         Centre in X values

                  centre = rchn2rx(x,in,centre)

*         Maximum data value

                  count = hpfy(icentre)
                  ioldcen=icentre
*
*         Calculate sigma and store in array
*
                  numb = numb + 1
                  call calc_sigma(x,hpfy,in,centre,mindis,sigma)
                  chan1(numb)=centre-3.0*sigma
                  chan2(numb)=centre+3.0*sigma

* Now make sure that trams don't overlap. Ideally should search for a
* minimum, but this will do for the moment.

                  if(numb.gt.1) then
                     if(chan1(numb).lt.chan2(numb-1)) then
                        chan1(numb) = 0.5 * (centre + last_centre)
                        chan2(numb-1) = chan1(numb)
                     end if
                  end if
                  last_centre = centre

                  if (numb.ge.nyp) then
                     call par_wruser
     :                    ('Maximum number of lines reached',status)
                     go to 255
                  end if

*       pkcnts.ge.(gmean+siglev*sigg) etc.

               end if

*     diff.lt.0.5

            end if

*   (enchan-stchan).gt.nwidth

         end if
 444     continue

* i=1,npk

      end do
 255  continue
*
* Print number of lines found
*
      write(chars,'(a,i4)') 'Lines found =',numb
      call par_wruser(chars,status)

      end
