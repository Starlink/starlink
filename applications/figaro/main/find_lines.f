      subroutine find_lines(x,y,in,tram1,tram2,line_count,nyp,batch,
     :     xlabel,xunits,zunits,status)
*+
* Name:
*    FIND_LINES

* Invocation:
*    CALL FIND_LINES(X,Y,IN,TRAM1,TRAM2,LINE_COUNT,NYP,BATCH,XLABEL,
*                    XUNITS,ZUNITS,STATUS)

* Purpose:
*    Find lines in a spectrum

* Description:
*    Initially the whole spectrum is displayed. The user select lines
*    by marking to either side of them, zoom in/out, and (if zoomed) move
*    around the spectrum. An option is available to use an automatic line
*    location algorithm.

* Arguments:
*    Y(IN) = REAL ARRAY (Given)
*        Intensity data
*    X(IN) = REAL ARRAY (Given)
*        X axis data
*    IN = INTEGER (Given)
*        Number of channels in data
*    BATCH = LOGICAL (Given)
*        If running in batch mode
*    XLABEL = CHARACTER*(*) (Given)
*        X label
*    XUNITS = CHARACTER*(*) (Given)
*        X units
*    ZUNITS = CHARACTER*(*) (Given)
*        Z units (i.e. intensity)
*    NYP = INTEGER (Given)
*        Number of line slots available
*    STATUS = LOGICAL (Given and returned)
*        Error status, 0=ok
*    LINE_COUNT = INTEGER (Returned)
*        total number of lines(<50)
*    TRAM1(NYP) = REAL ARRAY (Returned)
*        Left hand tram line for the line
*    TRAM2(NYP) = REAL ARRAY (Returned)
*        right " " " " " " "  " "  " "
*
* Subroutines/functions referenced:
*    GR_VLINE              : Draw vertical line on plot
*    LINE_SHOW             :
*    PICK_LINES            : Pick regions with cursor
*    SEG_DISP              : Display a segment
*    SEG_SEARCH            : Search for lines
*    TRAM_OPTS             : Tell user cursor options
*
*    PAR_WRUSER            : Write character string to user
*
* Authors:
*  TNW: T.N.Wilkins, Cambridge until 9/92, then Durham
* History:
*  TNW: 30-OCT-1989 Modified to use QMENU
*  TNW: 12/4/91 Displaying full spectrum merely made a special case of
*        segments,
*  TNW: 18/6/92 Use of mod in altering jseg.
*  TNW: 6/8/93 Whole option from segments now just another segment.
*  TNW: 9/8/93 Forget segmentation as we knew it, just allow user to
*       zoom in/out etc.
*  TNW: 26/8/93 Allow user to ask for help, and write this to grapphics
*       screen (or window), rather than always writing a list of options to
*       the alpha screen.
*-
      implicit none
      include 'SAE_PAR'
* integer
*
      integer line_count
      logical batch
      integer nyp
      character*(*) xlabel,xunits,zunits
*
* menus
*
      real segbounds(2)
      integer curkey
      integer in
      integer status,pstat
      real x(in)
      real y(in)
      real tram2(nyp)
      real tram1(nyp)
      integer nopts
      logical cscale,auto
      integer inlincnt,i,lc1
      real zcen, zhrange
      real ymax,xloc
      INTEGER OPT_END, OPT_SCALE, OPT_IN, OPT_OUT, OPT_WHOLE, OPT_BACK
     :     ,OPT_FORW, OPT_HELP ,OPT_AUTO
      PARAMETER (OPT_HELP = 1,
     :     OPT_END =  3,
     :     OPT_SCALE =  5,
     :     OPT_IN =  6,
     :     OPT_OUT =  7,
     :     OPT_WHOLE =  8,
     :     OPT_BACK =  9,
     :     OPT_FORW =  10,
     :     OPT_AUTO = 11)

      inlincnt = line_count

*  Check not in batch - can occur if NEW mode attempted
*
      if(batch) then
         call par_wruser('Lines cannot be selected in batch',pstat)
         status = SAI__ERROR
      end if
*
* full
*
      segbounds(1) = x(1)
      segbounds(2) = x(in)

      call gr_soft(status)
      if(status.ne.SAI__OK) return

* Display spectrum for identification and loop until user hits E

      curkey = 0

      cscale = .false.

* Allow auto mode if no lines already found

      auto = line_count.eq.0

      do while(curkey.ne.OPT_END)
         call seg_disp(segbounds,x,y,in,cscale,xlabel,xunits,zunits,
     :        ymax)
         call pgmtext('t',1.0,1.0,1.0,'Hit ? for help')

*           Show locations of lines

         if(line_count.gt.0) then
            call line_show(tram1,segbounds,line_count)
            call line_show(tram2,segbounds,line_count)
         end if

*         How many meaningful cursor options?

         if((segbounds(1).le.x(1)).and.(segbounds(2).ge.x(in)))
     :        then
            nopts = 6
         else
            nopts = 10
         endif
         lc1 = line_count
         call pick_lines(tram1,tram2,line_count,nyp,nopts,curkey,ymax,
     :        xloc,auto)
         if(lc1.lt.line_count) auto = .false.
*
*    CURKEY:
*          3 - End
*          5 - Change scale
*          6 - Zoom in
*          7 - Zoom out
*          8 - Whole
*          9 - Back
*          10 - Forward
*          11 - Auto

         cscale = curkey.eq.OPT_SCALE
         zcen =  0.5 * (segbounds(1) + segbounds(2))
         zhrange =  0.5 * (segbounds(2) - segbounds(1))
         if(curkey.eq.OPT_HELP) then
            call tram_opts(nopts,auto)
         else if (curkey.eq.OPT_WHOLE) then

*      display whole

            segbounds(1) = x(1)
            segbounds(2) = x(in)

         else if(curkey.eq.OPT_BACK) then

* Go back 1 segment

            if(segbounds(1).le.x(1)) then
               segbounds(1) = x(in) - zhrange * 2.0
               segbounds(2) = x(in)
            else
               zcen = zcen - zhrange * 1.5
               zcen = max(zcen,x(1))
               segbounds(1) = zcen - zhrange
               segbounds(2) = zcen + zhrange
            endif

         else if(curkey.eq.OPT_IN) then

*  Zoom in

            zcen = xloc
            zhrange =  0.5 * zhrange
            segbounds(1) = zcen - zhrange
            segbounds(2) = zcen + zhrange
         else if(curkey.eq.OPT_OUT) then

*  Zoom out

            zhrange =  2.0 * zhrange
            segbounds(1) = zcen - zhrange
            segbounds(2) = zcen + zhrange

*     Automatic line search

         else if(curkey.eq.OPT_AUTO) then

            if(line_count.ne.0) then
               call zero_real(tram1,line_count)
               call zero_real(tram1,line_count)
            endif
            line_count = 0
            call seg_search(x,y,in,line_count,tram1,tram2,nyp,status)
            if(status.ne.SAI__OK) return

* Advance segment

         else if(curkey.eq.OPT_FORW) then

            if(segbounds(2).ge.x(in)) then
               segbounds(1) = x(1)
               segbounds(2) = x(1) + zhrange * 2.0
            else
               zcen = zcen + zhrange * 1.5
               zcen = min(zcen,x(in))
               segbounds(1) = zcen - zhrange
               segbounds(2) = zcen + zhrange
            endif
         end if

*  Allow segbounds to be just 1 pixel outside the limits of the data (seg_disp
*  will bring them into range, but this is convenient to indicate we're at
*  the end of the spectrum)

         segbounds(1) = max(segbounds(1),(2.0*x(1) - x(2)))
         segbounds(2) = min(segbounds(2),(2.0*x(in) - x(in-1)))

      end do


* Make sure points in range (just in case). Differences are likely to
* be marginal, and the user wouldn't notice them, but can cause
* problems in fitting.

      do i = inlincnt+1, line_count
         if(tram1(i).lt.x(1)) tram1(i) = x(1)
         if(tram1(i).gt.x(in)) tram1(i) = x(in)
         if(tram2(i).lt.x(1)) tram2(i) = x(1)
         if(tram2(i).gt.x(in)) tram2(i) = x(in)
      enddo
      end
