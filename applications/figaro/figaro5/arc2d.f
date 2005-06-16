      subroutine arc2d( STATUS )
*+
* Name:
*    ARC2D

* Invocation:
*    CALL ARC2D( STATUS )
*
* Purpose:
*   Wavelength calibration

* Description:
*     This program controls both 1D and 2D wavlength calibration and
*   can operate either in BATCH or INTERACTIVE modes.
*     The philosophy behind it is somewhat different to those presented
*   in the existing SPICA/SDRSYS and FIGARO software in many respects.
*   In particular its exclusive use of gausian fitting of arclines, its
*   demand for "intellegent" users, who can decide which lines they want
*   to use initially and then allow them to make objective assesments of
*   which,if any are erroneous. Typical diagnostic information given are
*   plots of residuals from the fit versus line width,flux and position.
*   This is all made possible by the use of the Gaussian fitting.
*     The least squares polynomial fitting allows weights to be included
*   for each line(again derived from the formal Gaussian fits).Thus it
*   is possible to constrain the polynomial in difficult regions
*   eg "the 5100 gap" without distorting the global fit.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image for input
*          This should be a file containing an arc spectrum.
*    ARC_OPTS = CHARACTER (Read)
*        Enter arc fit option
*          NEW    : set up a new wavelength calibration
*          REPEAT : Itterate on previous calibration
*          CLONE  : CLone a previous calibration
*    YSTART = INTEGER (Read)
*        analysis lower limit
*            The data between the limits ystart and yend is extracted
*            and the resultant spectrum is used to locate the lines.
*    YEND = INTEGER (Read)
*        analysis upper limit
*            The data between the limits ystart and yend is extracted
*            and the resultant spectrum is used to locate the lines.
*    YBLOCK = INTEGER (Read)
*        Enter analysis x-sect width
*            Each window is of this width (except perhaphs the final one).
*    ITERATION = INTEGER*2 (Read)
*        New value of iteration
*    ORDER = INTEGER (Read)
*        order for polynomial fitting
*          This is for the continuity correction of the data. Idealy the
*          arc should have been pre-processed with ARCSDI, so a low
*          order e.g. 2 should be used.
*    MAXLINES = INTEGER (Read)
*        Maximum number of lines to allow room for
*          This must be greater than or equal to the number of lines
*          fitted, so room should be allowed in case any more are
*          to be added later.
*    CLFILE = FILE (Read)
*        Name of image for cloning from
*          This should be a file containing an arc spectrum.
*    TOLS = CHARACTER (Read)
*        For use in batch only
*    KEEP_ITT = LOGICAL (Read)
*        keep itteration files'
*    PRFITS = LOGICAL (Read)
*        Print out details of fitting

* Files:
*     NAME                  PURPOSE
*  (image).IAR Stores polynomial coefficients, for use by ISCRUNCH.
*  IMAGE       The arc data.  This should be a FIGARO data file with
*              a data array. If there is a data array for the first
*              axis the information it contains will be used during
*              the program.
*
* Subroutines/functions referenced:
*    APPLY_TOLS         : Apply tolerances
*    ARCFIT             : this does the actual fitting of polynomials
*                         in the channel direction (wavelength as a
*                         function of channel number). Either the line
*                         positions or the positions interpolated by
*                         control_cpoly_w may be used.
*    ARC_WINDOW         : Find line centres by fitting Gaussians
*    CLGRAP             : Close graphics
*    CNF_PVAL           : Full pointer to dynamically allocated memory
*    CONTIN_CORR        : this fits polynomials in the x-sect direction
*                         (i.e. along the lines), so as to remove
*                         discontinuities due to noise. It then set the
*                         results from this into the .RES.DATA structure
*                         for use by arcfit.
*    GET_LINE_IDS       : This obtains the line ids etc., if in new or
*                         clone mode. Otherwise it simply maps the data
*                         file with some checking.
*    TWO_OPEN           : Open input file and get its dimensions etc.
*    DSA_GET_WORK_ARRAY : Get virtual memory
*    IROUTP             : this outputs the results of the fitting by
*                         arcfit to a file for use by ISCRUNCH, and
*                         also a summary to the terminal. This was
*                         copied from the FIGARO IARC program.
*    LOOK               : Output values of main results array
*    UNMAP_RES          : Unmap results  structure
*
*
*    DSA_FREE_WORKSPACE : Free workspace
*    DSA_CLOSE          : Close DSA system
*    PAR_QUEST          : Obtain YES/NO response from user
*    PAR_WRUSER         : Write character string to user
*
* Authors:
*   DJA: D.J.Axon Manchester
*   TNW: T.N.Wilkins Manchester, Cambridge from 2/89
*   AJH: A.J. Holloway Manchester 97+

* History:
*   Altered TNW 4/7/88 to have separate routine
*   for main menu and 4/8/88 to use virtual mem for arcfit a lot more.
*   ACCEPTFITS flag added 26/9/88 TNW
*   Altered to use GET_LINE_IDS TNW 3/10/88, tidied 5/10/88
*   Character workspace passed for ARCFIT TNW 11/10/88
*   PARDESCR added to arguments of SHOW_DIAGNOSIS 20/7/89 TNW
*   Changes to character mapping/VM TNW 18/9/89
*   TNW 21/9/89, more moved to GET_LINE_IDS
*   TNW 8/12/89, arc_window called directly
*    "  1-10/7/91 Changes for new results structure
*   AJH Oct 1997 Included PDA required variables
*-
* _____________________________________________________________________
      implicit none

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
*
* include common blocks
*
      integer status
      include 'arc_dims'
*
*  integer
*
      integer norder

* max order of polynomial

      integer MAX_KPLUS1
      parameter (MAX_KPLUS1 = 10)

* PDA required vars

      integer maxnpts
      parameter (maxnpts = 2048)
      double precision athree(3*maxnpts + 3*max_kplus1,50)
*      double precision athre2(3*maxnpts + 3*max_kplus1,50)
* pointers


* to coeffs of poly fits in channel direction

      integer coptr
      integer slot,slot2,slot3,slot4,slot5,slot6,slot7,slot8,slot9
      integer slot10,slot11,slot12,slot13,slotco
      integer w1ptr,w2ptr,w3ptr,w4ptr,w5ptr,w6ptr,w7ptr,w8ptr,w9ptr
      integer w10ptr,w11ptr,w12ptr,w13ptr

      real rmsmax
      integer nfits
      logical acceptfits
*
      logical polydata
      integer iopt
      logical loop
      include 'bytesdef'
      logical par_quest
      integer chr_len
*
* data statement
*
      data norder/3/
* -------------------------------------------------------------------
      status = SAI__OK

* increased TNW 30/11/89 from 13 to 14

      mxpars = 7
      nyp = 20
      polydata = .false.

* Perform initialisation and get line identifications

      call get_line_ids(.true.,status)
      if(status.ne.SAI__OK) goto 500

* interogate the data cube. Apply tols if required

      loop=.true.
      do while(loop)

        if(batch) then
          iopt = 7
        else
          call arcmenu(spdim1,iopt,status)
        end if

* Look at values of cube

        if(status.ne.SAI__OK) then

          goto 500

        else if(iopt.eq.1) then

          call look(%VAL(CNF_PVAL(d_rptr)),.false.,
     :              %VAL(CNF_PVAL(staptr)),%VAL(CNF_PVAL(d_vptr)))

* Plots

        else if ((iopt.eq.2).or.(iopt.eq.3)) then

          call shdiagnosis((iopt.eq.2),.false.,status)

* Apply tols if required

        else if(iopt.eq.4) then

          call apply_tols(.true.,status)

        else if (iopt.eq.5) then

          loop=.false.

        else if (iopt.eq.6) then

*   Fit arc line positions and wavelength with a polynomial (in channel
* direction).

*   Get workspace (d):
*     W1(LINE_COUNT)
*     W2(LINE_COUNT)
*     W3(LINE_COUNT)
*     W4(MAX_KPLUS1)
*     W5(MAX_KPLUS1,MAX_KPLUS1)
*     W6(MAX_KPLUS1)
*     W7(LINE_COUNT)
*     W8(LINE_COUNT)
*     W9(LINE_COUNT)
*     W10(LINE_COUNT) (r)
*     W11(LINE_COUNT) (r)
*     W12(LINE_COUNT)
*     COPTR (11*SPDIM1)
*     W13PTR(LINE_COUNT) (l)

          call dsa_get_work_array(line_count,'double',w1ptr,slot,status)
          call dsa_get_work_array(line_count,'double',w2ptr,slot2,
     :                            status)
          call dsa_get_work_array(line_count,'double',w3ptr,slot3,
     :                            status)
          call dsa_get_work_array(MAX_KPLUS1,'double',w4ptr,slot4,
     :                            status)
          call dsa_get_work_array(MAX_KPLUS1*MAX_KPLUS1,'double',w5ptr,
     :                            slot5,status)
          call dsa_get_work_array(MAX_KPLUS1,'double',w6ptr,slot6,
     :                            status)
          call dsa_get_work_array(line_count,'double',w7ptr,slot7,
     :                            status)
          call dsa_get_work_array(line_count,'double',w8ptr,slot8,
     :                            status)
          call dsa_get_work_array(line_count,'double',w9ptr,slot9,
     :                            status)
          call dsa_get_work_array(line_count,'float',w10ptr,slot10,
     :                            status)
          call dsa_get_work_array(line_count,'float',w11ptr,slot11,
     :                            status)
          call dsa_get_work_array(line_count,'double',w12ptr,slot12,
     :                            status)
          call dsa_get_work_array(11*spdim1,'double',coptr,slotco,
     :                            status)
          call dsa_get_work_array(line_count,'logical',w13ptr,slot13,
     :                            status)
          if(status.ne.SAI__OK) goto 500

* fit polynomials

          call arcfit(norder,%VAL(CNF_PVAL(d_rptr)),
     :                %VAL(CNF_PVAL(d_vptr)),%VAL(CNF_PVAL(staptr)),
     :                %VAL(CNF_PVAL(d_wptr)),%VAL(CNF_PVAL(d_aptr)),
     :                %VAL(CNF_PVAL(coptr)),rmsmax,nfits,
     :                %VAL(CNF_PVAL(w1ptr)),%VAL(CNF_PVAL(w2ptr)),
     :                %VAL(CNF_PVAL(w3ptr)),%VAL(CNF_PVAL(w4ptr)),
     :                %VAL(CNF_PVAL(w5ptr)),%VAL(CNF_PVAL(w6ptr)),
     :                %VAL(CNF_PVAL(w7ptr)),%VAL(CNF_PVAL(w8ptr)),
     :                %VAL(CNF_PVAL(w9ptr)),%VAL(CNF_PVAL(w10ptr)),
     :                %VAL(CNF_PVAL(w11ptr)),%VAL(CNF_PVAL(w12ptr)),
     :                polydata,acceptfits,%VAL(CNF_PVAL(w13ptr)),status,
     :                maxnpts)

          if(acceptfits) then
            if(par_quest('Fits OK?',.true.)) then

* Output results to file and to terminal

              call iroutp(datafile(:chr_len(datafile))//'.iar',
     :                    datafile,wavdim,spdim1,%VAL(CNF_PVAL(coptr)),
     :                    norder,nfits,rmsmax,status)
            end if
          end if

* Free workspace

          call dsa_free_workspace(slot13,status)
          call dsa_free_workspace(slotco,status)
          call dsa_free_workspace(slot12,status)
          call dsa_free_workspace(slot11,status)
          call dsa_free_workspace(slot10status)
          call dsa_free_workspace(slot9,status)
          call dsa_free_workspace(slot8,status)
          call dsa_free_workspace(slot7status)
          call dsa_free_workspace(slot6,status)
          call dsa_free_workspace(slot5,status)
          call dsa_free_workspace(slot4,status)
          call dsa_free_workspace(slot3,status)
          call dsa_free_workspace(slot2,status)
          call dsa_free_workspace(slot,status)

        else if(iopt.eq.7) then

          call dsa_get_work_array(line_count,'float',w1ptr,slot,status)
          call dsa_get_work_array(line_count,'float',w2ptr,slot2,status)
          if(status.ne.SAI__OK) goto 500
          call arc_window(%VAL(CNF_PVAL(d_xptr)),
     :                    %VAL(CNF_PVAL(d_tlptr)),
     :                    %VAL(CNF_PVAL(d_trptr)),idstring,
     :                    %VAL(CNF_PVAL(w1ptr)),%VAL(CNF_PVAL(w2ptr)),
     :                    status)
          call dsa_free_workspace(slot,status)
          call dsa_free_workspace(slot2,status)
          if(batch) then
            call apply_tols(.true.,status)
            loop = .false.
          end if

        else if(iopt.eq.8) then

          call setup_arc(status)

        else

*      Get workspace:
*         W1PTR(20,LINE_COUNT) (d):
*         W2PTR(2,LINE_COUNT)  (d):
*         W3PTR(LINE_COUNT)    (l):

          call dsa_get_work_array(20*line_count,'double',w1ptr,slot,
     :                            status)
          call dsa_get_work_array(2*line_count,'double',w2ptr,slot2,
     :                            status)
          call dsa_get_work_array(line_count,'logical',w3ptr,slot3,
     :                            status)
          if(status.ne.SAI__OK) goto 500

          call contin_corr(%VAL(CNF_PVAL(w1ptr)),%VAL(CNF_PVAL(w2ptr)),
     :                     %VAL(CNF_PVAL(w3ptr)),polydata,status,
     :                     athree,maxnpts)

          call dsa_free_workspace(slot3,status)
          call dsa_free_workspace(slot2,status)
          call dsa_free_workspace(slot,status)
        end if
      end do
      call clgrap
      call unmap_res(status)
  500 continue
      call dsa_close(status)
      end
