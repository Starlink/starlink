      subroutine fill_ext(results,resvar,fitsta,results2,errors,miss,
     :     skew,cauchy,dims1,dims2,vcorr,varnam,wavelength,vmin,vmax,
     :     samnam,ids,line_name,x,y,units)
*+
* Name:
*    FILL_EXT

* Invocation:
*    CALL FILL_EXT(RESULTS,RESVAR,FITSTA,RESULTS2,ERRORS,MISS,
*          SKEW,CAUCHY,DIMS1,DIMS2,VCORR,VARNAM,WAVELENGTH,VMIN,VMAX,
*          SAMNAM,IDS,LINE_NAME,X,Y,UNITS)

* Purpose:
*   To fill a newly create output figaro file for the program extactic.
*
* Description:
*   To fill a newly create output figaro file for the program extactic.
*
* Arguments:-
*     RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube
*     RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube
*     FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Results cube
*     SKEW = LOGICAL (Given)
*        If a skew fit present
*     CAUCHY = LOGICAL (Given)
*        If a Cauchy fit present
*     DIMS1 = INTEGER (Given)
*        Dimension 1 of arrays in Extatic file
*     DIMS2 = INTEGER (Given)
*        Dimension 2 of arrays in Extatic file
*     VCORR = REAL (Given)
*        Correction for Earths motion e.t.c.
*                                 subtracted from observed velocities
*     WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*     LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*     RESULTS2(DIMS1,DIMS2) = REAL ARRAY (Returned)
*        Extatic file results array
*     ERRORS(DIMS1,DIMS2) = REAL ARRAY (Returned)
*        Errors (extatic file)
*     MISS(DIMS1,DIMS2) = INTEGER ARRAY (Returned)
*        Missing values array
*     VARNAM(DIMS1) = CHARACTER*32 ARRAY (Returned)
*        Variable names
*     VMIN(DIMS1) = REAL ARRAY (Returned)
*        Minimum values of parameters
*     VMAX(DIMS1) = REAL ARRAY (Returned)
*        Maximum values of parameters
*     SAMNAM(LINE_COUNT) = CHARACTER*32 ARRAY (Returned)
*        Sample names
*     IDS(DIMS2) = CHARACTER*32 ARRAY (Returned)
*        Identifications
*     X(DIMS1) = REAL ARRAY (Returned)
*        X array
*     Y(DIMS2) = REAL ARRAY (Returned)
*        Y array
*     UNITS(DIMS1) = CHARACTER*32 ARRAY (Returned)
*        Units
* Global variables:
*     MXPARS,NYP,NXP = INTEGER (Given)
*        Dimensions of above (passed in common) (include file arc_dims)
*     NCNTRL = INTEGER (Given)
*        1st dimension of fit status array (include file arc_dims)
*     LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*
* Authors:
*   T.N.Wilkins. Manchester 12/12/86
* History:
*   TNW, DSA routine rather than DTA used to get data units
*   TNW 27/1/89 Change to allow up to 9 Gaussians
*   TNW 23/11/90 Take data units from common ARCDIMS
*   TNW 28/11/90 Bug fix-dimension of SAMNAM, and way it was filled.
*   TNW 25/3/91 Change to checking for new fit.
*   TNW  3/9/91 Use same names in VARNAM as in TWODSPEC RESULTS
*               structure, for some parameters
*-
      implicit none
      include 'arc_dims'
      include 'status_inc'
      include 'fit_coding_inc'
      include 'PRM_PAR'
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      integer dims1,dims2
      logical skew,cauchy
      real wavelength(line_count)
      real vcorr
      character*32 varnam(dims1),samnam(line_count),ids(dims2)
      character*32 units(dims1)
      character*10 line_name(line_count)
      real results2(dims1,dims2)
      real vmin(dims1),vmax(dims1)
      real errors(dims1,dims2)
      real x(dims1),y(dims2)
      integer*2 miss(dims1,dims2)
      integer k,v0,v1,cmp,hp,cp,wp
      integer xsect,line
      integer case
      character*8 vrnam(8)
      integer i,len1
      real wave,ewave,width,ewidth,vel,evel,vwid,evwid
      integer nelems,ppos
      real rhnwin,rcen
      integer get_parnum
      character*32 vunits(0:2)
      integer vlookup(5)
      data vrnam/'XSECT','Base','Centre','Width','Height','Skew_1',
     :         'Cauchy_1','MASK'/
      data vlookup/0,1,2,2,1/
      data vunits/' ',' ','km/s'/

      vunits(1) = zunits

* Central x-sect,base,c1,w1,h1,...,skew,cauchy,mask

      case = 0

* How many parameters do we have (excluding skew/cauchy/mask)?

      v0 = dims1 - 1
      if(skew) v0 = v0 - 1
      if(cauchy) v0 = v0 - 1

      do i = 1, v0

*  What component number are we up to?
*  Get number pointing to parameter type (e.g. centre) to 5 or less.

        cmp = i/3
        if(cmp.eq.0) then
          v1 = i
        else
          v1 = mod((i-3),3)+3
        end if

        len1 = 0
        call chr_fill(' ',varnam(i))
        call chr_appnd(vrnam(v1),varnam(i),len1)
        if(cmp.ne.0) then
          call chr_putc('_',varnam(i),len1)
          call chr_puti(cmp,varnam(i),len1)
        end if

* Write units of variables to output file

        units(i) = vunits(vlookup(v1))
      end do

* Name for mask

      varnam(dims1) = vrnam(8)

* Skew and Cauchy

      if(skew) then
        v0 = v0 + 1
        units(v0) = vunits(0)
        varnam(v0) = vrnam(7)
      end if
      if(cauchy) then
        v0 = v0 + 1
        units(v0) = vunits(0)
        varnam(v0) = vrnam(8)
      end if

* Zero output arrays

      nelems = dims1*dims2
      call zero_real(results2,nelems)
      call zero_real(errors,nelems)
      call zero_short(miss,nelems)

      do line = 1, line_count

* Fill SAMNAM array (character).

        len1 = 0
        call chr_fill(' ',samnam(line))
        call chr_putc(line_name(line),samnam(line),len1)

        do xsect = 1, nxp

* Check if fit different from previous x-sect

          ppos = get_parnum('Space1_pos')
          rcen = results(ppos,line,xsect)
          if(rcen.eq.VAL__BADR) then
            rcen = -999.0
          else
            rhnwin = sqrt(resvar(ppos,line,xsect))
          end if

*     new fit (not already output)

          if(nint(rcen).eq.xsect) then

*       Get and decode fit_status, with meaningful output

*        Output position and id of line

            call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)
            if((deccntr(FIT_MODEL).ne.0).and.
     :                    (deccntr(FIT_STAT).eq.1)) then
              case = case +1

*          Range

              results2(1,case) = rcen
              errors(1,case) = rhnwin
              miss(1,case) = 1

*          Mask

              results2(dims1,case) = real(line)
              miss(dims1,case) = 1

*          Base

              if(deccntr(BACK_MODEL).eq.1) then
                ppos = get_parnum(varnam(2)(:10))
                results2(2,case) = results(ppos,line,xsect)
                errors(2,case) = sqrt(resvar(ppos,line,xsect))
                miss(2,case) = 1
              end if

              do cmp = 1, deccntr(FIT_NCMP)
                cp = cmp * 3
                wp = cp + 1
                hp = wp + 1

*          Centre

                k = get_parnum(varnam(cp)(:10))
                wave = results(k,line,xsect)
                ewave = sqrt(resvar(k,line,xsect))

*          Width

                k = get_parnum(varnam(wp)(:10))
                width = results(k,line,xsect)
                ewidth = sqrt(resvar(k,line,xsect))

*          Combine centre and width results

                call wave2vel(wavelength(line),wave,ewave,width,
     :                 ewidth,vel,evel,vwid,evwid)

                results2(cp,case) = vel-vcorr
                errors(cp,case) = evel
                miss(cp,case) = 1

                results2(wp,case) = vwid
                errors(wp,case) = evwid
                miss(wp,case) = 1

*          Height

                k = get_parnum(varnam(hp)(:10))
                results2(hp,case) = results(k,line,xsect)
                errors(hp,case) = sqrt(resvar(k,line,xsect))
                miss(hp,case) = 1
              end do


*           Single

              if(deccntr(FIT_TYPE).eq.SINGLE) then

*          Skew

                if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
                  if(cauchy) then
                    v1 = dims1 - 2
                  else
                    v1 = dims1 - 1
                  end if
                  k = get_parnum(varnam(v1)(:10))
                  results2(v1,case) = results(k,line,xsect)
                  errors(v1,case) = sqrt(resvar(k,line,xsect))
                  miss(v1,case) = 1

*           Cauchy

                else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
                  v1 = dims1 - 1
                  k = get_parnum(varnam(v1)(:10))
                  results2(v1,case) = results(k,line,xsect)
                  errors(v1,case) = sqrt(resvar(k,line,xsect))
                  miss(v1,case) = 1

*             type of single fit

                end if

*           single fit

              end if

* Fill IDS array (character).

              len1 = 0
              call chr_fill(' ',ids(case))
              call chr_puti(case,ids(case),len1)

*         fit a success or nag error if allowed

            end if

*     new fit

          end if

*   over x-sects

        end do

* over lines

      end do

* Fill Vmin and Vmax arrays

      call gen_cfill(1,dims1,VAL__MAXR,vmin)
      call gen_cfill(1,dims1,VAL__MINR,vmax)
      call gen_nfillf(dims1,x)
      call gen_nfillf(dims2,y)

      do case = 1,dims2
        do i =1,dims1
          if(miss(i,case).eq.1) then
            vmin(i) = min(vmin(i),results2(i,case))
            vmax(i) = max(vmax(i),results2(i,case))
          end if
        end do
      end do
      end
