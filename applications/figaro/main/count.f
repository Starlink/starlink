      subroutine count(results,fitsta,dims,skew,cauchy)
*+
* Name:
*    COUNT

* Invocation:
*    CALL COUNT(RESULTS,FITSTA,DIMS,SKEW,CAUCHY)

* Purpose:
*   Count fits in results cube

* Description:
*    To find and count results from the data cube. The maximum number
*   of gaussians fitted, and whether skew and/or cauchy fits are
*   present, plus the total number of fits (cases) present are returned.
*
* Arguments:-
*   RESULTS(MXPARS,NYP,NXP) = INTEGER ARRAY (Given)
*        Results "cube"
*   SKEW = LOGICAL (Returned)
*        If any skew fits
*   CAUCHY = LOGICAL (Returned)
*        If any Cauchy fits
*   DIMS(2) = INTEGER ARRAY (Returned)
*        Dimensions of output extatic file
* Global variables:
*   MXPARS,NYP,NXP = INTEGER (Given)
*        Dimensions of RESULTS
*   LINE_COUNT = INTEGER (Given)
*        Number of lines
*
* Authors:
*   T.N.Wilkins. Manchester 12/12/86
* History:
*   Setting of dims moved from create_ext to count TNW/Cambridge
*   Change to checking for new fit, TNW 25/3/91
*   New method for decoding fit status, TNW 28/5/91
*   Minor changes, TNW 3/9/91
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      logical skew,cauchy
      integer dims(2)
* ----------------------------------------------------------------------
      integer ncases,mcmp
      integer line
      integer xsect
      integer get_parnum
      real value
      include 'PRM_PAR'
      include 'status_inc'
      include 'fit_coding_inc'

      ncases = 0
      mcmp = 0
      cauchy = .false.
      skew = .false.

* Loop over lines

      do line = 1, line_count

        do xsect = 1, nxp

*     Check if at centre x-sect of fit

          value = results(get_parnum('Space1_pos'),line,xsect)
          if(value.ne.VAL__BADR) then


*     new fit (not already output)

            if(nint(value).eq.xsect) then

*         Get and decode fit_status

              call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)

*           Was the fit successful? If so how many components, and was
*           it skew or Cauchy?

              if(deccntr(FIT_STAT).eq.1) then
                skew = skew.or.(deccntr(FIT_MODEL).eq.SKEW_MODEL)
                cauchy = cauchy.or.(deccntr(FIT_MODEL).eq.CAUCHY_MODEL)
                mcmp = max(mcmp,deccntr(FIT_NCMP))
                ncases = ncases + 1

*         fit a success

              end if

*     new fit

            end if
          end if

*   over x-sects

        end do

* over lines

      end do

* Get value for dims(1) (parameters), and copy ncases to dims(2)

      dims(1) = mcmp*3+3
      if(skew) dims(1) = dims(1) + 1
      if(cauchy) dims(1) = dims(1) + 1
      dims(2) = ncases
      end
