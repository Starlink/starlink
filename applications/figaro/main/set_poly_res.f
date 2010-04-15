      subroutine set_poly_res(results,resvar,aa,mord,ylim,kp1,w,npts,x,
     :     used,arc,work,polydata,wav,athree,maxnpts,a3all)
*+
* Name:
*    SET_POLY_RES

* Invocation:
*    CALL SET_POLY_RES(RESULTS,RESVAR,AA,MORD,YLIM,KP1,W,
*                 NPTS,X,USED,ARC,WORK,POLYDATA,WAV,
*                 ATHREE,MAXNPTS,A3ALL)

* Purpose:
*   Store values of Chebyshev fits in results structure

* Description:
*       This subroutine is to evaluate the Chebyshev polynomials fitted
*    by control_cpoly and to put the values interpolated by the
*    polynomial into the results structure.
*
* Arguments:
*    AA(MORD,LINE_COUNT) = DOUBLE PRECISION ARRAY (Given)
*
*    MORD = INTEGER (Given)
*
*    YLIM(2,LINE_COUNT) = DOUBLE PRECISION ARRAY (Given)
*
*    KP1 = INTEGER (Given)
*
*    W(SPDIM1,LINE_COUNT) = DOUBLE PRECISION ARRAY (Given)
*
*    NPTS(LINE_COUNT) = INTEGER ARRAY (Given)
*
*    USED(LINE_COUNT) = LOGICAL ARRAY (Given)
*
*    RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Returned)
*
*    RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Returned)
*
*    ARC(NSLCT,LINE_COUNT) = INTEGER*2 ARRAY (Returned)
*
*    POLYDATA = LOGICAL (Returned)
*
*    X(SPDIM1,LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*
*    WORK(SPDIM1*4) = DOUBLE PRECISION ARRAY (Workspace)
*
*    WAV(LINE_COUNT) = DOUBLE PRECISION ARRAY (Workspace)
*
*    MAXNPTS = INTEGER (Given)
*        Max number of data points
*    ATHREE(3*MAXNPTS+3*MAX_KPLUS1) = DOUBLE PRECISION ARRAY
*     PDA_DPOLFT fit co-effs
*    A3ALL(3*MAXNPTS+3*MAX_KPLUS1,LINE_COUNT) = DOUBLE PRECISION ARRAY
*     PDA_DPOLFT fit co-effs for all lines.

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then Durham
*   AJH: A.J. Holloway Manchester

* History:
*   TNW: Original version
*   TNW: 30/11/89 Results here moved 1 later in results array.
*   TNW: 11/2/91 workspace changes
*   TNW: 9/7/93 Corrected call to fill_map
*   TNW: 1/8/94 Bug fix-dimensions of ARC corrected.
*   AJH: Oct 97 support of PDA fit co-effs array
*-
      implicit none
      include 'SAE_PAR'
      include 'arc_dims'

      integer mord
      integer kp1
      integer npts(line_count)
      integer maxnpts
      double precision aa(mord,line_count)
      double precision athree(3*maxnpts+3*mord)
      double precision a3all(3*maxnpts+3*mord,line_count)
      double precision ylim(2,line_count)
      double precision w(spdim1,line_count)
      logical used(line_count)
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer*2 arc(nslct,line_count)
      logical polydata
      double precision work(spdim1*4)
      double precision x(spdim1,line_count)
      double precision wav(line_count)

      integer xin
      integer yin
      integer xout
      integer yout
      integer i,j,status,ppos,get_parnum
* ---------------------------------------------------------------------
      status = SAI__OK

*  Divide up workspace. Since all of 1 data type can be done here

      xin = 1
      yin = xin + spdim1
      xout = yin + spdim1
      yout = xout + spdim1

      call fill_map(ylim,x,spdim1,line_count,aa,kp1,mord,work(xin),
     :      work(yin),work(xout),work(yout),status,athree,maxnpts,
     :      a3all)

      ppos = get_parnum('Contincent')

      do i=1,line_count

* If line was used in the fit then put results into results cube,
* otherwise set ARC to 1 for that line (if not 4 beforehand) to tell
* ARCFIT not to use that line for continuity corrected data

        if(used(i)) then
          do j=1,spdim1
            results(ppos,i,j) = real(x(j,i))
          end do

* Evaluate weights

          wav(i)=0.0d0
          do j=1,npts(i)
            wav(i)=wav(i)+1.0d0/(w(j,i)*w(j,i))
          end do
          wav(i)=wav(i)/real(npts(i))
          arc(1,i) = ARC_OK
        else if(arc(1,i).ne.ARC_NO_FITS) then
          arc(1,i) = ARC_ORIG
        end if
      end do
      do i=1,spdim1
        do j=1,line_count
          resvar(ppos,j,i) = real(wav(j))
        end do
      end do
      polydata = .true.
      end
