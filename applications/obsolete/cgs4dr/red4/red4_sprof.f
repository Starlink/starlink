*+  RED4_SPROF - Determine spatial seeing profiles.
      SUBROUTINE RED4_SPROF(DA,L,M,DV,DQ,NORD,CFORD,NTERM,
     :                      NCLO,NCHI,NXREF,PX,NPX,STATUS)
*    Description :
*	Routine to determine the spatial seeing profile from a two
*	dimensional echelle spectrum. nord midys must be pre-determined,
*	and the start and end increments relative to the midys
*	must be specified
*    Invocation :
*     CALL RED4_SPROF(DA,L,M,DV,DQ,NORD,CFORD,NTERM,
*     :               NCLO,NCHI,NXREF,PX,NPX,STATUS)
*    Parameters :
*	INPUT:	da	l (dispersion) x m (space) array of data
*		dv	l x m array of variances / flags
*		nord	number of echelle orders to be processed
*		cford	array of order fit coeffts
*               nterm   number of coefficients in the polynomial
*		nclo	low offset from midy for inclusion
*		nchi	high offset
*		nxref	reference x-value for midy fit coeffts
c
*	OUTPUT:	npx	number of profiles in a subpixel
*		px	array for midy profile sums - for up to 41
*			substeps per increment, and up to 21 increments
*			total width
*
*	COMMON:	/coext/ (determined in routine SPROF, i.e. here.)
*		xav	shift of dataset relative to the MIDY
*		nsuby	number of seeing profile substeps
*		nwob	object total width (odd)
*		nclot	midy - low edge
*		nchit	high edge - midy
*		nex	no increments excluded (gap+1)
c
*		/skyfix/
*		skymultx  sky multiplier (usually 1.0)
c
*	LOCAL:	xval	array of max values relative to peak in px
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Bob Carswell.   (CAVAD::RFC)
*     Steven Beard.   (REVAD::SMB)
*    History :
*              ??: Original version.                               (RFC)
*     14-Jan-1991: Copied into CGS4 software.                      (SMB)
*     19-Jan-1991: Modifications: ADAM prologue added. Maximum of
*                  100 assumption removed from minimum algorithm.
*                  Code spaced out. Fortran writes to the terminal
*                  replaced by ADAM calls. STATUS argument included.
*                  Array bounds violation trapped. Description for
*                  NTERM included. GOTOs flagged. ADAM include files
*                  included.                                       (SMB)
*     20-Jan-1991: Divide by zero check included.                  (SMB)
*    endhistory
*    Type Definitions :
!      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMERRS'
      INCLUDE 'SAI_ERR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      integer status
*    External references :
*    Global variables :
	character*1 char
	byte dq(l,m)
 	integer nclo(nord),nchi(nord),npx(41)
	real cford(12,50),da(l,m),px(21,41),dv(l,m)
	real xval(41),ymidy(41)
c
	common/coext/xav,nsuby,nwob,nclot,nchit,nex,char
	common/cosmrth/cthres
	common/prolims/ldlo,ldhi
	common/skyfix/skymultx
*	variables for JGR profile weighted extraction
	common/jgrwt/wtf(21,41)
*    Local Constants :
	byte good
	parameter (good=0)
*    Local variables :
*    Internal References :
*    Local data :
*-
*
*       Abort if there is a bad status on entry
*
	if ( status .ne. adam__ok ) return
c
*	add 1 to object/sky gap so that it can be used for start/end
	nex=nex+1

*	find total width to be used - minimum because of possible
*	order overlap. NCTOT is the minimum number of rows used in the
*       data window in any order.

	ntem=nchi(1)+nclo(1)+1
	nctot=ntem
	nclot=nclo(1)
	nchit=nchi(1)

	do i=2,nord

	  ntem=nchi(i)+nclo(i)+1
	  if(ntem.lt.nctot) nctot=ntem
	  if(nclo(i).lt.nclot) nclot=nclo(i)
	  if(nchi(i).lt.nchit) nchit=nchi(i)
	end do

*	check object width is less than useful total (i.e. it is not
*       too large to fit in the data window).

	if(nctot.lt.nwob+2) then

          STATUS = SAI__ERROR
          CALL MSG_SETI( 'NWOB', NWOB )
          CALL MSG_SETI( 'NCTOT', NCTOT )
	  CALL ERR_REP( ' ', 'RED4_SPROF: '/
     :      /'Object width ^NWOB  is too large; '/
     :      /'Window size is ^NCTOT', STATUS )
	  return
	end if

*	zero variables

	do i=1,nsuby

	  ymidy(i)=0.0
	  npx(i)=0

	  do j=1,21	! MAXINCREMENTS ?

	    px(j,i)=0.0
*	    Gordon Robertson weights (in case they are needed)
	    wtf(j,i)=0.0
	  end do
	end do

*	set up slit profiles at each increment substep

	do k=1,nord

*	  cycle through dispersion direction

	  do i=ldlo,ldhi

*           Normalise X about the reference position, to prevent
*           rounding errors.

	    xn=i-nxref
	    xterm=1.0
	    kterm=1
	    ych=0.0

*	    spatial increment
*           Work out the polynomial whose coefficients are stored in
*           CFORD to the NTERMth term.

	    do while (kterm.le.nterm)

	      ych=ych+xterm*cford(kterm,k)
	      xterm=xterm*xn
	      kterm=kterm+1
	    end do

*	    add 0.5 since center of increment is integer position

	    ychp=ych+0.5
	    nych=ychp

*	    fractional increment, 0.5 if exactly in the middle

	    fych=ychp-nych
	    nfych=float(nsuby)*fych+1

*	    add in the information to px(j,nfych)

	    lo=nych-nclot

*	    ... unless there is a noise spike

	    do j=1,nctot

	      jj=lo-1+j

*	      or outside range

	      if(jj.lt.1.or.jj.gt.m) goto 211	! ***

*	      cosmic ray threshold
	      if(dv(i,jj).lt.cthres.or.dq(i,jj).ne.good) goto 211	! ***
	    end do

	    do j=1,nctot

	      jj=lo-1+j
	      px(j,nfych)=px(j,nfych)+da(i,jj)
	    end do

	    ymidy(nfych)=ymidy(nfych)+ych+1-lo
	    npx(nfych)=npx(nfych)+1

211	    continue
	  end do
	end do

*	establish where the peak is to determine MIDY shift
*	fit to the local maximum, using parabolic approx

	CALL MSG_SETI( 'NSUBY', NSUBY )
	CALL MSG_OUT( ' ', '^NSUBY substeps used', STATUS )

	do j=1,nsuby

	  if(npx(j).gt.0) ymidy(j)=ymidy(j)/float(npx(j))
	  zval=0.0

	  do i=1,nctot

	    if(px(i,j).gt.zval) then

	      lval=i
	      zval=px(i,j)
	    end if
	  end do

	  if(lval.eq.nctot.or.lval.eq.1) then

            CALL MSG_SETI( 'J', J )
            CALL MSG_SETI( 'LVAL', LVAL )
	    CALL MSG_OUT( ' ', ' Edge of range. J=^J LVAL=^LVAL',
     :        STATUS )

	    xval(j)=-999.

	  else if ( lval .le. 0 ) then

            CALL MSG_SETI( 'J', J )
            CALL MSG_SETI( 'LVAL', LVAL )
	    CALL MSG_OUT( ' ', ' All points bad or outside range. '/
     :        /'J=^J LVAL=^LVAL', STATUS )

	    xval(j)=-999.
	  else

	    b=0.5*(px(lval+1,j)-px(lval-1,j))
	    c=0.5*(px(lval+1,j)+px(lval-1,j))-px(lval,j)
	    xinc=-b/(2.0*c)

*	    xvalac is position of peak in px array
	    xvalac=float(lval)+xinc

*	    xvalxp is position of MIDY in px array
	    xvalxp=ymidy(j)

*	    xval is shift relative to MIDY - ADD this to the MIDY
	    xval(j)=xvalac-xvalxp
	  end if
	end do

*	weighted average xval

	xav=0.0
	wsum=0.0
	do j=1,nsuby

	  if(xval(j).gt.-99.0.and.npx(j).gt.0) then

	    wsum=wsum+float(npx(j))
	    xav=xav+float(npx(j))*xval(j)
	  end if
	end do

*       Check that at least one point has contributed to the average
*       position, otherwise the data contains no valid points.

        if ( wsum .gt. 0.0 ) then

	   xav=xav/wsum
        else

           STATUS = SAI__ERROR
  	   CALL ERR_REP( ' ', 'RED4_SPROF: '/
     :       /'The data contains no valid points', STATUS )
           RETURN
        end if

*	now have average position in the window - print result

        CALL MSG_SETR( 'XAV', XAV )
	CALL MSG_OUT( ' ', 'Object shift relative to MIDY is ^XAV rows',
     :    STATUS )
*
	if(char.eq.'s') goto 991	! ***
c
*	If the seeing profile is needed, then
*	determine increment profile for the shifted dataset
*	zero variables again

	do i=1,nsuby

	  npx(i)=0

	  do j=1,21	! MAXINCREMENTS ?

	    px(j,i)=0.0
	  end do
	end do

*	set up slit profiles at each increment substep

	do k=1,nord

*	  cycle through dispersion direction

	  do i=ldlo,ldhi

	    xn=i-nxref
	    xterm=1.0
	    kterm=1
	    ych=0.0

*	    spatial increment

	    do while (kterm.le.nterm)

	      ych=ych+xterm*cford(kterm,k)
	      xterm=xterm*xn
	      kterm=kterm+1
	    end do
*
	    ych=ych+0.5
	    nych=ych
	    sych=ych+xav
	    nsych=sych

*	    fractional increment for data
	    fsych=sych-nsych

*	    determine sky

	    noblo=nsych-nwob/2
	    nobhi=nsych+nwob/2
	    nsklo=noblo-nex
	    nskhi=nobhi+nex
	    nsilo=nych-nclot
	    nsihi=nych+nchit

*	    if out of range, forget it

	    if(nsihi.gt.m.or.nsilo.lt.1) goto 212	! ***
c
	    sknum=0.0
	    skval=0.0
	    skvar=0.0

*	    NOTE ASSUMPTION: object may shift in slit, but slit does
*	    not shift on detector

	    if(nsklo.ge.nsilo) then

	      do j=nsilo,nsklo

	        if(dv(i,j).ge.0.0.and.dq(i,j).eq.good) then

	          skval=skval+da(i,j)
	          sknum=sknum+1.0
	          skvar=skvar+dv(i,j)
	        end if
	      end do
	    end if

*	    high channel sky

	    if(nskhi.le.nsihi) then

	      do j=nskhi,nsihi

	        if(dv(i,j).ge.0.0.and.dq(i,j).eq.good) then

	          skval=skval+da(i,j)
	          sknum=sknum+1.0
	          skvar=skvar+dv(i,j)
	        end if
	      end do
	    end if

*	    average sky value and variance

	    if(sknum.gt.0.0) then

	      skval=skval*skymultx/sknum
	      skvars=skvar*(skymultx)**2/sknum
	      skvar=skvars/sknum
	    else
*	      sky value zero and negative variance
	      skval=0.0
	      skvar=-1.0
	      skvars=-1.0
	      goto 212	! ***
	    end if

*	    determine the seeing profile relative to shifted MIDY

	    nsfych=float(nsuby)*fsych+1

*	    add in the information to px(j,nsfych)

	    los=nsych-nwob/2

*	    omit if contains a noise spike

	    sumdx=-1.0

	    do j=1,nwob

	      jj=los-1+j

	      if(dv(i,jj).lt.cthres.or.dq(i,jj).ne.good) goto 212	! ***

	      sumdx=amax1(dv(i,jj),sumdx)
	    end do

*	    omit if sky more than 6*sig+object

	    if(sumdx.lt.0.0) goto 212	! ***

	    temdx=6.0*sqrt(sumdx)+da(i,jj)

	    if(temdx.lt.skval) goto 212	! ***

	    do j=1,nwob

	      jj=los-1+j

	      px(j,nsfych)=px(j,nsfych)+da(i,jj)-skval

*	      global variance estimator

	      wtf(j,nsfych)=wtf(j,nsfych)+amax1(dv(i,jj),skvars)
     1                  +skvar
	    end do

	    npx(nsfych)=npx(nsfych)+1

212	    continue
	  end do
	end do

*	px now seeing profiles, i.e.weights
*	check for negative values

	do j=1,nsuby

	  pxsum=0.0

	  do i=1,nwob

	    if(px(i,j).lt.0.0) then

	      px(i,j)=0.0
	    end if

	    pxsum=pxsum+px(i,j)
	  end do

*	  normalize to unity

	  do i=1,nwob

	    px(i,j)=px(i,j)/pxsum

	    if(npx(j).gt.0) then

*	      normalize to single channel average

	      wtf(i,j)=wtf(i,j)/float(npx(j))
	    end if
	  end do
	end do

        CALL MSG_OUT( ' ', 'Substep distribution:-', STATUS )

        DO J = 1, NSUBY

           CALL MSG_SETI( 'J', J )
           CALL MSG_SETI( 'NPX', NPX(J) )
           CALL MSG_OUT( ' ', '^NPX data values used for substep ^J',
     :       STATUS )
        END DO

991	return
	end
