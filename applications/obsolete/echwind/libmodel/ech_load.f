*+  ECH_LOAD  -  Load COMMON blocks describing spectrograph parameters

      subroutine ech_load (param, file, status)
*
*   Description :
*
*     If a parameter name has been specified, ask the user which file contains
*     the parameters. Otherwise use the one which was supplied.
*
*     This file is a text file. Any line beginning with a "!" is a comment line
*     and is ignored. Non-comment lines are split up into space, tab or comma
*     separated tokens (quotes can be used to protect delimiters within tokens
*     should this be necessary) and these tokens are processed in pairs (the
*     number of tokens on a line must be even and will probably usually be 2).
*     The first of each pair is a keyword and the second is a value associated
*     with that keyword. The keyword determines the expected type of the value.
*     All non quoted tokens are converted to upper case.
*
*     As the file is processed, there is always a current instrument, a current
*     echelle and a current camera. Initially these are the defaults of
*     "UCLES", "31" and "LONG" respectively. They are changed by
*     "INSTRUMENT instrument", "ECHELLE echelle" and "CAMERA camera" entries in
*     the file. Individual spectrograph parameters are each associated with the
*     instrument and with either the echelle or the camera and their entries
*     always apply to the current instrument, echelle and camera.
*
*     Instrument-related parameters are:
*
*       [FCOL]     collimator focal length (REAL mm)
*       [NPR]      number of prisms in cross-disperser (INTEGER)
*       [ANGLE]    prism angle (REAL degrees)
*       [COLLXE]   collimated axis to echelle distance (REAL m)
*       [PRFACE]   prism face length (glass) (REAL m)
*       [PRAPEX]   prism apex along face (REAL m)
*       [GAMAFAC]  empirical gamma factor (REAL)
*
*     Echelle-related parameters are:
*
*       [D]        number of lines per mm (REAL)
*       [M0]       central order number (INTEGER)
*       [WAVE0]    central wavelength (REAL Angstroms)
*       [THETAB]   true blaze angle (REAL degrees)
*       [BLAZE0]   ideal blaze angle (REAL degrees)
*       [THETA0]   central theta (REAL degrees)
*       [GAMMA0]   central gamma (REAL degrees)
*
*     Camera-related parameters are:
*
*       [FCAM]     camera focal length (REAL mm)
*       [THETACAM] beam separation of camera (REAL degrees)
*
*     Note that the order of instruments, echelles and cameras within the
*     file may be significant. For example, the default instrument, echelle
*     or camera may be the first one that was defined in the file (although
*     this routine does not need to make any assumptions about this).
*
*     The following example describes UCLES and illustrates a notional short
*     camera of focal length 400mm.
*     \begin{verbatim}
*       !+ UCLES.DAT
*       !
*       !  UCLES spectrograph parameters.
*       !
*       INSTRUMENT UCLES
*          FCOL 6000.0
*          NPR 3
*          ANGLE 54.1
*          COLLXE -1.800
*          PRFACE 0.313
*          PRAPEX 0.015
*          GAMAFAC 1.011
*          ECHELLE 31
*             D 31.6046
*             M0 138
*             WAVE0 4119.68
*             THETAB 64.66
*             BLAZE0 64.66
*             THETA0 0.0
*             GAMMA0 0.0
*          ECHELLE 79
*             D 79.0115
*             M0 55
*             WAVE0 4097.99
*             THETAB 63.55
*             BLAZE0 63.55
*             THETA0 0.0
*             GAMMA0 0.0
*          CAMERA LONG
*             FCAM 700.0
*             THETACAM 12.0
*          CAMERA SHORT
*             FCAM 400.0
*             THETACAM 12.0
*     \end{verbatim}
*     This routine should be called before ECH_INIT is called. If it is not
*     called, ECH_INIT will only be able to support the 31 and 79 echelles on
*     the UCLES LONG camera.
*
*   Invocation :
*
*     CALL ECH_LOAD (PARAM, FILE, STATUS)
*
*   Arguments :
*
*     PARAM    =  CHARACTER (READ)        Name of program parameter
*                                         corresponding to the file to use.
*                                         If blank, no parameter is read
*     FILE     =  CHARACTER (READ, WRITE) File to use. Default file type is
*                                         .DAT. Is read if PARAM is blank. Is
*                                         written if PARAM is non-blank
*     STATUS   =  INTEGER (READ, WRITE)   Global status value
*
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     W.F. Lupton  AAO  (AAOEPP::WFL)
*
*   History :
*
*     08 Dec 1989  : Original version (AAOEPP::WFL)
*     23 Sep 1994  : Added COLLXE, PRFACE, PRAPEX, BLAZE0, THETACAM, GAMAFAC
*                    (MPF/RGO)
*     05 Feb 1997  : Modified calls to str$upcase to str_upcase for
*                    Linux port (BLY,RAL).
*     21 Aug 2005  : Use str_len rather than lnblnk to be more portable (TJ,JACH)
*
*   Type definitions :
*
      implicit none             ! no default typing allowed
*
*   Global constants :
*
      include 'SAE_PAR'        ! ADAM error codes
*
*   Import :
*
      character*(*) param       ! parameter name or blank
*
*   Import / Export :
*
      character*(*) file     ! file to use
*
*   Status :
*
      integer status            ! global status value
*
*   Global Variables :
*
      include 'ech_params'      ! parameters of all possible spectrographs
*
*   External functions :
*
      external ech_block        ! block data initialisation
*
*   Local Constants :
*
      integer MAXTOKS           ! maximum number of tokens per line
      parameter (MAXTOKS = 20)
      real RAD                  ! degrees per radian
      parameter (RAD = 57.29577951)
*
*   Local Constants :
*
      integer i                 ! counter
      integer lun               ! logical unit number
      integer ios               ! i/o status
      integer ntoks             ! number of tokens in current line
      real rval                 ! current real parameter value
      character line*132        ! current line
      character key*20          ! current keyword
      character cval*20         ! current character parameter value
      character toks(MAXTOKS)*20 ! tokens in current line
      integer start,end
      character*80 infile,echdir
      integer str_len
      external str_len
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   If a parameter name was supplied, get the file name from the parameter
*   system. Otherwise it was passed in the argument list.
*
      if(param.ne.' ')then
	 call par_get0c(param, file, status)
      endif
      if(status.ne.sai__ok) RETURN

*
*   Open the file.
*
      if (file .eq. 'ECHWIND_SPECTROGRAPHS') then
         call getenv("ECHWIND_HOME",echdir)
         infile = echdir(:str_len(echdir))//'spectrographs.dat'
      else
         infile = file(:str_len(file))
      endif

      lun = 28
      open(unit=lun, file=infile(:str_len(infile)), status='old',
     :     iostat=ios)
      if(ios.ne.0)then
         write(*,*) 'Error opening spectrograph parameter file ',
     :              infile(:str_len(infile))
	 RETURN
      endif

*
*   Initialise numbers of instruments, echelles and cameras to zero. The
*   values of all of the COMMON arrays have been initialised by the ECH_BLOCK
*   block data to correspond to the default values for everything. Any values
*   that appear prior to an INSTRUMENT, ECHELLE or CAMERA entry will be
*   associated with the default instrument, echelle or camera.
*
      ninsts = 0
      nechs(0) = 0
      ncams(0) = 0

*
*   Loop reading lines from the file and splitting them up into tokens.
*   Regard lines whose first token begins with '!' as being comment lines.
*
      read(lun, '(a)', iostat=ios) line
      do while (ios.eq.0)
	 status = sai__ok
         ntoks = 0
         start = 1
         do while (start.le.len(line) .and. line(start:start).eq.' ')
            start = start + 1
         end do
         do while (start.le.len(line).and.line(start:).ne.' ')
            end = index(line(start:), ' ') + start - 1
            if(end.lt.start)then
               end=len(line) + 1
            endif
            if(ntoks.lt.MAXTOKS)then
               ntoks = ntoks + 1
               call str_upcase (toks(ntoks), line(start:end-1))
            endif
            start = end + 1
         enddo
	 if(ntoks.gt.0.and.toks(1)(1:1).eq.'!')then
	    ntoks=0
         endif
*
*      Process them in pairs. The first of each pair is a keyword and the
*      second is a value associated with the keyword.
*
	 do i = 1,ntoks-1,2
	    key = toks(i)
	    cval = toks(i+1)
	    read(cval, '(bn,g20.0)', iostat=ios) rval
*
*         INSTRUMENT. Increment the number of instruments, save the instrument
*         name, copy default values and initialise the numbers of echelles and
*         cameras for this instrument.
*
	    if(key.eq.'INSTRUMENT')then
	       if(ninsts.lt.MAXINSTS)then
		  ninsts=ninsts+1
		  insts(ninsts)=cval
		  fcol(ninsts) = fcol(ninsts-1)
		  npr(ninsts) = npr(ninsts-1)
		  angle(ninsts) = angle(ninsts-1)
                  collxe(ninsts) = collxe(ninsts-1)
                  prface(ninsts) = prface(ninsts-1)
                  prapex(ninsts) = prapex(ninsts-1)
                  gamafac(ninsts) = gamafac(ninsts-1)
		  nechs(ninsts)=0
		  d(0,ninsts) = d(0,ninsts-1)
		  m0(0,ninsts) = m0(0,ninsts-1)
		  wave0(0,ninsts) = wave0(0,ninsts-1)
		  thetab(0,ninsts) = thetab(0,ninsts-1)
                  blaze0(0,ninsts) = blaze0(0,ninsts-1)
		  theta0(0,ninsts) = theta0(0,ninsts-1)
		  gamma0(0,ninsts) = gamma0(0,ninsts-1)
		  ncams(ninsts)=0
		  fcam(0,ninsts) = fcam(0,ninsts-1)
                  thetacam(0,ninsts) = thetacam(0,ninsts-1)
	       else
                  write(*,*) 'No room for instrument ',cval
	       endif
*
*         ECHELLE. Increment the number of echelles, save the echelle name and
*         copy default values.
*
	    elseif(key.eq.'ECHELLE')then
	       if(nechs(ninsts).lt.MAXECHS)then
		  nechs(ninsts)=nechs(ninsts)+1
		  echs(nechs(ninsts),ninsts)=cval
		  d(nechs(ninsts),ninsts)=
     :                             d(nechs(ninsts)-1,ninsts)
		  m0(nechs(ninsts),ninsts)=
     :                             m0(nechs(ninsts)-1,ninsts)
		  wave0(nechs(ninsts),ninsts)=
     :                             wave0(nechs(ninsts)-1,ninsts)
		  thetab(nechs(ninsts),ninsts)=
     :                             thetab(nechs(ninsts)-1,ninsts)
                  blaze0(nechs(ninsts),ninsts)=
     :                             blaze0(nechs(ninsts)-1,ninsts)
		  theta0(nechs(ninsts),ninsts)=
     :                             theta0(nechs(ninsts)-1,ninsts)
		  gamma0(nechs(ninsts),ninsts)=
     :                             gamma0(nechs(ninsts)-1,ninsts)
	       else
                  write(*,*) 'No room for echelle ',cval
	       endif
*
*         CAMERA. Increment the number of cameras, save the camera name and
*         copy default values.
*
	    elseif(key.eq.'CAMERA')then
	       if(ncams(ninsts).lt.MAXCAMS)then
		  ncams(ninsts)=ncams(ninsts)+1
		  cams(ncams(ninsts),ninsts)=cval
		  fcam(ncams(ninsts),ninsts)=
     :                             fcam(ncams(ninsts)-1,ninsts)
                  thetacam(ncams(ninsts),ninsts)=
     :                             thetacam(ncams(ninsts)-1,ninsts)
	       else
                  write(*,*) 'No room for camera ',cval
	       endif
*
*         Value must have been a valid real number, so report if not.
*
	    elseif(ios.ne.0)then
               write(*,*) key,' has illegal value ',cval
*
*         Instrument-related parameters.
*
	    elseif(key.eq.'FCOL')then
	       fcol(ninsts)=rval
	    elseif(key.eq.'NPR')then
	       npr(ninsts)=nint(rval)
	    elseif(key.eq.'ANGLE')then
	       angle(ninsts)=rval/RAD
	    elseif(key.eq.'COLLXE')then
	       collxe(ninsts)=rval
	    elseif(key.eq.'PRFACE')then
	       prface(ninsts)=rval
	    elseif(key.eq.'PRAPEX')then
	       prapex(ninsts)=rval
	    elseif(key.eq.'GAMAFAC')then
	       gamafac(ninsts)=rval
*
*         Echelle-related parameters.
*
	    elseif(key.eq.'D')then
	       d(nechs(ninsts),ninsts)=rval
	    elseif(key.eq.'M0')then
	       m0(nechs(ninsts),ninsts)=nint(rval)
	    elseif(key.eq.'WAVE0')then
	       wave0(nechs(ninsts),ninsts)=rval
	    elseif(key.eq.'THETAB')then
	       thetab(nechs(ninsts),ninsts)=rval/RAD
            elseif(key.eq.'BLAZE0')then
               blaze0(nechs(ninsts),ninsts)=rval/RAD
	    elseif(key.eq.'THETA0')then
	       theta0(nechs(ninsts),ninsts)=rval/RAD
	    elseif(key.eq.'GAMMA0')then
	       gamma0(nechs(ninsts),ninsts)=rval/RAD
*
*         Camera-related parameters.
*
	    elseif(key.eq.'FCAM')then
	       fcam(ncams(ninsts),ninsts)=rval
	    elseif(key.eq.'THETACAM')then
	       thetacam(ncams(ninsts),ninsts)=rval/RAD
*
*         If get here, illegal keyword.
*
            else
               write(*,*) 'Illegal keyword ',key
	    endif
	 enddo
*
*      Warn if odd number of tokens (last one is ignored).
*
        if(mod(ntoks,2).eq.1)then
            write(*,*) 'Odd number of tokens, ',toks(ntoks),' ignored'
	 endif
*
*      Read next line from the file and loop back.
*
	 read(lun, '(a)', iostat=ios) line
      enddo

*
*   Close file and release logical unit.
*
      close(lun, iostat=ios)

      end
