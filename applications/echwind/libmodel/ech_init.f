*+  ECH_INIT  -  Initialise COMMON blocks for a specified instrument combination

      subroutine ech_init (param, config, status)
*
*   Description :
*
*     If a parameter name has been specified, ask the user which configuration
*     to use. Otherwise use the one which was supplied.
*
*     The resulting configuration name is a string which consists of zero or
*     more words separated by slahes. They are converted to upper case and
*     then an attempt is made to interpret the first as the name of a supported
*     instrument. If no match is found, the first instrument defined in the
*     spectrograph parameter file read by ECH_LOAD is assumed or, if ECH_LOAD
*     was not called, the default instrument (UCLES) is used. Having determined
*     the instrument, an attempt is made to interpret the remaining words as
*     names of supported echelles and cameras respectively. If no match is
*     found, the first defined echelle and camera of the instrument are assumed.
*
*     Having determined the instrument, echelle and camera, copy the relevant
*     parameters to the ECH_COMMON COMMON block for use by the rest of the
*     spectrograph model routines.
*
*     The result is that simple input such as "31" and "79" can continue to
*     be accepted as it always has, but that it is easy to refer to other
*     instruments, echelles and cameras.
*
*     This routine must be called before any of the other spectrograph model
*     routines are called. ECH_LOAD may optionally be called before this
*     routine. If it is not, the only available instrument is UCLES with the
*     31 and 79 echelles and the LONG camera.
*
*   Invocation :
*
*     CALL ECH_INIT (PARAM, CONFIG, STATUS)
*
*   Arguments :
*
*     PARAM    =  CHARACTER (READ)        Name of program parameter
*                                         corresponding to the configuration
*                                         to use. If blank, no parameter is read
*     CONFIG  =  CHARACTER (READ, WRITE)  Configuration to use. See description
*                                         above. Is read if PARAM is blank. Is
*                                         set to echelle name if PARAM
*                                         is non-blank
*     STATUS   =  INTEGER (READ, WRITE)   Global status value
*
*   Bugs :
*
*     None known.
*
*   Authors :
*
*     C.J. Hirst  UCL  (ZUVAD::CJH)
*
*   History :
*
*     ?? ??? 1988  : Original version (ZUVAD::CJH)
*     01 Aug 1989  : Added comments (AAOEPP::WFL)
*     08 Dec 1989  : Added configuration handling (AAOEPP::WFL)
*     23 Sep 1994  : Added blaze0, thetacam, collxe, prface, prapex, gamafac.
*                    (MPF/RGO)
*     05 Feb 1997  : Modified calls to str$upcase to str_upcase for
*                    Linux port (BLY,RAL).
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
      character*(*) config      ! configuration to use
*
*   Status :
*
      integer status            ! global status value
*
*   Global Variables :
*
      include 'ech_params'      ! parameters of all possible spectrographs
      include 'ech_common'      ! current echelle parameters
*
*   External functions :
*
      external ech_block        ! block data initialisation
*
*   Local Constants :
*
      integer MAXTOKS           ! maximum number of tokens per line
      parameter (MAXTOKS = 20)
      real rad                  ! degrees per radian
      parameter (rad = 57.29577951)
*
*   Local Variables :
*
      integer ntoks             ! number of tokens in configuration
      integer start             ! start position of current token
      integer end               ! end position of current token
      integer tok               ! current token number
      integer inst              ! instrument index
      integer ech               ! echelle index
      integer cam               ! camera index
      logical found             ! whether something has been found
      character toks(MAXTOKS)*20 ! tokens in configuration
*
*-----------------------------------------------------------------------
*
*   Check status on entry - return if not OK.
*
      if (status.ne.sai__ok) RETURN
*
*   If a parameter name was supplied, get the configuration name from the
*   parameter system. Otherwise it was passed in the argument list.
*
      if(param.ne.' ')then
         call par_get0c(param, config, status)
      endif
      if(status.ne.sai__ok) RETURN

*
*   Split the configuration into tokens, using slash as the delimiter.
*
      ntoks = 0
      start = 1
      do while (start.le.len(config).and.config(start:).ne.' ')
         end = index(config(start:), '/') + start - 1
         if(end.lt.start)then
            end=len(config) + 1
         endif
         if(ntoks.lt.MAXTOKS)then
            ntoks = ntoks + 1
            call str_upcase (toks(ntoks), config(start:end-1))
         endif
         start = end + 1
      enddo
      tok = 1

*
*   Look up the first token (if present) in the list of supported instruments.
*   If not found, assume UCLES if ECH_LOAD was not called and the first defined
*   spectrograph if it was called.
*
      inst = 0
      found = .false.
      if(tok.le.ntoks)then
         do while (inst.lt.ninsts.and..not.found)
            inst = inst + 1
            found = toks(tok) .eq. insts(inst)
         enddo
      endif
      if(found)then
         tok = tok + 1
      elseif(ninsts.eq.0)then
         inst = 0
      else
         inst = min(1,ninsts)
      endif
*
*   Similarly look up the next token in the list of echelles.
*
      ech = 0
      found = .false.
      if(tok.le.ntoks)then
         do while (ech.lt.nechs(inst).and..not.found)
            ech = ech + 1
            found = toks(tok) .eq. echs(ech,inst)
         enddo
      endif
      if(found)then
         tok = tok + 1
      elseif(ninsts.eq.0)then
         ech = 0
      else
         ech = min(1,nechs(inst))
      endif
*
*   Similarly look up the next token in the list of cameras.
*
      cam = 0
      found = .false.
      if(tok.le.ntoks)then
         do while (cam.lt.ncams(inst).and..not.found)
            cam = cam + 1
            found = toks(tok) .eq. cams(cam,inst)
         enddo
      endif
      if(found)then
         tok = tok + 1
      elseif(ninsts.eq.0)then
         cam = 0
      else
         cam = min(1,ncams(inst))
      endif

*
*   Use the default for everything if there are unused tokens since this
*   probably implies a mis-type.
*
      if(tok.ne.ntoks+1)then
         if(ninsts.eq.0)then
            inst = 0
            ech = 0
            cam = 0
         else
            inst = 1
            ech = min(1,nechs(1))
            cam = min(1,ncams(1))
      endif
      write(*,*) 'Spectrograph configuration ',config,' illegal; ',
     :           'default of ',insts(inst),'/',echs(ech,inst),'/',
     :           cams(cam,inst),' assumed'
      endif

*
*   Copy values to ECH_COMMON.
*
      ech_instrument = insts(inst)
      ech_echelle = echs(ech,inst)
      ech_camera = cams(cam,inst)
      ech_fcol = fcol(inst)
      ech_npr = npr(inst)
      ech_angle = angle(inst)
      ech_collxe = collxe(inst)
      ech_prface = prface(inst)
      ech_prapex = prapex(inst)
      ech_gamafac = gamafac(inst)
      ech_d = d(ech,inst)
      ech_m0 = m0(ech,inst)
      ech_wave0 = wave0(ech,inst)
      ech_thetab = thetab(ech,inst)
      ech_blaze0 = blaze0(ech,inst)
      ech_theta0 = theta0(ech,inst)
      ech_gamma0 = gamma0(ech,inst)
      ech_fcam = fcam(cam,inst)
      ech_thetacam = thetacam(cam,inst)

*
*   If the configuration name was read using the parameter system, return the
*   echelle name.
*
      if(param.ne.' ')then
         config = ech_echelle
      endif

      end
