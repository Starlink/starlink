*+  RED4_ENG.INC - INCLUDE file for RED4 engineering parameters
*    Authors :
*     P N Daly  (JACH::PND)
*    History :
*     19-Jan-1995: Original Unix version                        (PND)
*    endhistory

*    Global parameters :
      REAL MAX_SLIT_ANGLE
      PARAMETER ( MAX_SLIT_ANGLE = 5.0 )

*    Global variables :
      INTEGER
     :  LOG_UNIT,         ! Unit for engineering log file 
     :  NMEAS,            ! =1 if READ_EMLT used odd number of times, =2 even
     :  EMLT_OBSNUM,      ! Observation number from READ_OBS (EMLT)
     :  EMLT_GORDER       ! Grating order
      COMMON /RED4ENGI/ LOG_UNIT, NMEAS, EMLT_OBSNUM, EMLT_GORDER
      SAVE /RED4ENGI/

      LOGICAL
     :  LOG_OPEN,         ! T if the log file is open
     :  EMLT_VALID,       ! T if valid EMLT info has been obtained
     :  SLIT_ANGLE_VALID, ! T if a valid slit angle has been calculated
     :  OBSERVATION_VALID ! T if a valid observationhas been read
      COMMON /RED4ENGL/  LOG_OPEN, EMLT_VALID, SLIT_ANGLE_VALID, OBSERVATION_VALID
      SAVE /RED4ENGL/

      REAL
     :  BCEN, BCEN_P,
     :  XCEN, XCEN_P,
     :  YCEN, YCEN_P,
     :  BFWHM, BFWHM_P,
     :  XFWHM, XFWHM_P,
     :  MEANBFWHM, MEANBFWHM_P,
     :  MEANXFWHM, MEANXFWHM_P,
     :  PEAK, PEAK_P,
     :  STRENGTH, STRENGTH_P,
     :  SLIT_ANGLE, EMLT_GANGLE, EMLT_GWAVE, EMLT_DFOCUS,
     :  EMLT_SSA, EMLT_STIME, EMLT_ETIME
      COMMON /RED4ENGR/ BCEN, BCEN_P, XCEN, XCEN_P, YCEN, YCEN_P, BFWHM, BFWHM_P,
     :  XFWHM, XFWHM_P, MEANBFWHM, MEANBFWHM_P, MEANXFWHM, MEANXFWHM_P,
     :  PEAK, PEAK_P, STRENGTH, STRENGTH_P, SLIT_ANGLE, EMLT_GANGLE, EMLT_GWAVE,
     :  EMLT_DFOCUS, EMLT_SSA, EMLT_STIME, EMLT_ETIME
      SAVE /RED4ENGR/
