*PAMELA
*
* Pamela NDF-based monolith to bundle together pamela data
* reduction routines. Started transfer in Jan 1998. 
*
* Routines transferred: 
*
* "Tested" implies the routine has been got to work at least once
* without obvious problems, but should not be regarded as a guarantee
* of course. I have tested everything using NDF sections to check for
* problems introduced by these, but I have not tested the bad pixel
* code at all.
*
*
*  extnor -- normal extraction of spectra. Tested: 27/02/98
*  extopt -- optimal extraction of straight(ish) spectra. Tested: 27/02/98
*  faker  -- fakes up a data frame for testing.  Tested: 27/02/98
*  idtype -- works out type of data. Tested: 27/02/98
*  medprf -- flats, medians filters and collapses a frame. Tested: 10/03/98
*  noise  -- for determination of noise parameters. Tested: 27/02/98
*  optext -- tilted optimal extraction. Tested: 03/03/98
*  picstat-- statistics. Tested: 04/03/98
*  polfit -- polynomial fitting to 1D data. Tested: 05/03/98
*  pplot  -- plots slices of data frames. Tested: 27/02/98
*  profit -- tilted profile fitting. Tested: 02/03/98
*  recomp -- recompute tilted profile. Tested: 10/03/98
*  regpic -- picks sky regions. Tested: 27/02/98
*  skew   -- tilt/de-tilt frame. Tested: 09/03/98
*  skyfit -- fits polys to sky. Tested: 27/02/98
*  skymov -- move sky regions. Tested: 10/03/98
*  splfit -- spline fitting to 1D data. Tested: 04/03/98
*  track  -- fits poly to track of a spectrum.  Tested: 27/02/98
*
* 'fixhead', 'expo', and 'root' have been retired.
*
*PAMELA
      SUBROUTINE PAMELA_MON(STATUS)
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
      INTEGER STATUS
      CHARACTER*(PAR__SZNAM) ACTION
C
      IF(STATUS.NE.SAI__OK) RETURN
C
C Get action name
C
      CALL TASK_GET_NAME(ACTION, STATUS)
C
C Go through possible commands
C
      IF(ACTION.EQ.'EXTNOR') THEN
         CALL EXTNOR(STATUS)
      ELSE IF(ACTION.EQ.'EXTOPT') THEN
         CALL EXTOPT(STATUS)
      ELSE IF(ACTION.EQ.'FAKER') THEN
         CALL FAKER(STATUS)
      ELSE IF(ACTION.EQ.'IDTYPE') THEN
         CALL IDTYPE(STATUS)
      ELSE IF(ACTION.EQ.'MEDPRF') THEN
         CALL MEDPRF(STATUS)
      ELSE IF(ACTION.EQ.'NOISE') THEN
         CALL NOISE(STATUS)
      ELSE IF(ACTION.EQ.'OPTEXT') THEN
         CALL OPTEXT(STATUS)
      ELSE IF(ACTION.EQ.'PICSTAT') THEN
         CALL PICSTAT(STATUS)
      ELSE IF(ACTION.EQ.'POLFIT') THEN
         CALL POLFIT(STATUS)
      ELSE IF(ACTION.EQ.'PPLOT') THEN
         CALL PPLOT(STATUS)
      ELSE IF(ACTION.EQ.'PROFIT') THEN
         CALL PROFIT(STATUS)
      ELSE IF(ACTION.EQ.'RECOMP') THEN
         CALL RECOMP(STATUS)
      ELSE IF(ACTION.EQ.'REGPIC') THEN
         CALL REGPIC(STATUS)
      ELSE IF(ACTION.EQ.'SKEW') THEN
         CALL SKEW(STATUS)
      ELSE IF(ACTION.EQ.'SKYFIT') THEN
         CALL SKYFIT(STATUS)
      ELSE IF(ACTION.EQ.'SKYMOV') THEN
         CALL SKYMOV(STATUS)
      ELSE IF(ACTION.EQ.'SPLFIT') THEN
         CALL SPLNFIT(STATUS)
      ELSE IF(ACTION.EQ.'TRACK') THEN
         CALL TRACK(STATUS)
      ELSE
         CALL MSG_SETC('COMMAND', ACTION)
         STATUS = SAI__ERROR
         CALL ERR_OUT(' ','Command ^COMMAND not recognised',
     &        STATUS)
      END IF
      END
