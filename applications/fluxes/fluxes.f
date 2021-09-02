*     This is the BLOCK DATA initialisation for the PLANET lookup
*     table common block. Since all code is in a single file I put
*     this at the top of the program rather than in a separate file.
*     This common simply provides a number to name planet lookup table.

      BLOCK DATA PLT_LOOKUP

      INTEGER LUP(10)
      CHARACTER * 7 PLANET(10)

      COMMON/INPUT/LUP,PLANET

      DATA PLANET/'SUN','MERCURY','VENUS','MARS','JUPITER','SATURN',
     :     'URANUS','NEPTUNE','PLUTO','MOON'/
      DATA LUP/ 11,1,2,4,5,6,7,8,9,10 /


      END

****************************************************************************

      SUBROUTINE FLUXES(STATUS)

*     Modifications:

*     26 Jan 2021 : DSB- Add custom filter support
*     22 Jun 12 : TIMJ- Add two component gaussian beam support
*     30 Aug 11 : TIMJ- Add simple SCUBA-2
*     31 Mar 05 : TIMJ- Fix infinite loop when time invalid
*     18 Mar 05 : TIMJ- A THUMPER tweak to the output formatting
*     2  Feb 04 : TIMJ- Fix TT vs UT confusion. RJDATE now returns TT MJD
*                       since that is the most common variant. LST calculation
*                       now (correctly) uses UT but all other SLA routines
*                       use TT. LST calculation factored out into separate
*                       function.
*                     - Use TELESCOPE common block for telescope parameters
*                       rather than duplicating the information. Now use
*                       SLA_OBS to obtain coordinates.
*     4  Dec 98 : TIMJ- Fix problems with linux port: Have to use
*                       BLOCK DATA to initialise COMMON. Fix MSG_FMTD
*                       (concerning SIGN variable) since SIGN is
*                       a CHAR*
*     29 Sep 98 : TIMJ- Allow Filter 1350 to be the same as 1300
*                       This is a kluge for SCUBA since we changed
*                       the name of the filter after commissioning!!
*     17 Sep 98 : TIMJ- Remove variables that are never used
*                       Use Message filtering (MSG_FILTER) and convert
*                       MSG_OUT to MSG_OUTIF
*                       Store semi-diameter as a parameter (SEMI_DIAM)
*                       and solid angle (SOLID_ANG)
*     04 Sep 98 : RPT - Add FILTER parameter and store results in
*                       as ADAM parameters.
*     03 Sep 98 : RPT - If a particular planet is specified just print
*                       fluxes for that planet rather than all.
*                       Use same index for a planet throughout.
*     06 Oct 97 : TIMJ- Fixed bug in calculation of LST. Sometimes came
*                       out negative (but sign was ignored). Now checks
*                       for this and adds 2 PI.
*     11 Jul 97 : TIMJ- Found bug when reading time from parameter TIME
*                       if less than 3 entries specified (ie HH MM does
*                       not work!) Change it such that HH MM assumes SS=0
*                       and HH assumes MM and SS = 0
*                       Change name of STOP variable to ASTOP (reserved
*                       keyword).
*                       Initialise LSTAT to good status.
*        May 97 : TIMJ- Revert back to GMTIME() since there is no
*                       PSX equivalent (PSX_TIME was incorrect)
*     06 Feb 97 : GJP - Linux port.
*                       Needed TIME()/GMTIME() replaced with
*                       PSX equivalents. Also, slight mods to
*                       a FORMAT statment -  "X" must be "1X"
*     10 Oct 96 : GJP - Modified output headings.
*     16 Sep 96 : GJP - Modified file reading code to ensure
*                       that an unused unit is always used.
*                       Also added the FLUDAT/JPL env variables
*                       to the shell script
*     12 Sep 96 : GJP - Added FIO usage to file writing sections.
*     11 Sep 96 : GJP - Modified UNIT=10 and CLOSE(10) statements
*                       to use the variable LUNO. Removed SPLIT2
*                       subroutine.
*     10 Sep 96 : GJP - Removed CHEBYBLK common block, CHEBY
*                       and READCHEB subroutines.
*     09 Sep 96 : GJP - Modified to use JPL and SLALIB routines for
*                       generating ephemeris and planet distances.
*     08 Jul 96 : GJP - Modified SCREEN output to use MSG system.
*     03 Jul 96 : GJP - Restructured some of the input routines to use the
*                       Starlink parameter system.
*     05 Jun 96 : GJP - Modified the TIMEBLK common block to avoid boundary.
*                       Modified the OUTPUT common block to avoid boundary.
*                       Introduced FLUXES.IFL and SAE_PAR.
*     05 Jun 96 : GJP - Modify the TIMEBLK common block to avoid boundary.
*                       First Starlink Version.
*     19 Mar 96 : TIMJ - Ported to unix (see notes after this list)
*     26 Oct 95 : HEM - modified subroutine TB350 to extend range of useful MJD's
*     26 Aug 95 : HEM - minor change to planet body question
*     30 Apr 93 : HEM - change to deal properly with month increment
*     12 Sep 92 : Use different values if date > 6 Aug 1992
*     14 Nov 90 : Reorganize program i/o logic
*     12 Nov 90 : Reorganize external file, plus small program mods.
*     10 Nov 90 : Changed to read external file containing data
*     25 Oct 90 : Moved program (plus ephemeris files) to [HEM.PROGS(.EPH)]
*     06 Jun 90 : FLUXNOW version, taking "now" as a default
*     20 Mar 89 : Modified by GDW
*     09 Mar 89 : Restructure of entire file structure and output format - GDW
*     06 Mar 89 : Adjustment of beam-sizes to those from Sandell's calibration
*                      note, and program now lists out which beam-sizes have been
*     16 Jun 88 : New table of brightness temperatures and errors.
*     03 Jun 88 : Remove wrong calibration of fluxes when planet is specified
*                      instead of ALL. assumed for "flux in DL beam". - Jim Emerson.
*     April 1985: Original - Catherine Hohenkerk - HM Nautical Almanac Office

*     Purpose:
*     This program calculates the positions of the planets and/or
*     the fluxes of five planetery calibrators for the effective
*     frequencies and beam-sizes of various receivers on the JCMT.

*     Arrays of effective frequencies and half-power beam-widths
*     for filters and brightness temperatures for planets (except Mars) are
*     read from a file (e.g. UKT14.DAT) . Brightness temperatures and errors
*     given by Matt Griffin. HPBW's due to G. Sandell.

*     Alternatively, if "FILTER=CUSTOM" is included on the command line,
*     the parameters of a single filter to use are obtained from the user
*     using a set of environment parameters, rather than being read from
*     a file. The planetary brightness temperature can either be supplied
*     explicitly or can be looked up from a specified NDF using the supplied
*     frequency as the index (the FLUXES package includes files esa2_uranus.sdf
*     and esa4_uranus.sdf that can be used for this purpose). See parameter
*     BTEMP.


*     Description of (non-ADAM) Parameters:

*     NF       - Number of filters at which fluxes are to be calculated.
*     TBNORM   - Array of brightness temperatures at nf
*                frequencies for five planets.
*     FREQ     - Array of effective frequencies of nf
*                filters for observing a planet.
*     TB       - Array of brightness temperatures at nf
*                frequencies for one planet.
*     FLUX     - Array of integrated flux densities at
*                nf frequencies for one planet.
*     FLUXBC   - Array of beam-corrected flux densities
*                at nf frequencies for one planet.
*     HPBW     - Array of half-power beam-widths at nf
*                frequencies.
*     GHZ857   - Frequency at which wright's model gives
*                martian brightness temperature.
*     GHZ90    - Frequency at which ulich's work gives
*                martian brightness temperature.
*     TB857    - Martian brightness temperature from
*                wright's model.
*     TB90     - Martian brightness temperature from
*                ulich's work.
*     RAD      - Constant for converting degrees to
*                radians and vice versa.
*     RJD      - Julian date for which fluxes of planetary
*                calibrators are required.
*     RA       - Apparent geocentric right ascension of planet
*                for date, in degrees
*     DEC      - Apparent geocentric declination of planet
*                for date, in degrees
*     GD       - True geocentric distance of planet for date, in au
*     SRA      - Apparent ra of sun for date, ( used for mars helio dist.)
*     SDEC     - Apparent dec of sun for date (           ..             )
*     SGD      - True geocnetric distance earth-sun (     ..             )
*     HD       - Martian heliocentric distance for julian date
*     I        - Index which determines filter under consideration
*     J        - Index which determines planet under consideration
*                (4=mars,5=jupiter,6=saturn,7=uranus,8=neptune).
*     FNAME    - Wavelength of filter

****************************************************************************

*     Changes undertaken to make this run under UNIX

*     1) Output files opened as STATUS='UNKNOWN'
*     2) CHEBY common block renamed CHEBYBLK (clashes with CHEBY sub)
*     3) Function RJDATE defined as RJDATE () not RJDATE
*     4) UKT14.DAT now opened on unit 21 NOT unit 5!! This screwed up STDIN
*     5) Subroutine SPLIT re-written to use internal writes NOT for$cnv_out!
*     6) LIB$DATE_TIME replaced with call to gmtime()
*     6a) the to-UT code no longer necessary
*     6b) Renamed TIME common block as TIMEBLK as clashed with time()
*     7) The CHEBY data files converted to Sun binary - used all 16 decimal places

****************************************************************************

      IMPLICIT NONE

*  Variables:
      INTEGER      NDAYS(12),DATE
      CHARACTER*3  SYSMO(12),CMON
      CHARACTER*2  SYSMON(12)

      DOUBLE PRECISION COSE,DEC,DIFF,ER(15),ERROR(15,10),
     :     FREQ(15,2),GD,GHZ90,GHZ857,HD,
     :     HPBW(15), HPBW2(15), AMP1(15), AMP2(15), OMEGA,PI,R,RA,
     :     RAD,RJD,RJDATE,S,SDEC,SGD,SRA,TB(15),
     :     TBNORM(15,10),TB90,TB857,TLONG,TLAT,THEIGHT
      REAL*8  FREQUENCY,WIDTH,BW,BW2,A1,A2
      INTEGER I,IC,ID,IH,IM,IP,IY,IR,IQ,J,M,NF,NB,FIOD,FIOD2,IS
      INTEGER START(3),ASTOP(3),LSTAT,LPATH,LEN,LUP(10)
      INTEGER II,JJ,KK,INOTE,JUNK

      CHARACTER*6  WORDS(3)
      CHARACTER*7  REQBODY,BODY, PLANET(10), FPLAN(5)
      CHARACTER*10 REQFILT,FILT,DUMMY, FNAME(15),TNAME
      CHARACTER*25 ALINE
      CHARACTER*40 TFULLNAME
      CHARACTER*256 STRING
      CHARACTER*79 NOTE(50)
      CHARACTER*256 STRING1,STRING2
      CHARACTER*128 PATH,OUTFILE

      LOGICAL FLU,OFL,POS,SCREEN,REPEAT,CURT,VALID
      LOGICAL APASS,OPENF,EXCLAIM

      INTEGER NTICKS

*  External functions:
      INTEGER CHR_LEN

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'MSG_PAR'         ! Msg constants

*  Status:
      INTEGER STATUS            ! Global status
      INTEGER IOSTATUS          ! For OPEN command

C     For the time
      INTEGER WDAY
      INTEGER TSTRCT
      INTEGER YDAY

*  Data:
      DATA SYSMO/'Jan','Feb','Mar','Apr','May','Jun',
     :     'Jul','Aug','Sep','Oct','Nov','Dec'/

      DATA SYSMON/'01','02','03','04','05','06',
     :     '07','08','09','10','11','12'/

      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/

      DATA GHZ857,GHZ90/857.0D0,90.0D0/

      DATA R,PI/0.0174532925D0,3.141592654D0/

*  Common blocks:
      COMMON /TIMEBLK/IY,M,ID,IH,IM,IS
      COMMON /TIMEBLK2/S
      COMMON /TIMEBLK3/CMON

      COMMON /OUTPUT/FIOD,SCREEN,POS,FLU,OFL
      COMMON /OUTPUT2/OUTFILE

      COMMON/INPUT/LUP,PLANET
      COMMON /TELESCOPE/TLONG,TLAT,THEIGHT,TNAME,TFULLNAME
*.

*     Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*     Initialise values.
      IQ = 0
      INOTE = 0
      NF =8
      RAD=PI/180.0D0
      FIOD=7

      LSTAT = SAI__OK

*     Populate the telescope details common block. We use a common
*     block since 1) the numbers were originally in the code twice
*     2) that seems to be the fluxes style 3) we may want to allow
*     a parameter to control the telescope position
      TNAME = 'JCMT'
      CALL SLA_OBS(-1,TNAME,TFULLNAME,TLONG,TLAT,THEIGHT)
      TLONG = -1.0D0 * TLONG

*     Tell the user we are running.
      CALL MSG_BLANK(STATUS)
      CALL MSG_OUTIF(MSG__NORM, ' ','JCMT FLUXES',STATUS)
      CALL MSG_BLANK(STATUS)

*     Zero out parameters
      CALL PAR_PUT0R('F_CENTRE', -1.0, STATUS)
      CALL PAR_PUT0R('F_WIDTH', -1.0, STATUS)
      CALL PAR_PUT0R('F_TOTAL', -1.0, STATUS)
      CALL PAR_PUT0R('F_BEAM',  -1.0, STATUS)
      CALL PAR_PUT0R('T_BRIGHT', -1.0, STATUS)
      CALL PAR_PUT0R('T_ERROR', -1.0, STATUS)
      CALL PAR_PUT0R('HPBW', -1.0, STATUS)

      CALL PAR_PUT0R('SOLID_ANG', -1.0, STATUS)
      CALL PAR_PUT0R('SEMI_DIAM', -1.0, STATUS)

*     Look for the environmental variable
*     dealing with the .DAT files and grab the name.
      CALL PSX_GETENV('FLUXES_DIR',PATH,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999
*     Find its length.
      LPATH=CHR_LEN(PATH)

*     Planetary data?
      CALL PAR_GET0L('POS',POS,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Flux data?
      CALL PAR_GET0L('FLU',FLU,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Check inputs so far.
      IF ((.NOT.FLU).AND.(.NOT.POS)) THEN
         CALL MSG_OUT(' ',
     :        'No output requested. Exiting program.',STATUS)
         GOTO 9999
      END IF

*     Display on terminal?
 401  CALL MSG_BLANK(STATUS)
      CALL PAR_GET0L('SCREEN',SCREEN,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Save to a file?
      CALL PAR_GET0L('OFL',OFL,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Get file name.
      IF (OFL) THEN

*     Open the output file.
         OPENF=.FALSE.
         CALL ERR_MARK
         CALL FLU_AIF_ASFIO('OUTFILE','APPEND','LIST',80,
     :        FIOD,OPENF,EXCLAIM,STATUS)
         IF(STATUS.NE.SAI__OK) THEN
            CALL ERR_REP(' ',
     :           'Problems opening an output file',STATUS)
            CALL ERR_RLSE
            GOTO 9999
         END IF
         CALL FIO_FNAME(FIOD,OUTFILE,STATUS)

*     Tell the user that the file name is duff.
         IF ((.NOT.OPENF).AND.(.NOT.EXCLAIM)) THEN
            CALL ERR_REP(' ','Bad file name.',STATUS)
            CALL ERR_REP(' ','To quit, type !',STATUS)
            CALL ERR_ANNUL(STATUS)
         END IF

*     Cancel the error context and abort if the STATUS is bad.
         CALL ERR_RLSE
         IF (STATUS.NE.SAI__OK) GOTO 9999
         IF (EXCLAIM) GOTO 9999

*     Tell the user.
         STRING='Data will be written to file: '//OUTFILE
         I=CHR_LEN(STRING)
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ',STRING(1:I),STATUS)

      ENDIF

*     Check inputs so far. - this check removed now that it is possible
*     to store the results to parameters as well
*     IF((.NOT.SCREEN).AND.(.NOT.OFL)) THEN
*     CALL MSG_OUT(' ',
*     :        'No output direction specified. Try again.',STATUS)
*     GOTO 401
*     END IF
*     CALL MSG_BLANK(STATUS)

*     Read in date for which fluxes of planetary calibrators are
*     required -- see ephemeris B8 et seq.
      IP=0
      IC=0
      REPEAT = .FALSE.

*     Loop for each pass requested.

 1000 IC=IC+1
      IF (IC.NE.1) THEN
         CALL PAR_CANCL('PREVUT',STATUS)
         CALL PAR_GET0L('PREVUT',REPEAT,STATUS)
         IF (.NOT.REPEAT) THEN
            CALL PAR_CANCL('NOW',STATUS)
            CALL PAR_GET0L('NOW',CURT,STATUS)
         END IF
      ELSE
         CALL PAR_GET0L('NOW',CURT,STATUS)
      END IF
      IF (STATUS.NE.SAI__OK) GOTO 9999
      IF (SCREEN) CALL MSG_BLANK(STATUS)

*     Avoid next section if want to use old date.
      IF (.NOT.REPEAT) THEN

*     Find out the time/date required.
         IF ( (.NOT.CURT) ) THEN

*     Date section

*     Bypass input system if current value is to be used.

            IF (.NOT. CURT) THEN

*     Get input string.
 2             IF (IC .NE. 1) CALL PAR_CANCL('DATE',STATUS)
               CALL PAR_GET0C('DATE',ALINE,STATUS)
               IF(STATUS.NE.SAI__OK) GOTO 9999

*     Remove leading blanks and clean.
               CALL CHR_LDBLK(ALINE)
               CALL CHR_CLEAN(ALINE)

*     Break up string to get M, ID, Y.
               CALL CHR_DCWRD(ALINE,3,I,START,ASTOP,WORDS,LSTAT)
               IF(STATUS.NE.SAI__OK) GOTO 9999

*     Set values for ID, M, IY.
               IF ((I.EQ.3).AND.(LSTAT.EQ.SAI__OK)) THEN
                  CALL CHR_CTOI(WORDS(1),ID,STATUS)
                  CALL CHR_CTOI(WORDS(2), M,STATUS)
                  CALL CHR_CTOI(WORDS(3),IY,STATUS)
               END IF

*     Testing the date is sensible
               IF (I.NE.3.OR.LSTAT.NE.SAI__OK
     :              .OR.ID.LT.1.OR.ID.GT.31.OR.M.LT.1.OR.M.GT.12
     :              .OR.IY.LT.0.OR.IY.GT.99) THEN

                  IF (IY.LT.0 .OR. IY.GT.99) THEN
                     CALL MSG_OUT(' ',
     :                    'YEAR OUT OF RANGE 1950-2050!',STATUS)
                  ELSE
                     CALL MSG_OUT(' ','ERROR INTERPRETING DATE',
     :                    STATUS)
                  END IF
                  IC = IC + 1
                  GOTO 2

               END IF

*     Take next century into account.
               IF(IY.LE.49) THEN
                  IY=IY+2000
               ELSE
                  IY=IY+1900
               END IF
               CMON=SYSMO(M)

            END IF

*     Time section

*     Bypass input system if current value is to be used.

            IF (.NOT. CURT) THEN

*     Get input string.
 3             IF (IC.NE.1) CALL PAR_CANCL('TIME',STATUS)
               CALL PAR_GET0C('TIME',ALINE,STATUS)
               IF(STATUS.NE.SAI__OK) GOTO 9999

*     Remove leading blanks and clean.
               CALL CHR_LDBLK(ALINE)
               CALL CHR_CLEAN(ALINE)

*     Break up string to get IH, IM, S.
               CALL CHR_DCWRD(ALINE,3,I,START,ASTOP,WORDS,LSTAT)
               IF (STATUS.NE.SAI__OK) GOTO 9999

*     Set values for IH, IM, S.
*     Assume S is 0 if only 2 entries supplied
*     Similarly for IM if only one is supplied
               IF (LSTAT .EQ. SAI__OK) THEN

                  IF (I.EQ.3) THEN
                     CALL CHR_CTOD(WORDS(3), S,STATUS)
                     IS = INT( S + 0.5D0 )
                  ELSE
                     S = 0.0D0
                     IS = 0
                  END IF

                  IF (I .GE. 2) THEN
                     CALL CHR_CTOI(WORDS(2),IM,STATUS)
                  ELSE
                     IM = 0
                  END IF

*     There has to be at least one entry
                  IF (I .GE. 1) THEN
                     CALL CHR_CTOI(WORDS(1),IH,STATUS)
                  ELSE
                     IH = 0
                  END IF

               END IF

*     Test the time value.
               IF (IH.LT.0.OR.IH.GT.23.OR.IM.LT.0.OR.IM.GT.59
     :              .OR. S.LT.0.0D0 .OR. S.GE.60.D0) THEN
                  CALL MSG_OUT(' ',
     :                 'ERROR INTERPRETING TIME',STATUS)
                  IC = IC + 1
                  GOTO 3
               END IF

            END IF

         ELSE

*     Get UT time/date from computer. This section
*     modified during the Linux port removing
*     TIME() and GMTIME() calls. [but there is no PSX GMTIME
*     so have to leave GMTIME in.

C     The system time in seconds
            CALL PSX_TIME(NTICKS,STATUS)


*     We now need the GM time.
            CALL PSX_GMTIME(NTICKS, IS, IM, IH, ID, M, IY,
     :           WDAY, YDAY, TSTRCT, STATUS)

            IY = IY + 1900
            M  =  M + 1
            S  = DBLE(IS)
            CMON=SYSMO(M)

         ENDIF

      END IF

      RJD = RJDATE()

*     Ask for PLANET
*
 210  CALL PAR_GET0C('PLANET',REQBODY,STATUS)
      IF (STATUS.NE.SAI__OK) GOTO 9999

*     Remove leading blanks and clean.
      CALL CHR_LDBLK(REQBODY)
      CALL CHR_CLEAN(REQBODY)
      CALL CHR_UCASE(REQBODY)
      LEN=CHR_LEN(REQBODY)

      VALID = .FALSE.
      IF(REQBODY.EQ.'ALL' .OR. REQBODY.EQ.'all'
     :     .OR. REQBODY.EQ. '   ') THEN
         VALID = .TRUE.
         REQBODY = 'ALL'
      ELSE
         DO I = 1,10
            IF(PLANET(I)(1:LEN).EQ.REQBODY(1:LEN)) THEN
               VALID = .TRUE.
               IQ=I
            ENDIF
         ENDDO
         IF(.NOT. VALID) THEN
 220        CALL MSG_OUT(' ','Invalid body name !',STATUS)
            CALL PAR_CANCL('PLANET',STATUS)
            GOTO 210
         ENDIF
      ENDIF

*     Set up frequencies at which Wright's model and Ulich's work give
*     Martian brightness temperature.

*     Read filter file.
      IF (FLU) THEN
         DATE=10000*(IY-1900)+100*M+ID

*     Get an unused unit number.
         CALL FIO_GUNIT(FIOD2,STATUS)

*     Open the file. This is fatal if they can not be opened.
*     Should really be using FIO but this is historical.
*     SCUBA-2 starts in 2007
         IF (DATE.GT.1070101) THEN
            OPEN(UNIT=FIOD2,FILE=PATH(1:LPATH)//'/scuba2.dat',
     1           STATUS='OLD',IOSTAT=IOSTATUS)
            IF (IOSTATUS .NE. 0) THEN
               IF (STATUS .EQ. SAI__OK) STATUS = SAI__ERROR
               CALL MSG_SETC('PATH',PATH(1:LPATH))
               CALL ERR_REP(' ','Error opening ^PATH/scuba2.dat',STATUS)
               GOTO 9999
            END IF
         ELSE IF (DATE.GT.960523) THEN
            OPEN(UNIT=FIOD2,FILE=PATH(1:LPATH)//'/scuba.dat',
     1           STATUS='OLD',IOSTAT=IOSTATUS)
            IF (IOSTATUS .NE. 0) THEN
               IF (STATUS .EQ. SAI__OK) STATUS = SAI__ERROR
               CALL MSG_SETC('PATH',PATH(1:LPATH))
               CALL ERR_REP(' ','Error opening ^PATH/scuba.dat',STATUS)
               GOTO 9999
            END IF
         ELSE IF (DATE.GE.920807.AND.DATE.LE.960523) THEN
            OPEN(UNIT=FIOD2,FILE=PATH(1:LPATH)//'/ukt14.dat',
     1           STATUS='OLD',IOSTAT=IOSTATUS)
            IF (IOSTATUS .NE. 0) THEN
               IF (STATUS .EQ. SAI__OK) STATUS = SAI__ERROR
               CALL MSG_SETC('PATH',PATH(1:LPATH))
               CALL ERR_REP(' ','Error opening ^PATH/ukt14.dat',STATUS)
               GOTO 9999
            END IF
         ELSE
            OPEN(UNIT=FIOD2,FILE=PATH(1:LPATH)//'/ukt14_old.dat',
     1           STATUS='OLD',IOSTAT=IOSTATUS)
            IF (IOSTATUS .NE. 0) THEN
               IF (STATUS .EQ. SAI__OK) STATUS = SAI__ERROR
               CALL MSG_SETC('PATH',PATH(1:LPATH))
               CALL ERR_REP(' ','Error opening ^PATH/ukt14_old.dat',
     :              STATUS)
               GOTO 9999
            END IF
         ENDIF
         READ(FIOD2,122) NF
 122     FORMAT(29X,I2)
         READ(FIOD2,1122) NB
 1122    FORMAT(35X,I2)
         READ(FIOD2,322) DUMMY
         READ(FIOD2,322) DUMMY
 322     FORMAT(A10)
         DO II=1,15
            IF (NB .EQ. 1) THEN
               READ(FIOD2,123)KK,FILT,FREQUENCY,WIDTH,BW
 123           FORMAT(I3,2X,A10,2X,2(F7.1),F6.1)
               BW2 = 0.0D0
               A1 = 1.0D0
               A2 = 0.0D0
            ELSE
               READ(FIOD2,1123)KK,FILT,FREQUENCY,WIDTH,BW,BW2,A1,A2
 1123          FORMAT(I3,2X,A10,2X,2(F7.1),F6.1,F7.1,2(F6.2))
            END IF
            FNAME(II)=FILT
            FREQ(II,1)=FREQUENCY
            FREQ(II,2)=WIDTH
            HPBW(II)=BW
            HPBW2(II)=BW2
            AMP1(II)=A1
            AMP2(II)=A2
         ENDDO
         DO JJ=1,4
            READ(FIOD2,322)DUMMY
         ENDDO
         READ(FIOD2,124) (FPLAN(II),II=1,5)
 124     FORMAT(15X,5(A7,6X))
         DO JJ=1,15
            READ(FIOD2,125)FILT,(TBNORM(JJ,II),ERROR(JJ,II),II=4,8)
 125        FORMAT(1X,A10,5(2(F6.1),1X))
         ENDDO
         DO I=1,50
            READ(FIOD2,'(A79)',END=127)NOTE(I)
            INOTE=I
         ENDDO
 127     CONTINUE

*     Close the unit and return the number for later use.
         CLOSE(FIOD2)
         CALL FIO_PUNIT(FIOD2,STATUS)

*     Now make sure that the planets read from the file are indeed
*     Mars through Neptune.
         DO II=LUP(4),LUP(8)
            BODY = FPLAN(II-3)
            CALL CHR_UCASE(BODY)
            IF (PLANET(II)(1:4) .NE. BODY(1:4)) THEN
               STATUS = SAI__ERROR
               CALL MSG_OUT(' ',
     :              'FATAL: PLANET sequence not MARS-NEPTUNE'//
     :              ' in flux file.', STATUS)
               GOTO 9999
            END IF
         ENDDO


*     Ask for FILTER
*
 260     CALL PAR_GET0C('FILTER',REQFILT,STATUS)
         IF (STATUS.NE.SAI__OK) GOTO 9999

*     Remove leading blanks and clean.
         CALL CHR_LDBLK(REQFILT)
         CALL CHR_CLEAN(REQFILT)
         CALL CHR_UCASE(REQFILT)
         LEN=CHR_LEN(REQFILT)

         VALID = .FALSE.
         IF(REQFILT.EQ.'CUSTOM' ) THEN
            CALL CUSTOMFILT( REQBODY, FNAME, NF, NB, FREQ, HPBW, HPBW2,
     :                       AMP1, AMP2, TBNORM(1,IQ), ERROR(1,IQ),
     :                       INOTE, NOTE, VALID, STATUS )
            FREQ(1,2) = 0.0D0

         ELSE IF(REQFILT.EQ.'ALL' .OR. REQFILT.EQ. '   ') THEN
            REQFILT = 'ALL'
            VALID = .TRUE.

         ELSE
            DO II = 1,NF
               FILT = FNAME(II)
               CALL CHR_UCASE(FILT)
               IF(FILT(1:LEN).EQ.REQFILT(1:LEN)) THEN
                  REQFILT = FILT
                  VALID = .TRUE.
               ENDIF
            ENDDO
            IF(.NOT. VALID) THEN

*     Could not find a matching filter name.
*     Check to see whether the person actually asked for 1350
*     since this is the new name of the SCUBA 1300 filter
*     Only gets to here if the filter definition file does not
*     have a 1350 entry

               IF (REQFILT(1:LEN) .EQ. '1350') THEN
                  REQFILT = '1300'
                  VALID   = .TRUE.
               ELSE
                  CALL MSG_SETC('FILT', REQFILT)
                  CALL MSG_OUT(' ','Invalid filter name !: ^FILT',
     :                 STATUS)
                  CALL PAR_CANCL('FILTER',STATUS)
                  GOTO 260
               END IF
            ENDIF
         ENDIF

      ENDIF


*     For fluxes calculation, obtain Martian brightness temperature.
      IF (FLU) CALL TB350(RJD,TB857,STATUS)

*     Print out the datetime and topocentric planetary positions.

      CALL TOPEPH(REQBODY,RA,DEC,GD,RJD,STATUS)

*     Do not do if fluxes not needed.
      IF (FLU) THEN

         IF (POS) IP=1
         POS = .FALSE.

*     For planet (or planets in turn) work out solid angle,
*     integrated and beam-corrected flux densities.

*     --------------------------------------------------------------------
*     NOTE: FLUX DATA IS ONLY AVAILABLE FOR MARS (4) THROUGH  NEPTUNE (8).
*     Make sure loop does not execute for other planets!
*     --------------------------------------------------------------------

         IR = -1
         IF(REQBODY.EQ.'ALL') THEN
            IQ = LUP(4)
            IR = LUP(8)
         ELSE IF (IQ .GE. LUP(4) .AND. IQ .LE. LUP(8)) THEN
            IR = IQ
         ENDIF

         DO J=IQ,IR

*     Display heading on screen or file.

*     Planet name
            STRING2=PLANET(J)

*     Screen/file.
            IF (SCREEN) THEN
               CALL MSG_BLANK(STATUS)
               CALL MSG_BLANK(STATUS)
               CALL MSG_OUT(' ',STRING2(1:80),STATUS)
            END IF
            IF(OFL) THEN
               CALL FIO_WRITE(FIOD,' ',STATUS)
               CALL FIO_WRITE(FIOD,' ',STATUS)
               CALL FIO_WRITE(FIOD,STRING2(1:80),STATUS)
            END IF

*     Obtain Apparent Geocentric Right Ascension Declination and
*     True Distance of the planet

            CALL GEOEPH(J,RA,DEC,GD,RJD,STATUS)
            CALL SOLIDANGLE(J,RJD,RA,DEC,GD,OMEGA,STATUS)

            IF(PLANET(J)(1:4).EQ.'MARS') THEN

*     Use geocentric position of Mars and the Sun to calculate the
*     heliocentric distance of Mars

               CALL GEOEPH(1,SRA,SDEC,SGD,RJD,STATUS)

*     Finds the heliocentric distance of a body from its
*     geocentric postion and the geocentric position of the Sun.

               COSE = DSIN(DEC*R)*DSIN(SDEC*R) + DCOS(DEC*R)
     :              *DCOS(SDEC*R)*DCOS((SRA-RA)*R)
               HD = DSQRT(GD*GD + SGD*SGD - 2.0D0*GD*SGD*COSE)

*     Calculate the 3.33 millimetre brightness temperature of Mars
*     for day in question (following Ulich's work)

               TB90 = 206.8D0*DSQRT(1.524D0/HD)
               DIFF = (TB857-TB90)/(DLOG(GHZ857)-DLOG(GHZ90))

*     Interpolate to get Martian brightness temperatures at effective
*     frequencies of NF filters


*     Do a logarithmic interpolation between two points.

               DO I=1,NF
                  TBNORM(I,4) = TB90 + DIFF*(DLOG(FREQ(I,1))
     :                 -DLOG(GHZ90))
               ENDDO

            ENDIF

            IF (PLANET(J)(1:4).EQ.'MARS') THEN
               DO I = 1,NF
                  ERROR(I,4) = 0.0D0
               ENDDO
            ENDIF
            DO I = 1,NF
               TB(I) = TBNORM(I,LUP(J))
               ER(I) = ERROR(I,LUP(J))
            ENDDO
            CALL PBFLUX(OMEGA,FREQ,TB,RAD,HPBW,HPBW2,AMP1,AMP2,
     :           NF,FNAME,ER,REQFILT,PLANET(J),STATUS)

         ENDDO

      END IF

*     Loop again?
      IF (IP.EQ.1) POS=.TRUE.
      IF (SCREEN) CALL MSG_BLANK(STATUS)

*     Cancel loop again parameters if not first time through..
*     Needed for the FLUXNOW version.
      IF(IC.NE.1) CALL PAR_CANCL('APASS',STATUS)
      CALL PAR_GET0L('APASS',APASS,STATUS)

*     Cancel planet selection.
      CALL PAR_CANCL('PLANET',STATUS)
      IF (SCREEN) CALL MSG_BLANK(STATUS)

*     Cancel filter selection.
      CALL PAR_CANCL('FILTER',STATUS)
      IF (SCREEN) CALL MSG_BLANK(STATUS)

      IF (APASS) GOTO 1000

*     Write the user notes at the end of the file.
      IF (OFL) THEN
         DO I=1,INOTE
            CALL MSG_FMTC('P1','A79',NOTE(I))
            CALL MSG_LOAD(' ','^P1',STRING2,JUNK,STATUS)
            CALL FIO_WRITE(FIOD,STRING2(1:80),STATUS)
         ENDDO
      ENDIF


*     Close down the output device.
 999  CONTINUE

*     Message to tell the user what file to look in.
      IF (OFL) THEN
         CALL FIO_CLOSE(FIOD,STATUS)
         IF (SCREEN) THEN
            CALL MSG_BLANK(STATUS)
            STRING1='File output is in this directory.'//
     :           ' See file: '//OUTFILE
            I=CHR_LEN(STRING1)
            CALL MSG_OUT(' ',STRING1(1:I),STATUS)
            CALL MSG_BLANK(STATUS)
         ENDIF
      END IF

 9999 CONTINUE

*     Reset the current directory
      END


****************************************************************************

      SUBROUTINE PBFLUX(OMEGA,FREQ,TB,RAD,HPBW,HPBW2,AMP1,AMP2,
     :     NF,FNAME,ER,FILTER,BODY,STATUS)

*  Purpose:
*     Calculate integrated flux densities of planet at earth
*     and beam-corrected flux densities of planet at earth
*     for NF frequencies.

****************************************************************************

      IMPLICIT NONE

*  Variables:
      DOUBLE PRECISION DENOM, ER(15), FLUX(15), FLUXBC(15), FREQ(15,2),
     :     HPBW(15), HPBW2(15), AMP1(15), AMP2(15), OMEGA,
     :     RAD, RATIO, TB(15)

      DOUBLE PRECISION PDIAM    ! Planet diameter in arcsec
      DOUBLE PRECISION LN2      ! LN(2)

      INTEGER I,NF,NP,FIOD,K,LUP(10),JUNK

      CHARACTER FNAME(15)*10,PLANET(10)*7,BODY*7,FILTER*10,FILT*4
      CHARACTER*80 HEAD1,HEAD2,HEAD3,STRING1,STRING2
      CHARACTER*128 OUTFILE

      LOGICAL FLU,OFL,POS,SCREEN

*  Common blocks:
      COMMON /OUTPUT/FIOD,SCREEN,POS,FLU,OFL
      COMMON /OUTPUT2/OUTFILE

      COMMON/INPUT/LUP,PLANET

*  Parameters
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.141592654D0 )
      DOUBLE PRECISION DR2AS
      PARAMETER ( DR2AS = (3600.0 * 180.0 / PI ) )

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status
*.

*     Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

      NP=NF

*     The multi-beam formula is written in terms of arcsec values
*     so convert OMEGA back to a diameter in ARCSEC.
      PDIAM = 2.0D0 * DR2AS * SQRT( OMEGA / PI )
      LN2 = LOG(2.0D0)

      DO I=1,NP
         DENOM = DEXP(0.04799D0*FREQ(I,1)/TB(I))-1.0D0
         FLUX(I) = 1.475D3*OMEGA*(FREQ(I,1)**3)/DENOM

         RATIO = (1/LN2) * (
     :        AMP1(I) * (HPBW(I)/PDIAM)**2 *
     :        (1-EXP(-LN2*(PDIAM/HPBW(I))**2)) +
     :        AMP2(I) * (HPBW2(I)/PDIAM)**2 *
     :        (1-EXP(-LN2*(PDIAM/HPBW2(I))**2))
     :        )

         FLUXBC(I) = RATIO*FLUX(I)

      ENDDO

*     Output integrated and beam-corrected flux densities for six
*     frequencies

      HEAD1='Filter    Centre   Filter   Total    Flux in  '//
     :     '  Brightness         HPBW'
      HEAD2='Wavel.     Freq     Width    Flux     beam '//
     :     '     Temperature       assumed'
      HEAD3='micron     (GHz)    (GHz)    (Jy)      (Jy)'//
     :     '         (K)          (arcsecs)'

*     Screen.
      IF (SCREEN) THEN
         CALL MSG_OUT(' ',HEAD1,STATUS)
         CALL MSG_OUT(' ',HEAD2,STATUS)
         CALL MSG_OUT(' ',HEAD3,STATUS)
      END IF

*     File.
      IF (OFL) THEN
         CALL FIO_WRITE(FIOD,HEAD1,STATUS)
         CALL FIO_WRITE(FIOD,HEAD2,STATUS)
         CALL FIO_WRITE(FIOD,HEAD3,STATUS)
      END IF

      CALL PAR_PUT0R('F_CENTRE', -1.0, STATUS)
      CALL PAR_PUT0R('F_WIDTH', -1.0, STATUS)
      CALL PAR_PUT0R('F_TOTAL', -1.0, STATUS)
      CALL PAR_PUT0R('F_BEAM',  -1.0, STATUS)
      CALL PAR_PUT0R('T_BRIGHT', -1.0, STATUS)
      CALL PAR_PUT0R('T_ERROR', -1.0, STATUS)
      CALL PAR_PUT0R('HPBW', -1.0, STATUS)

*     Suppress the 600 micron (np=6) output for Jupiter (j=5) and Saturn (j=6)
      DO I = 1,NP

         FILT=FNAME(I)(1:4)
         CALL CHR_UCASE(FILT)
         IF(FILTER .EQ. 'ALL' .OR. FILT(1:4).EQ.FILTER(1:4)) THEN

            IF(((BODY(1:3).EQ.'JUP').OR.(BODY(1:3).EQ.'SAT'))
     1           .AND.(FILT(1:3).EQ.'600')) THEN

               STRING2 = '  600     no flux'

            ELSE

*     Create output string.
               IF( FNAME(I) .EQ. 'CUSTOM' ) THEN
                  K = -1
               ELSE
                  CALL CHR_CTOI(FNAME(I),K,STATUS)
               END IF

               IF (FILTER .NE. 'ALL') THEN
                  CALL PAR_PUT0D('F_CENTRE', FREQ(I,1), STATUS)
                  CALL PAR_PUT0D('F_WIDTH', FREQ(I,2), STATUS)
                  CALL PAR_PUT0D('F_TOTAL', FLUX(I), STATUS)
                  CALL PAR_PUT0D('F_BEAM', FLUXBC(I), STATUS)
                  CALL PAR_PUT0D('T_BRIGHT', TB(I), STATUS)
                  CALL PAR_PUT0D('T_ERROR', ER(I), STATUS)
                  CALL PAR_PUT0D('HPBW', HPBW(I), STATUS)
               ENDIF

               IF( K .EQ. -1 ) THEN
                  CALL MSG_SETC('P1','custom')
               ELSE
                  CALL MSG_FMTI('P1','I4',K)
               END IF
               CALL MSG_FMTD('P2','F6.1',FREQ(I,1))
               CALL MSG_FMTD('P3','F5.1',FREQ(I,2))
               CALL MSG_FMTD('P4','F9.2',FLUX(I))
               CALL MSG_FMTD('P5','F9.2',FLUXBC(I))
               CALL MSG_FMTD('P6','F5.1',TB(I))
               CALL MSG_FMTD('P7','F5.1',ER(I))
               CALL MSG_FMTD('P8','F10.1',HPBW(I))
               STRING1=' ^P1     ^P2   ^P3 ^P4 ^P5    '//
     :              '^P6 +-^P7 ^P8'
               CALL MSG_LOAD(' ',STRING1,STRING2,JUNK,STATUS)

            ENDIF

*     Write the output.
            IF (SCREEN) CALL MSG_OUT(' ',STRING2,STATUS)
            IF (OFL)    CALL FIO_WRITE(FIOD,STRING2,STATUS)

         ENDIF

      ENDDO

 9999 CONTINUE

      END



****************************************************************************

      SUBROUTINE SOLIDANGLE(J,RJD,RA,DEC,GD,OMEGA,STATUS)

*  Purpose:
*     Calculate solid angle subtended by planet at Earth.

*     Variable Type  Function
*     J        I    Planet code:  Mars J=4  Jupiter J=5  Saturn J=6
*     Uranus J=7  Neptune J=8
*     RJD      D    Julian date
*     RA       D    Apparent Geocentric Right Ascension of the planet in degrees
*     DEC      D    Apparent Geocentric Declination of the planet in degrees
*     GD       D    True Geocentric Distance in au
*     OMEGA    D

****************************************************************************

      IMPLICIT NONE

*  Variables:
      DOUBLE PRECISION APPDEC,APPRA,BC,DE,DE1,DE2,DEC,DECNPJ
      DOUBLE PRECISION DECP,EPSLN(10),GD,GM,OMEGA,P,P1,PI,R
      DOUBLE PRECISION RA,RANPJ,RAP,REQ(10),RJD,SD,SDRAD
      INTEGER J,FIOD,JUNK
      CHARACTER*80 STRING1,STRING2
      CHARACTER*128 OUTFILE
      LOGICAL FLU, OFL, POS, SCREEN

*  Common blocks:
      COMMON /OUTPUT/FIOD,SCREEN,POS,FLU,OFL
      COMMON /OUTPUT2/OUTFILE

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status

*  Data: (Only array items for Mars thru Neptune)
      DATA REQ(4),EPSLN(4)/ 3397.D0 ,0.005D0/
      DATA REQ(5),EPSLN(5)/71495.D0 ,0.065D0/
      DATA REQ(6),EPSLN(6)/60233.D0 ,0.096D0/
      DATA REQ(7),EPSLN(7)/25563.D0 ,0.024D0/
      DATA REQ(8),EPSLN(8)/24760.D0 ,0.021D0/

      DATA R,PI/0.0174532925D0,3.141592654D0/
*.

*     Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*     Only implemented at present for Mars through Neptune
      IF (J .LT. 4 .OR. J .GT. 8) RETURN

*     Set up arrays of equatorial radii and flattening
*     See:Astronomical Almanac (1985, E88) and Hildebrand's work
*     Convert apparent RA and DEC to radians

      APPRA = RA*R
      APPDEC = DEC*R

*     Calculate the RA and Dec of the Pole of the planet and convert to radians

      CALL POLEPLAN(J,RJD+2400000.5D0,RAP,DECP)
      RANPJ  = RAP*R
      DECNPJ = DECP*R

*     Calculate planetocentric declination of Earth

      DE1 = -DSIN(DECNPJ)*DSIN(APPDEC)
      DE2 = -DCOS(DECNPJ)*DCOS(APPDEC)*DCOS(RANPJ-APPRA)
      DE =   DASIN(DE1+DE2)

*     Calculate which pole is Earth-facing and polar inclination angle
*     of planet. Output Earth-facing pole and convert polar inclination
*     angle to degrees and output

      IF (DE.LE.0.0D0) THEN

*     Calc inclination.
         P = DE + 1.5708D0
         P1= P/R

*     Create output string.
         CALL MSG_FMTD('P1','F6.2',P1)
         STRING1='North pole is Earth-facing; '//
     :        'Inclination Angle = ^P1 degrees'
         CALL MSG_LOAD(' ',STRING1,STRING2,JUNK,STATUS)

*     Output to file and screen.
         IF (SCREEN) CALL MSG_OUT(' ',STRING2,STATUS)
         IF (OFL)    CALL FIO_WRITE(FIOD,STRING2,STATUS)

      ELSE

*     Calc inclination.
         P = 1.5708D0 - DE
         P1= P/R

*     Create output string.
         CALL MSG_FMTD('P1','F6.2',P1)
         STRING1='South pole is Earth-facing; '//
     :        'Inclination Angle = ^P1 degrees'
         CALL MSG_LOAD(' ',STRING1,STRING2,JUNK,STATUS)

*     Output to file and screen.
         IF (SCREEN) CALL MSG_OUT(' ',STRING2,STATUS)
         IF (OFL)    CALL FIO_WRITE(FIOD,STRING2,STATUS)

      ENDIF


*     Viewing-modifies semi-major axis of planet

      BC = (1.0D0 - EPSLN(J))*REQ(J) / (1.0D0 - EPSLN(J)*DCOS(P))

*     Geometrical mean radius of planet

      GM = DSQRT(REQ(J)*BC)

*     Semi-diameter of planet

      SDRAD = GM/(GD*1.49598D8)
      SD = SDRAD*3600.0D0/R

*     Solid Angle subtended at the planet by the Earth

      OMEGA = PI*(SDRAD**2)

*     Create output string.
      CALL MSG_FMTD('P1','F5.2',SD)
      CALL MSG_FMTD('P2','1PE9.2',OMEGA)
      STRING1='Semi-diameter = ^P1 arcsecs    '//
     :     'Solid angle = ^P2 sterads'
      CALL MSG_LOAD(' ',STRING1,STRING2,JUNK,STATUS)

*     Store parameter values
      CALL PAR_PUT0D('SOLID_ANG', OMEGA, STATUS)
      CALL PAR_PUT0D('SEMI_DIAM', SD, STATUS)



*     Output to file and screen.
      IF (SCREEN) CALL MSG_OUT(' ',STRING2,STATUS)
      IF (OFL)    CALL FIO_WRITE(FIOD,STRING2,STATUS)

      END


****************************************************************************

      SUBROUTINE POLEPLAN(IB,AJD,RAP,DECP)

*  Purpose:
*     Right Ascension and Declination of the Pole of the Planets
*     for Mean Equinox and Equator of date.

*     See: Report of the IAU working group on cartographic coordinates
*     and rotational elements of the planets and satellites (1982)

*     Variable Type  Function
*     IB       I    Planet code: Sun IB=0, Mercury IB=1, Venus   IB=2,
*     Earth IB=3, Mars    IB=4, Jupiter IB=5,
*     Saturn IB=6, Uranus  IB=7, Neptune IB=8,
*     Pluto IB=9
*     AJD      D    Julian date
*     RAP      D    Right ascension of the pole of the planet for date (degrees)
*     DECP     D    Declination of the pole of the planet for date (degrees)

****************************************************************************

      IMPLICIT NONE

*     Variables:
      DOUBLE PRECISION AJD, AZ, C, CA, CD, CT, DEC, DEC0(2,0:9), DECP,
     :     DEG, EPOCH, PC(3,6), R, RA, RA0(2,0:9), RAP, S, SA, SD,
     :     SD1, ST, T, T1, TH, TYEAR, XI, Z
      INTEGER IB

*     Data:
      DATA  PC/+2306.2181   D0, +2306.2181   D0 , +2004.3109   D0 ,
     :     +1.39656  D0,    +1.39656  D0 ,    -0.85330  D0 ,
     :     -0.000139 D0,    -0.000139 D0 ,    -0.000217 D0 ,
     :     +0.30188  D0,    +1.09468  D0 ,    -0.42665  D0 ,
     :     -0.000344 D0,    +0.000066 D0 ,    -0.000217 D0 ,
     :     +0.017998 D0,    +0.018203 D0 ,    -0.041833 D0 /

*     RA0, DEC0 are the standard equatorial coordinates with equinox J2000,
*     together with their rate. See IAU report Table 3

      DATA RA0/ 285.96,  0.0,    281.02,  -0.033,  272.78,  0.0,
     :     0.00, -0.641,  317.681, -0.108,  268.05, -0.009,
     :     40.66, -0.036,  257.43,   0.0,    295.33,  0.0,
     :     311.63,  0.0/
      DATA DEC0/ 63.96,  0.0,     61.45,  -0.005,   67.21,  0.0,
     :     90.00, -0.557,   52.886, -0.061,   64.49, +0.003,
     :     83.52, -0.004,  -15.10,   0.0,     40.65,  0.0,
     :     4.18,  0.0/
      DATA    R/0.01745329252D0/,EPOCH/2451545.0D0/


      TYEAR = 1.0D0/36525.0D0
      T = (AJD - EPOCH)*TYEAR
      RA  =  RA0(1,IB) + T * RA0(2,IB)
      DEC = DEC0(1,IB) + T * DEC0(2,IB)

*     Compute precession angles zeta, z and theta to precess
*     data from J2000.0 (2451545.0) to date (AJD).
*     XI, Z, TH are equatorial precession parameters (in degrees) using
*     constants defined by IAU (1978)
*     These equations are from Astron.Astrophys., 73, 282-284, (1979) by
*     J H Liesle.

      T1  = (EPOCH - 2451545.0D0)*TYEAR
      DEG = 1.0D0/3600.0D0

      XI = (PC(1,1) + PC(1,2)*T1 + PC(1,3)*T1*T1)*T
     :     +(PC(1,4) + PC(1,5)*T1)*T*T
     :     +PC(1,6)*T*T*T
      XI = XI*DEG
      Z  = (PC(2,1) + PC(2,2)*T1 + PC(2,3)*T1*T1)*T
     :     +(PC(2,4) + PC(2,5)*T1)*T*T
     :     +PC(2,6)*T*T*T
      Z  = Z*DEG
      TH = (PC(3,1) + PC(3,2)*T1 + PC(3,3)*T1*T1)*T
     :     +(PC(3,4) + PC(3,5)*T1)*T*T
     :     +PC(3,6)*T*T*T
      TH = TH*DEG

      CD = DCOS(DEC*R)
      SD = DSIN(DEC*R)
      AZ = RA + XI
      CA = DCOS(AZ*R)
      SA = DSIN(AZ*R)
      CT = DCOS(TH*R)
      ST = DSIN(TH*R)

      C  = -SD*ST + CD*CT*CA
      S  =  CD*SA

*     Right Ascension and declination of the Pole

      RAP =  Z + DATAN2(S,C)/R
      IF (RAP.LT.0.0D0) RAP =RAP + 360.0D0
      IF (IB.EQ.3) RAP = 0.0D0

      SD1 = SD*CT + CD*ST*CA
      IF (DABS(SD1).LE.0.99D0) THEN
         DECP = DASIN(SD1)/R
      ELSE
         DECP = DACOS( DSQRT(S*S +C*C) )/R
         IF(SD1.LT.0.0D0) DECP = -DECP
      ENDIF

      END

*************************************************************************

      SUBROUTINE GEOEPH(NOPL,EXRA,EXDEC,DIST,RJD,STATUS)

*  Purpose:
*     Uses calls the SLA_LIB and the JPL ephemeris to create
*     geocentric planet coordinates.

*  Modified: GJP Starlink 07 September 1996

*************************************************************************

      IMPLICIT NONE

*  Variables:
      INTEGER NOPL,NP,LUP(10)
      DOUBLE PRECISION EXDEC,EXRA,TDB,RAV1,DECV1,DIST
      DOUBLE PRECISION RJD
      DOUBLE PRECISION TLONG,TLAT,THEIGHT
      CHARACTER PLANET(10)*7
      CHARACTER *10 TNAME
      CHARACTER *40 TFULLNAME

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status

*  Common
      COMMON/INPUT/LUP,PLANET
      COMMON/TELESCOPE/TLONG,TLAT,THEIGHT,TNAME,TFULLNAME
*.

*     Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*     Calculate the modified time.
      TDB=RJD

*     Calculate approximate topographical positions.
      NP=LUP(NOPL)

*     Call JPL/SLA_LIB locations.
      CALL SLAJPL2(TDB,NP,TLONG,RAV1,DECV1,DIST)

*     Convert radians to degrees.
      EXRA=RAV1  /2.d0/3.141592653d0*360.d0
      EXDEC=DECV1/2.d0/3.141592653d0*360.d0

 9999 CONTINUE

      END


*************************************************************************

      SUBROUTINE TOPEPH(REQBODY,EXRA,EXDEC,EXGD,RJD,STATUS)

*  Purpose:
*     Uses calls the SLA_LIB and the JPL ephemeris to create an approximate
*     topocentric ephemeris for the planets, moon and sun.

*     GJP Starlink 07 September 1996

*************************************************************************

      IMPLICIT NONE

*  Variables:
      DOUBLE PRECISION EXDEC,EXGD,EXRA
      DOUBLE PRECISION S,RJD
      INTEGER I,ID,IH,IM,IS,IQ,IR,IY,FIOD,M,JUNK,TEMPI
      CHARACTER*7   REQBODY
      CHARACTER*128 OUTFILE
      CHARACTER*80 HEAD1,HEAD2,STRING1,STRING2
      CHARACTER   PLANET(10)*7
      CHARACTER*3 CMON
      LOGICAL ALL, FLU, OFL, POS, SCREEN
      INTEGER LEN,CHR_LEN

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status

      DOUBLE PRECISION PI

      DOUBLE PRECISION TDB,TDB2,RAV2,DECV2,DLEN3,DLEN4
      DOUBLE PRECISION RAV1,DECV1,DIST,TLONG,TLAT,THEIGHT
      INTEGER NP,IVR1(4),IVR2(4),LUP(10)
      DOUBLE PRECISION FRAC1,FRAC2,STL
      DOUBLE PRECISION AIRM,EPOCH
      CHARACTER *1 SIGN
      CHARACTER *10 TNAME
      CHARACTER *40 TFULLNAME

*  External function:
      DOUBLE PRECISION SLA_EPJ, SLALAST

*  Data:
      DATA PI/3.141592654D0/

*  Common blocks:
      COMMON /TIMEBLK/IY,M,ID,IH,IM,IS
      COMMON /TIMEBLK2/S
      COMMON /TIMEBLK3/CMON

      COMMON /OUTPUT/FIOD,SCREEN,POS,FLU,OFL
      COMMON /OUTPUT2/OUTFILE

      COMMON /INPUT/LUP,PLANET
      COMMON /TELESCOPE/TLONG,TLAT,THEIGHT,TNAME,TFULLNAME
*.

      IR = 0
      IQ = 0

*     Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*     Initialise flags.
      ALL = .FALSE.

*     Set up requested PLanets
      LEN=CHR_LEN(REQBODY)
      IF(REQBODY.EQ.'ALL' .OR. REQBODY.EQ.'all'
     :     .OR. REQBODY.EQ. '   ') THEN
         ALL = .TRUE.
         IQ=1
         IR=10
      ELSE
         DO I = 1,10
            IF(PLANET(I)(1:LEN).EQ.REQBODY(1:LEN)) THEN
               IQ=I
               IR=I
            ENDIF
         ENDDO
      ENDIF

*     Headings.
      IF ( ALL .OR. (IQ .GE. 4 .AND. IQ .LE. 8) ) THEN
         HEAD1='Planetary Submillimetre Fluxes for the JCMT'
      ELSE IF (POS) THEN
         HEAD1='Positional data only for this Planet'
      ELSE
         HEAD1='Date information only for this Planet'
      ENDIF

*     Create substrings.
      TEMPI=IH-10
      IF (TEMPI.LT.0) TEMPI=TEMPI+24
      CALL MSG_FMTI('P1','I2.2',ID)
      CALL MSG_FMTC('P2','A3',CMON)
      CALL MSG_FMTI('P3','I4',IY)
      CALL MSG_FMTI('P4','I2.2',IH)
      CALL MSG_FMTI('P5','I2.2',IM)
      CALL MSG_FMTI('P6','I2.2',IS)
      CALL MSG_FMTI('P7','I2',TEMPI)

*     Create header string.
      STRING1='UT:  ^P4:^P5:^P6'//
     :     '       UT Date:^P1-^P2-^P3'//
     :     '   HST:^P7:^P5:^P6'
      CALL MSG_LOAD(' ',STRING1,STRING2,JUNK,STATUS)

*     Display.

      IF (SCREEN) THEN
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ',HEAD1,STATUS)
         CALL MSG_BLANK(STATUS)
         CALL MSG_OUT(' ',STRING2,STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

*     File.
      IF (OFL) THEN
         CALL FIO_WRITE(FIOD,' ',STATUS)
         CALL FIO_WRITE(FIOD,HEAD1,STATUS)
         CALL FIO_WRITE(FIOD,' ',STATUS)
         CALL FIO_WRITE(FIOD,STRING2,STATUS)
         CALL FIO_WRITE(FIOD,' ',STATUS)
      ENDIF

*     Calculate the modified time.[RJD now is the MJD]
      TDB=RJD
*     And a time 1 second into the future
      TDB2=TDB+1.D0/86400.0D0

*     Current epoch.
      EPOCH=SLA_EPJ(TDB)

*     Call the slalib jpl routines to get the local sid. time.
      CALL SLA_DR2TF(4,SLALAST(TDB,TLONG),SIGN,IVR1)

*     Create output strings.
      CALL MSG_FMTI('P1','I2.2',IVR1(1))
      CALL MSG_FMTI('P2','I2.2',IVR1(2))
      FRAC1=IVR1(3)+DBLE(IVR1(4))/10000.D0
      CALL MSG_FMTD('P3','F7.4',FRAC1)
      CALL MSG_FMTD('P4','F12.3',RJD)
      CALL MSG_FMTD('P5','F9.4',EPOCH)
      STRING1='LST: ^P1:^P2:^P3  MJD (TT): ^P4  Epoch: ^P5'
      CALL MSG_LOAD(' ',STRING1,STRING2,JUNK,STATUS)

*     Create display / file headings.
      IF (SCREEN) THEN
         CALL MSG_OUT(' ',STRING2,STATUS)
         CALL MSG_BLANK(STATUS)
      END IF

      IF (OFL) THEN
         CALL FIO_WRITE(FIOD,STRING2,STATUS)
         CALL FIO_WRITE(FIOD,' ',STATUS)
      END IF

*     Return after date info if positional data not required.
      IF (.NOT. POS) GO TO 9999

*     Header strings.
      HEAD1='Body         RA        TRIMRA         '//
     :     'Dec       TRIMDEC        GD     AMASS'
      HEAD2='           (h m s)  (arcsec/sec)'//
     :     '    (d m s)  (arcsec/sec)     (au)'

*     Create display / file headings.
      IF (SCREEN) THEN
         CALL MSG_OUT(' ',HEAD1,STATUS)
         CALL MSG_OUT(' ',HEAD2,STATUS)
      END IF

      IF (OFL) THEN
         CALL FIO_WRITE(FIOD,HEAD1,STATUS)
         CALL FIO_WRITE(FIOD,HEAD2,STATUS)
      END IF

*     Loop through all objects.
      DO 100 I=IQ,IR

*     Ensure that values come out in expected order.
         NP=LUP(I)

*     Call the slalib jpl routines.
         CALL SLAJPL(TDB,NP,TLONG,TLAT,THEIGHT,
     :        RAV1,DECV1,DIST,STL,AIRM)

*     Convert radians to hh mm ss and dd mm ss.
         CALL SLA_DR2TF(4,RAV1,SIGN,IVR1)
         CALL SLA_DR2AF(4,DECV1,SIGN,IVR2)

*     Convert secs and fractional secs to a double precision for printing.
         FRAC1=IVR1(3)+DBLE(IVR1(4))/10000.D0
         FRAC2=IVR2(3)+DBLE(IVR2(4))/10000.D0

*     Set up parameter values to display.
         CALL MSG_FMTI('P1','I2',IVR1(1))
         CALL MSG_FMTI('P2','I2',IVR1(2))
         CALL MSG_FMTD('P3','F7.4',FRAC1)
         CALL MSG_FMTC('P10','A1',SIGN)
         CALL MSG_FMTI('P4','I2',IVR2(1))
         CALL MSG_FMTI('P5','I2',IVR2(2))
         CALL MSG_FMTD('P6','F7.4',FRAC2)
         CALL MSG_FMTD('P7','F9.6',DIST)
         CALL MSG_FMTD('P11','F6.3',AIRM)

*     Recalc positions 1 sec later and deduce movement rate.
         CALL SLAJPL(TDB2,NP,TLONG,TLAT,THEIGHT,
     :        RAV2,DECV2,DIST,STL,AIRM)
         DLEN3=(RAV2-RAV1)*360./2./3.1415926*3600.
         DLEN4=(DECV2-DECV1)*360./2./3.1415926*3600.

*     Set up parameter values to display.
         CALL MSG_FMTD('P8','F7.4',DLEN3)
         CALL MSG_FMTD('P9','F7.4',DLEN4)

*     Expand string.
         STRING1=PLANET(I)(1:7)//
     :        ' ^P1 ^P2 ^P3 ^P8  '//
     :        ' ^P10^P4 ^P5 ^P6'//
     :        '  ^P9   ^P7  ^P11'
         CALL MSG_LOAD(' ',STRING1,STRING2,JUNK,STATUS)

*     Place output on the screen or in the file.
         IF (POS) THEN
            IF (SCREEN) CALL MSG_OUT(' ',STRING2,STATUS)
            IF (OFL)    CALL FIO_WRITE(FIOD,STRING2,STATUS)
         ENDIF

 100  CONTINUE

 9999 CONTINUE

      END


*************************************************************************

      DOUBLE PRECISION FUNCTION RJDATE ()

*  Purpose:
*     To calculate real top Julian date from Gregorian date.
*     Returned as a modified Julian date.
*     Values are returned in TT rather than UT since this is the most common
*     usage in the FLUXES program.

*  Note:
*     Now uses SLA_CALDJ

************************************************************************

      IMPLICIT NONE

*  Variables:
      DOUBLE PRECISION S
      DOUBLE PRECISION FDUTC
      INTEGER ID, IH,IM,IY,IS,J,M

*  External Functions
      DOUBLE PRECISION SLA_EPJ,SLA_DT,SLA_DTT

*  Common blocks:
      COMMON /TIMEBLK/IY,M,ID,IH,IM,IS
      COMMON /TIMEBLK2/S
*.

      CALL SLA_CALDJ(IY,M,ID,RJDATE,J)
      CALL SLA_DTF2D(IH,IM,S,FDUTC,J)
      RJDATE = RJDATE + FDUTC

*     Correct to TT [older than 1960 we need to guess]
      IF (RJDATE .GT. 36934.0D0) THEN
         RJDATE = RJDATE + (SLA_DTT(RJDATE) / 86400.D0)
      ELSE
         RJDATE = RJDATE + (SLA_DT(SLA_EPJ(RJDATE)) / 86400.D0)
      END IF

      END


************************************************************************

      SUBROUTINE TB350(RJD,TBAR,STATUS)

*     14 Jul04        : TIMJ - RJD is MJD
*     26 Apr01        : TIMJ - Extend model for until 2011 using
*                              numbers posted on Wright's web page
*                              http://www.astro.ucla.edu/~wright/old_mars.txt
*     26 Oct95        :  HEM - add in simplified model to extend MJD range
*     Original version:  Unknown origin

*  Purpose:
*     Do linear interpolation of Martian 350-micron brightness temp-
*     eratures (M.350.TB.'s) produced by Wright's model to get value
*     for any Modified Julian Date (MJD=J.D.-2400000.5) between
*     42760 < MJD < 52000. Two versions are used: Wright's full model
*     (Ap.J. 210, 250; 1976) is used for 46040 < MJD < 50000, and
*     outside these dates, a simplified version (Wright, private
*     communication to H.E. Matthews; 1995) is used. The latter uses a
*     modification of a rotating cratered asteroid program. After approx.
*     correction for the cosmic background (-2.56 K) this model reproduces
*     the original Wright (1976) values with an rms error of 0.13 K.

*  Description of Parameters:

*  Data arrays:
*     TB1AR  - array of 100 TB'S for MJD's 46040 - 50000 in steps of 40 days
*     TBWRIGHT - array of 326 TB's from simplified Wright model; step 40 days
*  Input:
*     RJD    - J.D. for which M.350.TB. is calculated by interpolation
*     Output:
*     TBAR   - M.350.TB. calculated by interpolation for RJD
*     IERR   - Error condition (0 = OK; 1 = MJD out of valid range)

*     IERR is not returned. STARLINK status is returned instead but is
*     not actually set in this routine. Routine returns immediately if
*     STATUS is bad on entry.

************************************************************************

      IMPLICIT NONE

      INTEGER NWRIGHT
      PARAMETER( NWRIGHT = 1167 )

*     Variables:
      REAL*8 DAT1I,DAT1I1,RJD,TBAR,TB1I,TB1I1,
     :     TB1AR(100),TBWRIGHT(NWRIGHT)
      INTEGER I,IERR
      REAL*8 JD

*     Data:
      DATA TB1AR/ 213.07, 215.42, 215.25, 213.54, 211.09, 210.47,
     :     211.17, 211.63, 211.75, 211.63, 210.13, 208.91, 211.40,
     :     218.38, 226.78, 219.98, 207.64, 206.85, 207.87, 207.42,
     :     206.14, 204.41, 203.76, 205.17, 208.00, 210.09, 211.98,
     :     213.38, 212.96, 213.10, 215.70, 220.10, 224.58, 229.15,
     :     233.53, 227.64, 212.36, 203.60, 199.02, 196.68, 196.97,
     :     200.10, 204.55, 208.20, 211.76, 214.81, 216.40, 218.74,
     :     222.62, 226.30, 228.04, 227.08, 222.44, 220.21, 222.70,
     :     212.15, 197.96, 192.10, 192.14, 196.70, 201.25, 206.03,
     :     210.86, 215.56, 219.23, 223.74, 229.09, 232.12, 232.14,
     :     228.31, 220.36, 215.72, 214.28, 215.34, 208.91, 195.23,
     :     190.67, 193.85, 198.52, 203.65, 209.22, 214.85, 220.20,
     :     226.64, 232.62, 234.88, 233.72, 228.15, 221.11, 215.95,
     :     212.87, 211.65, 212.30, 208.76, 197.66, 194.8 , 196.96,
     :     201.34, 206.73, 212.44/

      data tbwright/
     :     218.0, 203.0, 194.0, 193.0, 197.0, 202.0, 208.0, 213.0,
     :     218.0, 223.0, 228.0, 234.0, 236.0, 236.0, 231.0, 224.0,
     :     218.0, 216.0, 216.0, 214.0, 202.0, 194.0, 196.0, 200.0,
     :     205.0, 211.0, 217.0, 223.0, 230.0, 235.0, 238.0, 236.0,
     :     230.0, 223.0, 218.0, 215.0, 213.0, 213.0, 214.0, 205.0,
     :     199.0, 199.0, 203.0, 208.0, 214.0, 220.0, 227.0, 233.0,
     :     234.0, 232.0, 227.0, 221.0, 218.0, 215.0, 213.0, 212.0,
     :     213.0, 215.0, 210.0, 203.0, 202.0, 205.0, 209.0, 215.0,
     :     221.0, 226.0, 227.0, 225.0, 221.0, 218.0, 216.0, 215.0,
     :     214.0, 213.0, 212.0, 212.0, 217.0, 219.0, 209.0, 205.0,
     :     205.0, 208.0, 215.0, 218.0, 218.0, 216.0, 214.0, 213.0,
     :     213.0, 214.0, 214.0, 214.0, 213.0, 212.0, 214.0, 221.0,
     :     229.0, 224.0, 211.0, 209.0, 210.0, 210.0, 209.0, 207.0,
     :     206.0, 207.0, 210.0, 212.0, 214.0, 216.0, 216.0, 216.0,
     :     218.0, 223.0, 227.0, 232.0, 236.0, 231.0, 216.0, 206.0,
     :     202.0, 199.0, 199.0, 202.0, 206.0, 210.0, 214.0, 218.0,
     :     219.0, 221.0, 225.0, 229.0, 231.0, 230.0, 226.0, 223.0,
     :     225.0, 216.0, 201.0, 195.0, 194.0, 197.0, 203.0, 208.0,
     :     213.0, 218.0, 222.0, 226.0, 232.0, 235.0, 235.0, 232.0,
     :     224.0, 219.0, 217.0, 218.0, 212.0, 198.0, 193.0, 195.0,
     :     201.0, 206.0, 211.0, 218.0, 223.0, 229.0, 235.0, 238.0,
     :     237.0, 232.0, 224.0, 219.0, 215.0, 214.0, 214.0, 212.0,
     :     200.0, 197.0, 199.0, 203.0, 209.0, 215.0, 221.0, 228.0,
     :     234.0, 236.0, 234.0, 229.0, 223.0, 218.0, 215.0, 213.0,
     :     212.0, 213.0, 213.0, 205.0, 201.0, 202.0, 206.0, 211.0,
     :     216.0, 223.0, 229.0, 230.0, 228.0, 224.0, 220.0, 217.0,
     :     215.0, 213.0, 212.0, 212.0, 213.0, 217.0, 212.0, 205.0,
     :     204.0, 206.0, 210.0, 217.0, 221.0, 221.0, 220.0, 217.0,
     :     215.0, 214.0, 214.0, 214.0, 214.0, 213.0, 211.0, 214.0,
     :     224.0, 212.0, 206.0, 206.0, 211.0, 212.0, 212.0, 211.0,
     :     209.0, 209.0, 211.0, 213.0, 214.0, 215.0, 215.0, 214.0,
     :     216.0, 220.0, 227.0, 234.0, 235.0, 220.0, 211.0, 207.0,
     :     204.0, 202.0, 202.0, 204.0, 207.0, 211.0, 214.0, 217.0,
     :     218.0, 219.0, 222.0, 226.0, 229.0, 230.0, 229.0, 229.0,
     :     228.0, 212.0, 201.0, 196.0, 196.0, 199.0, 204.0, 209.0,
     :     213.0, 218.0, 221.0, 224.0, 229.0, 233.0, 234.0, 232.0,
     :     225.0, 219.0, 219.0, 220.0, 208.0, 196.0, 193.0, 195.0,
     :     201.0, 206.0, 212.0, 217.0, 222.0, 228.0, 234.0, 237.0,
     :     237.0, 233.0, 225.0, 219.0, 216.0, 215.0, 215.0, 207.0,
     :     196.0, 195.0, 199.0, 204.0, 210.0, 216.0, 222.0, 228.0,
     :     235.0, 237.0, 236.0, 231.0, 224.0, 219.0,
C     These are the new numbers from astro-ph/0703640
     :     216.0, 213.0, 213.0, 214.0, 210.0, 201.0, 199.0, 202.0,
     :     207.0, 212.0, 218.0, 225.0, 231.0, 233.0, 231.0, 227.0,
     :     221.0, 218.0, 216.0, 214.0, 212.0, 212.0, 214.0, 215.0,
     :     206.0, 203.0, 204.0, 207.0, 212.0, 219.0, 224.0, 225.0,
     :     224.0, 220.0, 217.0, 216.0, 215.0, 214.0, 213.0, 212.0,
     :     211.0, 215.0, 221.0, 215.0, 206.0, 204.0, 206.0, 213.0,
     :     216.0, 216.0, 215.0, 212.0, 211.0, 212.0, 214.0, 214.0,
     :     215.0, 214.0, 212.0, 214.0, 219.0, 228.0, 233.0, 220.0,
     :     211.0, 210.0, 209.0, 207.0, 205.0, 204.0, 206.0, 209.0,
     :     212.0, 214.0, 216.0, 217.0, 217.0, 219.0, 223.0, 227.0,
     :     230.0, 233.0, 235.0, 222.0, 209.0, 202.0, 198.0, 198.0,
     :     200.0, 205.0, 209.0, 213.0, 217.0, 220.0, 222.0, 226.0,
     :     230.0, 232.0, 231.0, 226.0, 221.0, 222.0, 220.0, 205.0,
     :     195.0, 193.0, 196.0, 201.0, 207.0, 212.0, 217.0, 222.0,
     :     226.0, 232.0, 236.0, 236.0, 233.0, 225.0, 219.0, 216.0,
     :     216.0, 215.0, 202.0, 194.0, 195.0, 199.0, 204.0, 210.0,
     :     216.0, 222.0, 228.0, 234.0, 238.0, 237.0, 232.0, 225.0,
     :     219.0, 216.0, 214.0, 214.0, 214.0, 205.0, 198.0, 199.0,
     :     202.0, 208.0, 213.0, 219.0, 226.0, 233.0, 235.0, 234.0,
     :     229.0, 223.0, 219.0, 216.0, 214.0, 212.0, 213.0, 215.0,
     :     209.0, 202.0, 202.0, 205.0, 209.0, 214.0, 221.0, 227.0,
     :     228.0, 227.0, 223.0, 219.0, 217.0, 215.0, 214.0, 213.0,
     :     212.0, 212.0, 216.0, 217.0, 208.0, 204.0, 205.0, 208.0,
     :     214.0, 219.0, 219.0, 218.0, 216.0, 214.0, 214.0, 214.0,
     :     214.0, 214.0, 213.0, 211.0, 213.0, 219.0, 227.0, 221.0,
     :     209.0, 207.0, 210.0, 211.0, 210.0, 209.0, 208.0, 208.0,
     :     210.0, 212.0, 214.0, 215.0, 216.0, 215.0, 217.0, 221.0,
     :     226.0, 232.0, 237.0, 230.0, 215.0, 208.0, 203.0, 201.0,
     :     200.0, 202.0, 206.0, 210.0, 214.0, 217.0, 218.0, 220.0,
     :     223.0, 227.0, 230.0, 230.0, 228.0, 225.0, 227.0, 217.0,
     :     203.0, 196.0, 194.0, 197.0, 202.0, 207.0, 212.0, 217.0,
     :     221.0, 224.0, 230.0, 234.0, 235.0, 233.0, 226.0, 220.0,
     :     217.0, 218.0, 213.0, 199.0, 193.0, 194.0, 200.0, 205.0,
     :     210.0, 217.0, 222.0, 227.0, 234.0, 237.0, 237.0, 233.0,
     :     226.0, 220.0, 216.0, 214.0, 215.0, 212.0, 200.0, 196.0,
     :     198.0, 203.0, 208.0, 214.0, 220.0, 227.0, 233.0, 236.0,
     :     236.0, 231.0, 225.0, 219.0, 216.0, 214.0, 212.0, 213.0,
     :     213.0, 204.0, 200.0, 201.0, 205.0, 211.0, 216.0, 223.0,
     :     229.0, 231.0, 230.0, 226.0, 221.0, 218.0, 216.0, 214.0,
     :     212.0, 212.0, 213.0, 217.0, 211.0, 204.0, 204.0, 206.0,
     :     210.0, 216.0, 222.0, 223.0, 222.0, 219.0, 216.0, 215.0,
     :     215.0, 214.0, 213.0, 213.0, 211.0, 213.0, 220.0, 222.0,
     :     211.0, 205.0, 205.0, 211.0, 214.0, 214.0, 213.0, 211.0,
     :     210.0, 211.0, 213.0, 214.0, 215.0, 215.0, 213.0, 214.0,
     :     219.0, 226.0, 234.0, 231.0, 218.0, 211.0, 208.0, 206.0,
     :     204.0, 203.0, 204.0, 207.0, 211.0, 214.0, 216.0, 217.0,
     :     218.0, 220.0, 224.0, 228.0, 230.0, 230.0, 231.0, 229.0,
     :     213.0, 202.0, 197.0, 196.0, 199.0, 203.0, 208.0, 212.0,
     :     217.0, 220.0, 223.0, 227.0, 231.0, 233.0, 232.0, 227.0,
     :     221.0, 220.0, 221.0, 209.0, 197.0, 193.0, 195.0, 200.0,
     :     205.0, 211.0, 216.0, 221.0, 226.0, 232.0, 236.0, 237.0,
     :     234.0, 226.0, 220.0, 216.0, 215.0, 216.0, 208.0, 196.0,
     :     195.0, 198.0, 203.0, 209.0, 215.0, 221.0, 227.0, 234.0,
     :     237.0, 237.0, 233.0, 226.0, 220.0, 216.0, 214.0, 213.0,
     :     214.0, 210.0, 200.0, 199.0, 201.0, 206.0, 212.0, 217.0,
     :     224.0, 231.0, 234.0, 233.0, 229.0, 223.0, 219.0, 216.0,
     :     214.0, 212.0, 212.0, 214.0, 214.0, 205.0, 202.0, 204.0,
     :     207.0, 212.0, 218.0, 225.0, 226.0, 226.0, 222.0, 218.0,
     :     216.0, 215.0, 214.0, 213.0, 212.0, 212.0, 214.0, 220.0,
     :     214.0, 206.0, 204.0, 206.0, 212.0, 217.0, 217.0, 217.0,
     :     214.0, 213.0, 213.0, 214.0, 214.0, 214.0, 214.0, 212.0,
     :     213.0, 218.0, 226.0, 230.0, 216.0, 209.0, 210.0, 210.0,
     :     209.0, 207.0, 206.0, 206.0, 209.0, 211.0, 214.0, 216.0,
     :     216.0, 216.0, 217.0, 221.0, 226.0, 230.0, 234.0, 236.0,
     :     222.0, 209.0, 203.0, 200.0, 199.0, 200.0, 204.0, 209.0,
     :     213.0, 216.0, 219.0, 221.0, 224.0, 228.0, 231.0, 231.0,
     :     228.0, 223.0, 224.0, 222.0, 206.0, 196.0, 194.0, 196.0,
     :     201.0, 206.0, 211.0, 216.0, 221.0, 225.0, 230.0, 234.0,
     :     236.0, 234.0, 227.0, 220.0, 217.0, 217.0, 216.0, 203.0,
     :     194.0, 194.0, 198.0, 203.0, 209.0, 215.0, 221.0, 227.0,
     :     233.0, 237.0, 237.0, 234.0, 227.0, 221.0, 216.0, 214.0,
     :     214.0, 214.0, 204.0, 197.0, 198.0, 201.0, 207.0, 213.0,
     :     219.0, 225.0, 232.0, 236.0, 235.0, 231.0, 225.0, 220.0,
     :     216.0, 214.0, 212.0, 212.0, 214.0, 209.0, 202.0, 201.0,
     :     204.0, 209.0, 214.0, 220.0, 227.0, 230.0, 229.0, 225.0,
     :     221.0, 218.0, 216.0, 214.0, 213.0, 212.0, 212.0, 216.0,
     :     216.0, 207.0, 204.0, 205.0, 208.0, 214.0, 220.0, 221.0,
     :     220.0, 218.0, 215.0, 214.0, 215.0, 214.0, 214.0, 213.0,
     :     212.0, 212.0, 218.0, 226.0, 218.0, 208.0, 205.0, 210.0,
     :     212.0, 212.0, 211.0, 209.0, 209.0, 210.0, 212.0, 214.0,
     :     215.0, 215.0, 214.0, 215.0, 219.0, 224.0, 231.0, 237.0,
     :     227.0, 215.0, 208.0, 205.0, 203.0, 201.0, 202.0, 206.0,
     :     210.0, 213.0, 216.0, 218.0, 219.0, 221.0, 225.0, 228.0,
     :     230.0, 229.0, 228.0, 230.0, 219.0, 204.0, 197.0, 195.0,
     :     197.0, 202.0, 207.0, 211.0, 216.0, 220.0, 223.0, 227.0,
     :     232.0, 234.0, 233.0, 228.0, 221.0, 218.0, 219.0, 214.0,
     :     200.0, 193.0, 194.0, 199.0, 204.0, 210.0, 216.0, 221.0,
     :     226.0, 232.0, 236.0, 237.0, 235.0, 227.0, 221.0, 217.0,
     :     215.0, 215.0, 212.0, 200.0, 195.0, 197.0, 202.0, 207.0,
     :     214.0, 219.0, 226.0, 233.0, 237.0, 237.0, 233.0, 226.0,
     :     221.0, 217.0, 214.0, 213.0, 213.0, 213.0, 204.0, 199.0,
     :     201.0, 205.0, 210.0, 215.0, 222.0, 229.0, 232.0, 232.0,
     :     228.0, 223.0, 219.0, 216.0, 214.0, 213.0, 212.0, 213.0,
     :     216.0, 210.0, 204.0, 203.0, 206.0, 210.0, 216.0, 222.0,
     :     225.0, 224.0, 221.0, 218.0, 216.0, 215.0, 214.0, 213.0,
     :     213.0, 211.0, 212.0, 219.0, 220.0, 210.0, 205.0, 205.0,
     :     210.0, 215.0, 215.0, 215.0, 213.0, 211.0, 212.0, 213.0,
     :     214.0, 215.0, 214.0, 213.0, 213.0, 217.0, 224.0, 233.0,
     :     228.0, 214.0, 211.0, 209.0, 207.0, 206.0, 204.0, 205.0,
     :     207.0, 210.0, 213.0, 215.0, 217.0, 217.0, 218.0, 222.0,
     :     226.0, 229.0, 231.0, 234.0, 230.0, 213.0, 204.0, 199.0,
     :     197.0, 199.0, 203.0, 208.0, 212.0, 216.0, 219.0, 221.0,
     :     225.0, 229.0, 232.0, 232.0, 228.0, 222.0, 221.0, 223.0,
     :     211.0, 198.0, 193.0, 194.0, 199.0, 205.0, 210.0, 215.0,
     :     220.0, 225.0, 230.0, 235.0, 236.0, 235.0, 228.0, 221.0,
     :     217.0/


*     Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*     Status:
      INTEGER STATUS            ! Global status

*     Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*     Convert to JD
      JD = RJD + 2400000.5

*     Interpolate to get M.350.TB for JD

      TB1I = 0.0D0
      I = DINT((JD - 2442760.5D0)/40.0D0) + 1
      DAT1I = 2442760.5D0 + ((I-1) * 40.0D0)
      DAT1I1 = DAT1I + 40.0D0
      IERR=0
      IF ((I.LE.0).OR.(I.GE.NWRIGHT)) THEN
         IERR=1
      ELSEIF ((I.LE.82).OR.(I.GE.183)) THEN
         TB1I = TBWRIGHT(I)-2.56
      ELSEIF ((I.GT.82).AND.(I.LT.183)) THEN
         TB1I = TB1AR(I-82)
      ENDIF

      I=I+1
      IF ((I.LE.0).OR.(I.GT.NWRIGHT)) THEN
         IERR=1
         CALL MSG_OUT(' ',
     :        'Interpolation problem in subroutine TB350!',
     :        STATUS)
         CALL MSG_OUT(' ',
     :        'Mars fluxes will be incorrect!',
     :        STATUS)
      ELSEIF ((I.LE.82).OR.(I.GE.183)) THEN
         TB1I1 = TBWRIGHT(I)-2.56
      ELSEIF ((I.GT.82).AND.(I.LT.183)) THEN
         TB1I1 = TB1AR(I-82)
      ENDIF

*     Do linear interpolation between two points.

      IF (IERR .EQ. 0) THEN
         TBAR = TB1I + (TB1I1-TB1I)*(JD-DAT1I)/(DAT1I1-DAT1I)
      ELSE
         TBAR = 0.0
      END IF

      END

****************************************************************************

      SUBROUTINE SLAJPL(DATE,NP,ELONG,PHI,HEIGHT,RA,DEC,
     :     R,STL,AIRM)
*     +
*     Approximate topocentric apparent RA,Dec of a planet, and its
*     angular diameter.

*  Given:
*     DATE        d       MJD of observation (JD - 2400000.5)
*     NP          i       planet: 1 = Mercury
*                                 2 = Venus
*                                10 = Moon
*                                 4 = Mars
*                                 5 = Jupiter
*                                 6 = Saturn
*                                 7 = Uranus
*                                 8 = Neptune
*                                 9 = Pluto
*                                11 = Sun

*     ELONG,PHI   d       observer's east longitude and geodetic
*                                               latitude (radians)
*     HEIGHT      d       observers altitude

*  Returned:
*     RA,DEC      d       RA, Dec (topocentric apparent, radians)
*     R           d       geocentric distance to the planet/moon/sun
*     STL         d       Approx local ST.
*     AIRM        d       Approx. air mass

*  Notes:

*     1  The date is in a dynamical timescale (TDB, formerly ET) and is
*     in the form of a Modified Julian Date (JD-2400000.5).  For all
*     practical purposes, TT can be used instead of TDB, and for many
*     applications UT will do (except for the Moon).

*     2  The longitude and latitude allow correction for geocentric
*     parallax.  This is a major effect for the Moon, but in the
*     context of the limited accuracy of the present routine its
*     effect on planetary positions is small (negligible for the
*     outer planets).  Geocentric positions can be generated by
*     calls to the routines sla_EVP, sla_DMOON and sla_PLANET.

*     Called: sla_GMST, sla_DT, sla_EPJ, sla_PVOBS, sla_PRENUT,
*     sla_DMXV, sla_DCC2S, sla_DRANRM, pleph, sla_EQEQX

*     Based on sla_rdplan             P.T.Wallace  Starlink 30 November 1994
*     Modified to use JPL ephemeris   G.J.Privett  Starlink 02 September 1996

*     Copyright (C) 1995 Rutherford Appleton Laboratory
*     -
*****************************************************************************

      IMPLICIT NONE

*     Variables:
      LOGICAL OK
      INTEGER NP,I
      DOUBLE PRECISION DATE,ELONG,PHI,RA,DEC,HEIGHT
      DOUBLE PRECISION AIRM,STL,VGM(6),V(6),RMAT(3,3)
      DOUBLE PRECISION VSE(6),VSG(6),VSP(6),VGO(6)
      DOUBLE PRECISION DX,DY,DZ,R,TL

*     External functions.
      DOUBLE PRECISION SLA_DRANRM,SLALAST
      DOUBLE PRECISION SLA_ZD,SLA_AIRMAS

*     Calculate Local Apparent Sidereal Time
      STL = SLALAST( DATE, ELONG)

*     Geocentric Moon position.
      CALL SLA_DMOON(DATE,V)

*     Create nutation matrix.
      CALL SLA_NUT(DATE,RMAT)

*     Multiply a 3D vector by a rotation matrix.
      CALL SLA_DMXV(RMAT,V,VGM)
      CALL SLA_DMXV(RMAT,V(4),VGM(4))

*     Moon?
      IF (NP.EQ.10) THEN

*     Yes: geocentre to Moon (true of date)
         DO I=1,6
            V(I)=VGM(I)
         END DO

      ELSE

*     No: precession/nutation matrix, J2000 to date
         CALL SLA_PRENUT(2000.D0,DATE,RMAT)

*     Sun to Earth-Moon Barycentre (J2000). Heliocentric position.
         CALL SLA_PLANET(DATE,3,V,OK)

*     Precession and nutation to date
         CALL SLA_DMXV(RMAT,V,VSE)
         CALL SLA_DMXV(RMAT,V(4),VSE(4))

*     Sun to geocentre
         DO I=1,6
            VSG(I)=VSE(I)-0.012150581D0*VGM(I)
         END DO

*     Sun?
         IF (NP.EQ.11) THEN

*     Yes: geocentre to Sun
            DO I=1,6
               V(I)=-VSG(I)
            END DO

         ELSE

*     No: Sun to Planet. Heliocentric position.
            CALL SLA_PLANET(DATE,NP,V,OK)

*     Precession and nutation to date
            CALL SLA_DMXV(RMAT,V,VSP)
            CALL SLA_DMXV(RMAT,V(4),VSP(4))

*     Geocentre to planet
            DO I=1,6
               V(I)=VSP(I)-VSG(I)
            END DO

         END IF

      END IF

*     Refer to origin at the observer
      CALL SLA_PVOBS(PHI,HEIGHT,STL,VGO)
      DO I=1,6
         V(I)=V(I)-VGO(I)
      END DO

*     Geometric distance (AU)
      DX=V(1)
      DY=V(2)
      DZ=V(3)
      R=SQRT(DX*DX+DY*DY+DZ*DZ)

*     Light time (sec)
      TL=499.004782D0*R

*     Correct position for planetary aberration
      DO I=1,3
         V(I)=V(I)-TL*V(I+3)
      END DO

*     To RA,Dec
      CALL SLA_DCC2S(V,RA,DEC)
      RA=SLA_DRANRM(RA)

*     Calculate the zenith distance.
      AIRM=SLA_AIRMAS(SLA_ZD((RA-STL),DEC,PHI))

      END

*****************************************************************************

      SUBROUTINE SLAJPL2(DATE,NP,ELONG,RA,DEC,R)

*     +
*     Approximate geocentric apparent RA,Dec of a planet.
*     Does not cater for the moon.

*  Given:
*     DATE        d       MJD of observation (JD - 2400000.5)
*     NP          i       planet: 1 = Mercury
*                                 2 = Venus
*                                 4 = Mars
*                                 5 = Jupiter
*                                 6 = Saturn
*                                 7 = Uranus
*                                 8 = Neptune
*                                 9 = Pluto
*                                11 = Sun
*     ELONG       d       Longtitude

*  Returned:
*     RA,DEC      d       RA, Dec (geocentric apparent, radians)
*     R           d       geocentric distance to the planet/moon/sun

*  Notes:

*     1  The date is in a dynamical timescale (TDB, formerly ET) and is
*     in the form of a Modified Julian Date (JD-2400000.5).  For all
*     practical purposes, TT can be used instead of TDB, and for many
*     applications UT will do (except for the Moon).

*     Called: sla_GMST, sla_DT, sla_EPJ, sla_PRENUT,
*     sla_DMXV, sla_DCC2S, sla_DRANRM, pleph

*     Based on sla_rdplan             P.T.Wallace  Starlink 30 November 1994
*     Modified to use JPL ephemeris   G.J.Privett  Starlink 02 September 1996

*     Copyright (C) 1995 Rutherford Appleton Laboratory
*     -
*****************************************************************************
      IMPLICIT NONE

*     Variables:
      LOGICAL OK
      INTEGER NP,I
      DOUBLE PRECISION DATE,ELONG,RA,DEC
      DOUBLE PRECISION STL,VGM(6),V(6),RMAT(3,3)
      DOUBLE PRECISION VSE(6),VSG(6),VSP(6)
      DOUBLE PRECISION DX,DY,DZ,R,TL

*     External functions.
      DOUBLE PRECISION SLALAST,SLA_DRANRM

*     Approximate local ST
      STL = SLALAST( DATE, ELONG )

*     Geocentric Moon position.
      CALL SLA_DMOON(DATE,V)

*     Create nutation matrix.
      CALL SLA_NUT(DATE,RMAT)

*     Multiply a 3D vector by a rotation matrix.
      CALL SLA_DMXV(RMAT,V,VGM)
      CALL SLA_DMXV(RMAT,V(4),VGM(4))

*     No: precession/nutation matrix, J2000 to date
      CALL SLA_PRENUT(2000.D0,DATE,RMAT)

*     Sun to Earth-Moon Barycentre (J2000). Heliocentric position.
      CALL SLA_PLANET(DATE,3,V,OK)

*     Precession and nutation to date
      CALL SLA_DMXV(RMAT,V,VSE)
      CALL SLA_DMXV(RMAT,V(4),VSE(4))

*     Sun to geocentre
      DO I=1,6
         VSG(I)=VSE(I)-0.012150581D0*VGM(I)
      END DO

*     Sun?
      IF (NP.EQ.11) THEN

*     Yes: geocentre to Sun
         DO I=1,6
            V(I)=-VSG(I)
         END DO

      ELSE

*     No: Sun to Planet. Heliocentric position.
         CALL SLA_PLANET(DATE,NP,V,OK)

*     Precession and nutation to date
         CALL SLA_DMXV(RMAT,V,VSP)
         CALL SLA_DMXV(RMAT,V(4),VSP(4))

*     Geocentre to planet
         DO I=1,6
            V(I)=VSP(I)-VSG(I)
         END DO

      END IF

*     Geometric distance (AU)
      DX=V(1)
      DY=V(2)
      DZ=V(3)
      R=SQRT(DX*DX+DY*DY+DZ*DZ)

*     Light time (sec)
      TL=499.004782D0*R

*     Correct position for planetary aberration
      DO I=1,3
         V(I)=V(I)-TL*V(I+3)
      END DO

*     To RA,Dec
      CALL SLA_DCC2S(V,RA,DEC)
      RA=SLA_DRANRM(RA)

      END

      DOUBLE PRECISION FUNCTION SLALAST( DATE, ELONG )
*     +
*     Calculate Local Apparent Sidereal time given the DATE
*     (MJD TT) and the longitude (radians, east +ve).
*     Returns the LAST in radians.
*     -
      DOUBLE PRECISION DATE
      DOUBLE PRECISION ELONG

      DOUBLE PRECISION UTDATE

      DOUBLE PRECISION SLA_DTT,SLA_DT,SLA_EPJ,SLA_GMST,
     :     SLA_EQEQX, SLA_DRANRM

*     Need UT for GMST calculation. Note the break at epoch 1960.0
      IF (DATE .GT. 36934.0) THEN
         UTDATE = DATE - (SLA_DTT( DATE ) / 86400.D0)
      ELSE
         UTDATE = DATE - (SLA_DT(SLA_EPJ(DATE)) / 86400.D0)
      END IF

*     Note that DATE is UT despite what it says on the packet.
*     It is not in TT.
      SLALAST = SLA_DRANRM(SLA_GMST(UTDATE) + ELONG + SLA_EQEQX(DATE))

      END

      SUBROUTINE FLU_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
     :     EXCLAIM,STATUS)
*+
*  Description :
*
*     This routine opens a sequential file via FIO_ASSOC.  Up to four
*     attempts may be made to open the file.  If a null response is
*     supplied the file is not opened, and the flag returned indicates
*     this fact.
*
*  Invocation :
*
*     CALL FLU_AIF_ASFIO (PNFILE,ACMODE,FORM,RECSZ,FD,OPEN,
*     EXCLAIM,STATUS)

*
*  Arguments :
*
*     PNFILE=CHARACTER*(*)
*     Parameter name by which file is to be opened
*     ACMODE=CHARACTER*(*)
*     Expression giving the required access mode.
*     Valid modes are: 'WRITE', 'UPDATE', 'APPEND'..
*     FORM=CHARACTER*(*)( READ )
*     Expression giving the required formatting of the file.
*     Valid formats are: 'FORTRAN', 'LIST', 'NONE' and
*     'UNFORMATTED'. For details, see FIO_OPEN.
*     RECSZ=INTEGER( READ )
*     Expression giving the maximum record size in bytes.
*     Set it to zero if the Fortran default is required.
*     FD=INTEGER( WRITE )
*     Variable to contain the file descriptor.
*     OPEN=LOGICAL( WRITE )
*     If true the file has been opened.
*     EXCLAIM=LOGICAL( WRITE )
*     If true then the user input was '!'.
*     STATUS=INTEGER( READ, WRITE )
*     Global status value
*
*  Method :
*
*     Check for error on entry - return if not o.k.
*     Initialise looping flag
*     Do while no error obtaining the name and opening the output file
*       and maximum number of attempts not exceeded
*        Get file name and open file
*        If null returned then
*            Set flag so that a log file will not be created
*            Annul the error
*            Exit from the loop
*        Else if error occurred then
*            If abort requested, do so
*            Increment loop counter
*            If maximum number of attempts not exceeded then
*               Report error
*            Else
*               Set looping flag to exit
*            Endif
*            Cancel parameter used to get filename
*        Else
*            Set flag to indicate that the file has been opened
*            Set looping flag to false
*        Endif
*     Enddo
*     If error then
*        Report and abort
*     Endif
*     Return
*
*  Bugs :
*
*     None known.
*     -
*  Authors :
*
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*
*  History :
*
*     1989 Jul 25: Original (RL.STAR::CUR).
*     1990 Feb 20: Renamed from AIF_OPFIO (RAL::CUR).
*     1994 mar 1: Modified to return EXCLAIM (CARDIFF::GJP).
*
*  Type definitions :

      IMPLICIT  NONE            ! no implicit typing allowed

*  Global constants :
      INCLUDE  'SAE_PAR'        ! SSE global definitions
      INCLUDE  'PAR_ERR'        ! Parameter-system errors
      INCLUDE  'FIO_ERR'        ! FIO errors

*  Import :
      CHARACTER*(*) PNFILE      ! File Parameter Name
      CHARACTER*(*) ACMODE      ! File access mode
      CHARACTER*(*) FORM        ! Required form of carriagecontrol
      INTEGER RECSZ             ! File record size

*  Export :
      LOGICAL OPEN              ! File opened successfully
      LOGICAL EXCLAIM           ! File name was exclaimation
      INTEGER FD                ! File descriptor

*  Status :
      INTEGER STATUS

*  Local Variables :
      INTEGER MXLOOP            ! Maximum number of attempts at
! opening a data file
      PARAMETER ( MXLOOP=4 )
      INTEGER LOOP              ! Number of attempts to open the file
      LOGICAL LOOPAG            ! Loop again to open output file.
      CHARACTER *128 PATH,ALINE,PATH2
      INTEGER LPATH

*  External functions :
      INTEGER CHR_LEN

*.

*     Check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) RETURN

*     Initialise
      EXCLAIM = .FALSE.

*     If $FLUXPWD is set we use it, else we write to CWD
      CALL PSX_GETENV('FLUXPWD',PATH,STATUS)
      IF (STATUS .NE. SAI__OK) THEN
         CALL ERR_ANNUL( STATUS )
         CALL PSX_GETCWD( PATH, STATUS )
      END IF
      IF (STATUS .NE. SAI__OK) GOTO 999

*     Find its length.
      LPATH=CHR_LEN(PATH)
      LOOP=0
      LOOPAG=.TRUE.
      OPEN=.FALSE.
      DO WHILE (LOOPAG)

*     Access the file (appending original directory name).
         CALL PAR_GET0C(PNFILE,ALINE,STATUS)
         IF(ALINE(1:1).EQ.'/') THEN
            PATH2=ALINE
         ELSE
            PATH2=PATH(1:LPATH)//'/'//ALINE
         END IF
         CALL PAR_PUT0C(PNFILE,PATH2,STATUS)
         CALL FIO_ASSOC(PNFILE,ACMODE,FORM,RECSZ,FD,STATUS)

*     Check the status value.
         IF ( STATUS .EQ. PAR__NULL ) THEN
            OPEN=.FALSE.
            LOOPAG=.FALSE.
            EXCLAIM=.TRUE.
            CALL ERR_ANNUL( STATUS )
         ELSE IF ( STATUS .NE. SAI__OK ) THEN

            IF ( STATUS .EQ. PAR__ABORT ) GOTO 999

*     Here if filename is not allowed or file is not opened
*     - try again
*     Need to flush error here, as not quitting routine

            LOOP=LOOP + 1
            IF ( LOOP .LE. MXLOOP ) THEN
               CALL MSG_SETC( 'FILNAM', PNFILE )
               CALL ERR_REP( 'ERR_AIF_ASFIO_NOFI',
     :              'AIF_ASFIO: Could not open file $^FILNAM -'//
     :              ' try again',STATUS )
               CALL ERR_FLUSH( STATUS )
            ELSE

*     end looping as user is having serious problems

               LOOPAG=.FALSE.

            END IF

            CALL PAR_CANCL( PNFILE, STATUS )

         ELSE

*     no problem, so exit loop

            LOOPAG=.FALSE.
            OPEN=.TRUE.

*     end of file-opened-successfully check

         END IF

      END DO

*     abort for repeated error

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ERR_AIF_ASFIO_NOOPEN',
     :        'AIF_ASFIO: Repeatedly unable to open a file.', STATUS )
      END IF

 999  CONTINUE

      END





      SUBROUTINE CUSTOMFILT( REQBODY, FNAME, NF, NB, FREQ, HPBW1, HPBW2,
     :                       AMP1, AMP2, TBNORM, ERROR, INOTE, NOTE,
     :                       VALID, STATUS )
*+
*  Name:
*     CUSTOMFILT

*  Purpose:
*     Get the properties of a custom filter using environment parameters.

*  Invocation:
*     CALL CUSTOMFILT( REQBODY, FNAME, NF, NB, FREQ, HPBW1, HPBW2, AMP1, AMP2,
*                      TBNORM, ERROR, INOTE, NOTE, VALID, STATUS )

*  Description:
*     This routine gets the properties of a custom filter, using a set of
*     environment parameters, which correspond to the columns in the
*     scuba2.dat file. This is mainly intended for sitiuations where a
*     arbitrary centra frequency needs to be specified.

*  Arguments:
*     REQBODY = CHARACTER * ( * ) (Given)
*        Name of the planet for which fluxes are being calculated.
*     FNAME = CHARACTER * ( * ) (Returned)
*        The name of the custom filter. This will always be 'CUSTOM'.
*     NF = INTEGER (Returned)
*        The number of filtered defined by the returned arguments. This
*        will always be one.
*     NB = INTEGER (Returned)
*        The number of Gaussian components used to model the instrument beam.
*        Will be either 1 or 2.
*     FREQ = DOUBLE PRECISION (Returned)
*        FREQ be returned holding the central frequency of the
*        filter in GHz.
*     HPBW1 = DOUBLE PRECISION (Returned)
*        Half-power beam width of the first Gaussian component of the
*        beam, in arcsec.
*     HPBW2 = DOUBLE PRECISION (Returned)
*        Half-power beam width of the second Gaussian component of the
*        beam, in arcsec. Returned holding zero if NB is one.
*     AMP1 = DOUBLE PRECISION (Returned)
*        Amplitude of the first Gaussian component of the beam. Normalised
*        so that AMP1 plus AMP2 is one.
*     AMP2 = DOUBLE PRECISION (Returned)
*        Amplitude of the second Gaussian component of the beam. Normalised
*        so that AMP1 plus AMP2 is one. Returned equal to zero if NB is one.
*     TBNORM = DOUBLE PRECISION (Returned)
*        The numerical planetary brightness temperature at the required
*        frequency. This is obtained from environment parameter BTEMP,
*        which may give the numerical planetary brightness temperature
*        directly, or may be the name of a 1-dimensional NDF. If an NDF
*        name is given, it chould contain the required brightness temperature
*        as a function of frequency. The current WCS Frame should contain
*        a single spectral axis giving frerquency in units of GHz. The
*        NDF is used to look up the required temperature at the frequency
*        specified by parameter FREQ.
*     ERROR = DOUBLE PRECISION (Returned)
*        Error in TBNORM. Always returned equal to zero.
*     INOTE = INTEGER (Returned)
*        The number of elements returned in the NOTE array.
*     NOTE( 50 ) = CHARACTER*(*) (Returned)
*        The number of elements returned in the NOTE array.
*     VALID = LOGICAL (Returned)
*        Returned .FALSE. if any of the user-supplied values were
*        invalid, in which case all the environment parameters used by
*        this routine are cancelled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'GRP_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AST_PAR'
      INCLUDE 'CNF_PAR'
      INCLUDE 'PAR_ERR'

*  Arguments Given:
      CHARACTER REQBODY*(*)

*  Arguments Returned:
      CHARACTER FNAME(*)*(*)
      INTEGER NF
      INTEGER NB
      DOUBLE PRECISION FREQ(*)
      DOUBLE PRECISION HPBW1(*)
      DOUBLE PRECISION HPBW2(*)
      DOUBLE PRECISION AMP1(*)
      DOUBLE PRECISION AMP2(*)
      DOUBLE PRECISION TBNORM(*)
      DOUBLE PRECISION ERROR(*)
      INTEGER INOTE
      CHARACTER NOTE( 50 )*(*)
      LOGICAL VALID

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER TEXT*255
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      INTEGER DIM
      INTEGER IGRP
      INTEGER INDF
      INTEGER IPD
      INTEGER IWCS
      INTEGER NDIM
      INTEGER NEL
      LOGICAL FIRST
      REAL TB
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF( REQBODY .EQ. 'ALL' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'CUSTOM filters can only be used with a'//
     :                 'single planet - not ALL.', STATUS )
      END IF

      FIRST = .TRUE.

      FNAME(1) = 'CUSTOM'
      NF = 1
      ERROR(1) = 0.0D0

      CALL HELPER( 'NB', REQBODY, FIRST, STATUS )
      CALL PAR_GDR0I( 'NB', 1, 1, 2, .FALSE., NB, STATUS )

      CALL HELPER( 'FREQ', REQBODY, FIRST, STATUS )
      CALL PAR_GDR0D( 'FREQ', 1.0D0, 1.0D0, 1.0D5, .FALSE., FREQ,
     :                STATUS )

      CALL HELPER( 'HPBW1', REQBODY, FIRST, STATUS )
      CALL PAR_GDR0D( 'HPBW1', 0.0D0, 0.0D0, 1.0D3, .FALSE., HPBW1,
     :                STATUS )

      CALL HELPER( 'AMP1', REQBODY, FIRST, STATUS )
      CALL PAR_GDR0D( 'AMP1', -1.0D3, -1.0D3, 1.0D3, .FALSE., AMP1,
     :                STATUS )

      IF( NB .EQ. 2 ) THEN
         CALL HELPER( 'HPBW2', REQBODY, FIRST, STATUS )
         CALL PAR_GDR0D( 'HPBW2', 0.0D0, 0.0D0, 1.0D3, .FALSE., HPBW2,
     :                   STATUS )

         CALL HELPER( 'AMP2', REQBODY, FIRST, STATUS )
         CALL PAR_GDR0D( 'AMP2', -1.0D3, -1.0D3, 1.0D3, .FALSE., AMP2,
     :                   STATUS )
      END IF

      IF( REQBODY .NE. 'MARS' ) THEN
         CALL HELPER( 'BTEMP', REQBODY, FIRST, STATUS )
         CALL PAR_GET0C( 'BTEMP', TEXT, STATUS )
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_CTOD( TEXT, TBNORM(1), STATUS )
            IF( STATUS .NE. SAI__OK ) THEN
               STATUS = SAI__OK
               CALL NDF_FIND( DAT__ROOT, TEXT, INDF, STATUS )
               CALL NDF_MAP( INDF, 'DATA', '_REAL', 'READ', IPD, NEL,
     :                       STATUS )
               CALL NDF_DIM( INDF, 1, DIM, NDIM, STATUS )
               CALL NDF_GTWCS( INDF, IWCS, STATUS )
               IF( AST_GETC( IWCS, 'UNIT(1)', STATUS ) .EQ. 'GHz' ) THEN
                  CALL AST_TRAN1( IWCS, 1, FREQ, .FALSE., X, STATUS )
                  CALL INTERP( DIM, X, %VAL(CNF_PVAL(IPD)), TBNORM,
     :                         STATUS )
               ELSE IF( STATUS .EQ. SAI__OK ) THEN
                  STATUS = SAI__ERROR
                  CALL NDF_MSG( 'N', INDF )
                  CALL MSG_SETC( 'U', AST_GETC( IWCS, 'UNIT(1)',
     :                                          STATUS ) )
                  CALL ERR_REP( ' ', 'Supplied NDF ''^N'' has axis '//
     :                          'units ''^U'' - must be ''GHz''.',
     :                          STATUS )
               END IF

               CALL AST_ANNUL( IWCS, STATUS )
               CALL NDF_ANNUL( INDF, STATUS )
            END IF
         END IF
      END IF

      INOTE = 0
      IF( STATUS .EQ. SAI__OK ) THEN
         IGRP = GRP__NOID
         CALL KPG1_GTGRP( 'NOTE', IGRP, INOTE, STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            INOTE = MIN( INOTE, 50 )
            CALL GRP_GET( IGRP, 1, INOTE, NOTE, STATUS )
            CALL GRP_DELET( IGRP, STATUS )
         END IF
      END IF

      VALID = ( STATUS .EQ. SAI__OK )

      END


*  Issue a helpful message if required. Used when getting the details of
*  a custom filter.
      SUBROUTINE HELPER( PARAM, REQBODY, FIRST, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'

      CHARACTER PARAM*(*), REQBODY*(*)
      LOGICAL FIRST
      INTEGER STATUS, STATE

      CALL PAR_STATE( PARAM, STATE, STATUS )
      IF(  STATE .NE. PAR__ACTIVE .AND. FIRST ) THEN
         CALL MSG_SETC( 'P', REQBODY )
         CALL MSG_OUT( ' ', 'Please enter details of custom filter'//
     :                 ' for ^P:', STATUS )
         FIRST = .FALSE.
      END IF

      END

      SUBROUTINE INTERP( DIM, X, ARRAY, TBNORM, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      INTEGER DIM
      DOUBLE PRECISION X
      REAL ARRAY( * )
      DOUBLE PRECISION TBNORM
      INTEGER STATUS

      REAL A
      INTEGER LO

      IF( STATUS .NE. SAI__OK ) RETURN

      IF( X .LE. 1.0D0 ) THEN
         TBNORM = ARRAY( 1 )
      ELSE IF( X .GE. DBLE( DIM ) ) THEN
         TBNORM = ARRAY( DIM )
      ELSE
         LO = INT( X )
         A = X - LO
         TBNORM = ( 1.0D0 - A )*ARRAY( LO ) + A*ARRAY( LO + 1 )
      END IF

      END



************************************************************************
*     END OF PROGRAM
************************************************************************
