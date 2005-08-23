*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused SIGN
*-----------------------------------------------------------------------

      SUBROUTINE MAKE_XTITLE (XTITLE)

* Routine to generate a suitable label for the X-axis of a 1-D
* plot. Must include all relevant information on velocity scales,
* frame and velocity scaling law.

      IMPLICIT   NONE

*     Formal parameters

      CHARACTER  XTITLE*(*)      ! returned, answer

*     Include files

      INCLUDE   'STACKCOMM'
      INCLUDE   'FLAGCOMM'

*     Functions

      INTEGER     GEN_ILEN

*     Local variables

      INTEGER     IL1, ILD, ILV, ILR
      INTEGER     IXN, IXU
      INTEGER     IERR
      INTEGER     VELDAT
      CHARACTER   OFFSETV*20
      CHARACTER   NULL10*10
      CHARACTER   VELOCITY_REF*12
      CHARACTER   VELOCITY_DEF*12
      CHARACTER   VREF(4)*12
      CHARACTER   VDEF(3)*12


      DATA NULL10 / ' ' /
      DATA VREF  /'Telluric    ', 'LSR         ',
     &            'Heliocentric', 'Geocentric  '/
      DATA VDEF  /'Radio       ', 'Optical     ',
     &            'Relativistic'/
* Ok, go...

      IXN = GEN_ILEN (XAXIS_NAME)
      IXU = GEN_ILEN (XAXIS_UNITS)

*     Easy to make title if it is not vel or freq...

      IF (NXS.EQ.1 .OR. NXS.EQ.4) THEN
        XTITLE = XAXIS_NAME(:IXN) // '  (' // XAXIS_UNITS(:IXU) // ')'

*     Otherwise first work out which frame to display it in and get
*     common English words to describe it

      ELSE

        IF (CHANGE_FRAME) THEN
          CALL VELENCODE (VEL_REF, VEL_DEF, VELDAT)
        ELSE
          VELDAT = LSRFLG
        END IF

        VELOCITY_REF = VREF(VELDAT - 16*(VELDAT/16) + 1)
        VELOCITY_DEF = VDEF(VELDAT/16 + 1)

*       For the case of a velocity reference not equal to 0, produce
*       an informative bit of label

        OFFSETV = ' '

        IF (VELOUT.NE.0.0 .AND. CHANGE_FRAME) THEN

          WRITE (OFFSETV, '(1X,G13.4)', IOSTAT=IERR) VELOUT

          IL1 = 1
          ILV = MAX (GEN_ILEN (OFFSETV), 1)
          DO WHILE (OFFSETV(IL1+1:IL1+1).EQ.' ' .AND. IL1.LT.ILV)
            IL1 = IL1+1
          END DO
          OFFSETV(ILV+1:ILV+5) = ' km/s'
          ILV = ILV+5

          IF (VELOUT.GT.0.0) THEN
            OFFSETV(IL1:IL1) = '+'
          ELSE
            OFFSETV(IL1:IL1) = '-'
          END IF

        ELSE
          IL1 = 1
          ILV = 1
        END IF

        ILD = GEN_ILEN (VELOCITY_DEF)
        ILR = GEN_ILEN (VELOCITY_REF)

*       Now stick it all together for vel or freq case

        XTITLE = XAXIS_NAME(:IXN) //  ' / ('  // XAXIS_UNITS(:IXU) //
     &           ')     ' // VELOCITY_REF(:ILR) // ' frame ' //
     &           OFFSETV(IL1:ILV) // '  (' // VELOCITY_DEF(:ILD) //
     &          ' Def''n)'
      END IF

CD    PRINT *, ' -- make_xtitle --'
CD    PRINT *, '    VELOCITY_REF : ', VELOCITY_REF
CD    PRINT *, '    XAXIS_NAME   : ', XAXIS_NAME(:IXN)
CD    PRINT *, '    OFFSETV      : ', OFFSETV(IL1:ILV)
CD    PRINT *, '    XAXIS_UNITS  : ', XAXIS_UNITS(:IXU)
CD    PRINT *, '    VELOCITY_DEF : ', VELOCITY_DEF(:ILD)
CD    PRINT *, '    New X-axis title is: ', XTITLE

      RETURN
      END

*-----------------------------------------------------------------------
