*+  CONV_RA - converts RA in string form to DP degrees
	SUBROUTINE CONV_RA(RA,RAD,STATUS)
*
*    Description :
*
*         RA/DEC is input in one of the following string forms and
*         output as DOUBLE PREC degrees
*
*             235.789        decimal degrees
*               2.56879rad   radian  (r is sufficient)
*             12:34:24.5     hms/dms
*             12:34.465
*             12 34 24.5     hms/dms
*             12h34m24.5s    hms
*             -12d45m20.5s   dms
*              -12d45.34
*
*    History :
*
*     ?? ??? ?? : Original (RJV)
*     16 Aug 93 : Fixed bugs handling HH MM.M type input (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
       INCLUDE 'SAE_PAR'
*
*    Import :
*
      CHARACTER*(*) RA
*
*    Export :
*
      DOUBLE PRECISION RAD
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*20 RAS
      INTEGER LRA
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  make local copy and convert to lower case
        RAS=RA
        CALL CHR_LCASE(RAS)

*  remove any leading spaces
        CALL CHR_LDBLK( RAS )

* get used lengths of strings
        LRA=CHR_LEN(RAS)

*  RA

*  radian
        IF (INDEX(RAS,'r').NE.0) THEN
          CALL CONV_RADEC_RADIAN(RAS,RAD,STATUS)

*  h m s
        ELSEIF (INDEX(RAS,'h').NE.0) THEN
          CALL CONV_RADEC_HMS1(RAS,RAD,STATUS)

*  : : :
        ELSEIF (INDEX(RAS,':').NE.0) THEN
          CALL CONV_RADEC_HMS2(RAS,':',RAD,STATUS)

*  spaces
        ELSEIF (INDEX(RAS(:LRA),' ').NE.0) THEN
          CALL CONV_RADEC_HMS2(RAS,' ',RAD,STATUS)

*  decimal
        ELSE
          CALL CONV_RADEC_DECIMAL(RAS,RAD,STATUS)

        ENDIF


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_RA',STATUS)
        ENDIF

      ENDIF

      END
