*+  CONV_DEC - converts DEC in string form to DP degrees
	SUBROUTINE CONV_DEC(DEC,DECD,STATUS)
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
      CHARACTER*(*) DEC
*
*    Export :
*
      DOUBLE PRECISION DECD
*    Status :
      INTEGER STATUS
*    Functions :
      INTEGER CHR_LEN
*    Local variables :
      CHARACTER*20 DECS
      INTEGER LDEC
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  make local copy and convert to lower case
        DECS=DEC
        CALL CHR_LCASE(DECS)

*  remove any leading spaces
        CALL CHR_LDBLK( DECS )

* get used lengths of strings
        LDEC=CHR_LEN(DECS)


*  radian
        IF (INDEX(DECS,'r').NE.0) THEN
          CALL CONV_RADEC_RADIAN(DECS,DECD,STATUS)


*  d m s
        ELSEIF (INDEX(DECS,'d').NE.0) THEN
          CALL CONV_RADEC_DMS1(DECS,DECD,STATUS)

*  : : :
        ELSEIF (INDEX(DECS,':').NE.0) THEN
          CALL CONV_RADEC_DMS2(DECS,':',DECD,STATUS)

*  spaces
        ELSEIF (INDEX(DECS(:LDEC),' ').NE.0) THEN
          CALL CONV_RADEC_DMS2(DECS,' ',DECD,STATUS)

*  decimal
        ELSE
          CALL CONV_RADEC_DECIMAL(DECS,DECD,STATUS)

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from CONV_DEC',STATUS)
        ENDIF

      ENDIF

      END
