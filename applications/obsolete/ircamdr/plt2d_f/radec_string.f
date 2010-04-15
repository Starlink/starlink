	SUBROUTINE RADEC_STRING( J, RA, DEC, POSTYPE, XINTERVAL, YINTERVAL,
     :	                         ARCSEC_PIXEL, NUMTYPE_RA, NUMTYPE_DEC,
     :	                         POS_RA, POS_DEC, STRING_RA, STRING_DEC)

* Description : Subroutine to for text strings from the input RA and DEC
*               positions. The RA and DEC are input as real numbers and
*               the output text string is in the x,y,z.z, a,b,c
*
* HISTORY
*  22-Jul-1994 Changed LIB$ calls to CHR_, IFIX to INT (SKL@JACH)
*

	IMPLICIT NONE

        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'
	INCLUDE 'PLT2DCOM'

	INTEGER DECD
	INTEGER DECDS
	INTEGER DECM
	INTEGER DECS
	INTEGER DECTOP
	INTEGER J
	INTEGER L1
	INTEGER L2
	INTEGER L3
	INTEGER L4
	INTEGER POS_RA( 5)
	INTEGER POS_DEC( 5)
	INTEGER RADS
	INTEGER RAH
	INTEGER RAM
	INTEGER RAS
	INTEGER RATOP

	REAL COSDELTA
	REAL DECDIFF
	REAL DECPOS
	REAL RADIFF
	REAL RAPOS
	REAL XINTERVAL
	REAL YINTERVAL
	REAL ARCSEC_PIXEL
	REAL DEC
	REAL RA
        REAL XRADS
        REAL XDECDS

	CHARACTER*(*) NUMTYPE_DEC
	CHARACTER*(*) NUMTYPE_RA
	CHARACTER*(*) POSTYPE
	CHARACTER*20 ST1
	CHARACTER*20 ST2
	CHARACTER*20 ST3
	CHARACTER*20 ST4
	CHARACTER*80 STRING_RA
	CHARACTER*80 STRING_DEC

	SAVE RATOP
	SAVE DECTOP

	COSDELTA = COS( DEC*3.141592654/180.0)

*      Calculate the value to be added/subtracted to form the new RA

	RADIFF = XINTERVAL*( J-1)*ARCSEC_PIXEL/( 3600.0*15.0)

*      Correct the ra difference for the declination : by cos( delta)

	IF( COSDELTA .GT. 0.0) THEN
	  RADIFF = RADIFF/COSDELTA
	ELSE
	  RADIFF = RADIFF/0.001
	END IF

*      Test if numbers are going positive or negative

	IF( POSTYPE .EQ. 'POSITIVE') THEN

*        Subtract the difference

	  RAPOS = RA - RADIFF
	ELSE

*        Add the difference

	  RAPOS = RA + RADIFF
	END IF

*      Test if the new RA is less than 0 i.e. the value has gone to 23hrs

	IF( RAPOS .LT. 0.0) THEN
	  RAPOS = RAPOS + 24
	END IF

*      Test if the new RA is greater than 24 i.e. the value has gone thorugh 0

	IF( RAPOS .GT. 24.0) THEN
	  RAPOS = RAPOS - 24
	END IF

*      Calculate the hrs,mins,secs,dsecs

	RAH = INT( RAPOS)
	RAM = INT( ( RAPOS - RAH)*60.0)
	RAS = INT( ( RAPOS - RAH - RAM/60.0)*3600.0)

	IF( POSTYPE .EQ. 'POSITIVE') THEN
	  XRADS = ( RAPOS - RAH - RAM/60.0 - RAS/3600.0)*36000.0
D          print *, 'rads = ', rads
	  RADS = IFIX( RADS/10.0 + 0.5)
	ELSE
	  XRADS = ( RAPOS - RAH - RAM/60.0 - RAS/3600.0)*36000.0
D          print *, 'rads = ', rads
	  RADS = IFIX( RADS/10.0 + 0.5)
	END IF
D	print *, 'rah,ram,ras,rads = ', rah, ram, ras, rads

*      Test if the dsecs are full secs and correct if it is

	IF( RADS .EQ. 10) THEN
	  RADS = RADS - 10
	  RAS = RAS + 1
	  IF( RAS .EQ. 60) THEN
	    RAS = RAS - 60
	    RAM = RAM + 1
	    IF( RAM .EQ. 60) THEN
	      RAM = RAM - 60
	      RAH = RAH + 1
	      IF( RAH .EQ. 24) THEN
	        RAH = RAH - 24
	      END IF
	    END IF
	  END IF
	END IF

*      Convert the hrs,mins,secs,dsecs to strings

        CALL CHR_ITOC( RAH, ST1, L1 )
        CALL CHR_ITOC( RAM, ST2, L2 )
        CALL CHR_ITOC( RAS, ST3, L3 )
        CALL CHR_ITOC( RADS, ST4, L4 )

*      Test if this is the first position, if it is then return full RA and
*      set the first position variable

	IF( J .EQ. 1) THEN
	  RATOP = RAH

*        Call subroutine to form the full RA string

	  NUMTYPE_RA = 'FULL'

	  CALL RADEC_CONCAT1( ST1( 1:L1), ST2( 1:L2), ST3( 1:L3),
     :	                      ST4( 1:L4), POS_RA, STRING_RA)

	ELSE

*        Test if the current ra is different in hrs from the start ra and
*        then execute either the full or partial ra subroutine

	  IF( RATOP .NE. RAH) THEN
	    RATOP = RAH

*        Call subroutine to form the full RA string

	    NUMTYPE_RA = 'PARTIAL'

	    CALL RADEC_CONCAT2( ST2( 1:L2), ST3( 1:L3), ST4( 1:L4),
     :	                        POS_RA, STRING_RA)

	  ELSE

*        Call subroutine to form the partial RA string

	    NUMTYPE_RA = 'PARTIAL'

	    CALL RADEC_CONCAT2( ST2( 1:L2), ST3( 1:L3), ST4( 1:L4),
     :	                        POS_RA, STRING_RA)

	  END IF
	END IF

*      Test what type of incrementing wanted

	IF( POSTYPE .EQ. 'POSITIVE') THEN

*        Calculate the value to be subtracted to form the new DEC

	  DECDIFF = YINTERVAL*(J-1)*ARCSEC_PIXEL/( 3600.0)

*        Add the difference

	  DECPOS = DEC + DECDIFF

	ELSE

*        Calculate the value to be subtracted to form the new DEC

	  DECDIFF = YINTERVAL*(J-1)*ARCSEC_PIXEL/( 3600.0)

*        Subtract the difference

	  DECPOS = DEC - DECDIFF

	END IF

*      Calculate the degs.mins.secs,dsecs

	DECD = INT( DECPOS)
	DECM = INT( ( DECPOS - DECD)*60.0)
	DECS = INT( ( DECPOS - DECD - DECM/60.0)*3600.0)

	IF( DECD .GT. 0.0) THEN
	  XDECDS = ( DECPOS - DECD - DECM/60.0 - DECS/3600.0)*36000.0
	  DECDS = IFIX( XDECDS/10.0 + 0.5)
	ELSE
	  XDECDS = ( DECPOS - DECD - DECM/60.0 - DECS/3600.0)*36000.0
	  DECDS = IFIX( XDECDS + 0.5)
	END IF

*      Test if the dsecs are full secs and correct if it is

	IF( DECDS .EQ. 10) THEN
	  DECDS = DECDS - 10
	  DECS = DECS + 1
	  IF( DECS .EQ. 60) THEN
	    DECS = DECS - 60
	    DECM = DECM + 1
	    IF( DECM .EQ. 60) THEN
	      DECM = DECM - 60
	      DECD = DECD + 1
	    END IF
	  END IF
	END IF

	IF( DECDS .EQ. -10) THEN
	  DECDS = DECDS + 10
	  DECS = DECS - 1
	  IF( DECS .EQ. -60) THEN
	    DECS = DECS + 60
	    DECM = DECM - 1
	    IF( DECM .EQ. -60) THEN
	      DECM = DECM + 60
	      DECD = DECD - 1
	    END IF
	  END IF
	END IF

*      Test for negative dec

	IF( DECD .LT. 0.0) THEN

	  DECM = ABS( DECM)
	  DECS = ABS( DECS)
	  DECDS = ABS( DECDS)

	END IF

*      Convert the degs,mins,secs,dsecs to strings

        CALL CHR_ITOC( DECD, ST1, L1 )
        CALL CHR_ITOC( DECM, ST2, L2 )
        CALL CHR_ITOC( DECS, ST3, L3 )
        CALL CHR_ITOC( DECDS, ST4, L4 )

*      Test if this is the first position, if it is then return full DEC and
*      set the first position variable

	IF( J .EQ. 1) THEN

	  DECTOP = DECD

*        Call subroutine to form the full DEC string

	  NUMTYPE_DEC = 'FULL'

	  CALL RADEC_CONCAT1( ST1( 1:L1), ST2( 1:L2), ST3( 1:L3),
     :	                      ST4( 1:L4), POS_DEC, STRING_DEC)

	ELSE

*        Test if the current dec is different in hrs from the start dec and
*        then execute either the full or partial dec subroutine

	  IF( DECTOP .NE. DECD) THEN

	    DECTOP = DECD

*        Call subroutine to form the full DEC string

	    NUMTYPE_DEC = 'PARTIAL'

	    CALL RADEC_CONCAT2( ST2( 1:L2), ST3( 1:L3), ST4( 1:L4),
     :	                        POS_DEC, STRING_DEC)

	  ELSE

*        Call subroutine to form the partial DEC string

	    NUMTYPE_DEC = 'PARTIAL'

	    CALL RADEC_CONCAT2( ST2( 1:L2), ST3( 1:L3), ST4( 1:L4),
     :	                        POS_DEC, STRING_DEC)

	  END IF

	END IF

	END
