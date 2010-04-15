*+  POLLY2 - calculates polarization from 8 (dual beam) input intensities

      SUBROUTINE POLLY2 ( STATUS )

*    Description :
*
*    Invocation :
*
*     CALL POLLY ( STATUS )
*
*    Parameters :
*
*     INTEN1  =  REAL( READ )
*           o- intensity at 0 DEGREES
*     INTEN2  =  REAL( READ )
*           e- intensity at 0 DEGREES
*     INTEN3  =  REAL( READ )
*           o- intensity at 45 DEGREES
*     INTEN4  =  REAL( READ )
*           e- intensity at 45 DEGREES
*     INTEN5  =  REAL( READ )
*           o- intensity at 22.5 DEGREES
*     INTEN6  =  REAL( READ )
*           e- intensity at 22.5  DEGREES
*     INTEN7  =  REAL( READ )
*           o- intensity at 67.5 DEGREES
*     INTEN8  =  REAL( READ )
*           e- intensity at 67.5 DEGREES
*     ELDN    =  REAL( READ )
*           electrons/dn in data (for shot-noise)
*     ZP      = REAL( READ )
*           zeropoijt for magnitude calculation
*
*    Method :
*
*     Check status on entry - return if not o.k.
*     Get input intensities (8) and electrons/dn
*     If no error then
*       calculate polarization and tell user result
*     Endif
*     Return
*
*    Authors :
*
*     Colin Aspin (JACH::CAA)
*
*    History :
*
*     19-10-1988 : Original version (JACH::CAA)
*     23-08-1995 : created this version for dual-beam pol from POLLY
*
*    Type Definitions :

      IMPLICIT NONE                 ! no default typing allowed

*    Global constants :

      INCLUDE 'SAE_PAR'             ! SSE global definitions

*    Status :

      INTEGER STATUS                ! global status parameter

*    Local Constants :

      REAL
     :	  MINVAL,
     :	  MAXVAL

      PARAMETER ( MINVAL = -1.0E-37)
      PARAMETER ( MAXVAL = 1.0E37)

*    Local variables :

      REAL
     :	  OINTEN0,
     :	  EINTEN0,
     :	  OINTEN45,
     :	  EINTEN45,
     :	  OINTEN22,
     :	  EINTEN22,
     :	  OINTEN67,
     :	  EINTEN67,
     :	  ELDN,
     :	  I,
     :	  Q,
     :	  U,
     :	  P,
     :	  T,
     :	  PE,
     :	  TE,
     :	  ZP,
     :	  MAG

*-
*    check status on entry - return if not o.k.
      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      END IF

*    get a 8 intensity values from user
      CALL AIF_GET0R( 'OINTEN0', 0.0, MINVAL, MAXVAL, OINTEN0,
     :	STATUS )
      CALL AIF_GET0R( 'EINTEN0', 0.0, MINVAL, MAXVAL, EINTEN0,
     :	STATUS )
      CALL AIF_GET0R( 'OINTEN45', 0.0, MINVAL, MAXVAL, OINTEN45,
     :	STATUS )
      CALL AIF_GET0R( 'EINTEN45', 0.0, MINVAL, MAXVAL, EINTEN45,
     :	STATUS )
      CALL AIF_GET0R( 'OINTEN22', 0.0, MINVAL, MAXVAL, OINTEN22,
     :	STATUS )
      CALL AIF_GET0R( 'EINTEN22', 0.0, MINVAL, MAXVAL, EINTEN22,
     :	STATUS )
      CALL AIF_GET0R( 'OINTEN67', 0.0, MINVAL, MAXVAL, OINTEN67,
     :	 STATUS )
      CALL AIF_GET0R( 'EINTEN67', 0.0, MINVAL, MAXVAL, EINTEN67,
     :	STATUS )

*    get electrons/dn value
      CALL AIF_GET0R( 'ELDN', 6.0, MINVAL, MAXVAL, ELDN, STATUS )

*    get zeropoint value
      CALL AIF_GET0R( 'ZP', 0.0, MINVAL, MAXVAL, ZP, STATUS )

*    check for error
      IF( STATUS .EQ. SAI__OK ) THEN

	CALL POL2_STOKESCAL( OINTEN0, EINTEN0, OINTEN45, EINTEN45, Q)
	CALL POL2_STOKESCAL( OINTEN22, EINTEN22, OINTEN67, EINTEN67, U)
 	CALL POL2_POLCAL( Q, U, P)
	CALL POL2_THETACAL( Q, U, T)
	CALL POL2_ERRCAL( OINTEN0, EINTEN0, OINTEN45, EINTEN45,
     :	                  OINTEN22, EINTEN22, OINTEN67, EINTEN67,
     :	                  P, ELDN, PE, TE)
	CALL POL2_INTCAL( OINTEN0, EINTEN0, OINTEN45, EINTEN45,
     :	                  OINTEN22, EINTEN22, OINTEN67, EINTEN67,
     :	                  I)

	MAG = ZP - 2.5*LOG10( I)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

	CALL MSG_SETR( 'OI1', OINTEN0)
	CALL MSG_OUT( 'MESS', 'o- intensity at  0 degrees           = ^OI1',
     :	  STATUS)
	CALL MSG_SETR( 'EI1', EINTEN0)
	CALL MSG_OUT( 'MESS', 'e- intensity at  0 degrees           = ^EI1',
     :	  STATUS)
	CALL MSG_SETR( 'OI2', OINTEN45)
	CALL MSG_OUT( 'MESS', 'o- intensity at 45 degrees           = ^OI2',
     :	  STATUS)
	CALL MSG_SETR( 'EI2', EINTEN45)
	CALL MSG_OUT( 'MESS', 'e- intensity at 45 degrees           = ^EI2',
     :	  STATUS)
	CALL MSG_SETR( 'OI3', OINTEN22)
	CALL MSG_OUT( 'MESS', 'o- intensity at 22 degrees           = ^OI3',
     :	  STATUS)
	CALL MSG_SETR( 'EI3', EINTEN22)
	CALL MSG_OUT( 'MESS', 'e- intensity at 22 degrees           = ^EI3',
     :	  STATUS)
	CALL MSG_SETR( 'OI4', OINTEN67)
	CALL MSG_OUT( 'MESS', 'o- intensity at 67 degrees           = ^OI4',
     :	  STATUS)
	CALL MSG_SETR( 'EI4', EINTEN67)
	CALL MSG_OUT( 'MESS', 'e- intensity at 67 degrees           = ^EI4',
     :	  STATUS)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

	CALL MSG_SETR( 'I', I)
	CALL MSG_OUT( 'MESS', 'Total intensity                   = ^I',
     :	  STATUS)
	CALL MSG_SETR( 'Z', ZP)
	CALL MSG_OUT( 'MESS', 'Zeropoint                         = ^Z',
     :	  STATUS)
	CALL MSG_SETR( 'M', MAG)
	CALL MSG_OUT( 'MESS', 'Magnitude                         = ^M',
     :	  STATUS)
	CALL MSG_SETR( 'Q', Q)
	CALL MSG_OUT( 'MESS', 'Q-Stokes parameter (%)            = ^Q',
     :	  STATUS)
	CALL MSG_SETR( 'U', U)
	CALL MSG_OUT( 'MESS', 'U-Stokes parameter (%)            = ^U',
     :	  STATUS)
	CALL MSG_SETR( 'P', P)
	CALL MSG_OUT( 'MESS', 'Percentage polarization (%)       = ^P',
     :	  STATUS)
!	CALL MSG_SETR( 'PE', PE)
!	CALL MSG_OUT( 'MESS', 'Polarization shot-noise error (%) = ^PE',
!     :	  STATUS)
	CALL MSG_SETR( 'T', T)
	CALL MSG_OUT( 'MESS', 'Position angle (degrees)          = ^T',
     :	  STATUS)
!	CALL MSG_SETR( 'TE', TE)
!	CALL MSG_OUT( 'MESS', 'Position angle error (degrees)    = ^TE',
!     :	  STATUS)

	CALL MSG_OUT( 'BLANK' ,' ', STATUS)

*    end of if-no-error-getting-input-values check
      END IF

*    end
      END
