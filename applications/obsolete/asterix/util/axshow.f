*+  AXSHOW - Display axes of a dataset
      SUBROUTINE AXSHOW( STATUS )
*    Description :
*    Environment parameters :
*
*     INP = UNIV(R)
*           Input object
*     DEV = CHAR(R)
*           Output device
*
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      8 May 91 : V1.4-0  Original (DJA)
*      4 May 94 : V1.7-0  Use AIO_ for output (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*     28 Mar 95 : V1.8-1  Use new data interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*80              LABEL, UNITS      	! Axis attributes
      CHARACTER*132		OBUF			! Output buffer
      CHARACTER*30              RSTR              	! Axis range description
      CHARACTER*20              WSTR              	! Axis width description

      REAL                      LO, HI            	! Axis range
      REAL                      BASE, SCALE       	! Regular axis parameters
      REAL                      WIDTH             	! Axis width

      INTEGER                   DEVWID            	! Device width
      INTEGER                   FSTAT             	! i/o status code
      INTEGER                   I                 	! Loop over dimensions
      INTEGER			IFID			! Input dataset id
      INTEGER                   OCH               	! Output channel
      INTEGER                   NAX               	! Number of axes
      INTEGER                   NVAL              	! Number of axis values
      INTEGER                   NWID              	! Number of axis widths
      INTEGER                   PTR               	! Ptr to mapped component
      INTEGER                   TLEN              	! Text length

      LOGICAL                   INPRIM            	! Input primitive?
      LOGICAL                   OK, WOK           	! Input objects ok?
      LOGICAL                   REG               	! Regular axis values?
      LOGICAL                   UNIF              	! Uniform axis widths?
*
*    Local data :
*
      CHARACTER*1               TRUTH(-1:1)
      DATA                      TRUTH/'Y','N','Y'/
*
*    Version :
*
      CHARACTER*30		VERSION
        PARAMETER 		( VERSION = 'AXSHOW Version 1.8-1' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CALL AST_INIT

*    Version id
      CALL MSG_PRNT( VERSION )

*    Get input object
      CALL USI_TASSOCI( 'INP', '*', 'READ', IFID, STATUS )
      CALL BDI_PRIM( IFID, INPRIM, STATUS )
      IF ( INPRIM ) THEN
        CALL MSG_PRNT( 'Primitive input object - no axes present' )
        STATUS = SAI__OK
      ELSE

*      Set up output channel
        CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, DEVWID, STATUS )

*      Get number of axes
        CALL BDI_CHKAXES( IFID, NAX, STATUS )

*      Heading
        WRITE( OBUF, '(1X,A,T67,A)' ) 'Axis Label                '/
     :            /'   Size  Range', 'Reg Widths'
        CALL AIO_WRITE( OCH, OBUF, STATUS )
        CALL AIO_BLNK( OCH, STATUS )

*      Loop over axes and print out data
        DO I = 1, NAX

*        Get label and units
          CALL BDI_GETAXLABEL( IFID, I, LABEL, STATUS )
          CALL BDI_GETAXUNITS( IFID, I, UNITS, STATUS )

*        Get dimension
          CALL BDI_CHKAXVAL( IFID, I, OK, REG, NVAL, STATUS )

*        Construct range string
          IF ( REG ) THEN
            CALL BDI_GETAXVAL( IFID, I, BASE, SCALE, NVAL, STATUS )
            LO = BASE
            HI = BASE + FLOAT(NVAL-1)*SCALE
          ELSE
            CALL BDI_MAPAXVAL( IFID, 'READ', I, PTR, STATUS )
            CALL ARR_ELEM1R( PTR, NVAL, 1, LO, STATUS )
            CALL ARR_ELEM1R( PTR, NVAL, NVAL, HI, STATUS )
            CALL BDI_UNMAPAXVAL( IFID, I, STATUS )
          END IF
          CALL MSG_SETR( 'LO', LO )
          CALL MSG_SETR( 'HI', HI )
          CALL MSG_SETC( 'UNITS', UNITS )
          CALL MSG_MAKE( '^LO to ^HI ^UNITS', RSTR, TLEN )

*        Widths present
          CALL BDI_CHKAXWID( IFID, I, WOK, UNIF, NWID, STATUS )
          IF ( WOK ) THEN
            IF ( UNIF ) THEN
              CALL BDI_GETAXWID( IFID, I, WIDTH, STATUS )
              CALL MSG_SETR( 'WID', WIDTH )
            ELSE
              CALL MSG_SETC( 'WID', 'Non-uniform' )
            END IF
          ELSE
            CALL MSG_SETC( 'WID', 'Absent' )
          END IF
          CALL MSG_MAKE( '^WID', WSTR, TLEN )

*        Write to output
          WRITE( OBUF, '(1X,I3,2X,A22,I6,2X,A,T67,1X,A1,2X,A)',
     :        IOSTAT=FSTAT ) I, LABEL, NVAL, RSTR, TRUTH(REG), WSTR
          CALL AIO_WRITE( OCH, OBUF, STATUS )

        END DO

*      Free device
        CALL AIO_CANCL( 'DEV', STATUS )

      END IF

*    Release dataset
      CALL BDI_RELEASE( IFID, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
