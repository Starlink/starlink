
      SUBROUTINE ADI2_IMGCNV ( PTR, TYPE, DTYPE, NDIM, DIMS, STATUS )

      INTEGER			PTR
      INTEGER			STATUS
      INTEGER			NDIM, DIMS(*)
      CHARACTER*(*)		TYPE, DTYPE
      INTEGER			NELM
      INTEGER			NID, NPTR
c     CHARACTER*20		TEXT
c     INTEGER			NDM, DM(7)

      CALL ADI_NEW( DTYPE, NDIM, DIMS, NID, STATUS )
      CALL ADI_MAP( NID, DTYPE, 'WRITE', NPTR, STATUS )

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Do we need to bother?
      IF ( TYPE .EQ. DTYPE ) GOTO 99

*  Hard-coded image array - needs malloc-ing in future
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )
      IF ( NELM .GT. 512*512 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP ( 'ADI2_IMGCNV', 'Can''t cope with more than 512*512 elements', STATUS )
      END IF

*  Switch on the final data type
      IF ( DTYPE .EQ. 'REAL' ) THEN
        CALL ADI2_IC2R( PTR, TYPE, NELM, STATUS )
      ELSE IF ( DTYPE .EQ. 'DOUBLE' ) THEN
        CALL ADI2_IC2D( PTR, TYPE, NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', DTYPE )
        CALL ERR_REP( 'ADI2_IMGCNV', 'Can''t cope with final data type ^T', STATUS )
      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IMGCNV', STATUS )
      END IF

      END


      SUBROUTINE ADI2_IC2R( PTR, TYPE, NELM, STATUS )

      INTEGER			PTR
      INTEGER			NELM
      INTEGER			STATUS
      CHARACTER*(*)		TYPE
      REAL			DATA(512*512)

      INTEGER			UTIL_PLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on initial data type
      IF ( TYPE .EQ. 'WORD' ) THEN
        CALL ADI2_ICW2R( %VAL(PTR), DATA(1:NELM), NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI2_IC2R', 'Can''t cope with initial data type ^T', STATUS )
      END IF

*  Swap over the data pointers
      PTR = UTIL_PLOC( DATA )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IC2R', STATUS )
      END IF

      END


      SUBROUTINE ADI2_ICW2R( DIN, DOUT, NELM, STATUS )

      INTEGER*2			DIN(*)
      REAL			DOUT(*)
      INTEGER			NELM
      INTEGER			STATUS
      INTEGER			I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = REAL(DIN(I))
      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_ICW2R', STATUS )
      END IF

      END


      SUBROUTINE ADI2_IC2D( PTR, TYPE, NELM, STATUS )

      INTEGER			PTR
      INTEGER			NELM
      INTEGER			STATUS
      CHARACTER*(*)		TYPE
      DOUBLE PRECISION		DATA(512*512)

      INTEGER			UTIL_PLOC

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on initial data type
      IF ( TYPE .EQ. 'WORD' ) THEN
        CALL ADI2_ICW2D( %VAL(PTR), DATA(1:NELM), NELM, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ADI2_IC2D', 'Can''t cope with initial data type ^T', STATUS )
      END IF

*  Swap over the data pointers
      PTR = UTIL_PLOC( DATA )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_IC2D', STATUS )
      END IF

      END


      SUBROUTINE ADI2_ICW2D( DIN, DOUT, NELM, STATUS )

      INTEGER*2			DIN(*)
      DOUBLE PRECISION		DOUT(*)
      INTEGER			NELM
      INTEGER			STATUS
      INTEGER			I

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Convert all the data
      DO I = 1, NELM
        DOUT(I) = DBLE(DIN(I))
      END DO

      END
