*+  STR_RRADTOC - Single precision radians to character conversion
      SUBROUTINE STR_RRADTOC( ANG, FMT, TEXT, STATUS )
*
*    Description :
*
*     Converts a radian angle to a string using the format FMT. Allowed
*     formats
*
*       Time             Angle
*
*       _HH[.HH]_        _[S_]DDD[.DD]_
*       _HH_MM[.MM]_     _[S_]DDD_MM[.MM]_
*       _HH_MM_SS[.SS]_  _[S_]DDD_MM_SS[.SS]_
*
*     All characters other than "H","M","S","." are echoed to the output
*     buffer. The "_" character above shows where these characters may
*     legitamately appear. Note that FMT is case sensitive.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Dec 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      REAL                       ANG                   ! Radian angle
      CHARACTER*(*)              FMT                   ! Output format
*
*    Export :
*
      CHARACTER*(*)              TEXT                  ! Output buffer
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      DOUBLE PRECISION           RTOD
        PARAMETER                ( RTOD = 57.295779513082320876798D0 )
*
*    Local variables :
*
      CHARACTER*16               IFMT                  ! Writing format
      CHARACTER*20               SBIT                  ! Buffer for a BIT

      DOUBLE PRECISION           ANGU                  !

      INTEGER                    BITS(4)               ! Items
      INTEGER                    FBIT                  ! Number of integer items
      INTEGER                    FM(4)                 ! Item widths
      INTEGER                    FP(4)                 ! Item insert positions
      INTEGER                    FSTAT                 ! i/o status code
      INTEGER                    IBIT                  ! Loop over BITS
      INTEGER                    IC                    ! Loop over format
      INTEGER                    LIMIT                 ! 1st item limit
      INTEGER                    NBIT                  ! Number of BITS used
      INTEGER                    OC                    ! Loop over output text
      INTEGER                    RC                    ! Characters inserted

      LOGICAL                    DECIMAL               ! Last format decimal
*
*    Inline functions :
*
      CHARACTER*1                DIG
      DIG(IC) = CHAR(IC+48)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      OC = 1
      TEXT = ' '

*    First significant character in format
      IC = INDEX( FMT, 'H' )
      IF ( IC .EQ. 0 ) IC = INDEX( FMT, 'S' )
      IF ( IC .EQ. 0 ) IC = INDEX( FMT, 'D' )
      IF ( IC .EQ. 0 ) IC = 1

*    Copy any preceding stuff to output
      IF ( IC .EQ. 0 ) THEN
        CALL MSG_PRNT( '! Expect S,D,H in format' )
        STATUS = SAI__ERROR
        GOTO 99
      ELSE IF ( IC .GT. 1 ) THEN
        TEXT(:IC-1) = FMT(:IC-1)
        OC = IC
      END IF

*    Time or angle
      NBIT = 0
      IF ( FMT(IC:IC) .EQ. 'H' ) THEN

*      Convert to hours
        ANGU = DBLE(ANG) * RTOD / 15.0D0
        LIMIT = 24
        IF ( ANGU .LT. 0.0 ) ANGU = ANGU + 24.0D0

*      Read the degree format
        CALL STR_RADTOC_GFMT( 'H', IC, FMT, NBIT, FM, DECIMAL )

      ELSE

*      Convert to degrees
        ANGU = DBLE(ANG) * RTOD
        LIMIT = 360

*      Output the sign
        IF ( FMT(IC:IC) .EQ. 'S' ) THEN
          IF ( ANG .GT. 0.0 ) THEN
            TEXT(OC:OC) = '+'
          ELSE
            TEXT(OC:OC) = '-'
          END IF
          IC = IC + 1
          OC = OC + 1
          ANGU = ABS(ANGU)
        END IF

*      Echo any intervening characters
        CALL STR_RADTOC_ECHO( 'D', IC, FMT, OC, TEXT )

*      Read the degree format
        CALL STR_RADTOC_GFMT( 'D', IC, FMT, NBIT, FM, DECIMAL )

      END IF

*    First item H/D integer
      FP(1) = OC
      IF ( .NOT. DECIMAL ) THEN

*      Scan between H/D and M
        CALL STR_RADTOC_ECHO( 'M', IC, FMT, OC, TEXT )

*      Read the minutes format
        FP(2) = OC
        CALL STR_RADTOC_GFMT( 'M', IC, FMT, NBIT, FM, DECIMAL )

*      Integer minutes?
        IF ( .NOT. DECIMAL ) THEN

*        Scan between M and S
          CALL STR_RADTOC_ECHO( 'S', IC, FMT, OC, TEXT )

*        Read the seconds format
          FP(3) = OC
          CALL STR_RADTOC_GFMT( 'S', IC, FMT, NBIT, FM, DECIMAL )

        END IF

      END IF

*    Split number into bits
      CALL STR_RADTOC_SPLIT( ANGU, NBIT, FM, LIMIT, DECIMAL, BITS )

*    Write bits into output buffer
      RC = 0
      IF ( DECIMAL ) THEN
        FBIT = NBIT - 2
      ELSE
        FBIT = NBIT
      END IF
      DO IBIT = 1, FBIT
        IFMT = '(I'//DIG(FM(IBIT))//'.'//DIG(FM(IBIT))//')'
        WRITE( SBIT, IFMT, IOSTAT=FSTAT ) BITS(IBIT)
        IF ( OC .EQ. 1 ) THEN
          TEXT = SBIT(:FM(IBIT))
        ELSE
          TEXT = TEXT(:FP(IBIT)+RC-1)//SBIT(:FM(IBIT))/
     :                               /TEXT(FP(IBIT)+RC:OC-1)
        END IF
        RC = RC + FM(IBIT)
        OC = OC + FM(IBIT)
      END DO

      IF ( DECIMAL ) THEN
        IFMT = '(I'//DIG(FM(NBIT-1))//'.'//DIG(FM(NBIT-1))//',A1,I'/
     :           /DIG(FM(NBIT))//'.'//DIG(FM(NBIT))//')'
        WRITE( SBIT, IFMT, IOSTAT=FSTAT ) BITS(NBIT-1), '.', BITS(NBIT)
        IF ( OC .EQ. 1 ) THEN
          TEXT = SBIT(:FM(NBIT-1)+FM(NBIT)+1)
        ELSE
          TEXT = TEXT(:FP(NBIT-1)+RC-1)//SBIT(:FM(NBIT-1)+FM(NBIT)+1)/
     :                                   /TEXT(FP(NBIT-1)+RC:OC-1)
        END IF
        OC = OC + FM(NBIT-1) + FM(NBIT) + 1
      END IF

*    Echo any remaining characters in the format
      IF ( IC .LE. LEN(FMT) ) TEXT(OC:) = FMT(IC:)

*    Fortran error abort point
 89   IF ( FSTAT .NE. 0 ) THEN
        CALL FIO_SERR( FSTAT, STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from STR_RRADTOC', STATUS )
      END IF

      END



*+  STR_DRADTOC - Double precision radians to character conversion
      SUBROUTINE STR_DRADTOC( ANG, FMT, TEXT, STATUS )
*
*    Description :
*
*     Converts a radian angle to a string using the format FMT. Allowed
*     formats
*
*       Time             Angle
*
*       _HH[.HH]_        _[S_]DDD[.DD]_
*       _HH_MM[.MM]_     _[S_]DDD_MM[.MM]_
*       _HH_MM_SS[.SS]_  _[S_]DDD_MM_SS[.SS]_
*
*     All characters other than "H","M","S","." are echoed to the output
*     buffer. The "_" character above shows where these characters may
*     legitamately appear. Note that FMT is case sensitive.
*
*    Method :
*
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Dec 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      DOUBLE PRECISION           ANG                   ! Radian angle
      CHARACTER*(*)              FMT                   ! Output format
*
*    Export :
*
      CHARACTER*(*)              TEXT                  ! Output buffer
*
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      DOUBLE PRECISION           RTOD
        PARAMETER                ( RTOD = 57.295779513082320876798D0 )
*
*    Local variables :
*
      CHARACTER*16               IFMT                  ! Writing format
      CHARACTER*20               SBIT                  ! Buffer for a BIT

      DOUBLE PRECISION           ANGU                  !

      INTEGER                    BITS(4)               ! Items
      INTEGER                    FBIT                  ! Number of integer items
      INTEGER                    FM(4)                 ! Item widths
      INTEGER                    FP(4)                 ! Item insert positions
      INTEGER                    FSTAT                 ! i/o status code
      INTEGER                    IBIT                  ! Loop over BITS
      INTEGER                    IC                    ! Loop over format
      INTEGER                    LIMIT                 ! 1st item limit
      INTEGER                    NBIT                  ! Number of BITS used
      INTEGER                    OC                    ! Loop over output text
      INTEGER                    RC                    ! Characters inserted

      LOGICAL                    DECIMAL               ! Last format decimal
*
*    Inline functions :
*
      CHARACTER*1                DIG
      DIG(IC) = CHAR(IC+48)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      OC = 1
      TEXT = ' '

*    First significant character in format
      IC = INDEX( FMT, 'H' )
      IF ( IC .EQ. 0 ) IC = INDEX( FMT, 'S' )
      IF ( IC .EQ. 0 ) IC = INDEX( FMT, 'D' )
      IF ( IC .EQ. 0 ) IC = 1

*    Copy any preceding stuff to output
      IF ( IC .EQ. 0 ) THEN
        CALL MSG_PRNT( '! Expect S,D,H in format' )
        STATUS = SAI__ERROR
        GOTO 99
      ELSE IF ( IC .GT. 1 ) THEN
        TEXT(:IC-1) = FMT(:IC-1)
        OC = IC
      END IF

*    Time or angle
      NBIT = 0
      IF ( FMT(IC:IC) .EQ. 'H' ) THEN

*      Convert to hours
        ANGU = ANG * RTOD / 15.0D0
        LIMIT = 24
        IF ( ANGU .LT. 0.0 ) ANGU = ANGU + 24.0D0

*      Read the degree format
        CALL STR_RADTOC_GFMT( 'H', IC, FMT, NBIT, FM, DECIMAL )

      ELSE

*      Convert to degrees
        ANGU = ANG * RTOD
        LIMIT = 360

*      Output the sign
        IF ( FMT(IC:IC) .EQ. 'S' ) THEN
          IF ( ANG .GT. 0.0 ) THEN
            TEXT(OC:OC) = '+'
          ELSE
            TEXT(OC:OC) = '-'
          END IF
          IC = IC + 1
          OC = OC + 1
          ANGU = ABS(ANGU)
        END IF

*      Echo any intervening characters
        CALL STR_RADTOC_ECHO( 'D', IC, FMT, OC, TEXT )

*      Read the degree format
        CALL STR_RADTOC_GFMT( 'D', IC, FMT, NBIT, FM, DECIMAL )

      END IF

*    First item H/D integer
      FP(1) = OC
      IF ( .NOT. DECIMAL ) THEN

*      Scan between H/D and M
        CALL STR_RADTOC_ECHO( 'M', IC, FMT, OC, TEXT )

*      Read the minutes format
        FP(2) = OC
        CALL STR_RADTOC_GFMT( 'M', IC, FMT, NBIT, FM, DECIMAL )

*      Integer minutes?
        IF ( .NOT. DECIMAL ) THEN

*        Scan between M and S
          CALL STR_RADTOC_ECHO( 'S', IC, FMT, OC, TEXT )

*        Read the seconds format
          FP(3) = OC
          CALL STR_RADTOC_GFMT( 'S', IC, FMT, NBIT, FM, DECIMAL )

        END IF

      END IF

*    Split number into bits
      CALL STR_RADTOC_SPLIT( ANGU, NBIT, FM, LIMIT, DECIMAL, BITS )

*    Write bits into output buffer
      RC = 0
      IF ( DECIMAL ) THEN
        FBIT = NBIT - 2
      ELSE
        FBIT = NBIT
      END IF
      DO IBIT = 1, FBIT
        IFMT = '(I'//DIG(FM(IBIT))//'.'//DIG(FM(IBIT))//')'
        WRITE( SBIT, IFMT, IOSTAT=FSTAT ) BITS(IBIT)
        IF ( OC .EQ. 1 ) THEN
          TEXT = SBIT(:FM(IBIT))
        ELSE
          TEXT = TEXT(:FP(IBIT)+RC-1)//SBIT(:FM(IBIT))/
     :                               /TEXT(FP(IBIT)+RC:OC-1)
        END IF
        RC = RC + FM(IBIT)
        OC = OC + FM(IBIT)
      END DO

      IF ( DECIMAL ) THEN
        IFMT = '(I'//DIG(FM(NBIT-1))//'.'//DIG(FM(NBIT-1))//',A1,I'/
     :           /DIG(FM(NBIT))//'.'//DIG(FM(NBIT))//')'
        WRITE( SBIT, IFMT, IOSTAT=FSTAT ) BITS(NBIT-1), '.', BITS(NBIT)
        IF ( OC .EQ. 1 ) THEN
          TEXT = SBIT(:FM(NBIT-1)+FM(NBIT)+1)
        ELSE
          TEXT = TEXT(:FP(NBIT-1)+RC-1)//SBIT(:FM(NBIT-1)+FM(NBIT)+1)/
     :                                   /TEXT(FP(NBIT-1)+RC:OC-1)
        END IF
        OC = OC + FM(NBIT-1) + FM(NBIT) + 1
      END IF

*    Echo any remaining characters in the format
      IF ( IC .LE. LEN(FMT) ) TEXT(OC:) = FMT(IC:)

*    Fortran error abort point
 89   IF ( FSTAT .NE. 0 ) THEN
        CALL FIO_SERR( FSTAT, STATUS )
      END IF

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL ERR_REP( ' ', '...from STR_DRADTOC', STATUS )
      END IF

      END



*+  STR_RADTOC_ECHO - Echo characters from IN to OUT, until CH is met
      SUBROUTINE STR_RADTOC_ECHO( CH, IP, IN, OP, OUT )
*
*    Description :
*
*    History :
*
*      6 Dec 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      CHARACTER*1              CH                ! Target character
      CHARACTER*(*)            IN                ! Input buffer
*
*    Export :
*
      INTEGER                  IP, OP            ! Character pointers
      CHARACTER*(*)            OUT               ! Output buffer
*-

*    Loop over IN
      DO WHILE ( (IN(IP:IP).NE.CH) .AND. (IP.LE.LEN(IN)) )

*      Write to OUT if enough room
        IF ( OP .LE. LEN(OUT) ) THEN
          OUT(OP:OP) = IN(IP:IP)
          OP = OP + 1
        END IF

*      Next input character
        IP = IP + 1

      END DO

      END



*+  STR_RADTOC_GFMT - Construct format represented by character CH
      SUBROUTINE STR_RADTOC_GFMT( CH, IP, IN, NF, FMT, DECIMAL )
*
*    Description :
*
*    History :
*
*      6 Dec 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      CHARACTER*1              CH                ! Target character
      CHARACTER*(*)            IN                ! Input buffer
*
*    Export :
*
      INTEGER                  IP                ! Character pointer
      INTEGER                  NF                ! Format counter
      INTEGER                  FMT(*)            ! The formats
      LOGICAL                  DECIMAL           ! Decimal format?
*
*    Local variables :
*
      INTEGER                  I1, I2            ! Character counts
*-

*    Check at the right character
      IF ( IN(IP:IP) .NE. CH ) RETURN

*    Count number of CH
      I1 = 0
      DO WHILE ( (IN(IP:IP).EQ.CH) .AND. (IP.LE.LEN(IN)) )
        I1 = I1 + 1
        IP = IP + 1
      END DO

*    Construct format - assumes I1 < 10
      NF = NF + 1
      FMT(NF) = I1

*    Decimal point found
      IF ( IN(IP:IP) .EQ. '.' ) THEN

*      Count CH after point
        IP = IP + 1
        I2 = 0
        DO WHILE ( (IN(IP:IP).EQ.CH) .AND. (IP.LE.LEN(IN)) )
          I2 = I2 + 1
          IP = IP + 1
        END DO

*      Construct format - assumes I1 and I2 < 10
        NF = NF + 1
        FMT(NF) = I2
        DECIMAL = .TRUE.

      ELSE

        DECIMAL = .FALSE.

      END IF

      END



*+  STR_RADTOC_SPLIT - Split D.P. number into sexigessimal parts
      SUBROUTINE STR_RADTOC_SPLIT( NUM, NBIT, WID, LIMIT,
     :                               LAST_DECIMAL, BITS )
*
*    Description :
*
*    History :
*
*      6 Dec 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      DOUBLE PRECISION         NUM               ! The number to format
      INTEGER                  NBIT              ! Number of numeric sections
      INTEGER                  WID(*)            ! Widths of sections
      INTEGER                  LIMIT             ! Highest value in 1st item
*
*    Export :
*
      LOGICAL                  LAST_DECIMAL      ! Last item is a decimal pair
      INTEGER                  BITS(4)           ! Formatted numbers
*
*    Local variables :
*
      REAL                     REM               ! Remainder

      INTEGER                  DB                ! Adjust indexing of FACTOR
      INTEGER                  FACTOR(0:3)       ! Division factors
      INTEGER                  I                 ! Loop over bits
*-

*    Initialise
      FACTOR(0) = LIMIT
      REM = NUM
      IF ( LAST_DECIMAL ) THEN
        FACTOR(NBIT-1) = 10**WID(NBIT)
        DB = NBIT - 2
      ELSE
        DB = NBIT - 1
      END IF
      DO I = 1, DB
        FACTOR(I) = 60
      END DO

*    Find numbers up to last item
      DO I = 1, NBIT - 1
        BITS(I) = INT(REM)
        REM = (REM-DBLE(BITS(I))) * DBLE(FACTOR(I))
      END DO

*    Last item
      BITS(NBIT) = NINT(REM)

*    Check for overflow in last place
      DO I = NBIT, 1, -1
        IF ( BITS(I).EQ.FACTOR(I-1) ) THEN
          IF ( I .GT. 1 ) BITS(I-1) = BITS(I-1) + 1
          BITS(I) = 0
        ELSE
          GOTO 99
        END IF
      END DO

 99   CONTINUE

      END
