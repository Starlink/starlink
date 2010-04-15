      PROGRAM TESTFC

*  This is a program to test FORTRAN routines calling C ones where the
*  subroutine linkage is provided by CNF.

      INTEGER NI, NR, ND, NL, NB, NW, NUB, NUW, NC
      PARAMETER( NB = 11 )
      PARAMETER( NC = 5 )
      PARAMETER( ND = 10 )
      PARAMETER( NI = 10 )
      PARAMETER( NL = 9 )
      PARAMETER( NR = 10 )
      PARAMETER( NUB = 12 )
      PARAMETER( NUW = 21 )
      PARAMETER( NW = 20 )

      BYTE B, BA ( NB )
      BYTE FB, FUB
      BYTE UB, UBA ( NUB )
      CHARACTER * ( 1 ) CH
      CHARACTER * ( 10 ) C, CA( NC )
      CHARACTER * ( 40 ) LINE
      CHARACTER * (40) FC
      DOUBLE PRECISION D, DA( ND )
      DOUBLE PRECISION FD
      INTEGER CBI, CNI
      INTEGER FI
      INTEGER I, IA( NI )
      INTEGER PTR
      INTEGER*2 FW, FUW
      INTEGER*2 UW, UWA( NUW )
      INTEGER*2 W, WA( NW )
      LOGICAL FL
      LOGICAL ISNEW
      LOGICAL L, LA( NL )
      REAL CBR, CNR
      REAL FR
      REAL R, RA( NR )

      INCLUDE 'CNF_PAR'

      COMMON CBI, CBR
      COMMON/COMM1/ CNI, CNR

      PRINT '(A)', '--> This is a test of FORTRAN calling C'

*  Test the passing of integers.
      DO J = 1, NI
         IA( J ) = J
      END DO
      CALL TI( IA, NI, I )
      PRINT '(A,I4,A,I9)',
     :   'The sum of the first ',NI,' integers is ',I

*  Test the passing of reals.
      DO J = 1, NR
         RA( J ) = FLOAT( J )
      END DO
      CALL TR( RA, NR, R )
      PRINT '(A,I4,A,F10.2)',
     :   'The mean of the first ',NR,' integers is ',R

*  Test the passing of doubles precision variables.
      DO J = 1, ND
         DA( J ) = DBLE( FLOAT( J ) )
      END DO
      CALL TD( DA, ND, D )
      PRINT '(A,I4,A,F10.1)',
     :   'The sum of squares of the first ',ND,' integers is ',D

*  Test the passing of logical variables.
      DO J = 1, NL
         LA( J ) = MOD( J, 2 ) .EQ. 1
      END DO
      CALL TL( LA, NL, L )
      PRINT '(A,I4,A,L1)',
     :   'The statement ''The number of odd values in the first ',NL,
     :   ' integers is 5'' is ',L

*  Test the passing of byte variables.
      DO J = 1, NB
         BA( J ) = J
      END DO
      CALL TB( BA, NB, B )
      PRINT '(A,I4,A,I9)'
     :   ,'The sum of the first ',NB,' integers is ',B

*  Test the passing of word variables.
      DO J = 1, NW
         WA( J ) = J
      END DO
      CALL TW( WA, NW, W )
      PRINT '(A,I4,A,I9)',
     :   'The sum of the first ',NW,' integers is ',W

*  Test the passing of unsigned byte variables.
      DO J = 1, NUB
         UBA( J ) = J
      END DO
      CALL TUB( UBA, NUB, UB )
      PRINT '(A,I4,A,I9)',
     :   'The sum of the first ',NB,' integers is ',UB

*  Test the passing of unsigned word variables.
      DO J = 1, NUW
         UWA( J ) = J
      END DO
      CALL TUW( UWA, NUW, UW )
      PRINT '(A,I4,A,I9)',
     :   'The sum of the first ',NW,' integers is ',UW

*  Test the passing of character variables.
      CALL TC1( C )
      PRINT '(A,A)',
     :   'The character variable has been set to: ',C

      DO J = 1, NC
         CA(J) = ' ' ! Initialise full array (needed on some compilers)
         CH = CHAR( 64 + J )
         CA( J )( 1:5 ) = CH // CH // CH // CH // CH
      END DO
      CALL TC2( CA, NC, LINE )
      PRINT '(A,A)',
     :   'The built up character string is: ', LINE

*  By value.
#if HAVE_PERCENTLOC
      PTR = CNF_PREG( %LOC( CA ), ISNEW )
#else
      PTR = CNF_PREG( LOC( CA ), ISNEW )
#endif
      CALL TC2( %VAL( CNF_PVAL( PTR ) ), NC, LINE,
     :          %VAL( CNF_CVAL( 10 ) ), %VAL( CNF_CVAL( 40 ) ) )
      PRINT '(A,A)',
     :   'The built up character string is: ', LINE

*  Test the use of common blocks.
      CBI = 27
      CBR = 35.0
      CALL TCOM
      PRINT '(A,I3,A,F10.4)',
     :   'The values of the variables in named common are ',CNI,' and ',
     :   CNR

*  Test functions.
      PRINT '(A,I4)',
     :   'The value of function FI is ',FI(0)
      PRINT '(A,F10.4)',
     :   'The value of function FR is ',FR(0.0)
      PRINT '(A,F10.4)',
     :   'The value of function FD is ',FD(0.0D0)
      PRINT '(A,L1)',
     :   'The value of function FL is ',FL(.TRUE.)
      PRINT '(A,A)',
     :   'The value of function FC is ',FC('Hello')
      PRINT '(A,I4)',
     :   'The value of function FB is ',FB(B)
      PRINT '(A,I4)',
     :   'The value of function FW is ',FW(W)
      PRINT '(A,I4)',
     :   'The value of function FUB is ',FUB(UB)
      PRINT '(A,I4)',
     :   'The value of function FUW is ',FUW(UW)


*  Test the use of pointers.
      CALL GETMEM( PTR, 4*10 )
      CALL SETMEM( %VAL(CNF_PVAL(PTR)), 10 )
      CALL PRTMEM( %VAL(CNF_PVAL(PTR)), 10 )
      CALL FREEMEM( %VAL(CNF_PVAL(PTR)))
      END

      SUBROUTINE SETMEM( A, N )
      INTEGER N, A(N)
      INTEGER J
      DO J = 1, N
         A(J) = J
      END DO
      END

      SUBROUTINE PRTMEM( A, N )
      INTEGER N, A(N)
      INTEGER J
      DO J = 1, N
         PRINT '(A,I4)',
     :   'The array element = ', A(J)
      END DO
      END
