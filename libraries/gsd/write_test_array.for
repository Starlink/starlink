      PROGRAM WRITE_TEST_ARRAY
*+
*  Name:
*     gsd_test

*  Purpose:
*     Write test data array.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Notes:
*     Currently not ported to UNIX. VAX debug routine.

*-
      BYTE BBAD
      PARAMETER (BBAD = '81'X)               ! -127
      INTEGER*2 WBAD
      PARAMETER (WBAD = '8001'X)             ! -32767
      INTEGER*4 IBAD
      PARAMETER (IBAD = '80000001'X)         ! -2147483647
      REAL*4 RBAD
      PARAMETER (RBAD = 'FFF7FFFF'X)         ! -1.7014109E+38
      REAL*8 DBAD
      PARAMETER (DBAD = 'FFFFFFFFFFF7FFFF'X) ! -1.701410233083081E+38

      DOUBLE PRECISION DARRAY( 20 )
      REAL             RARRAY( 20 )
      INTEGER          IARRAY( 20 )
      INTEGER * 2      WARRAY( 20 )
      BYTE             BARRAY( 20 )

      OPEN( 41, FILE='TEST_ARRAY.DAT', RECL=10, FORM='UNFORMATTED',
     :   STATUS='NEW', ACCESS='DIRECT' )

      DARRAY(1)  =   0.
      DARRAY(2)  =  12.3456
      DARRAY(3)  = -78.9012
      DARRAY(4)  =  BBAD
      DARRAY(5)  =  WBAD
      DARRAY(6)  =  IBAD
      DARRAY(7)  =  RBAD
      DARRAY(8)  =  DBAD
      DARRAY(9)  =  234.567
      DARRAY(10) =  567.89
      DARRAY(11) =  34567.8
      DARRAY(12) =  67890.1
      DARRAY(13) =  2.34567E9
      DARRAY(14) =  DBAD
      DARRAY(15) = -234.567
      DARRAY(16) = -567.89
      DARRAY(17) = -34567.8
      DARRAY(18) = -67890.1
      DARRAY(19) = -2.34567E9
      DARRAY(20) =  DBAD

      WRITE( 41, REC=1 ) ( DARRAY(I), I =  1,  5 )
      WRITE( 41, REC=2 ) ( DARRAY(I), I =  6, 10 )
      WRITE( 41, REC=3 ) ( DARRAY(I), I = 11, 15 )
      WRITE( 41, REC=4 ) ( DARRAY(I), I = 16, 20 )

      RARRAY(1)  =   0.
      RARRAY(2)  =  12.3456
      RARRAY(3)  = -78.9012
      RARRAY(4)  =  BBAD
      RARRAY(5)  =  WBAD
      RARRAY(6)  =  IBAD
      RARRAY(7)  =  RBAD
      RARRAY(8)  =  RBAD
      RARRAY(9)  =  234.567
      RARRAY(10) =  567.89
      RARRAY(11) =  34567.8
      RARRAY(12) =  67890.1
      RARRAY(13) =  2.34567E9
      RARRAY(14) =  RBAD
      RARRAY(15) = -234.567
      RARRAY(16) = -567.89
      RARRAY(17) = -34567.8
      RARRAY(18) = -67890.1
      RARRAY(19) = -2.34567E9
      RARRAY(20) =  RBAD

      WRITE( 41, REC=5 ) ( RARRAY(I), I =  1, 10 )
      WRITE( 41, REC=6 ) ( RARRAY(I), I = 11, 20 )

      IARRAY(1)  =   0.
      IARRAY(2)  =  12.3456
      IARRAY(3)  = -78.9012
      IARRAY(4)  =  BBAD
      IARRAY(5)  =  WBAD
      IARRAY(6)  =  IBAD
      IARRAY(7)  =  IBAD
      IARRAY(8)  =  IBAD
      IARRAY(9)  =  234.567
      IARRAY(10) =  567.89
      IARRAY(11) =  34567.8
      IARRAY(12) =  67890.1
      IARRAY(13) =  IBAD
      IARRAY(14) =  IBAD
      IARRAY(15) = -234.567
      IARRAY(16) = -567.89
      IARRAY(17) = -34567.8
      IARRAY(18) = -67890.1
      IARRAY(19) =  IBAD
      IARRAY(20) =  IBAD

      WRITE( 41, REC=7 ) ( IARRAY(I), I =  1, 10 )
      WRITE( 41, REC=8 ) ( IARRAY(I), I = 11, 20 )

      WARRAY(1)  =   0.
      WARRAY(2)  =  12.3456
      WARRAY(3)  = -78.9012
      WARRAY(4)  =  BBAD
      WARRAY(5)  =  WBAD
      WARRAY(6)  =  WBAD
      WARRAY(7)  =  WBAD
      WARRAY(8)  =  WBAD
      WARRAY(9)  =  234.567
      WARRAY(10) =  567.89
      WARRAY(11) =  WBAD
      WARRAY(12) =  WBAD
      WARRAY(13) =  WBAD
      WARRAY(14) =  WBAD
      WARRAY(15) = -234.567
      WARRAY(16) = -567.89
      WARRAY(17) =  WBAD
      WARRAY(18) =  WBAD
      WARRAY(19) =  WBAD
      WARRAY(20) =  WBAD

      WRITE( 41, REC=9 ) ( WARRAY(I), I =  1, 20 )

      BARRAY(1)  =   0.
      BARRAY(2)  =  12.3456
      BARRAY(3)  = -78.9012
      BARRAY(4)  =  BBAD
      BARRAY(5)  =  BBAD
      BARRAY(6)  =  BBAD
      BARRAY(7)  =  BBAD
      BARRAY(8)  =  BBAD
      BARRAY(9)  =  BBAD
      BARRAY(10) =  BBAD
      BARRAY(11) =  BBAD
      BARRAY(12) =  BBAD
      BARRAY(13) =  BBAD
      BARRAY(14) =  BBAD
      BARRAY(15) =  BBAD
      BARRAY(16) =  BBAD
      BARRAY(17) =  BBAD
      BARRAY(18) =  BBAD
      BARRAY(19) =  BBAD
      BARRAY(20) =  BBAD

      WRITE( 41, REC=10 ) ( BARRAY(I), I =  1, 20 )

      DO 1 I = 1, 20
         WRITE( *, 101 )
     :      I, DARRAY(I), RARRAY(I), IARRAY(I), WARRAY(I), BARRAY(I)
 1    CONTINUE

 101  FORMAT( ' ',I2,' ',G21.15,' ',G14.8,' ',I11,' ',I6,' ',I4 )

      CLOSE( 41 )

      END
