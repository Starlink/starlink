      SUBROUTINE EXERCISE( STATUS )
*+
*  Name:
*     exercise

*  Purpose:
*     Simple exercise of IMG library

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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

*  Authors:
*     PWD: Peter Draper (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     03-JUN-1998 (PWD):
*         Original Version
*     16-AUG-2004 (TIMJ):
*         Use CNF_PVAL

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR

*  Local Constants:
      INTEGER MXNDF              ! Max. number of NDF identifiers
      PARAMETER ( MXNDF = 100 )

*  Local variables:
      LOGICAL MAP( MXNDF )
      INTEGER NI
      CHARACTER * ( 10 ) CMD
      CHARACTER * ( 80 ) PARAM
      CHARACTER * ( 80 ) PARAM1
      CHARACTER * ( 15 ) MTYPE( MXNDF )
      INTEGER MEL( MXNDF )
      INTEGER PNTR( MXNDF )
      INTEGER MDIM( NDF__MXDIM, MXNDF )
      INTEGER MNDIM( MXNDF )
      REAL INC
      REAL START
      INTEGER II

*.

555   FORMAT( A )
666   FORMAT( 1X, A, $ )

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      II = 0
      NI = 0
1     CONTINUE                   ! Start of 'DO WHILE' loop
      IF ( .TRUE. ) THEN

*  Read command specifying operation to perform.
         CALL PAR_GET0C( 'COMMAND', CMD, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
         CALL PAR_CANCL( 'COMMAND', STATUS )

*  Free an image.
         IF ( CHR_SIMLR( CMD, 'FREE' ) ) THEN
            CALL MSG_OUT( ' ', 'Free an image...', STATUS )
            WRITE(*,666)'Parameter name? '
            READ(*,'(A)')PARAM
            CALL IMG_FREE( PARAM, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI
            MAP( II ) = .FALSE.

*  Obtain input image.
         ELSE IF ( CHR_SIMLR( CMD, 'IN' ) ) THEN
            CALL MSG_OUT( ' ', 'Obtain input image...', STATUS )
            WRITE(*,666)'Parameter name? '
            READ(*,'(A)')PARAM
            CALL IMG_IN( PARAM, MDIM( 1, NI + 1 ), MDIM( 2, NI + 1 ),
     :                   PNTR( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI
            CALL MSG_SETI( 'NX', MDIM( 1, II ) )
            CALL MSG_SETI( 'NY', MDIM( 2, II ) )
            CALL MSG_OUT( ' ', '   Image dimensions: ^NX x ^NY',
     :                    STATUS )
            MAP( II ) = .TRUE.
            MNDIM( II ) = 2
            MEL( II ) = MDIM( 1, II ) * MDIM( 2, II )
            MTYPE( II ) = '_REAL'

*  Obtain an output image.
         ELSE IF ( CHR_SIMLR( CMD, 'OUT' ) ) THEN
            CALL MSG_OUT( ' ', 'Obtain output image...', STATUS )
            WRITE(*,666)'Reference image parameter name? '
            READ(*,'(A)')PARAM1
            WRITE(*,666)'Output parameter name? '
            READ(*,'(A)')PARAM
            CALL IMG_OUT( PARAM1, PARAM, PNTR( NI + 1 ), STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 66
            NI = NI + 1
            II = NI
            MAP( II ) = .FALSE.

*  Examine mapped data values.
         ELSE IF ( CHR_SIMLR( CMD, 'EX' ) ) THEN
            CALL MSG_OUT( ' ', 'Examine mapped data values...',
     :                    STATUS )
            IF ( .NOT. MAP( II ) ) THEN
               CALL MSG_OUT( ' ', 'Image is not mapped for access',
     :                       STATUS )
            ELSE
               CALL WRN( MTYPE( II ), MNDIM( II ), MDIM( 1, II ),
     :                   PNTR( II ), STATUS )
            END IF

*  Set mapped array values.
         ELSE IF ( CHR_SIMLR( CMD, 'SET' ) ) THEN
            CALL MSG_OUT( ' ', 'Set mapped array values...',
     :                    STATUS )
            IF ( .NOT. MAP( II ) ) THEN
               CALL MSG_OUT( ' ', 'Image is not mapped for access',
     :                       STATUS )
            ELSE
               WRITE(*,666)'Starting value? '
               READ(*,*) START
               WRITE(*,666)'Increment? '
               READ(*,*) INC
               CALL SET( MTYPE( II ), START, INC,
     :                   MEL( II ), PNTR( II ),
     :                   STATUS )
            END IF

*  Go to specified image.
         ELSE IF ( CHR_SIMLR( CMD, 'GO' ) ) THEN
            CALL MSG_OUT( ' ', 'Go to specified image number...',
     :                    STATUS )
            WRITE(*,666)'Image number? '
            READ(*,*) II

         END IF

*  Display current status.
         CALL MSG_OUT( ' ', ' ', STATUS )
         CALL MSG_SETI( 'II', II )
         CALL MSG_OUT( ' ', 'Current image is no. ^II', STATUS )

66       IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_REP( ' ', '^STATUS', STATUS )
            CALL ERR_FLUSH( STATUS )
         END IF
         STATUS = SAI__OK
         GO TO 1
      END IF

99    CONTINUE

      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', '^STATUS', STATUS )
         CALL ERR_FLUSH( STATUS )
      END IF

      END

      SUBROUTINE WRN( TYPE, NDIM, DIM, PNTR, STATUS )
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CNF_PAR'
      CHARACTER * ( * ) TYPE
      CHARACTER * ( DAT__SZNAM ) UTYPE
      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER PNTR
      INTEGER STATUS
      IF ( STATUS .NE. SAI__OK ) RETURN

      UTYPE = TYPE
      CALL CHR_UCASE( UTYPE )

      IF ( UTYPE .EQ. '_BYTE' ) THEN
         CALL WRNB( NDIM, DIM, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( UTYPE .EQ. '_UBYTE' ) THEN
         CALL WRNUB( NDIM, DIM, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( UTYPE .EQ. '_DOUBLE' ) THEN
         CALL WRND( NDIM, DIM, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( UTYPE .EQ. '_INTEGER' ) THEN
         CALL WRNI( NDIM, DIM, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( UTYPE .EQ. '_REAL' ) THEN
         CALL WRNR( NDIM, DIM, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( UTYPE .EQ. '_WORD' ) THEN
         CALL WRNW( NDIM, DIM, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( UTYPE .EQ. '_UWORD' ) THEN
         CALL WRNUW( NDIM, DIM, %VAL( CNF_PVAL( PNTR ) ), STATUS )
      END IF

      END

      SUBROUTINE WRNB( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'

      INTEGER NDIM
      INTEGER DIM( NDIM )
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE

      DO 2 I = 1, N
         CALL WRB( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         WRITE(*,*)
2     CONTINUE

      END

      SUBROUTINE WRNUB( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'

      INTEGER NDIM
      INTEGER DIM( NDIM )
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE

      DO 2 I = 1, N
         CALL WRUB( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ),
     :   STATUS )
         WRITE(*,*)
2     CONTINUE

      END

      SUBROUTINE WRND( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'

      INTEGER NDIM
      INTEGER DIM( NDIM )
      DOUBLE PRECISION ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE

      DO 2 I = 1, N
         CALL WRD( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         WRITE(*,*)
2     CONTINUE

      END

      SUBROUTINE WRNI( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'

      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE

      DO 2 I = 1, N
         CALL WRI( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         WRITE(*,*)
2     CONTINUE

      END

      SUBROUTINE WRNR( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'

      INTEGER NDIM
      INTEGER DIM( NDIM )
      REAL ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE

      DO 2 I = 1, N
         CALL WRR( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         WRITE(*,*)
2     CONTINUE

      END

      SUBROUTINE WRNW( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'

      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE

      DO 2 I = 1, N
         CALL WRW( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ), STATUS )
         WRITE(*,*)
2     CONTINUE

      END

      SUBROUTINE WRNUW( NDIM, DIM, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'

      INTEGER NDIM
      INTEGER DIM( NDIM )
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER N
      INTEGER I

      IF ( STATUS .NE. SAI__OK ) RETURN

      N = 1
      DO 1 I = 2, NDIM
         N = N * DIM ( I )
1     CONTINUE

      DO 2 I = 1, N
         CALL WRUW( DIM( 1 ), ARRAY( ( I - 1 ) * DIM( 1 ) + 1 ),
     :   STATUS )
         WRITE(*,*)
2     CONTINUE

      END

      SUBROUTINE SET( TYPE, START, INC, EL, PNTR, STATUS )
      INCLUDE 'SAE_PAR'
      INCLUDE 'CNF_PAR'
      CHARACTER * ( * ) TYPE
      REAL START, INC
      INTEGER EL, PNTR, STATUS
      LOGICAL CHR_SIMLR

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( CHR_SIMLR( TYPE, '_BYTE' ) ) THEN
         CALL SETB( START, INC, EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_UBYTE' ) ) THEN
         CALL SETUB( START, INC, EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_DOUBLE' ) ) THEN
         CALL SETD( START, INC, EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_INTEGER' ) ) THEN
         CALL SETI( START, INC, EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_REAL' ) ) THEN
         CALL SETR( START, INC, EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_WORD' ) ) THEN
         CALL SETW( START, INC, EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ELSE IF ( CHR_SIMLR( TYPE, '_UWORD' ) ) THEN
         CALL SETUW( START, INC, EL, %VAL( CNF_PVAL( PNTR ) ), STATUS )

      ENDIF

      END

      SUBROUTINE SETB( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER I
      BYTE VAL_RTOB

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, EL
         STATUS = SAI__OK
         ARRAY( I ) = VAL_RTOB( .FALSE.,
     :   START + INC * REAL( I -1 ), STATUS )
         STATUS = SAI__OK
1     CONTINUE

      END

      SUBROUTINE SETUB( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      BYTE ARRAY( * )
      INTEGER STATUS
      INTEGER I
      BYTE VAL_RTOUB

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, EL
         STATUS = SAI__OK
         ARRAY( I ) = VAL_RTOUB( .FALSE.,
     :   START + INC * REAL( I -1 ), STATUS )
         STATUS = SAI__OK
1     CONTINUE

      END

      SUBROUTINE SETD( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      DOUBLE PRECISION ARRAY( * )
      INTEGER STATUS
      INTEGER I
      DOUBLE PRECISION VAL_RTOD

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, EL
         STATUS = SAI__OK
         ARRAY( I ) = VAL_RTOD( .FALSE.,
     :   START + INC * REAL( I -1 ), STATUS )
         STATUS = SAI__OK
1     CONTINUE

      END

      SUBROUTINE SETI( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      INTEGER ARRAY( * )
      INTEGER STATUS
      INTEGER I
      INTEGER VAL_RTOI

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, EL
         STATUS = SAI__OK
         ARRAY( I ) = VAL_RTOI( .FALSE.,
     :   START + INC * REAL( I -1 ), STATUS )
         STATUS = SAI__OK
1     CONTINUE

      END

      SUBROUTINE SETR( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      REAL ARRAY( * )
      INTEGER STATUS
      INTEGER I
      REAL VAL_RTOR

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, EL
         STATUS = SAI__OK
         ARRAY( I ) = VAL_RTOR( .FALSE.,
     :   START + INC * REAL( I -1 ), STATUS )
         STATUS = SAI__OK
1     CONTINUE

      END

      SUBROUTINE SETW( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER I
      INTEGER*2 VAL_RTOW

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, EL
         STATUS = SAI__OK
         ARRAY( I ) = VAL_RTOW( .FALSE.,
     :   START + INC * REAL( I -1 ), STATUS )
         STATUS = SAI__OK
1     CONTINUE

      END

      SUBROUTINE SETUW( START, INC, EL, ARRAY, STATUS )
      INCLUDE 'SAE_PAR'
      REAL START
      REAL INC
      INTEGER EL
      INTEGER*2 ARRAY( * )
      INTEGER STATUS
      INTEGER I
      INTEGER*2 VAL_RTOUW

      IF ( STATUS .NE. SAI__OK ) RETURN

      DO 1 I = 1, EL
         STATUS = SAI__OK
         ARRAY( I ) = VAL_RTOUW( .FALSE.,
     :   START + INC * REAL( I -1 ), STATUS )
         STATUS = SAI__OK
1     CONTINUE

      END
