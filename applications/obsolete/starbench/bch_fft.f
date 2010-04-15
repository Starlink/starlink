      SUBROUTINE BCH_FFT( STATUS )
*+
* Name:
*    BCH_FFT

*  Purpose:
*    Run a forward FFT algorithm.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     BCH_FFT

*  Description:
*     Run a forward FFT algorithm on a test array. Intended as part of a
*     benchmarking package. No output is produced.

*  Authors:
*     TMG: Tim Gledhill (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1994 (TMG):
*        Original version.
*    20-MAR-1996 (TMG):
*        Change to use the FFTPACK routines implemented in the PDA
*        package.
*    15-APR-2008 (TIMJ):
*        Use CNF_PVAL
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Global Status:
      INTEGER STATUS             ! global status

*  Local Constants:
      INTEGER MAXX               ! max x dimension of array
      PARAMETER ( MAXX = 1024 )
      INTEGER MAXY               ! max y dimension of array
      PARAMETER ( MAXY = 1024 )
      INTEGER NTIMES             ! number of times to loop
      PARAMETER ( NTIMES = 1 )

*  Local Variables:
      INTEGER ILOOP              ! loop counter
      INTEGER NX, NY             ! image dimensions
      INTEGER NPNTS, FTSIZE      ! workspace size
      INTEGER MP_IN, MP_WK1      !\
      INTEGER MP_WK2, MP_WK3     !- workspace pointers
      INTEGER MP_WK4             !/

      DOUBLE PRECISION DFILL     ! fill value

*.

* Check the status.

      IF ( STATUS .NE. SAI__OK ) GO TO 99

* Get space for the array to be transformed and the workspace arrays.

      NX = MAXX
      NY = MAXY
      NPNTS = NX * NY
      FTSIZE = 3 * MAX( NX, NY ) + 15
      CALL PSX_CALLOC( NPNTS, '_DOUBLE', MP_IN, STATUS )
      CALL PSX_CALLOC( NPNTS, '_DOUBLE', MP_WK1, STATUS )
      CALL PSX_CALLOC( NPNTS, '_DOUBLE', MP_WK2, STATUS )
      CALL PSX_CALLOC( NPNTS, '_DOUBLE', MP_WK3, STATUS )
      CALL PSX_CALLOC( FTSIZE, '_DOUBLE', MP_WK4, STATUS )

* Populate the array with double precision  values. The values range
* from 1 to MAXX * MAXY.

      CALL BCH_SETIP( %VAL( CNF_PVAL( MP_IN ) ), NX, NY )
      DFILL = 0.0D0

* Loop to transform the array.

      DO 3 ILOOP = 1, NTIMES

* Call the FFT preparation routine then do the forward FFT.

        CALL KPS1_FOPRD( NX, NY, %VAL( CNF_PVAL( MP_IN ) ),NX,NY,DFILL,
     :                   .FALSE., %VAL( CNF_PVAL(MP_WK1) ), STATUS )
        CALL KPS1_FOFOD( NX, NY, .FALSE., %VAL( CNF_PVAL(MP_WK1) ),
     :                   %VAL( CNF_PVAL(MP_WK2) ),
     :                   %VAL( CNF_PVAL(MP_WK4) ),
     :                   %VAL( CNF_PVAL(MP_WK3) ), STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 99
 3    CONTINUE

* Release the workspace.

      CALL PSX_FREE( MP_IN, STATUS )
      CALL PSX_FREE( MP_WK1, STATUS )
      CALL PSX_FREE( MP_WK2, STATUS )
      CALL PSX_FREE( MP_WK3, STATUS )
      CALL PSX_FREE( MP_WK4, STATUS )

* Return.

 99   CONTINUE
      END


*----------------------------------------------------------------------------

      SUBROUTINE BCH_SETIP( ARRAY, NX, NY )

      INTEGER           I, J, NX, NY
      DOUBLE PRECISION  ARRAY( NX, NY )

* Set array values.

      DO 1 J = 1, NY
         DO 2 I = 1, NX
            ARRAY( I, J ) = DBLE( I + ( J - 1 ) * NX )
 2       CONTINUE
 1    CONTINUE
      END
