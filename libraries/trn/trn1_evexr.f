      SUBROUTINE TRN1_EVEXR ( CODE, ICODE, CON, ICON, BAD, ND1, NDVEC,
     :                          DATA, NDAT, NS1, NSTACK, S, NCODE,
     :                          NCON, IERR, NERR, NSTAT, STATUS)








*+
*  Name:
*     TRN1_EVEXR

*  Purpose:
*     evaluate a compiled REAL expression.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN1_EVEXR ( CODE, ICODE, CON, ICON, BAD, ND1, NDVEC,
*                         DATA, NDAT, NS1, NSTACK, S, NCODE,
*                         NCON, IERR, NERR, NSTAT, STATUS)

*  Description:
*     The routine evaluates an arithmetic expression for a vectorised
*     array of input values using REAL arithmetic.  Arithmetic is
*     performed on a stack, each element of which is a vectorised
*     numeric array. The expression being evaluated is determined by an
*     input array of integer arithmetic operation codes (produced by
*     the routine TRN1_CMPEX). These codes are defined in the include
*     file TRN_CONST_STM.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     R.F. Warren-Smith (DUVAD::RFWS)
*     D.S. Berry (DSB)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1988:  Original version (DUVAD::RFWS)
*     28-MAR-1988:  Improved error handling (DUVAD::RFWS)
*     16-AUG-1988:  Added "bad value" handling (DUVAD::RFWS)
*     12-JAN-2006:  Added IDV, QIF and boolean operators. Moved LDBAD
*                   from label 35 to 37.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT primitive data constants
      INCLUDE 'TRN_CONST_STM'    ! TRN_ private constants for routines
                                 ! which perform processing of standard
                                 ! transformation modules
      INCLUDE 'TRN_ERR'          ! TRN_ error codes


*  Arguments Given:
      INTEGER CODE( * )         ! Input operation codes from TRN1_CMPEX
      INTEGER ICODE             ! Initial element of CODE array to use
      DOUBLE PRECISION CON( * ) ! Array of constants for expression
      INTEGER ICON              ! Initial element of CON array to use
      LOGICAL BAD               ! Whether input data may be "bad"
      INTEGER ND1               ! First dimension size of DATA array
      INTEGER NDVEC             ! Number of input data vectors
      REAL DATA( ND1, NDVEC ) ! Array containing sequence of input
                                ! data vectors
      INTEGER NDAT              ! Number of data values in each vector
      INTEGER NS1               ! First dimension of workspace/output
                                ! stack S - must be at least equal to
                                ! NDAT
      INTEGER NSTACK            ! Number of vectors in stack S; must be
                                ! at least equal to the maximum stack
                                ! size for the expression as calculated
                                ! by TRN1_CMPEX


*  Arguments Given and Returned:
*     <declarations and descriptions for imported/exported arguments>


*  Arguments Returned:
      REAL S( NS1, NSTACK )   ! Array used as arithmetic stack for
                                ! workspace/output
      INTEGER NCODE             ! Number of operation codes consumed
                                ! from the CODE array
      INTEGER NCON              ! Number of constants consumed from the
                                ! CON array
      INTEGER IERR              ! Pointer to the first data element to
                                ! generate a numerical error
      INTEGER NERR              ! Number of numerical errors which occur
      INTEGER NSTAT             ! Numerical error status - identifies
                                ! the first error (pointed to by IERR)


*  Status:
      INTEGER STATUS            ! Error status


*  External References:
      REAL VAL_DTOR         ! Convert DOUBLE PRECISION to REAL


*  Global Variables:
*     <any INCLUDE files for global variables held in named COMMON>


*  Local Constants:
*     <local constants defined by PARAMETER>


*  Local Variables:
      LOGICAL BADL              ! Local bad data flag
      INTEGER TOS               ! Pointer to top of arithmetic stack
      INTEGER IOP               ! Counter for operations performed
      INTEGER OPER              ! Current operation code
      INTEGER NVAR              ! Identification number for variables
      INTEGER NARG              ! Argument count for operations with
                                ! a variable number of arguments
      INTEGER I                 ! General loop counter
      INTEGER IERRL             ! Local numerical error pointer
      INTEGER NERRL             ! Local numerical error count
      INTEGER NSTATL            ! Local numerical error status variable
      REAL CONST              ! Variable to hold REAL constant


*  Internal References:
*     <declarations for internal functions>


*  Local Data:
*     <any DATA initialisations for local variables>


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Initialise the numerical error pointer, error count and status
*   variable.
      IERR = 0
      NERR = 0
      NSTAT = SAI__OK


*   Initialise the local bad data flag.
      BADL = BAD


*   Initialise the arithmetic stack pointer and the count of constants
*   consumed.
      TOS=0
      NCON=0


*   Return the number of code elements which will be used (the first
*   code element to be used contains a count of the number which
*   follow).
      NCODE = CODE( ICODE ) + 1


*   Loop to obtain successive arithmetic operation codes from the input
*   stream.
      IOP = 0
      DO WHILE ( ( IOP .LT. NCODE - 1 ) .AND. ( STATUS .EQ. SAI__OK ) )
        IOP = IOP + 1
        OPER = CODE( IOP + ICODE )


*   Branch to perform the required operation on the arithmetic stack.
*   After performing the operation, control passes on to statement 100
*   in all cases.
        NSTATL = SAI__OK
        GO TO (901,902,
     :           1,  2,  3,  4,  5,  6,  7,  8,  9, 10,
     :          11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
     :          21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
     :          31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
     :          41, 42, 43, 44, 45, 46 ) ( OPER - TRN_OP_MINOP + 1 )


*   If the operation code was not recognised, report an error.
        STATUS = TRN__OPCIN     ! operation code invalid
        CALL TRN1_ERROR( 'TRN1_EVEXR', ' ', STATUS )
        GO TO 100


*   TRN_OP_LDCON - load next constant on to stack.
  901   CONTINUE

*   ...extract the constant from the constant list and convert its type:
        NCON = NCON + 1
        CONST = VAL_DTOR( .FALSE., CON( ICON + NCON - 1 ), NSTATL )

*   ...if a conversion error occurred, set the local error pointer and
*      error count appropriately:
        IF( NSTATL .NE. SAI__OK ) THEN
          IERRL = 1
          NERRL = NDAT - NERR
        ENDIF

*   ...fill the next stack vector with the constant:
        TOS = TOS + 1
        DO I = 1, NDAT
          S( I, TOS ) = CONST
        ENDDO
        GO TO 100


*   TRN_OP_LDVAR - load variable on to stack.
  902   CONTINUE

*   ...the variable identification number is held in the next constant:
        NCON = NCON + 1
        NVAR = NINT( CON( ICON + NCON - 1 ) )
        TOS = TOS + 1
        CALL VEC_RTOR( BADL, NDAT, DATA( 1, NVAR ),
     :                     S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ADD - add.
    1   CONTINUE
        CALL VEC_ADDR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                   S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_SUB - subtract.
    2   CONTINUE
        CALL VEC_SUBR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                   S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_MUL - multiply.
    3   CONTINUE
        CALL VEC_MULR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                   S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_DIV - divide.
    4   CONTINUE
        CALL VEC_DIVR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                   S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_PWR - raise to power.
    5   CONTINUE
        CALL VEC_PWRR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                   S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_NEG - negate.
    6   CONTINUE
        CALL VEC_NEGR( BADL, NDAT, S( 1, TOS ),
     :                   S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_SQRT - square root.
    7   CONTINUE
        CALL VEC_SQRTR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_LOG - log (natural).
    8   CONTINUE
        CALL VEC_LOGR( BADL, NDAT, S( 1, TOS ),
     :                   S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_LG10 - common log (base 10).
    9   CONTINUE
        CALL VEC_LG10R( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_EXP - exponentiate.
   10   CONTINUE
        CALL VEC_EXPR( BADL, NDAT, S( 1, TOS ),
     :                   S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_SIN - sine function.
   11   CONTINUE
        CALL VEC_SINR( BADL, NDAT, S( 1, TOS ),
     :                   S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_COS - cosine function.
   12   CONTINUE
        CALL VEC_COSR( BADL, NDAT, S( 1, TOS ),
     :                   S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_TAN - tangent function.
   13   CONTINUE
        CALL VEC_TANR( BADL, NDAT, S( 1, TOS ),
     :                   S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_SIND - sine function (degrees).
   14   CONTINUE
        CALL VEC_SINDR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_COSD - cosine function (degrees).
   15   CONTINUE
        CALL VEC_COSDR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_TAND - tangent function (degrees).
   16   CONTINUE
        CALL VEC_TANDR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ASIN - inverse sine function.
   17   CONTINUE
        CALL VEC_ASINR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ACOS - inverse cosine function.
   18   CONTINUE
        CALL VEC_ACOSR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ATAN - inverse tangent function.
   19   CONTINUE
        CALL VEC_ATANR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ASND - inverse sine function (degrees).
   20   CONTINUE
        CALL VEC_ASNDR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ACSD - inverse cosine function (degrees).
   21   CONTINUE
        CALL VEC_ACSDR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ATND - inverse tangent function (degrees).
   22   CONTINUE
        CALL VEC_ATNDR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_SINH - hyperbolic sine function.
   23   CONTINUE
        CALL VEC_SINHR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_COSH - hyperbolic cosine function.
   24   CONTINUE
        CALL VEC_COSHR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_TANH - hyperbolic tangent function.
   25   CONTINUE
        CALL VEC_TANHR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_ABS - absolute value.
   26   CONTINUE
        CALL VEC_ABSR( BADL, NDAT, S( 1, TOS ),
     :                   S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_NINT - nearest integer.
   27   CONTINUE
        CALL VEC_NINTR( BADL, NDAT, S( 1, TOS ),
     :                    S( 1, TOS ), IERRL, NERRL, NSTATL )
        GO TO 100


*   TRN_OP_MIN - minimum.
   28   CONTINUE

*   ...the number of arguments is determined by the next constant:
        NCON = NCON + 1
        NARG = NINT( CON( ICON + NCON - 1 ) )

*   ...loop to repeatedly find the minimum of the two top of stack
*      entries, decrementing the stack pointer each time:
        DO I = 1, ( NARG - 1 )
          CALL VEC_MINR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                     S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
          TOS = TOS - 1
        ENDDO
        GO TO 100


*   TRN_OP_MAX - maximum.
   29   CONTINUE

*   ...the number of arguments is determined by the next constant:
        NCON = NCON + 1
        NARG = NINT( CON( ICON + NCON - 1 ) )

*   ...loop to repeatedly find the maximum of the two top of stack
*      entries, decrementing the stack pointer each time:
        DO I = 1, ( NARG - 1 )
          CALL VEC_MAXR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                     S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
          TOS = TOS - 1
        ENDDO
        GO TO 100


*   TRN_OP_DIM - Fortran DIM function.
   30   CONTINUE
        CALL VEC_DIMR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                   S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_MOD - Fortran MOD function.
   31   CONTINUE
        CALL VEC_MODR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                   S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_SIGN - Fortran SIGN function.
   32   CONTINUE
        CALL VEC_SIGNR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                    S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_ATN2 - Fortran ATAN2 function.
   33   CONTINUE
        CALL VEC_ATN2R( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                    S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_AT2D - Fortran ATAN2 function (degrees).
   34   CONTINUE
        CALL VEC_AT2DR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                    S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_IDV - Integer division.
   35   CONTINUE
        CALL VEC_IDVR( BADL, NDAT, S( 1, TOS - 1 ), S( 1, TOS ),
     :                  S( 1, TOS - 1 ), IERRL, NERRL, NSTATL )
        TOS = TOS - 1
        GO TO 100


*   TRN_OP_QIF - If-then-else function (like the C "a ? b : c" construct).
   36   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS - 2 ) .NE. 0 ) THEN
              S( I, TOS - 2 ) = S( I, TOS - 1 )
           ELSE
              S( I, TOS - 2 ) = S( I, TOS )
           END IF
        END DO
        TOS = TOS - 2
        GO TO 100

*   TRN_OP_LDBAD - Load "bad" value on to stack.
   37   CONTINUE
        TOS = TOS + 1
        DO I = 1, NDAT
          S( I, TOS ) = VAL__BADR
        ENDDO
        NSTATL = TRN__BADCN	! "bad" constant explicitly specified
        IERRL = 1
        NERRL = NDAT - NERR
        GO TO 100

*   TRN_OP_EQ - .EQ. operator
   38   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS - 1 ) .EQ. S( I, TOS ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_NE - .NE. operator
   39   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS - 1 ) .NE. S( I, TOS ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_GT - .GT. operator
   40   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS - 1 ) .GT. S( I, TOS ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_GE - .GE. operator
   41   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS - 1 ) .GE. S( I, TOS ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_LT - .LT. operator
   42   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS - 1 ) .LT. S( I, TOS ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_LE - .LE. operator
   43   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS - 1 ) .LE. S( I, TOS ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_OR - .OR. operator
   44   CONTINUE

        DO I = 1, NDAT
           IF( ( S( I, TOS - 1 ) .NE. 0 ) .OR.
     :         ( S( I, TOS ) .NE. 0 ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_AND - .AND. operator
   45   CONTINUE

        DO I = 1, NDAT
           IF( ( S( I, TOS - 1 ) .NE. 0 ) .AND.
     :         ( S( I, TOS ) .NE. 0 ) ) THEN
              S( I, TOS - 1 ) = 1
           ELSE
              S( I, TOS - 1 ) = 0
           END IF
        END DO
        TOS = TOS - 1
        GO TO 100

*   TRN_OP_NOT - .NOT. operator
   46   CONTINUE

        DO I = 1, NDAT
           IF( S( I, TOS ) .NE. 0 ) THEN
              S( I, TOS ) = 0
           ELSE
              S( I, TOS ) = 1
           END IF
        END DO
        GO TO 100


*   Control passes to here after each operation on the arithmetic stack
*   is complete.
  100   CONTINUE


*   If numerical errors occurred during the last stack operation, ensure
*   the local bad data flag is set and update the error count.
        IF( NSTATL .NE. SAI__OK ) THEN
          BADL = .TRUE.
          NERR = NERR + NERRL


*   If this is the first error, or if it occurred nearer the beginning
*   of the data array than any previous error, then set the error
*   pointer and the numerical error status.
          IF( ( NSTAT .EQ. SAI__OK ) .OR. ( IERRL .LT. IERR ) ) THEN
            IERR = IERRL
            NSTAT = NSTATL
          ENDIF
        ENDIF


*   End of "obtain successive operation codes from the input stream"
*   loop.
      ENDDO


*   Exit routine.
      END


