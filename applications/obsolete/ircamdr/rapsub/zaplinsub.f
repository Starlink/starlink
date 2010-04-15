
*+  ZAPLINSUB - interpolates over bad rows or columns

      SUBROUTINE ZAPLINSUB( DIMS1, DIMS2, ARRAY, START, FINISH,
     :                      ROW, NOISE, STATUS )

*    Description :
*
*     This routine interpolates across bad rows or columns as
*     specified by the user. More than one row or column may be
*     deleted at a time. If any of the rows or columns to be
*     replaced are edge rows or columns, interpolation is not
*     possible, and duplication of the nearest good row or column
*     is used. If NOISE is true on entry then pseudo Poisson
*     noise is added.
*
*    Method :
*
*     The routine takes as input a data array and its dimensions,
*     and also the start and finish rows/columns that are to be
*     modified. The logical ROW is set to true if rows are to be
*     worked on, and false if it is to be columns. If either START
*     or FINISH are at an edge, no interpolation can take place,
*     and the 'bad' data is replaced with the most adjacent row or
*     column of 'good' data. Interpolation is used if the rows or
*     columns concerned are flanked on either side by at least one
*     row or column of 'good' data. If the NOISE argument is set
*     to true on entry, then pseudo Poisson noise is added to the
*     interpolated data. After modification, the same array is
*     returned.
*
*    Deficiencies :
*
*     Uses the Vax specific function SECNDS to generate
*     a seed random number for Poisson noise addition.
*
*    Bugs :
*
*     None are known at this time.
*
*    Authors :
*
*     Mark McCaughrean UOE (REVA::MJM)
*
*    History :
*
*     27-07-85 : First implementation (REVA::MJM)
*     09-12-85 : Added Poisson noise option (UKTH::MARK)
*     20-Jul-1994 Changed random seed initialisation to NAG G05 routine
*                 also changed argum,ents so that DIMS input separately
*                 and routine will still compile  (SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE

*    Global constants :

      INCLUDE  'SAE_PAR'

*    Import :

      INTEGER
     :    DIMS1,     ! dimensions of input array
     :    DIMS2,     ! dimensions of input array
     :    START,         ! first row/column to be modified
     :    FINISH         ! last row/column to be modified

      LOGICAL
     :    ROW,           ! true if rows to be modified - else columns
     :    NOISE          ! true if Poisson noise to be added

*    Import-Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )    ! array with data to be altered

*    Status :

      INTEGER  STATUS

*    Local variables :

      INTEGER
     :      DUMMY,          ! used to swap START and FINISH if needed
     :      STARTM,         ! START minus 1
     :      FINISHP,        ! FINISH plus 1
     :      I, J, II, JJ    ! general counter variables
      INTEGER TICKS, SEED ! Random seed

      REAL
     :      DIFF,     ! intensity difference between START and FINISH
     :      DIFFINT,  ! increment in intensity per row/column
     :      VALUE     ! Poisson noisy value

*
*-
*    Error check on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF


*    make sure that START is less than or equal to FINISH

      IF( START .GT. FINISH ) THEN
         DUMMY  =  START
         START  =  FINISH
         FINISH =  DUMMY        ! swap START and FINISH over
      ENDIF

*    initialise STARTM and FINISHP

      STARTM   =  START  - 1
      FINISHP  =  FINISH + 1

*    if noise is to be added then initialise the seed value

      IF( NOISE ) THEN

*    initialise the random number generator seed using system clock
         CALL PSX_TIME( TICKS, STATUS )
         SEED = ( TICKS / 4 ) * 4 + 1
         CALL PDA_RNSED( SEED )

      ENDIF

*    now see whether it is rows or columns to be modified

      IF( ROW ) THEN            ! rows to be zapped

*    check to see where rows are in array

         IF( START .EQ. 1 ) THEN       ! bottom edge

            DO  I  =  1, DIMS1
               DO  J  =  START, FINISH
                  ARRAY( I, J ) = ARRAY( I, FINISHP )  ! duplicate
                  IF( NOISE ) THEN
                     CALL POISSON( ARRAY( I, J ), VALUE, STATUS )
                     ARRAY( I, J )  =  VALUE
                  END IF
               END DO
            END DO

         ELSE IF( FINISH .EQ. DIMS2 ) THEN     ! top edge

            DO  I  =  1, DIMS1
               DO  J  =  START, FINISH
                  ARRAY( I, J ) = ARRAY( I, STARTM )   ! duplicate
                  IF( NOISE ) THEN
                     CALL POISSON( ARRAY( I, J ), VALUE, STATUS )
                     ARRAY( I, J )  =  VALUE
                  END IF
               END DO
            END DO

         ELSE       ! neither edge - centre somewhere

            DO  I  =  1, DIMS1

*             find the intensity difference between the good rows
*             either side of the ones to be zapped

               DIFF = ARRAY( I, FINISHP ) - ARRAY( I, STARTM )

*             find the intensity step per pixel over the gap

               DIFFINT = DIFF / ( FINISHP - STARTM )

               DO  J  =  START, FINISH

*                work out how far across the gap you are

                  JJ  =  J - STARTM

*                now add the step per pixel times the number of steps
*                to the starting value to interpolate into the gap

                  ARRAY( I, J ) = ARRAY( I, STARTM ) + (DIFFINT * JJ )

                  IF( NOISE ) THEN
                     CALL POISSON( ARRAY( I, J ), VALUE, STATUS )
                     ARRAY( I, J )  =  VALUE
                  END IF

               END DO

            END DO

         END IF

      ELSE                 ! columns to be zapped

*    check to see where columns are in array

         IF( START .EQ. 1 ) THEN       ! left edge

            DO  J  =  1, DIMS2
               DO  I  =  START, FINISH
                  ARRAY( I, J ) = ARRAY( FINISHP, J )  ! duplicate
                  IF( NOISE ) THEN
                     CALL POISSON( ARRAY( I, J ), VALUE, STATUS )
                     ARRAY( I, J )  =  VALUE
                  END IF
               END DO
            END DO

         ELSE IF( FINISH .EQ. DIMS1 ) THEN     ! right edge

            DO  J  =  1, DIMS2
               DO  I  =  START, FINISH
                  ARRAY( I, J ) = ARRAY( STARTM, J )   ! duplicate
                  IF( NOISE ) THEN
                     CALL POISSON( ARRAY( I, J ), VALUE, STATUS )
                     ARRAY( I, J )  =  VALUE
                  END IF
               END DO
            END DO

         ELSE       ! neither edge - centre somewhere

            DO  J  =  1, DIMS2

*             find the intensity difference between the good
*             columns either side of the ones to be zapped

               DIFF = ARRAY( FINISHP, J ) - ARRAY( STARTM, J )

*             find the intensity step per pixel over the gap

               DIFFINT = DIFF / ( FINISHP - STARTM )

               DO  I  =  START, FINISH

*                work out how far across the gap you are

                  II  =  I - STARTM

*                now add the step per pixel times the number of steps
*                to the starting value to interpolate into the gap

                  ARRAY( I, J ) = ARRAY( STARTM, J ) + (DIFFINT * II )

                  IF( NOISE ) THEN
                     CALL POISSON( ARRAY( I, J ), VALUE, STATUS )
                     ARRAY( I, J )  =  VALUE
                  END IF

               END DO

            END DO

         END IF

      END IF


*    end and return

      END
