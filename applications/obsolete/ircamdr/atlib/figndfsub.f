      SUBROUTINE FIGNDFSUB( INARRAY, OUTARRAY, IDIMS1, IDIMS2, STATUS)

      IMPLICIT NONE                 ! no default typing allowed

      INTEGER IDIMS1, IDIMS2                  ! dimensions of data arrays

      REAL INARRAY( IDIMS1, IDIMS2 )  ! input data array

      REAL OUTARRAY( IDIMS1, IDIMS2 )  ! output data array

      INTEGER  STATUS                ! global status parameter

      INTEGER  I, J                  ! counters

*    loop round all rows in input image

      DO  J  =  1, IDIMS2

*       loop round all pixels in current row

         DO  I  =  1, IDIMS1
               OUTARRAY( I, J )  =  INARRAY( I, J)
         END DO

*    end of loop round all rows in input image

      END DO

*    end

      END
