*+  GEN_NORMFE - Normalise an array at a given element
      SUBROUTINE GEN_NORMFE( NELM, REF, DATA, VAR, QUAL, QUALITY,
     :  FLAGGED, FBAD, VARIANCE )
*    Description :
*     This routine normalises a given real array so that the value at
*     the given reference element becomes 1.0 and the other elements
*     are scaled accordingly. If there is a variance array it is also
*     scaled with the data. If the reference element just happens to
*     be a bad value, the nearest good value will be chosen. The actual
*     element used will be returned in REF.
*    Invocation :
*     CALL GEN_NORMFE( NELM, REF, DATA, VAR, QUAL, QUALITY, FLAGGED,
*     :  FBAD, VARIANCE )
*    Parameters :
*     NELM            = INTEGER( READ )
*           Number of elements in the arrays.
*     REF             = INTEGER( UPDATE )
*           Reference element such that DATA( REF ) will become 1.0
*     DATA( NELM )    = REAL( UPDATE )
*           Data array to be normalised.
*     VAR( NELM )     = REAL( UPDATE )
*           Variance array to be scaled with the data.
*     QUAL( NELM )    = BYTE( READ )
*           Quality array, indicating which elements to ignore.
*     QUALITY         = LOGICAL( READ )
*           Flag which is TRUE if the data has a quality array.
*     FLAGGED         = LOGICAL( READ )
*           Flag which is TRUE if the data quality is flagged by "magic" values.
*     FBAD            = REAL( READ )
*           "Magic" value used to flag bad data.
*     VARIANCE        = LOGICAL( READ )
*           Flag which is TRUE if there is a variance array.
*    Method :
*     <description of how the subroutine works>
*    Deficiencies :
*     There is no status argument. This has been done to be compatible
*     with the other Figaro GEN routines.
*    Bugs :
*    Authors :
*     S.M.Beard (ROE::SMB)
*    History :
*     6-Dec-1990: Original version.  (ROE::SMB)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
*    Import :
      INTEGER
     :  NELM          ! Number of elements in arrays
      LOGICAL
     :  QUALITY,      ! T if there is a valid quality array
     :  FLAGGED,      ! T if there are flagged values indicating quality
     :  VARIANCE      ! T if there is a valid variance array
      REAL
     :  FBAD          ! Value used to flag a bad value, if used.
      BYTE
     :  QUAL( NELM )  ! Data quality array, if any.
*    Import-Export :
      INTEGER
     :  REF           ! Reference element, such that DATA(REF) becomes 1.0
      REAL
     :  DATA( NELM ), ! Data array.
     :  VAR( NELM )   ! Variance array, if any.
*    External references :
*    Global variables :
*    Local Constants :
      REAL TOLER      ! Smallest real number indistinguishable from zero
      PARAMETER ( TOLER = 1.0E-30 )
      BYTE GOOD       ! Good data quality value
      PARAMETER ( GOOD = 0 )
*    Local variables :
      INTEGER
     :  I             ! Loop index
      REAL
     :  FACTOR,       ! Scaling factor to be applied to the data.
     :  FACTORSQ      ! Scaling factor squared.
*    Internal References :
*    Local data :
*-

*   Determine what sort of quality handling is required

      IF ( QUALITY ) THEN

*      Quality handling is by means of a quality array.

*      Obtain the data value at the reference element, and use this as
*      the scaling factor. (If the reference element happens to be a bad
*      point, use the next nearest good point).

         FACTOR = DATA(REF)

         DO WHILE ( (QUAL(REF).NE.GOOD) .AND. (REF.LT.NELM) )

            REF = REF + 1
            FACTOR = DATA(REF)
         END DO

*      Check that the factor is not almost zero. If it is, leave the
*      data alone.

         IF ( ABS(FACTOR) .GT. TOLER ) THEN

*         Initialise the factor squared.

            FACTORSQ = FACTOR * FACTOR

*         Scale the data array accordingly, and the variance array as
*         well if there is one (ignoring bad values).

            DO I = 1, NELM

               IF ( QUAL(I) .EQ. GOOD ) THEN

                  DATA(I) = DATA(I) / FACTOR

                  IF ( VARIANCE ) THEN

                     VAR(I) = VAR(I) / FACTORSQ
                  END IF
               END IF
            END DO
         END IF

      ELSE IF ( FLAGGED ) THEN

*      Quality handling is by means of a flagged values.

*      Obtain the data value at the reference element, and use this as
*      the scaling factor. (If the reference element happens to be a bad
*      point, use the next nearest good point).

         FACTOR = DATA(REF)

         DO WHILE ( (FACTOR.NE.FBAD) .AND. (REF.LT.NELM) )

            REF = REF + 1
            FACTOR = DATA(REF)
         END DO

*      If the factor is very nearly zero, leave the arrays alone.

         IF ( ABS(FACTOR) .LT. TOLER ) THEN

*         Initialise the factor squared.

            FACTORSQ = FACTOR * FACTOR

*         Scale the data array accordingly, and the variance array as
*         well if there is one (ignoring bad values).

            DO I = 1, NELM

               IF ( DATA(I) .NE. FBAD ) THEN

                  DATA(I) = DATA(I) / FACTOR

                  IF ( VARIANCE ) THEN

                     VAR(I) = VAR(I) / FACTORSQ
                  END IF
               END IF
            END DO
         END IF
      ELSE

*      There is no quality information.

*      Obtain the data value at the reference element, and use this as
*      the scaling factor.

         FACTOR = DATA(REF)

*      If the factor is very nearly zero, leave the arrays alone.

         IF ( ABS(FACTOR) .LT. TOLER ) THEN

*         Initialise the factor squared.

            FACTORSQ = FACTOR * FACTOR

*         Scale the data array accordingly, and the variance array as
*         well if there is one.

            DO I = 1, NELM

               DATA(I) = DATA(I) / FACTOR

               IF ( VARIANCE ) THEN

                  VAR(I) = VAR(I) / FACTORSQ
               END IF
            END DO
         END IF
      END IF

      END
