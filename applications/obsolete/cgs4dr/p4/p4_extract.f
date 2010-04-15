*+  P4_EXTRACT - extract an averaged slice from a 2-D array into a 1-D
      SUBROUTINE P4_EXTRACT( XDIM, YDIM, IN, INQUAL, QUALITY, CUT,
     :  SLICE_START, SLICE_END, COUNTER, OUT, OUTQUAL, STATUS)
*    Description :
*     This routine extracts a 1-D slice from a 2-D array by averaging
*     rows or columns together, in a similar manner to the FIGARO
*     EXTRACT function. However, this routine can deal with data quality
*     if required. It is mainly used when producing a 1-D graph from
*     a 2-D dataset.
*    Invocation :
*      CALL P4_EXTRACT( XDIM, YDIM, IN, INQUAL, QUALITY, CUT,
*     :  SLICE_START, SLICE_END, COUNTER, OUT, OUTQUAL, STATUS)
*    Parameters :
*     XDIM                 = INTEGER( READ )
*        First dimension of input array
*     YDIM                 = INTEGER( READ )
*        Second dimension of data array
*     IN( XDIM, YDIM )     = REAL( READ )
*        The array whose rows or columns are to be averaged together.
*     INQUAL( XDIM, YDIM ) = BYTE( XDIM, YDIM )
*        Quality array associated with the input array. This only
*        needs to contain valid data if the QUALITY flag is .TRUE.
*     QUALITY              = LOGICAL( READ )
*        Flag which is .TRUE. if the quality array is to be used.
*     CUT                  = CHARACTER*1( READ )
*        The direction of the slice:
*        'X' means that columns are to be averaged together to generate
*            a 1-D array in the X direction.
*        'Y' means that rows are to be averaged together to generate
*            a 1-D array in the Y direction.
*     SLICE_START          = INTEGER( READ )
*        The pixel position at the start of the slice to be averaged
*     SLICE_END            = INTEGER( READ )
*        The pixel position at the end of the slice to be averaged
*     COUNTER( * )         = INTEGER( WRITE )
*        A work array used for keeping a tally of the number of data
*        values which have been summed together. It is only used when
*        QUALITY is .TRUE.
*        If CUT='X' this array should be at least XDIM in size.
*        If CUT='Y' this array should be at least YDIM in size.
*     OUT( * )             = REAL( WRITE )
*        The 1-D output array produced.
*        If CUT='X' this array should be XDIM in size.
*        If CUT='Y' this array should be YDIM in size.
*     OUTQUAL( * )         = BYTE( WRITE )
*        Data quality associated with the 1-D output array.
*        This will only contain valid data if QUALITY is .TRUE.
*        If CUT='X' this array should be XDIM in size.
*        If CUT='Y' this array should be YDIM in size.
*     STATUS               = INTEGER( UPDATE )
*        Global ADAM status.
*    Method :
*    Deficiencies :
*     The size of the COUNTER, OUT and OUTQUAL arrays should perhaps
*     be passed in, since their size cannot be checked here.
*    Bugs :
*    Authors :
*     S.M.Beard (REVAD::SMB)
*     P.N.Daly (JACH::PND)
*    History :
*     30-Jan-1990: Original version, based on P4_SLICE.        (SMB)
*     18-Feb-1993: Tidy code                                   (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  XDIM,               ! Dimensions of the data array
     :  YDIM
      REAL
     :  IN( XDIM, YDIM )    ! 2-D input data array
      BYTE
     :  INQUAL( XDIM, YDIM )! Quality array associated with data
      LOGICAL
     :  QUALITY             ! .TRUE. if the data quality is valid
      CHARACTER*1
     :  CUT                 ! The direction of the slice, 'X' or 'Y'
      INTEGER
     :  SLICE_START,        ! Position of start of slice
     :  SLICE_END           ! Position of end of slice
*    Export :
      INTEGER
     :  COUNTER( * )        ! 1-D counter work array
      REAL
     :  OUT( * )            ! 1-D output data array
      BYTE
     :  OUTQUAL( * )        ! Quality associated with output data array
*    Status :
      INTEGER
     :  STATUS              ! Global status
*    External references :
*    Global variables :
*    Local Constants :
      BYTE
     :  GOOD,               ! Good quality
     :  BAD                 ! Bad quality
      PARAMETER ( GOOD = 0,
     :             BAD = 1 )
*    Local variables :
      INTEGER
     :  IX,                 ! Array X index
     :  IY                  ! Array Y index
      REAL
     :  SIZE                ! Temporary variable for holding slice size
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check in which direction the slice is to be made
      IF ( CUT .EQ. 'X' ) THEN

*      The slice is to be made in the X direction.
*      Determine if data quality is to be used.
         IF ( QUALITY ) THEN

*         Quality is to be used.
*         Initialise the output array and counter array to zero
            DO IX = 1, XDIM

               OUT( IX ) = 0.0
               COUNTER( IX ) = 0
            END DO

*         Scan the rows within the given slice
            DO IY = SLICE_START, SLICE_END

*            Scan the columns, totalling the good data values and
*            keeping track of the count.
               DO IX = 1, XDIM

                  IF ( INQUAL( IX, IY ) .EQ. GOOD ) THEN

                     OUT( IX ) = OUT( IX ) + IN( IX, IY )
                     COUNTER( IX ) = COUNTER( IX ) + 1
                  END IF
               END DO
            END DO

*         Finally divide by the count to get the mean. The quality
*         of the result should be good unless the counter array is
*         completely zero (i.e. every single data value was bad).
            DO IX = 1, XDIM

               IF ( COUNTER( IX ) .GT. 0 ) THEN

                  OUT( IX ) = OUT( IX ) / REAL( COUNTER( IX ) )
                  OUTQUAL( IX ) = GOOD
               ELSE

                  OUTQUAL( IX ) = BAD
               END IF
            END DO
         ELSE

*         Quality is not to be used.
*         Initialise the output array only to zero. (This time
*         the counter array is not required).
            DO IX = 1, XDIM

               OUT( IX ) = 0.0
            END DO

*         Scan the rows within the given slice
            DO IY = SLICE_START, SLICE_END

*            Scan the columns, totalling all the data values.
               DO IX = 1, XDIM

                  OUT( IX ) = OUT( IX ) + IN( IX, IY )
               END DO
            END DO

*         Finally divide by the size of the slice to get the mean.
            SIZE = REAL( SLICE_END - SLICE_START + 1 )
            DO IX = 1, XDIM

               OUT( IX ) = OUT( IX ) / SIZE
            END DO
         END IF
      ELSE

*      The slice is to be made in the Y direction.
*      Determine if data quality is to be used.
         IF ( QUALITY ) THEN

*         Quality is to be used.
*         Scan the rows in the input array.
            DO IY = 1, YDIM

*            Initialise the output array and counter array to zero.
               OUT( IY ) = 0.0
               COUNTER( IY ) = 0

*            Scan the columns within the given slice, totalling the
*            good data values and keeping track of the count.
               DO IX = SLICE_START, SLICE_END

                  IF ( INQUAL( IX, IY ) .EQ. GOOD ) THEN

                     OUT( IY ) = OUT( IY ) + IN( IX, IY )
                     COUNTER( IY ) = COUNTER( IY ) + 1
                  END IF
               END DO

*            Finally divide by the count to get the mean for this row.
*            The quality of the result should be good unless the
*            counter array is completely zero (i.e. every single data
*            value was bad).
               IF ( COUNTER( IY ) .GT. 0 ) THEN

                  OUT( IY ) = OUT( IY ) / REAL( COUNTER( IY ) )
                  OUTQUAL( IY ) = GOOD
               ELSE

                  OUTQUAL( IY ) = BAD
               END IF
            END DO
         ELSE

*         Quality is not to be used.
*         Initialise the size of the slice.
            SIZE = REAL( SLICE_END - SLICE_START + 1 )

*         Scan the rows in the input array.
            DO IY = 1, YDIM

*            Initialise the output array to zero.
               OUT( IY ) = 0.0

*            Scan the columns within the given slice, totalling all the
*            data values.
               DO IX = SLICE_START, SLICE_END

                  OUT( IY ) = OUT( IY ) + IN( IX, IY )
               END DO

*            Finally divide by the size to get the mean for this row.
               OUT( IY ) = OUT( IY ) / SIZE
            END DO
         END IF
      END IF

      END
