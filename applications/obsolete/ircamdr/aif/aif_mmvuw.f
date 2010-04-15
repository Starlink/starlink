*+  AIF_MMVUW - Return minimum and maximum values in vectorised INTEGER*2 array.
      SUBROUTINE AIF_MMVUW( NPTS, ARRAY, RANGE, STATUS )
*    Description :
*     The vectorised array ARRAY( NPTS ) of type INTEGER*2 is searched for the
*     minimum and maximum values. These are returned in the array RANGE; the
*     minimum value is in RANGE(1) and the maximum value in RANGE(2). The
*     array given to this routine need not be 1-D. Any array can be passed
*     provided NPTS is set to the total size of the array (e.g. a 3 X 4 array
*     can be passed in provided NPTS is set to 12). An immediate return will
*     occur if STATUS has an error value on entry.
*    Invocation :
*     CALL AIF_MMVUW( NPTS, ARRAY, RANGE, STATUS )
*    Parameters :
*     NPTS          = INTEGER( READ )
*           The size of the array to be searched. (i.e. the product of
*           the dimensions of ARRAY).
*     ARRAY( NPTS ) = INTEGER*2( READ )
*           Array to be searched for minimum and maximum values.
*     RANGE( 2 )    = INTEGER*2( WRITE )
*           Array to contain the minimum and maximum values. RANGE(1) will
*           contain the minimum value in ARRAY and RANGE(2) will contain the
*           maximum value in ARRAY.
*     STATUS        = INTEGER( READ )
*           This is the global status, if STATUS has an error value on entry
*           to the routine then an immediate return will occur.
*    Method :
*     If no error on entry then
*        Set RANGE(1) and RANGE(2) equal to the value of the first element of
*        ARRAY.
*        For all elements of array from the second to the last
*           If a smaller value than RANGE(1) is encountered thn set RANGE(1)
*           to this value.
*           If a larger value than RANGE(2) is encountered thn set RANGE(2)
*           to this value.
*        Endfor
*     Endif
*    Authors :
*     Dave Baines (ROE::ASOC5)
*     Steven Beard (ROE::SMB)
*    History :
*     24/02/1984 : Original version                   (ROE::ASOC5)
*     10/06/1984 : Modified into generic AIF_ version (ROE::SMB)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER
     :  NPTS          ! size of array
      INTEGER*2
     :  ARRAY( NPTS ) ! data array
*    Export :
      INTEGER*2
     :  RANGE( 2 )    ! minimum and maximum values in ARRAY
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER
     :  X             ! index to array elements
*-

*    check for error on entry
      IF( STATUS .EQ. SAI__OK ) THEN

*       initialise minimum and maximum values
         RANGE( 1 ) = ARRAY( 1 )
         RANGE( 2 ) = ARRAY( 1 )

*       search all the remaining values of ARRAY for the min and max values
         DO X = 2, NPTS

            RANGE( 1 ) = MIN( RANGE( 1 ), ARRAY( X ) )
            RANGE( 2 ) = MAX( RANGE( 2 ), ARRAY( X ) )
         ENDDO
      ENDIF

      END
