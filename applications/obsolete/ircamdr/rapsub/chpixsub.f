
*+  CHPIXSUB - replaces pixel value with specified value

      SUBROUTINE CHPIXSUB( DIMS1, DIMS2, ARRAY, XCOORD, YCOORD,
     :                     NEWVAL, OLDVAL, STATUS )

*    Description :
*
*     This routine replaces a specified pixel with an input
*     value and returns the old value of the pixel.
*
*    Parameters :
*
*     DIMS( 2 ) = INTEGER( READ )
*           The dimensions of the input array
*     ARRAY( DIMS(1), DIMS(2) ) = REAL( READ, WRITE )
*           The input data aray
*     XCOORD = INTEGER( READ )
*           The x coordinate of the pixel to be replaced
*     YCOORD = INTEGER( READ )
*           The y coordinate of the pixel to be replaced
*     NEWVAL = REAL( READ )
*           New value of deglitched pixel
*     OLDVAL = REAL( WRITE )
*           Old value of deglitched pixel
*
*    Method :
*
*     The routine takes as input a data array, its dimensions, and
*     location of the pixel to be deglitched within that array.
*     The mean of the eight surrounding pixels is calculated (or
*     the relevant number if the pixel is at an edge), and the
*     old pixel value is replaced with this mean, and the data
*     array returned, along with the original and new values of the
*     deglitched pixel.
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
*     28-06-85 : Modified GLITCHSUB to replace specified pixel with
*              : an input specified value instead of calculating it
*              : from local mean. (REVA::MJM)
*     12-AUG-1994 Changed DIM arguments so that routine will compile(SKL@JACH)
*
*    Type Definitions :

      IMPLICIT  NONE

*    Global constants :

      INCLUDE  'SAE_PAR'


*    Import :

      INTEGER
     :    DIMS1, DIMS2,  ! dimensions of input array
     :    XCOORD,        ! x coord of pixel to be deglitched
     :    YCOORD         ! y coord of pixel to be deglitched

      REAL
     :    NEWVAL         ! replacement value

*    Import-Export :

      REAL
     :    ARRAY( DIMS1, DIMS2 )    ! array with data to be altered

*    Export :

      REAL
     :    OLDVAL         ! old value of pixel


*    Status :

      INTEGER  STATUS

*
*-
*    Error check on entry - return if not ok

      IF ( STATUS .NE. SAI__OK ) THEN
         RETURN
      ENDIF

*    set old value

      OLDVAL  =  ARRAY( XCOORD, YCOORD )

*    replace old value with new value in data array

      ARRAY( XCOORD, YCOORD )  =  NEWVAL

*    end and return

      END
