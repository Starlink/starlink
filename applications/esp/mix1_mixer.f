      SUBROUTINE MIX1_MIXER(XMAX,YMAX,ELEMS,SEED,ARRAY1,ARRAY2,STATUS)
*+
*  Name: 
*     MIX1_MIXER

*  Purpose:
*     Swaps pixels in the image so that any original structure is 
*     destroyed but the pixel statistics are retained.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MIX1_MIXER(XMAX,YMAX,ELEMS,SEED,ARRAY1,ARRAY2,STATUS)
      
*  Description:
*     Selected pixel pairs randomly and swaps their values
*     as a way of scrambling the data in the image.

*  Arguments:
*     XMAX = INTEGER (Given)
*        The length of the image x axis. Units pixels.
*     YMAX = INTEGER (Given)
*        The length of the y axis of the image. Units pixels.
*     ELEMS = INTEGER (Given)
*        The number of pixels in the image.
*     SEED = INTEGER (Given)
*        The seed for the random number generator.
*     ARRAY1(ELEMS) = REAL (Given)
*        The array containing the source NDF image count values.
*     ARRAY2(ELEMS) = REAL (Returned)
*        The array that eventually contains the scrambled image.
*     STATUS = INTEGER (Given and Returned) 
*        The global status.     

*  Authors:
*     GJP: Grant Privett (STARLINK)
*     DSB: David S Berry (JAC,UCLan)

*  History:
*     14-OCT-1992 (GJP)
*     (Original version)
*     30-SEP-2008 (DSB)
*     Added SEED argument.

*  Bugs:
*     None known.

*-

*  Type Definitions:                  ! No implicit typing
      IMPLICIT NONE
                                                                        
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! Standard SAE constants
               
*  Arguments Given:
      INTEGER ELEMS                   ! Number of pixels in the data array
      INTEGER XMAX                    ! Length of the image x axis
      INTEGER YMAX                    ! Length of the image y axis
      INTEGER SEED                    ! Random number seed
      REAL ARRAY1(ELEMS)              ! Array containing input NDF 

*  Arguments Returned:
      REAL ARRAY2(ELEMS)              ! Image array containing the
                                      ! scrambled image

*  Status:     
      INTEGER STATUS                  ! Global status

*  Local variables:                 
      INTEGER I                       ! Array element index
      INTEGER J                       ! Array element index
      INTEGER NUMBER                  ! Number of swaps so far
      INTEGER PERC                    ! Percentage of work done
      REAL    N                       ! Number of pixel pairs swapped between
                                      ! displays of the progress
      REAL    VALUE                   ! Pixel count value 
      REAL    RND                     ! Random number  

*.

*   Check the inherited global status.
      IF (STATUS.NE.SAI__OK) RETURN

*   Transfer the source image into the output image.
      DO 5 I=1,ELEMS
         ARRAY2(I)=ARRAY1(I)
 5    CONTINUE

*   Initialise the random number generator.  
      CALL MIX1_RAND(0,SEED,RND,STATUS)

*   Number of pixel pairs swapped between 
*   displays of the progress.
 
*   Perform the pixel pair swaps.
      N=100000.

*   Forward through the array.
      NUMBER=0
      DO 10 I=1,ELEMS

*      Increment swap counter.
         NUMBER=NUMBER+1

*      Indicate that something is happening.
         IF (NUMBER.EQ.NINT(NUMBER/N)*N) THEN 
            PERC=NINT(NUMBER*50./ELEMS)
            CALL MSG_SETI('PERC',PERC)
            CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
         END IF

*      Determine second pixel to be swapped. I defines the first.
         CALL MIX1_RAND(1,666,RND,STATUS)
         J=INT(RND*ELEMS+1)
         
*      Swap the pixels.
         VALUE=ARRAY2(I)
         ARRAY2(I)=ARRAY2(J)
         ARRAY2(J)=VALUE

 10   CONTINUE   


*   Repeat again backward through array. Just in case! 
      
*   Perform the pixel pair swaps.
      DO 20 I=ELEMS,1,-1

*      Increment swap counter.
         NUMBER=NUMBER+1

*      Indicate that something is happening.
         IF (NUMBER.EQ.NINT(NUMBER/N)*N) THEN 
            PERC=NINT(NUMBER*50./ELEMS)
            CALL MSG_SETI('PERC',PERC)
            CALL MSG_OUT(' ','Percentage done so far: ^PERC',STATUS)
         END IF

*      Determine second pixel to be swapped. I defines the first.
         CALL MIX1_RAND(1,666,RND,STATUS)
         J=INT(RND*ELEMS+1)
         
*      Swap the pixels.
         VALUE=ARRAY2(I)
         ARRAY2(I)=ARRAY2(J)
         ARRAY2(J)=VALUE

 20   CONTINUE   


 9999 CONTINUE

      END
