      SUBROUTINE SCULIB_CALC_NA_OFFSET(OFFSET_X, OFFSET_Y, NUM_CHAN,
     :     NUM_ADC, N_BOL, BOL_CHAN, BOL_ADC, U3, U4, U3_CENTRE,
     :     U4_CENTRE, X_BOL, Y_BOL, STATUS)
*+
*  Name:
*     SCULIB_CALC_NA_OFFSET

*  Purpose:
*     Calculate the bolometer offsets in nasmyth coordinates

*  Invocation:
*     SUBROUTINE SCULIB_CALC_NA_OFFSET(OFFSET_X, OFFSET_Y, NUM_CHAN,
*    :     NUM_ADC, N_BOL, BOL_CHAN, BOL_ADC, U3, U4, U3_CENTRE,
*    :     U4_CENTRE, BOL_X, BOL_Y, STATUS)

*  Description:
*     This routine calculates the nasmyth offset of a specified set
*     of bolometers. No astronomical coordinate transformations are
*     necessary.
*           BOL_X(I) = OFFSET_X + U3(I) - U3_CENTRE
*     and similarly for Y.

*  Arguments:
*     OFFSET_X = REAL (Given)
*          the x offset of the array origin from the 'centre'
*     OFFSET_Y = REAL (Given)
*          the y offset of the array origin from the 'centre'
*     NUM_CHAN               = INTEGER (Given)
*           the number of channels per A/D card
*     NUM_ADC                = INTEGER (Given)
*           the number of A/D cards
*     N_BOL                  = INTEGER (Given)
*           the actual number of bolometers
*     BOL_CHAN (N_BOL)       = INTEGER (Given)
*           channel numbers of bolometers
*     BOL_ADC (N_BOL)        = INTEGER (Given)
*           ADC numbers of bolometers
*     U3 (NUM_ADC,NUM_CHAN)  = REAL (Given)
*           the U3 offsets of the bolometers (arcsec)
*     U4 (NUM_ADC,NUM_CHAN)  = REAL (Given)
*           the U4 offsets of the bolometers (arcsec)
*     U3_CENTRE              = REAL (Given)
*           the U3 offset of the tracking `centre' on the array (arcsec)
*     U4_CENTRE              = REAL (Given)
*           the U4 offset of the tracking `centre' on the array (arcsec)
*     X_BOL (N_BOL)         = DOUBLE PRECISION (Returned)
*           the X coordinate of the bolometer (radians)
*     Y_BOL (N_BOL)        = DOUBLE PRECISION (Returned)
*           the Y coordinate of the bolometer (radians)

*  Notes:
*     - Only NASMYTH supported

*  Implementation Status:
*     - Pointing offsets are ignored

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     {enter_new_authors_here}
 
*  History:
*     1996 October 13(TIMJ):
*       Original version
*     {enter_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
*-

*  Type Definitions:
      IMPLICIT NONE
 
*  Global Constants:
      INCLUDE 'SAE_PAR'               ! SSE global definitions
 
*  Arguments Given:
      INTEGER NUM_CHAN
      INTEGER NUM_ADC
      INTEGER N_BOL
      REAL    OFFSET_X
      REAL    OFFSET_Y

      INTEGER BOL_ADC(N_BOL)
      INTEGER BOL_CHAN(N_BOL)
      REAL    U3(NUM_ADC, NUM_CHAN)
      REAL    U3_CENTRE
      REAL    U4(NUM_ADC, NUM_CHAN)
      REAL    U4_CENTRE

*  Arguments Returned:
      DOUBLE PRECISION X_BOL(N_BOL)
      DOUBLE PRECISION Y_BOL(N_BOL)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants :
      DOUBLE PRECISION ARCSEC2RAD         ! arcsec 2 radians conversion
      PARAMETER (ARCSEC2RAD = 4.84813681110D-6)

*  Local Variables:
      INTEGER ADC                ! ADC number in loop
      INTEGER BOL                ! Loop counter
      INTEGER CHAN               ! Channel number in loop


*.

      IF (STATUS .NE. SAI__OK) RETURN

 
      DO BOL = 1, N_BOL
 
*     calculate the Nasmyth offset
 
         CHAN = BOL_CHAN (BOL)
         ADC = BOL_ADC (BOL)
               
         X_BOL(BOL) = (DBLE(U3(CHAN,ADC)) - DBLE(U3_CENTRE) - 
     :        DBLE(OFFSET_X)) * ARCSEC2RAD
         Y_BOL(BOL) = (DBLE(U4(CHAN,ADC)) - DBLE(U4_CENTRE) -
     :        DBLE(OFFSET_Y)) * ARCSEC2RAD
            
      END DO

      END
