      SUBROUTINE SCULIB_PUT_FITS_I (MAX_FITS, N_FITS, FITS, NAME,
     :  VALUE, COMMENT, STATUS)
*+
*  Name:
*     SCULIB_PUT_FITS_I

*  Purpose:
*     write an integer to a FITS item specified

*  Description:
*     This routine writes a FITS integer item into a character array
*     ready to be written out to the .MORE.FITS structure in an NDF file.
*     An error will be reported and bad status returned if the name of the
*     FITS item is blank or more than 8 characters long. An error will also
*     occur if the FITS character array is full.

*  Invocation:
*     CALL SCULIB_PUT_FITS_I (MAX_FITS, N_FITS, FITS, NAME, VALUE,
*    :  COMMENT, STATUS)

*  Arguments:
*     MAX_FITS                       = INTEGER (Given)
*           the size of the FITS array
*     N_FITS                         = INTEGER (Given and returned)
*           the number of FITS items currently in the FITS array
*     FITS (MAX_FITS)                = CHARACTER*80 (Given and returned)
*           array containing the FITS items
*     NAME                           = CHARACTER*(*) (Given)
*           the name of the FITS item to be written
*     VALUE                          = INTEGER (Given)
*           the value to be assigned to the item
*     COMMENT                        = CHARACTER*(*) (Given)
*           a comment applying to the FITS item
*     STATUS                         = INTEGER (Given and returned)
*           global status

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     15-AUG-1995: original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER MAX_FITS
      CHARACTER*(*) NAME
      INTEGER VALUE
      CHARACTER*(*) COMMENT

*  Arguments Given & Returned:
      INTEGER N_FITS
      CHARACTER*(80) FITS (MAX_FITS)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                      ! CHR used string length function

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER CPTR                         ! index of start of comment
      INTEGER NCHAR                        ! length of encoded number
      CHARACTER*32 STEMP                   ! scratch string

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      N_FITS = N_FITS + 1

      IF (N_FITS .LT. 1) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: bad input value of '//
     :     'N_FITS', STATUS)
      ELSE IF (N_FITS .GT. MAX_FITS) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: size of FITS array '//
     :     'has been exceeded', STATUS)
      ELSE IF (CHR_LEN(NAME) .GT. 8) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC ('NAME', NAME)
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: invalid name for '//
     :     'FITS item - ^NAME', STATUS)
      ELSE IF (CHR_LEN(NAME) .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: blank name given '//
     :     'for FITS item', STATUS)
      ELSE

         FITS (N_FITS) = NAME
         FITS (N_FITS)(9:9) = '='
         CALL CHR_ITOC (VALUE, STEMP, NCHAR)
         IF (NCHAR .LE. 20) THEN
            FITS (N_FITS)(31-NCHAR:30) = STEMP
            CPTR = 32
         ELSE
            FITS (N_FITS)(11:10+NCHAR) = STEMP
            CPTR = 12 + NCHAR
         END IF

         FITS (N_FITS)(CPTR:CPTR) = '/'
         FITS (N_FITS)(CPTR+2:) = COMMENT
      END IF

      END
