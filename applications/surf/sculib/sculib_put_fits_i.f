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
*     The FITS array must contain a final entry of simply 'END' unless
*     N_FITS is 0. The END entry is automatically moved to the last field
*     as entries are added to the FITS array.

*  Invocation:
*     CALL SCULIB_PUT_FITS_I (MAX_FITS, N_FITS, FITS, NAME, VALUE,
*    :  COMMENT, STATUS)

*  Arguments:
*     MAX_FITS                       = INTEGER (Given)
*           the size of the FITS array
*     N_FITS                         = INTEGER (Given and returned)
*           the number of FITS items currently in the FITS array.
*           This should include an 'END' entry. If N_FITS is 1 it will
*           be assumed to be an array with just an 'END'. If N_FITS=0
*           on entry the array will be empty and N_FITS will be changed to 2
*           on exit (the entry and the END card).
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
*     Copyright (C) 1995-2000 Particle Physics and Astronomy
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

      IF (N_FITS .LT. 0) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('N',N_FITS)
         CALL MSG_SETC ('NAME', NAME)
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: bad input value of '//
     :     'N_FITS for key ^NAME (N_FITS = ^N)', STATUS)
      ELSE IF (N_FITS+1 .GT. MAX_FITS) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI('N',N_FITS+1)
         CALL MSG_SETI('M',MAX_FITS)
         CALL MSG_SETC ('NAME', NAME)
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: size of FITS array '//
     :     'has been exceeded with key ^NAME (^N > ^M)', STATUS)
      ELSE IF (CHR_LEN(NAME) .GT. 8) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC ('NAME', NAME)
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: invalid name for '//
     :     'FITS item - ^NAME', STATUS)
      ELSE IF (CHR_LEN(NAME) .EQ. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ', 'SCULIB_PUT_FITS_I: blank name given '//
     :     'for FITS item', STATUS)
      ELSE IF (FITS(N_FITS) .NE. 'END' .AND. N_FITS .GT. 0) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP(' ','SCULIB_PUT_FITS_I: Last card was not END',
     :        STATUS)
      ELSE

*     If we have no entries, make sure we are using the first one
         IF (N_FITS .EQ. 0) N_FITS = 1

*     Copy in the name, value and comment, overwriting 'END' if
*     necessary
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

*     Increment the number of keywords
         N_FITS = N_FITS + 1

*     Rewrite the END card
         FITS(N_FITS) = 'END'

      END IF

      END
