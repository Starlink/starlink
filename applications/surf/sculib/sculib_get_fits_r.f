      SUBROUTINE SCULIB_GET_FITS_R (MAX_FITS, N_FITS, FITS, NAME,
     :  VALUE, STATUS)
*+
*  Name:
*     SCULIB_GET_FITS_R

*  Purpose:
*     get the value of specified FITS real keyword

*  Description:
*     This routine will get the value of a specified FITS real keyword
*     held in the FITS extension of an NDF file. The FITS extension must
*     have been read into the input array FITS before this routine is called.
*
*        The routine assumes that each line in the FITS array will contain
*     a string with format:-
*
*      "KEYWORD=VALUE           / this is a comment"
*
*     It will search the input array for a line containing the required
*     keyword and return VALUE. If the keyword is not found an error will
*     be reported and bad status returned. If the keyword is found but the
*     line does not conform to the above format an error will be reported
*     and bad status returned.

*  Invocation:
*     CALL SCULIB_GET_FITS_R (MAX_FITS, N_FITS, FITS, NAME, VALUE,
*    :  STATUS)

*  Arguments:
*     MAX_FITS             = INTEGER (Given)
*           the maximum number of items in the FITS array
*     N_FITS               = INTEGER (Given)
*           the actual number of items in the FITS array
*     FITS (MAX_FITS)      = CHARACTER*(*) (Given)
*           array containing the FITS items
*     NAME                 = CHARACTER*(*) (Given)
*           the name of the FITS keyword whose value is required
*     VALUE                = REAL (Returned)
*           the value of the FITS keyword
*     STATUS               = INTEGER (Given and returned)
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
*     26-JUL-1995: original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                ! for VAL__BADR

*  Arguments Given:
      INTEGER MAX_FITS
      INTEGER N_FITS
      CHARACTER*(*) FITS (MAX_FITS)
      CHARACTER*(*) NAME

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL    VALUE

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER      CHR_LEN             ! CHR used-string length function

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER      I                   ! DO loop index
      INTEGER      IPOS                ! position of = in string
      INTEGER      JPOS                ! position of / in string
      CHARACTER*80 KEYNAME             ! name of FITS keyword in string
      LOGICAL      LOOPING             ! .TRUE. while looping
      CHARACTER*80 STEMP               ! scratch string
      CHARACTER*80 UNAME               ! upper case version of NAME

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      UNAME = NAME
      CALL CHR_UCASE (UNAME)
      VALUE = VAL__BADR

*  loop through the FITS array

      IF (N_FITS .GT. 0) THEN
         I = 0
         LOOPING = .TRUE.

         DO WHILE (LOOPING)
            I = I + 1

*  the FITS keyword name should be in first part of the string, immediately
*  in front of the =

            IPOS = INDEX (FITS(I),'=')
            IF (IPOS .GT. 1) THEN
               KEYNAME = FITS(I) (1:IPOS-1)
               CALL CHR_UCASE (KEYNAME)
               CALL CHR_RMBLK (KEYNAME)

               IF (CHR_LEN(KEYNAME) .EQ. 0) THEN
                  STATUS = SAI__ERROR
               ELSE
                  IF (KEYNAME(:CHR_LEN(KEYNAME)) .EQ.
     :                UNAME(:CHR_LEN(UNAME)))    THEN

*  OK, we've found the FITS keyword, the value should be between the = and
*  a /

                     JPOS = INDEX (FITS(I),'/')

                     IF ((JPOS .EQ. 0)       .OR.
     :                   (JPOS .EQ. IPOS+1)) THEN
                        STATUS = SAI__ERROR
                     ELSE
                        STEMP = FITS(I) (IPOS+1:JPOS-1)
                        CALL CHR_LDBLK (STEMP)
                        CALL CHR_CTOR (STEMP, VALUE, STATUS)
                     END IF
                  END IF
               END IF
            END IF

*  break out of loop if we've found a value, reached the end of the FITS
*  array, or if an error has occured

            IF ((VALUE .NE. VAL__BADR).OR.
     :          (STATUS .NE. SAI__OK) .OR.
     :          (I .EQ. N_FITS))      THEN
               LOOPING = .FALSE.
            END IF

         END DO

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ', 'SCULIB_GET_FITS_R: error decoding -',
     :        STATUS)
            CALL MSG_SETC ('LINE', FITS(I))
            CALL ERR_REP (' ', '^LINE', STATUS)
         END IF
      END IF

*  check that a value for the parameter was found

      IF (VALUE .EQ. VAL__BADR) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('NAME', NAME)
            CALL ERR_REP (' ', 'SCULIB_GET_FITS_R: failed to find '//
     :        'FITS item ^NAME', STATUS)
         END IF
      END IF

      END
