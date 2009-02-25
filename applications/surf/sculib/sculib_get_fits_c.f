      SUBROUTINE SCULIB_GET_FITS_C (MAX_FITS, N_FITS, FITS, NAME,
     :  VALUE, STATUS)
*+
*  Name:
*     SCULIB_GET_FITS_C


*  Purpose:
*     get the value of specified FITS character keyword


*  Language:
*     Starlink Fortran 77
 

*  Invocation:
*     CALL SCULIB_GET_FITS_C (MAX_FITS, N_FITS, FITS, NAME,
*     :  VALUE, STATUS)


*  Description:
*     This routine will get the value of a specified FITS character keyword
*     held in the FITS extension of an NDF file. The FITS extension must
*     have been read into the input array FITS before this routine is called.
*
*        The routine assumes that each line in the FITS array will contain
*     a string with format:-
*
*      "KEYWORD= 'VALUE'           / this is a comment"
*
*     The string is extracted from between the quotes and the '/' is not
*     required.
*
*     It will search the input array for a line containing the required
*     keyword and return VALUE. If the keyword is not found and error will
*     be reported and bad status returned. If the keyword is found but the
*     line does not conform to the above format an error will be reported
*     and bad status returned.
*
*     The returned string will not contain any unprintable characters.

*  Arguments:
*     MAX_FITS             = INTEGER (Given)
*           the maximum number of items in the FITS array
*     N_FITS               = INTEGER (Given)
*           the actual number of items in the FITS array
*     FITS (MAX_FITS)      = CHARACTER*(*) (Given)
*           array containing the FITS items
*     NAME                 = CHARACTER*(*) (Given)
*           the name of the FITS keyword whose value is required
*     VALUE                = CHARACTER*(*) (Returned)
*           the value of the FITS keyword
*     STATUS               = INTEGER (Given and returned)
*           global status 

*  Authors:
*     J.Lightfoot (jfl@roe.ac.uk)
*     T. Jenness (timj@jach.hawaii.edu)

*  Copyright:
*     Copyright (C) 2009 Science and Technology Facilities Council.
*     Copyright (C) 1995, 1997, 1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1995-JUL-26 (JFL):
*        Original version.
*     1997-NOV-19 (TIMJ):
*        Decode string by searching for quotes instead of a '/'.
*        This fixes a bug in the DRT that sometimes does not put on a comment if the
*        string is too long.
*     1999-AUG-03 (TIMJ):
*        Add copyright message to header.
*        Convert old header style to new.
*     1999-AUG-06 (TIMJ):
*        Tweak headers for use with PROLAT.
*     1999-AUG-19 (TIMJ):
*        Header tweaks to ease production of SSN72 documentation.
*     2009-FEB-25 (TIMJ):
*        Clean the returned string. Some data files have embedded \0.

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER MAX_FITS
      INTEGER N_FITS
      CHARACTER*(*) FITS (MAX_FITS)
      CHARACTER*(*) NAME

*  Arguments Given & Returned:


*  Arguments Returned:
      CHARACTER*(*) VALUE


*  Status:
      INTEGER STATUS


*  External references:
      INTEGER      CHR_LEN             ! CHR used-string length function


*  Global variables:


*  Local Constants:


*  Local variables:
      INTEGER      I                   ! DO loop index
      INTEGER      IPOS                ! position of = in string
      INTEGER      IPOSN               ! Generic position in string
      INTEGER      JPOS                ! position of last quote in string
      CHARACTER*80 KEYNAME             ! name of FITS keyword in string
      INTEGER      LENGTH              ! length of a string
      LOGICAL      LOOPING             ! .TRUE. while looping
      CHARACTER*80 STEMP               ! scratch string
      CHARACTER*80 UNAME               ! upper case version of NAME


*.

      IF (STATUS .NE. SAI__OK) RETURN

      UNAME = NAME
      CALL CHR_UCASE (UNAME)
      VALUE = '%&'

*  loop through the FITS array

      IF (N_FITS .GT. 0) THEN
         I = 0
         LOOPING = .TRUE.

         DO WHILE (LOOPING .AND. (STATUS .EQ. SAI__OK))
            I = I + 1

*  the FITS keyword name should be in first part of the string, immediately 
*  in front of the =

            IPOS = INDEX (FITS(I),'=')
            IF (IPOS .GT. 1) THEN
               KEYNAME = FITS(I) (1:IPOS-1)
               CALL CHR_UCASE (KEYNAME)
               CALL CHR_RMBLK (KEYNAME)

               IF (CHR_LEN(KEYNAME) .EQ. 0) THEN
                  IF (STATUS .EQ. SAI__OK) THEN
                     STATUS = SAI__ERROR
                     CALL ERR_REP (' ', 
     :                    'SCULIB_GET_FITS_C: error decoding -',
     :                    STATUS)
                     CALL MSG_SETC ('LINE', FITS(I))
                     CALL ERR_REP (' ', '^LINE', STATUS)
                     CALL ERR_REP(' ', 'No keyword in front of =',
     :                    STATUS)
                     CALL ERR_FLUSH(STATUS)
                  END IF
               ELSE
                  IF (KEYNAME(:CHR_LEN(KEYNAME)) .EQ.
     :                UNAME(:CHR_LEN(UNAME)))    THEN

*  OK, we've found the FITS keyword, the value should be between the = and
*  a /
*  In fact the value should be inside some quotes
*     Change logic so that we only extract a string between 
*     quotes and we do not care about a '/' (in case it is missing)

*     Look for a quote
                     IPOSN = IPOS
                     LENGTH = CHR_LEN(FITS(I))

                     CALL CHR_FIND(FITS(I), '''', .TRUE., IPOSN)

                     IF (IPOSN .LT. LENGTH) THEN

                        IPOS = IPOSN
                        IPOSN = IPOS + 1
*     Look for the second quote

                        CALL CHR_FIND(FITS(I), '''', .TRUE., IPOSN)

                        IF (IPOSN .LE. LENGTH) THEN
                           JPOS = IPOSN

*     Everything looks okay so extract the string

*     Check that there is something in the quotes
                           IF (JPOS - IPOS .LE. 1) THEN

                              VALUE = ' '
                              
                           ELSE

                              STEMP = FITS(I)(IPOS+1:JPOS-1)
                              CALL CHR_LDBLK(STEMP)
                              VALUE = STEMP

                           END IF

                        ELSE
                           STATUS = SAI__ERROR

                           CALL ERR_REP(' ', 'Could not find closing'//
     :                          ' quote in FITS string', STATUS)

                        END IF
                     
                     ELSE

                        STATUS = SAI__ERROR

                        CALL ERR_REP(' ','Could not find a quoted '//
     :                       'string in FITS entry', STATUS)

                     END IF

                  END IF
               END IF
            END IF
                              
*  break out of loop if we've found a value, reached the end of the FITS
*  array, or if an error has occured
   
            IF ((VALUE .NE. '%&')     .OR. 
     :          (STATUS .NE. SAI__OK) .OR.
     :          (I .EQ. N_FITS))      THEN
               LOOPING = .FALSE.
            END IF
                      
         END DO

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_REP (' ', 'SCULIB_GET_FITS_C: error decoding -',
     :        STATUS)
            CALL MSG_SETC ('LINE', FITS(I))
            CALL ERR_REP (' ', '^LINE', STATUS)
         END IF
      END IF
 
*  check that a value for the parameter was found

      IF (VALUE .EQ. '%&') THEN
         VALUE = ' '
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('NAME', NAME)
            CALL ERR_REP (' ', 'SCULIB_GET_FITS_C: failed to find '//
     :        'FITS item ^NAME', STATUS)
         END IF
      END IF

*   clean the string. In some cases the datafiles have embedded \0 in the FITS
*   buffer. This was fixed at some point but since a FITS header from SCUBA
*   should not be including unprintable characters we remove them here
      CALL CHR_CLEAN( VALUE )

      END
