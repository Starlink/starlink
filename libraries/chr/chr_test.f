*+ CHR_TEST - Test the CHR routines
      PROGRAM CHR_TEST
*+
*  Name:
*     CHR_TEST

*  Purpose:
*     To test the CHR library.

*  Language:
*     Starlink Fortran 77

*  Description:
*    This program tests most of the CHR routines. It is self
*    checking and will report as it goes along, terminating
*    with an overall result. It prompts the user to indicate which test
*    should be performed unless something (anything) is supplied on
*    the command line, in which case all tests are performed and the
*    command then exits.

*  Copyright:
*     Copyright (C) 1989, 1990, 1993 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RLVAD::AJC: A J Chipperfield (STARLINK)
*     RLVAD::ACC: A C Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-AUG-1989 (RLVAD::AJC):
*        Original version.
*     26-JAN-1990 (RLVAD::AJC):
*        Do CHR_EQUAL and CHR_SIMLR test for unequal declared lengths
*     14-SEP-1993 (RLVAD::ACC):
*        Split single module into main program plus four subroutines,
*        using grouping as in Appendix A of SUN/40.3, for easier
*        maintenance.
*     15-MAY-2018 (DSB):
*        Do not prompt the suer unless something (anything) is supplied
*        on the command line. This is so that we can do a "make check" in
*        CHR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Subprograms called:
*     TEST_CASE, TEST_COMPARE, TEST_DECODE, TEST_EDIT,
*     TEST_ENCODE, TEST_ENQUIRE, TEST_PORT, TEST_SEARCH.

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHR_ERR'

*  Local Variables:
      INTEGER STATUS             ! Global Status
      INTEGER ISTAT              ! Status for each routine tested
      INTEGER JSTAT              ! Status for each test or set of tests
      LOGICAL LOOP               ! User indicates tests to perform?
      LOGICAL TEST_PERFORMED     ! Whether or not a test was performed
      CHARACTER*2 SELECT         ! Test Selection
      CHARACTER*2 TCASE          ! Case Test
      CHARACTER*2 TCOMPARE       ! Compare Test
      CHARACTER*2 TDECODE        ! Decode Test
      CHARACTER*2 TEDIT          ! Edit Test
      CHARACTER*2 TENCODE        ! Encode Test
      CHARACTER*2 TENQUIRE       ! Enquire Test
      CHARACTER*2 TPORT          ! Portability Test
      CHARACTER*2 TSEARCH        ! Search Test
      CHARACTER*2 ALL            ! All Tests
      CHARACTER*2 EXIT           ! Exit Program
      PARAMETER (TCASE = 'CA')
      PARAMETER (TCOMPARE = 'CO')
      PARAMETER (TDECODE = 'DE')
      PARAMETER (TEDIT = 'ED')
      PARAMETER (TENCODE = 'EN')
      PARAMETER (TENQUIRE = 'EQ')
      PARAMETER (TPORT = 'PO')
      PARAMETER (TSEARCH = 'SE')
      PARAMETER (ALL = 'AL')
      PARAMETER (EXIT = 'EX')

*.

*    Initialize STATUS
      STATUS = SAI__OK

*  We do not loop if the command line is empty..
      LOOP = ( IARGC() .GT. 0 )

*  Query which test to run

*     DO WHILE Loop (DO WHILE SELECT .NE. EXIT)

10    JSTAT = SAI__OK
      TEST_PERFORMED = .FALSE.

      IF( LOOP ) THEN
         PRINT *,' '
         PRINT *,'Which CHR test would you like to run? '
         PRINT *,
     :    '(CAse, COmpare, DEcode, EDit, ENcode, EnQuire(EQ), '//
     :    'POrtability, SEarch, '
         PRINT *,' ALl tests, EXit)'
         READ (*,'(A2)') SELECT

         CALL CHR_UCASE(SELECT)
      ELSE
         SELECT = 'ALL'
      END IF

      IF (SELECT .NE. EXIT) THEN

         IF (SELECT .EQ. TCASE .OR.
     :       SELECT .EQ. ALL) THEN

*  Test change case routines

            CALL TEST_CASE(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (SELECT .EQ. TCOMPARE .OR.
     :       SELECT .EQ. ALL) THEN

*  Test compare strings routines

            CALL TEST_COMPARE(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (SELECT .EQ. TDECODE .OR.
     :       SELECT .EQ. ALL) THEN

*  Test decode routines

            CALL TEST_DECODE(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (SELECT .EQ. TEDIT .OR.
     :       SELECT .EQ. ALL) THEN

*  Test editting routines

            CALL TEST_EDIT(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (SELECT .EQ. TENCODE .OR.
     :       SELECT .EQ. ALL) THEN

*  Test encode routines

            CALL TEST_ENCODE(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (SELECT .EQ. TENQUIRE .OR.
     :       SELECT .EQ. ALL) THEN

*  Test enquire routines

            CALL TEST_ENQUIRE(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (SELECT .EQ. TPORT .OR.
     :       SELECT .EQ. ALL) THEN

*  Test portability routines

            CALL TEST_PORT(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (SELECT .EQ. TSEARCH .OR.
     :       SELECT .EQ. ALL) THEN

*  Test search string routines

            CALL TEST_SEARCH(ISTAT)
            TEST_PERFORMED = .TRUE.
            IF (ISTAT .NE. SAI__OK) THEN
               JSTAT = ISTAT
            END IF
         END IF

         IF (TEST_PERFORMED) THEN
            IF (JSTAT .NE. SAI__OK) THEN
               PRINT *,
     :            '*** This test of CHR fails - Check summary above ***'
               STATUS = JSTAT
            ELSE
               PRINT *,'*** This test of CHR succesful ***'
            ENDIF
         END IF

         IF( LOOP ) GO TO 10
      END IF
*     End of Loop

*  End action

      IF (STATUS .NE. SAI__OK) THEN
         PRINT *,'*** Test of CHR fails - Check summary above ***'
      ELSE
         PRINT *,'*** Test of CHR succesful ***'
      ENDIF

      END
