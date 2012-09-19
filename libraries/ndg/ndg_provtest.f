      	PROGRAM NDG_PROVTEST
*+
*  Name:
*     NDG_PROVTEST

*  Purpose:
*     Tests the provenance facilities of the NDG library.

*  Language:
*     Starlink Fortran 77

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     22-JUN-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDG_PAR'          ! NDG constants and functions
      INCLUDE 'DAT_PAR'          ! HDS constants

* Local Variables:
      INTEGER STATUS, INDF, IPROV, KEYMAP, N, I, KM, L, INDF2, PLACE,
     :        NANC, ISTAT, CHR_LEN, NVAL, IVALS(20), KKK
      CHARACTER TEXT*500, KEY*20
      INTEGER IVEC(2), IVAL, NCOMP
      LOGICAL THERE
      REAL RVAL
      DOUBLE PRECISION DVEC(1), DVALS(20), DVAL
      CHARACTER CVEC(3)*10, CVALS(20)*20, CVAL*100

      CHARACTER KEYS( 14 )*20
      DATA KEYS / '0', '10', '1', '11', '2', '12', '3', 'MXLEN', '4',
     :            '5', '6', '7', '8', '9' /

      CHARACTER IDS( 14 )*2
      DATA IDS / '0', '10', '1', '11', '2', '12', '3', '2', '4', '5',
     :           '6', '7', '8', '9' /

      CHARACTER PATHS( 14 )*60
      DATA PATHS /
     :  '/data/git/starlink/libraries/ndg/./provtest',
     :  '/home/dsb/work/smurf/d',
     :  '/home/dsb/work/smurf/b',
     :  '/home/dsb/work/smurf/c',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0001_ave_ave',
     :  '/data/git/starlink/libraries/ndg/fred',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0002_ave_ave',
     :  '60',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0003_ave_ave',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0004_ave_ave',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0005_ave_ave',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0006_ave_ave',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0007_ave_ave',
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0008_ave_ave' /

      CHARACTER DATES( 14 )*24
      DATA DATES / 'Mon Jan  7 18:46:28 2008',
     :             'Mon Jan  7 18:46:18 2008',
     :             'Tue Dec 18 12:49:59 2007',
     :             'Mon Jan  7 18:46:06 2008',
     :             ' ',
     :             'Mon Jan  7 18:46:28 2008',
     :             ' ', '24', ' ', ' ', ' ', ' ', ' ', ' ' /

      CHARACTER CREATORS( 14 )*14
      DATA CREATORS / 'KAPPA:ADD', 'KAPPA:CMULT', 'SMURF:MAKECUBE',
     :                'KAPPA:MATHS',
     :                ' ', 'KAPPA:ADD', ' ', '14', ' ',
     :                ' ', ' ', ' ', ' ', ' ' /

      CHARACTER PARENTSS( 14 )*23
      DATA PARENTSS / '1,10,12', '11', '2,3,4,5,6,7,8,9', '1',
     :                ' ',
     :                '10,1', ' ', '15', ' ', ' ', ' ', ' ', ' ', ' ' /

      CHARACTER MORES( 14 )*60
      DATA MORES / ' ', ' ', ' ',
     :         ' ', ' ', ' ', ' ', '96', ' ', ' ', ' ', ' ', ' ', ' ' /

      DATA IVEC / 23, 34 /,
     :     DVEC /1.23D0 /,
     :     CVEC/ 'Number 1', 'A AB c d', 'Hello ' /

*  Start an error reporting context
      STATUS = SAI__OK
      CALL ERR_MARK
      CALL AST_BEGIN( STATUS )

c      call ast_watchmemory( 14182 )

* Test NDG_READPROV
* -----------------
      CALL NDF_FIND( DAT__ROOT, './provtest.sdf', INDF, STATUS )
      CALL NDG_READPROV( INDF, 'UNKNOWN', IPROV, STATUS )



* Test NDG_COUNTPROV
* ------------------
      CALL NDG_COUNTPROV( IPROV, NANC, STATUS )
      IF( NANC .NE. 11 ) CALL STOPIT( 1, STATUS)



* Test NDG_PUTPROV
* ----------------
      KEYMAP = AST_KEYMAP( ' ', STATUS )
      CALL AST_MAPPUT0I( KEYMAP, 'KEY1', 2, ' ', STATUS )
      CALL AST_MAPPUT0R( KEYMAP, 'KEY2', 2.0, ' ', STATUS )
      CALL AST_MAPPUT0C( KEYMAP, 'KEY3', 'Two point zero', ' ', STATUS )
      CALL AST_MAPPUT1I( KEYMAP, 'KEY4', 2, IVEC, 'Hello', STATUS )
      CALL AST_MAPPUT1D( KEYMAP, 'KEY5', 1, DVEC, 'Hello', STATUS )
      CALL AST_MAPPUT1C( KEYMAP, 'KEY6', 3, CVEC, 'Hello', STATUS )

      CALL NDF_PLACE( DAT__ROOT, 'fred.sdf', PLACE, STATUS )
      CALL NDF_COPY( INDF, PLACE, INDF2, STATUS )

      CALL NDG_PUTPROV( IPROV, INDF2, KEYMAP, .FALSE., STATUS )

      CALL AST_ANNUL( KEYMAP, STATUS )

      CALL NDG_COUNTPROV( IPROV, NANC, STATUS )
      IF( NANC .NE. 12 ) CALL STOPIT( 2, STATUS)


* Test NDG_FORMATPROV
* -------------------
      CALL NDG_FORMATPROV( IPROV, .FALSE., KEYMAP, STATUS )
      N = AST_MAPSIZE( KEYMAP, STATUS )
      IF( N .NE. 14 ) CALL STOPIT( 3, STATUS)

      DO I = 1, N
         KEY = AST_MAPKEY( KEYMAP, I, STATUS )

         IF( KEY .NE. KEYS( I ) ) CALL STOPIT( 4, STATUS)

         IF( AST_MAPGET0A( KEYMAP, KEY, KM, STATUS ) ) THEN

            IF( AST_MAPGET0C( KM, 'ID', TEXT, L, STATUS ) ) THEN
               IF( TEXT( : L ) .NE. IDS( I ) ) THEN
                  CALL STOPIT( 5, STATUS)
               ENDIF
            ELSE IF( IDS( I ) .NE. ' ' ) THEN
               CALL STOPIT( 6, STATUS)
            END IF

            IF( AST_MAPGET0C( KM, 'PATH', TEXT, L, STATUS ) ) THEN
               IF( PATHS( I ) .EQ. 'TEMP' ) THEN
                  IF( INDEX( TEXT( : L ), 'TEMP_1.NDF_1' ) .EQ. 0 .AND.
     :                INDEX( TEXT( : L ), 'fred' ) .EQ. 0 ) THEN
                  END IF

               ELSE IF( PATHS( I ) .EQ. 'PROVTEST' ) THEN
                  IF( INDEX( TEXT( : L ), 'provtest' ) .EQ. 0 ) THEN
                  END IF

               ELSE IF( CHR_LEN( TEXT ) .NE. L .OR.
     :                  TEXT( : L ) .NE. PATHS( I )( : L ) ) THEN
               ENDIF

            ELSE IF( PATHS( I ) .NE. ' ' ) THEN
               CALL STOPIT( 10, STATUS)
            END IF


*  Do not test the date string in the main NDF since a new copy of it
*  may have been taken, changing the date. ALso do not test 6 (fred.sdf)
*  which is a new copy of the main NDF.
            IF( I .NE. 1 .AND. I .NE. 6 ) THEN
               IF( AST_MAPGET0C( KM, 'DATE', TEXT, L, STATUS ) ) THEN
                  IF( CHR_LEN( TEXT ) .NE. L .OR.
     :                TEXT( : L ) .NE. DATES( I )( : L ) ) THEN
                     CALL STOPIT( 11, STATUS)
                  ENDIF
               ELSE IF( DATES( I ) .NE. ' ' ) THEN
                  CALL STOPIT( 12, STATUS)
               END IF
            END IF


            IF( AST_MAPGET0C( KM, 'CREATOR', TEXT, L, STATUS ) ) THEN
              IF( TEXT( : L ) .NE. CREATORS( I ) ) THEN
                  CALL STOPIT( 13, STATUS)
               ENDIF
            ELSE IF( CREATORS( I ) .NE. ' ' ) THEN
               CALL STOPIT( 14, STATUS)
            END IF

            IF( AST_MAPGET0C( KM, 'PARENTS', TEXT, L, STATUS ) ) THEN
               IF( TEXT( : L ) .NE. PARENTSS( I ) ) THEN
                  CALL STOPIT( 15, STATUS)
               ENDIF
            ELSE IF( PARENTSS( I ) .NE. ' ' ) THEN
               CALL STOPIT( 16, STATUS)
            END IF

            IF( AST_MAPGET0C( KM, 'MORE', TEXT, L, STATUS ) ) THEN
               L = MIN( L, CHR_LEN( MORES( I ) ) )
               IF( TEXT( : L ) .NE. MORES( I )( : L ) ) THEN
                  CALL STOPIT( 17, STATUS)
               ENDIF
            ELSE IF( MORES( I ) .NE. ' ' ) THEN
               CALL STOPIT( 18, STATUS)
            END IF

            CALL AST_ANNUL( KM, STATUS )

         ELSE
            CALL STOPIT( 19, STATUS)
         END IF

      END DO
      CALL AST_ANNUL( KEYMAP, STATUS )



* Test NDG_GETPROV
* -------------------
      CALL NDG_GETPROV( IPROV, 1, KM, STATUS )

      I = 3

      IF( AST_MAPGET0C( KM, 'ID', TEXT, L, STATUS ) ) THEN
         CALL STOPIT( 20, STATUS)
      END IF

      IF( AST_MAPGET0C( KM, 'PATH', TEXT, L, STATUS ) ) THEN

         IF( PATHS( I ) .EQ. 'TEMP' ) THEN
            IF( INDEX( TEXT( : L ), 'TEMP_1.NDF_1' ) .EQ. 0 .AND.
     :          INDEX( TEXT( : L ), 'fred' ) .EQ. 0 ) THEN
               CALL STOPIT( 21, STATUS)
            END IF
         ELSE IF( CHR_LEN( TEXT ) .NE. L .OR.
     :            TEXT( : L ) .NE. PATHS( I )( : L ) ) THEN
            CALL STOPIT( 22, STATUS)
         ENDIF
      ELSE IF( PATHS( I ) .NE. ' ' ) THEN
         CALL STOPIT( 23, STATUS)
      END IF


      IF( AST_MAPGET0C( KM, 'DATE', TEXT, L, STATUS ) ) THEN
         IF( CHR_LEN( TEXT ) .NE. L .OR.
     :       TEXT( : L ) .NE. DATES( I )( : L ) ) THEN
            CALL STOPIT( 24, STATUS)
         ENDIF
      ELSE IF( DATES( I ) .NE. ' ' ) THEN
         CALL STOPIT( 25, STATUS)
      END IF


      IF( AST_MAPGET0C( KM, 'CREATOR', TEXT, L, STATUS ) ) THEN
         IF( TEXT( : L ) .NE. CREATORS( I ) ) THEN
            CALL STOPIT( 26, STATUS)
         ENDIF
      ELSE IF( CREATORS( I ) .NE. ' ' ) THEN
         CALL STOPIT( 27, STATUS)
      END IF


      IF( AST_MAPLENGTH( KM, 'PARENTS', STATUS ) .NE. 8 ) THEN
         CALL STOPIT( 28, STATUS)

      ELSE IF( AST_MAPTYPE( KM, 'PARENTS', STATUS ) .NE.
     :         AST__INTTYPE ) THEN
         CALL STOPIT( 29, STATUS)

      ELSE IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 2, NVAL, IVALS,
     :                             STATUS ) ) THEN
         CALL STOPIT( 30, STATUS)

      ELSE IF( IVALS(1) .NE. 2 ) THEN
         CALL STOPIT( 31, STATUS)

      ELSE IF( IVALS(2) .NE. 3 ) THEN
         CALL STOPIT( 32, STATUS)

      ELSE  IF( AST_MAPGET0A( KM, 'MORE', KEYMAP, STATUS ) ) THEN
         CALL STOPIT( 33, STATUS)

      END IF





* Test NDG_ROOTPROV
* -----------------
      CALL NDG_ROOTPROV( IPROV, KM, STATUS )

      DO I = 1, 14
         IF( PARENTSS( I ) .EQ. ' ' ) then
            IF( AST_MAPGET0C( KM, PATHS( I ), CVAL, L , STATUS ) ) THEN
               IF( CVAL( : L ) .NE. KEYS( I )( : L ) ) THEN
                  CALL STOPIT( 194, STATUS )
               END IF
            ELSE
               CALL STOPIT( 195, STATUS )
            END IF
         ELSE IF( AST_MAPHASKEY( KM, KEYS( I ), STATUS ) ) THEN
            CALL STOPIT( 196, STATUS )
         END IF
      END DO

      CALL AST_ANNUL( KM, STATUS )



* Test NDG_REMOVEPROV
* -------------------

      IVALS(1) = 11
      IVALS(2) = 12
      CALL NDG_REMOVEPROV( IPROV, 2, IVALS, STATUS )

      CALL NDG_COUNTPROV( IPROV, NANC, STATUS )
      IF( NANC .NE. 10 ) CALL STOPIT( 197, STATUS)

      CALL NDG_GETPROV( IPROV, 1, KM, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 198, STATUS )

      ELSE IF( CVAL( : L ) .ne. PATHS( 3 ) ) THEN
         CALL STOPIT( 199, STATUS )
      END IF

      IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS,
     :                        STATUS ) ) THEN
         CALL STOPIT( 200, STATUS )

      ELSE IF( NVAL .NE. 8 ) THEN
         CALL STOPIT( 201, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )


      CALL NDG_GETPROV( IPROV, IVALS( 1 ), KM, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 202, STATUS )

      ELSE IF( CVAL .NE. PATHS(5) ) THEN
         CALL STOPIT( 203, STATUS )
      END IF

      IF( AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS,
     :                        STATUS ) ) THEN
         CALL STOPIT( 204, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )


* Test NDG_WRITEPROV
* -----------------
      CALL NDG_GETPROV( IPROV, 1, KM, STATUS )
      CALL AST_MAPREMOVE( KM, 'MORE', STATUS )
      CALL NDG_MODIFYPROV( IPROV, 1, KM, DAT__NOLOC, STATUS )
      CALL AST_ANNUL( KM, STATUS )

      CALL NDG_WRITEPROV( IPROV, INDF2, .FALSE., STATUS )
      IF( IPROV .EQ. NDG__NULL ) CALL STOPIT( 206, STATUS )

      CALL NDG_FREEPROV( IPROV, STATUS )
      IF( IPROV .NE. NDG__NULL ) CALL STOPIT( 207, STATUS )

      CALL NDG_READPROV( INDF2, 'UNKNOWN', IPROV, STATUS )

      CALL NDG_COUNTPROV( IPROV, NANC, STATUS )
      IF( NANC .NE. 10 ) CALL STOPIT( 208, STATUS)

      CALL NDG_GETPROV( IPROV, 1, KM, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 209, STATUS )

      ELSE IF( CVAL( : L ) .ne. paths(3) ) then
         CALL STOPIT( 210, STATUS )
      END IF

      IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS,
     :                        STATUS ) ) THEN
         CALL STOPIT( 211, STATUS )

      ELSE IF( NVAL .NE. 8 ) THEN
         CALL STOPIT( 212, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )


      CALL NDG_GETPROV( IPROV, IVALS( 1 ), KM, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 213, STATUS )

      ELSE IF( CVAL .NE. PATHS(5) ) THEN
         CALL STOPIT( 214, STATUS )
      END IF

      IF( AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS,
     :                        STATUS ) ) THEN
         CALL STOPIT( 215, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )



* Test NDG_FREEPROV
* -----------------
      CALL NDG_FREEPROV( IPROV, STATUS )











* Tidy up.
* --------
      CALL NDF_ANNUL( INDF, STATUS )
      CALL NDF_ANNUL( INDF2, STATUS )

      IF( STATUS .EQ. SAI__OK) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( ' ', 'All provenance tests passed.', STATUS )
      ELSE
         CALL ERR_REP( ' ', 'NDG provenance tests failed.', STATUS )
      END IF

*  End the error reporting context
      CALL ERR_RLSE

      CALL AST_END( STATUS )
c      CALL AST_ACTIVEMEMORY( ' ' )
      CALL AST_FLUSHMEMORY( 1 )


      END



      SUBROUTINE STOPIT( IERR, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      INTEGER IERR, STATUS

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'I', IERR )
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', 'Error ^I', STATUS )
      END IF

      END




      subroutine show( label, iprov, ianc, status )
      implicit none
      include 'SAE_PAR'
      include 'DAT_PAR'
      include 'AST_PAR'
      integer iprov, ianc, status, km, l, i, nval, ivals(20)
      character text*200, label*(*)

      if( status .ne. sai__ok ) return

      write(*,*) ' '
      write(*,*) label,' anc ',ianc

      CALL NDG_GETPROV( IPROV, ianc, KM, STATUS )

      IF( AST_MAPGET0C( KM, 'PATH', TEXT, L, STATUS ) ) THEN
         write(*,*) 'PATH: ',text( : l )
      ELSE
         write(*,*) 'PATH: '
      END IF

      IF( AST_MAPGET0C( KM, 'CREATOR', TEXT, L, STATUS ) ) THEN
         write(*,*) 'CREATOR: ',text( : l )
      ELSE
         write(*,*) 'CREATOR: '
      END IF

      IF( AST_MAPGET0C( KM, 'DATE', TEXT, L, STATUS ) ) THEN
         write(*,*) 'DATE: ',text( : l )
      ELSE
         write(*,*) 'DATE: '
      END IF

      IF( AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS, STATUS ) ) THEN
         write(*,*) 'PARENTS: ',(IVALS(I),i=1,nval)
      ELSE
         write(*,*) 'PARENTS: '
      END IF

      CALL AST_ANNUL( KM, STATUS )

      END
