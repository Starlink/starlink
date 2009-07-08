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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
     :        NANC, ISTAT, CHR_LEN, NVAL, IVALS(20)
      CHARACTER TEXT*500, KEY*20, LOC*(DAT__SZLOC), MORE*(DAT__SZLOC)
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
     :  'PROVTEST', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0008_ave_ave', 
     :  'TEMP', 
     :  '/home/dsb/work/smurf/d', 
     :  '/home/dsb/work/smurf/b', 
     :  '/home/dsb/work/smurf/c', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0001_ave_ave', 
     :  '60', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0002_ave_ave', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0003_ave_ave', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0004_ave_ave', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0005_ave_ave', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0006_ave_ave', 
     :  '/tiny/makecube-tests/1226_15/a20061226_00015_00_0007_ave_ave' /

      CHARACTER DATES( 14 )*24
      DATA DATES / 'Mon Jan  7 18:46:28 2008', ' ',
     :             'Mon Jan  7 18:46:28 2008',
     :             'Mon Jan  7 18:46:18 2008',
     :             'Tue Dec 18 12:49:59 2007', 
     :             'Mon Jan  7 18:46:06 2008', 
     :             ' ', '24', ' ', ' ', ' ', ' ', ' ', ' ' /

      CHARACTER CREATORS( 14 )*14
      DATA CREATORS / 'KAPPA:ADD', ' ', 'KAPPA:ADD', 'KAPPA:CMULT',
     :                'SMURF:MAKECUBE', 'KAPPA:MATHS', ' ', '14', ' ', 
     :                ' ', ' ', ' ', ' ', ' ' /

      CHARACTER PARENTSS( 14 )*23
      DATA PARENTSS / '1,2,11', ' ', '2,11', '12', '3,4,5,6,7,8,9,10', 
     :                '2', ' ', '16', ' ', ' ', ' ', ' ', ' ', ' ' /

      CHARACTER MORES( 14 )*60
      DATA MORES / ' ', ' ', 
     :         'DATA_ARRAY=<ARRAY>, LABEL=T%s60+%v30+A%^5',
     :         ' ', ' ', ' ', ' ', '211', ' ', ' ', ' ', ' ', ' ', ' ' /

      DATA IVEC / 23, 34 /,
     :     DVEC /1.23D0 /,
     :     CVEC/ 'Number 1', 'A AB c d', 'Hello ' /

*  Start an error reporting context
      STATUS = SAI__OK
      CALL ERR_MARK

c      call ast_watchmemory( 1 )

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
      CALL NDF_LOC( INDF2, 'READ', LOC, STATUS )  

      CALL NDG_PUTPROV( IPROV, INDF2, LOC, KEYMAP, .FALSE., STATUS )

      CALL DAT_ANNUL( LOC, STATUS )
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
                     CALL STOPIT( 7, STATUS)
                  END IF                

               ELSE IF( PATHS( I ) .EQ. 'PROVTEST' ) THEN
                  IF( INDEX( TEXT( : L ), 'provtest' ) .EQ. 0 ) THEN
                     CALL STOPIT( 8, STATUS)
                  END IF                

               ELSE IF( CHR_LEN( TEXT ) .NE. L .OR. 
     :                  TEXT( : L ) .NE. PATHS( I )( : L ) ) THEN
                  CALL STOPIT( 9, STATUS)
               ENDIF

            ELSE IF( PATHS( I ) .NE. ' ' ) THEN
               CALL STOPIT( 10, STATUS)
            END IF


            IF( AST_MAPGET0C( KM, 'DATE', TEXT, L, STATUS ) ) THEN
               IF( CHR_LEN( TEXT ) .NE. L .OR. 
     :             TEXT( : L ) .NE. DATES( I )( : L ) ) THEN
                  CALL STOPIT( 11, STATUS)
               ENDIF
            ELSE IF( DATES( I ) .NE. ' ' ) THEN
               CALL STOPIT( 12, STATUS)
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
      CALL NDG_GETPROV( IPROV, 1, KM, MORE, STATUS )

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

      
      IF( AST_MAPLENGTH( KM, 'PARENTS', STATUS ) .NE. 2 ) THEN
         CALL STOPIT( 28, STATUS)

      ELSE IF( AST_MAPTYPE( KM, 'PARENTS', STATUS ) .NE. 
     :         AST__INTTYPE ) THEN
         CALL STOPIT( 29, STATUS)

      ELSE IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 2, NVAL, IVALS,
     :                             STATUS ) ) THEN
         CALL STOPIT( 30, STATUS)

      ELSE IF( IVALS(1) .NE. 2 ) THEN
         CALL STOPIT( 31, STATUS)

      ELSE IF( IVALS(2) .NE. 11 ) THEN
         CALL STOPIT( 32, STATUS)

      ELSE  IF( .NOT. AST_MAPGET0A( KM, 'MORE', KEYMAP, STATUS ) ) THEN
         CALL STOPIT( 33, STATUS)

      ELSE IF( AST_MAPSIZE( KEYMAP, STATUS ) .NE. 7 ) THEN
         CALL STOPIT( 34, STATUS)

      ELSE IF( .NOT. AST_MAPGET0I( KEYMAP, 'KEY1', IVAL, STATUS ) ) THEN
         CALL STOPIT( 35, STATUS)

      ELSE IF( IVAL .NE. 2 ) THEN
         CALL STOPIT( 36, STATUS)

      ELSE IF( .NOT. AST_MAPGET0R( KEYMAP, 'KEY2', RVAL, STATUS ) ) THEN
         CALL STOPIT( 37, STATUS)

      ELSE IF( RVAL .NE. 2.0 ) THEN
         CALL STOPIT( 38, STATUS)

      ELSE IF( .NOT. AST_MAPGET0C( KEYMAP, 'KEY3', CVAL, L, 
     :                             STATUS ) ) THEN
         CALL STOPIT( 39, STATUS)

      ELSE IF( CVAL( : L ) .NE. 'Two point zero' ) THEN
         CALL STOPIT( 40, STATUS)

      ELSE IF( .NOT. AST_MAPGET1I( KEYMAP, 'KEY4', 20, NVAL, IVALS, 
     :                             STATUS ) ) THEN
         CALL STOPIT( 41, STATUS)

      ELSE IF( NVAL .NE. 2 ) THEN
         CALL STOPIT( 42, STATUS)

      ELSE IF( IVALS(1) .NE. IVEC(1) ) THEN
         CALL STOPIT( 43, STATUS)

      ELSE IF( IVALS(2) .NE. IVEC(2) ) THEN
         CALL STOPIT( 44, STATUS)

      ELSE IF( .NOT. AST_MAPGET1D( KEYMAP, 'KEY5', 20, NVAL, DVALS, 
     :                             STATUS ) ) THEN
         CALL STOPIT( 45, STATUS)

      ELSE IF( NVAL .NE. 1 ) THEN
         CALL STOPIT( 46, STATUS)

      ELSE IF( DVALS(1) .NE. DVEC(1) ) THEN
         CALL STOPIT( 47, STATUS)

      ELSE IF( .NOT. AST_MAPGET1C( KEYMAP, 'KEY6', 20, NVAL, CVALS, 
     :                             STATUS ) ) THEN
         CALL STOPIT( 48, STATUS)

      ELSE IF( NVAL .NE. 3 ) THEN
         CALL STOPIT( 49, STATUS)

      ELSE IF( CVALS(1) .NE. CVEC(1) ) THEN
         CALL STOPIT( 50, STATUS)

      ELSE IF( CVALS(2) .NE. CVEC(2) ) THEN
         CALL STOPIT( 51, STATUS)

      ELSE IF( CVALS(3) .NE. CVEC(3) ) THEN
         CALL STOPIT( 52, STATUS)

      ELSE IF( .NOT. AST_MAPGET0C( KEYMAP, 'LABEL', CVAL, L,
     :                             STATUS ) ) THEN
         CALL STOPIT( 53, STATUS)

      ELSE IF( CVAL( : 45 ) .NE. 'T%s60+%v30+A%^50+%<20+*%+   '//
     :         'corrected antenna' ) THEN
         CALL STOPIT( 54, STATUS)

      END IF

      CALL DAT_NCOMP( MORE, NCOMP, STATUS )
      IF( NCOMP .NE. 11 ) CALL STOPIT( 55, STATUS)

      CALL DAT_THERE( MORE, 'KEY1', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 56, STATUS )

      CALL DAT_THERE( MORE, 'KEY2', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 57, STATUS )

      CALL DAT_THERE( MORE, 'KEY3', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 58, STATUS )

      CALL DAT_THERE( MORE, 'KEY4', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 59, STATUS )

      CALL DAT_THERE( MORE, 'KEY5', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 60, STATUS )

      CALL DAT_THERE( MORE, 'KEY6', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 61, STATUS )

      CALL DAT_THERE( MORE, 'DATA_ARRAY', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 62, STATUS )

      CALL DAT_THERE( MORE, 'LABEL', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 63, STATUS )

      CALL DAT_THERE( MORE, 'WCS', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 64, STATUS )

      CALL DAT_THERE( MORE, 'HISTORY', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 65, STATUS )

      CALL DAT_THERE( MORE, 'MORE', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 66, STATUS )



      CALL DAT_FIND( MORE, 'KEY1', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_INTEGER' ) CALL STOPIT( 67, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 68, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 69, STATUS )
      CALL DAT_GET0I( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 2 ) CALL STOPIT( 70, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY2', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_REAL' ) CALL STOPIT( 71, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 72, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 73, STATUS )
      CALL DAT_GET0R( LOC, RVAL, STATUS ) 
      IF( RVAL .NE. 2.0 ) CALL STOPIT( 74, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY3', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_CHAR*14' ) CALL STOPIT( 75, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 76, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 77, STATUS )
      CALL DAT_GET0C( LOC, CVAL, STATUS ) 
      IF( CVAL .NE. 'Two point zero' ) CALL STOPIT( 78, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY4', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_INTEGER' ) CALL STOPIT( 79, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 2 ) CALL STOPIT( 80, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 81, STATUS )
      CALL DAT_GET1I( LOC, 20, IVALS, NVAL, STATUS ) 
      IF( IVALS(1) .NE. IVEC(1) ) CALL STOPIT( 82, STATUS )
      IF( IVALS(2) .NE. IVEC(2) ) CALL STOPIT( 83, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY5', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_DOUBLE' ) CALL STOPIT( 84, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 85, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 86, STATUS )
      CALL DAT_GET0D( LOC, DVAL, STATUS ) 
      IF( DVAL .NE. DVEC(1) ) CALL STOPIT( 87, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY6', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_CHAR*10' ) CALL STOPIT( 88, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 3 ) CALL STOPIT( 89, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 90, STATUS )
      CALL DAT_GET1C( LOC, 20, CVALS, NVAL, STATUS ) 
      IF( CVALS(1) .NE. CVEC(1) ) CALL STOPIT( 91, STATUS )
      IF( CVALS(2) .NE. CVEC(2) ) CALL STOPIT( 92, STATUS )
      IF( CVALS(3) .NE. CVEC(3) ) CALL STOPIT( 93, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'DATA_ARRAY', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. 'ARRAY' ) CALL STOPIT( 94, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 95, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 96, STATUS )
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 2 ) CALL STOPIT( 97, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'LABEL', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_CHAR*49' ) CALL STOPIT( 98, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 99, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 100, STATUS )
      CALL DAT_GET0C( LOC, CVAL, STATUS ) 
      IF( CVAL( : 45 ) .NE. 'T%s60+%v30+A%^50+%<20+*%+   '//
     :         'corrected antenna' ) CALL STOPIT( 101, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )


      CALL DAT_FIND( MORE, 'WCS', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. 'WCS' ) CALL STOPIT( 102, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 103, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 104, STATUS )
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 1 ) CALL STOPIT( 105, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'HISTORY', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. 'HISTORY' ) CALL STOPIT( 106, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 107, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 108, STATUS )
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 3 ) CALL STOPIT( 109, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'MORE', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. 'EXT' ) CALL STOPIT( 110, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 111, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 112, STATUS )
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 3 ) CALL STOPIT( 113, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )




* Test NDG_MODIFYPROV
* --------------------

      CALL AST_MAPPUT0C( KM, 'CREATOR', 'PROVTEST', ' ', STATUS )
      CALL AST_MAPREMOVE( KM, 'DATE', STATUS )

      CALL AST_MAPPUT0C( KEYMAP, 'LABEL', 'KeyMap label', ' ', STATUS )
      CALL AST_MAPPUT0I( KEYMAP, 'KEY7', 999, ' ', STATUS )

      CALL AST_MAPREMOVE( KEYMAP, 'KEY1', STATUS )

      CALL AST_MAPREMOVE( KEYMAP, 'KEY2', STATUS )
      CALL AST_MAPREMOVE( KEYMAP, 'KEY3', STATUS )

      CALL DAT_FIND( MORE, 'LABEL', LOC, STATUS )
      CALL DAT_PUT0C( LOC, 'HDS label', STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_ERASE( MORE, 'WCS', STATUS ) 
      CALL DAT_ERASE( MORE, 'KEY2', STATUS ) 
      CALL DAT_ERASE( MORE, 'KEY3', STATUS ) 

      CALL NDG_MODIFYPROV( IPROV, 1, KM, MORE, STATUS )

      CALL AST_ANNUL( KEYMAP, STATUS )
      CALL AST_ANNUL( KM, STATUS )
      CALL DAT_ANNUL( MORE, STATUS )

      CALL NDG_GETPROV( IPROV, 1, KM, MORE, STATUS )

      I = 3

      IF( AST_MAPGET0C( KM, 'PATH', TEXT, L, STATUS ) ) THEN
         IF( PATHS( I ) .EQ. 'TEMP' ) THEN
            IF( INDEX( TEXT( : L ), 'TEMP_1.NDF_1' ) .EQ. 0 .AND.
     :          INDEX( TEXT( : L ), 'fred' ) .EQ. 0 ) THEN
               CALL STOPIT( 114, STATUS)
            END IF                
         ELSE IF( CHR_LEN( TEXT ) .NE. L .OR. 
     :            TEXT( : L ) .NE. PATHS( I )( : L ) ) THEN
            CALL STOPIT( 115, STATUS)
         ENDIF
      ELSE IF( PATHS( I ) .NE. ' ' ) THEN
         CALL STOPIT( 116, STATUS)
      END IF

      IF( AST_MAPGET0C( KM, 'DATE', TEXT, L, STATUS ) ) THEN
         CALL STOPIT( 117, STATUS)
      END IF

      IF( AST_MAPGET0C( KM, 'CREATOR', TEXT, L, STATUS ) ) THEN
        IF( TEXT( : L ) .NE. 'PROVTEST' ) THEN
            CALL STOPIT( 118, STATUS)
         ENDIF
      ELSE IF( CREATORS( I ) .NE. ' ' ) THEN
         CALL STOPIT( 119, STATUS)
      END IF

      IF( AST_MAPLENGTH( KM, 'PARENTS', STATUS ) .NE. 2 ) THEN
         CALL STOPIT( 120, STATUS)

      ELSE IF( AST_MAPTYPE( KM, 'PARENTS', STATUS ) .NE. 
     :         AST__INTTYPE ) THEN
         CALL STOPIT( 121, STATUS)

      ELSE IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 2, NVAL, IVALS,
     :                             STATUS ) ) THEN
         CALL STOPIT( 122, STATUS)

      ELSE IF( IVALS(1) .NE. 2 ) THEN
         CALL STOPIT( 123, STATUS)

      ELSE IF( IVALS(2) .NE. 11 ) THEN
         CALL STOPIT( 124, STATUS)

      ELSE  IF( .NOT. AST_MAPGET0A( KM, 'MORE', KEYMAP, STATUS ) ) THEN
         CALL STOPIT( 125, STATUS)

      ELSE IF( AST_MAPSIZE( KEYMAP, STATUS ) .NE. 6 ) THEN
         CALL STOPIT( 126, STATUS)

      ELSE IF( .NOT. AST_MAPGET0I( KEYMAP, 'KEY7', IVAL, STATUS ) ) THEN
         CALL STOPIT( 127, STATUS)

      ELSE IF( IVAL .NE. 999 ) THEN
         CALL STOPIT( 128, STATUS)

      ELSE IF( .NOT. AST_MAPGET0I( KEYMAP, 'KEY1', IVAL, STATUS ) ) THEN
         CALL STOPIT( 129, STATUS)

      ELSE IF( AST_MAPGET0R( KEYMAP, 'KEY2', RVAL, STATUS ) ) THEN
         CALL STOPIT( 130, STATUS)

      ELSE IF( AST_MAPHASKEY( KEYMAP, 'KEY3', STATUS ) ) THEN
         CALL STOPIT( 131, STATUS)

      ELSE IF( .NOT. AST_MAPGET1I( KEYMAP, 'KEY4', 20, NVAL, IVALS, 
     :                             STATUS ) ) THEN
         CALL STOPIT( 132, STATUS)

      ELSE IF( NVAL .NE. 2 ) THEN
         CALL STOPIT( 133, STATUS)

      ELSE IF( IVALS(1) .NE. IVEC(1) ) THEN
         CALL STOPIT( 134, STATUS)

      ELSE IF( IVALS(2) .NE. IVEC(2) ) THEN
         CALL STOPIT( 135, STATUS)

      ELSE IF( .NOT. AST_MAPGET1D( KEYMAP, 'KEY5', 20, NVAL, DVALS, 
     :                             STATUS ) ) THEN
         CALL STOPIT( 136, STATUS)

      ELSE IF( NVAL .NE. 1 ) THEN
         CALL STOPIT( 137, STATUS)

      ELSE IF( DVALS(1) .NE. DVEC(1) ) THEN
         CALL STOPIT( 138, STATUS)

      ELSE IF( .NOT. AST_MAPGET1C( KEYMAP, 'KEY6', 20, NVAL, CVALS, 
     :                             STATUS ) ) THEN
         CALL STOPIT( 139, STATUS)

      ELSE IF( NVAL .NE. 3 ) THEN
         CALL STOPIT( 140, STATUS)

      ELSE IF( CVALS(1) .NE. CVEC(1) ) THEN
         CALL STOPIT( 141, STATUS)

      ELSE IF( CVALS(2) .NE. CVEC(2) ) THEN
         CALL STOPIT( 142, STATUS)

      ELSE IF( CVALS(3) .NE. CVEC(3) ) THEN
         CALL STOPIT( 143, STATUS)

      ELSE IF( .NOT. AST_MAPGET0C( KEYMAP, 'LABEL', CVAL, L,
     :                             STATUS ) ) THEN
         CALL STOPIT( 144, STATUS)

      ELSE IF( CVAL( : 12 ) .NE. 'KeyMap label' ) THEN
         CALL STOPIT( 145, STATUS)

      END IF

      CALL DAT_NCOMP( MORE, NCOMP, STATUS )
      IF( NCOMP .NE. 9 ) CALL STOPIT( 146, STATUS)

      CALL DAT_THERE( MORE, 'KEY1', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 147, STATUS )

      CALL DAT_THERE( MORE, 'KEY2', THERE, STATUS )
      IF( THERE ) CALL STOPIT( 148, STATUS )

      CALL DAT_THERE( MORE, 'KEY3', THERE, STATUS )
      IF( THERE ) CALL STOPIT( 149, STATUS )

      CALL DAT_THERE( MORE, 'KEY4', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 150, STATUS )

      CALL DAT_THERE( MORE, 'KEY5', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 151, STATUS )

      CALL DAT_THERE( MORE, 'KEY6', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 152, STATUS )

      CALL DAT_THERE( MORE, 'KEY7', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 153, STATUS )

      CALL DAT_THERE( MORE, 'DATA_ARRAY', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 154, STATUS )

      CALL DAT_THERE( MORE, 'LABEL', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 155, STATUS )

      CALL DAT_THERE( MORE, 'WCS', THERE, STATUS )
      IF( THERE ) CALL STOPIT( 156, STATUS )

      CALL DAT_THERE( MORE, 'HISTORY', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 157, STATUS )

      CALL DAT_THERE( MORE, 'MORE', THERE, STATUS )
      IF( .NOT. THERE ) CALL STOPIT( 158, STATUS )



      CALL DAT_FIND( MORE, 'KEY7', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_INTEGER' ) CALL STOPIT( 159, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 160, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 161, STATUS )
      CALL DAT_GET0I( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 999 ) CALL STOPIT( 162, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY4', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_INTEGER' ) CALL STOPIT( 163, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 2 ) CALL STOPIT( 164, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 165, STATUS )
      CALL DAT_GET1I( LOC, 20, IVALS, NVAL, STATUS ) 
      IF( IVALS(1) .NE. IVEC(1) ) CALL STOPIT( 166, STATUS )
      IF( IVALS(2) .NE. IVEC(2) ) CALL STOPIT( 167, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY5', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_DOUBLE' ) CALL STOPIT( 168, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 169, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 170, STATUS )
      CALL DAT_GET0D( LOC, DVAL, STATUS ) 
      IF( DVAL .NE. DVEC(1) ) CALL STOPIT( 171, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'KEY6', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_CHAR*10' ) CALL STOPIT( 172, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 3 ) CALL STOPIT( 173, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 174, STATUS )
      CALL DAT_GET1C( LOC, 20, CVALS, NVAL, STATUS ) 
      IF( CVALS(1) .NE. CVEC(1) ) CALL STOPIT( 175, STATUS )
      IF( CVALS(2) .NE. CVEC(2) ) CALL STOPIT( 176, STATUS )
      IF( CVALS(3) .NE. CVEC(3) ) CALL STOPIT( 177, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'DATA_ARRAY', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. 'ARRAY' ) CALL STOPIT( 178, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 179, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 180, STATUS )
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 2 ) CALL STOPIT( 181, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'LABEL', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. '_CHAR*12' ) CALL STOPIT( 182, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 183, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 184, STATUS )
      CALL DAT_GET0C( LOC, CVAL, STATUS ) 
      IF( CVAL( : 12 ) .NE. 'KeyMap label') CALL STOPIT( 185, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )


      CALL DAT_FIND( MORE, 'HISTORY', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. 'HISTORY' ) CALL STOPIT( 186, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 187, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 188, STATUS )
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 3 ) CALL STOPIT( 189, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL DAT_FIND( MORE, 'MORE', LOC, STATUS )
      CALL DAT_TYPE( LOC, CVAL, STATUS )
      IF( CVAL .NE. 'EXT' ) CALL STOPIT( 190, STATUS )
      CALL DAT_SIZE( LOC, IVAL, STATUS ) 
      IF( IVAL .NE. 1 ) CALL STOPIT( 191, STATUS )
      CALL DAT_SHAPE( LOC, 20, IVALS, IVAL, STATUS ) 
      IF( IVAL .NE. 0 ) CALL STOPIT( 192, STATUS )
      CALL DAT_NCOMP( LOC, NCOMP, STATUS )
      IF( NCOMP .NE. 3 ) CALL STOPIT( 193, STATUS )
      CALL DAT_ANNUL( LOC, STATUS )

      CALL AST_ANNUL( KEYMAP, STATUS )
      CALL AST_ANNUL( KM, STATUS )
      CALL DAT_ANNUL( MORE, STATUS )



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

      CALL NDG_GETPROV( IPROV, 1, KM, MORE, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 198, STATUS )

      ELSE IF( INDEX( CVAL( : L ), 'TEMP_1.NDF_1' ) .EQ. 0 .AND.
     :         INDEX( CVAL( : L ), 'fred' ) .EQ. 0 ) THEN
         CALL STOPIT( 199, STATUS )
      END IF

      IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS, 
     :                        STATUS ) ) THEN
         CALL STOPIT( 200, STATUS )

      ELSE IF( NVAL .NE. 1 ) THEN
         CALL STOPIT( 201, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )
      IF( MORE .NE. DAT__NOLOC ) CALL DAT_ANNUL( MORE, STATUS )


      CALL NDG_GETPROV( IPROV, IVALS( 1 ), KM, MORE, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 202, STATUS )

      ELSE IF( CVAL .NE. PATHS(5) ) THEN
         CALL STOPIT( 203, STATUS )
      END IF

      IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS, 
     :                        STATUS ) ) THEN
         CALL STOPIT( 204, STATUS )

      ELSE IF( NVAL .NE. 8 ) THEN
         CALL STOPIT( 205, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )
      IF( MORE .NE. DAT__NOLOC ) CALL DAT_ANNUL( MORE, STATUS )


* Test NDG_WRITEPROV
* -----------------
      CALL NDG_GETPROV( IPROV, 1, KM, MORE, STATUS )
      CALL AST_MAPREMOVE( KM, 'MORE', STATUS )
      CALL NDG_MODIFYPROV( IPROV, 1, KM, DAT__NOLOC, STATUS )
      CALL AST_ANNUL( KM, STATUS )
      IF( MORE .NE. DAT__NOLOC ) CALL DAT_ANNUL( MORE, STATUS )

      CALL NDG_WRITEPROV( IPROV, INDF2, .FALSE., STATUS )
      IF( IPROV .EQ. NDG__NULL ) CALL STOPIT( 206, STATUS )

      CALL NDG_FREEPROV( IPROV, STATUS )
      IF( IPROV .NE. NDG__NULL ) CALL STOPIT( 207, STATUS )

      CALL NDG_READPROV( INDF2, 'UNKNOWN', IPROV, STATUS )

      CALL NDG_COUNTPROV( IPROV, NANC, STATUS )
      IF( NANC .NE. 10 ) CALL STOPIT( 208, STATUS)

      CALL NDG_GETPROV( IPROV, 1, KM, MORE, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 209, STATUS )

      ELSE IF( INDEX( CVAL( : L ), 'TEMP_1.NDF_1' ) .EQ. 0 .AND.
     :         INDEX( CVAL( : L ), 'fred' ) .EQ. 0 ) THEN
         CALL STOPIT( 210, STATUS )
      END IF

      IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS, 
     :                        STATUS ) ) THEN
         CALL STOPIT( 211, STATUS )

      ELSE IF( NVAL .NE. 1 ) THEN
         CALL STOPIT( 212, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )
      IF( MORE .NE. DAT__NOLOC ) CALL DAT_ANNUL( MORE, STATUS )


      CALL NDG_GETPROV( IPROV, IVALS( 1 ), KM, MORE, STATUS )

      IF( .NOT. AST_MAPGET0C( KM, 'PATH', CVAL, L, STATUS ) ) THEN
         CALL STOPIT( 213, STATUS )

      ELSE IF( CVAL .NE. PATHS(5) ) THEN
         CALL STOPIT( 214, STATUS )
      END IF

      IF( .NOT. AST_MAPGET1I( KM, 'PARENTS', 20, NVAL, IVALS, 
     :                        STATUS ) ) THEN
         CALL STOPIT( 215, STATUS )

      ELSE IF( NVAL .NE. 8 ) THEN
         CALL STOPIT( 216, STATUS )
      END IF

      CALL AST_ANNUL( KM, STATUS )
      IF( MORE .NE. DAT__NOLOC ) CALL DAT_ANNUL( MORE, STATUS )



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
      character more*(dat__szloc), text*200, label*(*)

      if( status .ne. sai__ok ) return

      write(*,*) ' ' 
      write(*,*) label,' anc ',ianc

      CALL NDG_GETPROV( IPROV, ianc, KM, MORE, STATUS )

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

      if( more .ne. dat__noloc ) call dat_annul( more, status )
      call ast_annul( km, status )

      END
