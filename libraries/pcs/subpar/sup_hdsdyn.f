      SUBROUTINE SUBPAR_HDSDYN ( NAMECODE, ACCESS, LOC, STATUS )
*+
*  Name:
*     SUBPAR_HDSDYN

*  Purpose:
*     Get dynamic default and return a locator to it.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_HDSDYN ( NAMECODE, ACCESS, LOC, STATUS )

*  Description:
*     A locator is returned to a dynamic default value associated with
*     the indicated parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to a parameter
*     ACCESS=CHARACTER*(*) (given)
*        access required to the HDS structure
*     LOC=CHARACTER*(DAT__SZLOC) (returned)
*        locator to the stored data
*     STATUS=INTEGER

*  Algorithm:
*     The dynamic default for the indicated parameter is looked-up.
*     If it is a structure-name, a locator is obtained to it. If it is a
*     value, it is converted to the data type of the indicated parameter,
*     'private' storage is created for it, it is put into store, and a
*     locator to the store is returned.

*  Copyright:
*     Copyright (C) 1984, 1987, 1990, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     01-OCT-1984 (BDK):
*        Original
*     08-MAY-1987 (BDK):
*        check the default exists
*     09-JUL-1990 (AJC):
*        use 'type' as 'exists' flag
*     20-JUL-1992 (AJC):
*        Move DATA statement
*     19-AUG-1992 (AJC):
*        For dynamics check PARDYN(1,-) also
*     10-NOV-1992 (AJC):
*        Set SUBPAR__ERROR not PAR__
*        report error
*     26-FEB-1993 (AJC):
*        Add INCLUDE DAT_PAR
*      9-AUG-1993 (AJC):
*        Remove INCLUDE PAR_ERR
*      1-FEB-1996 (AJC):
*        Create storage of appropriate size for CHARACTER strings
*        (min 132).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_PAR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Given:
      INTEGER NAMECODE          ! pointer to a parameter
      CHARACTER*(*) ACCESS      ! access required to the HDS structure

*  Arguments Returned:
      CHARACTER*(DAT__SZLOC) LOC  ! locator to the stored data

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*  Local Variables:
      INTEGER COUNT                     ! number of default values
      INTEGER TYPE                      ! code for data type
      INTEGER SLEN                      ! string length
      INTEGER I                         ! index (discarded)
      CHARACTER*15 HDSTYPE              ! data type
      INTEGER NDIMS                     ! dimensionality of data
      INTEGER FIRST                     ! index of first value stored
      CHARACTER*15 POSTYPES(8)         ! possible primitive data types

*  External references:
      EXTERNAL CHR_LEN                  ! used length of string
      INTEGER CHR_LEN

*  Local Data:
      DATA POSTYPES / '_CHAR*', '_REAL', '_DOUBLE', '_INTEGER',
     :  '_LOGICAL', ' ', ' ', '_INT64' /

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( ( PARDYN(1,NAMECODE) .EQ. 0 )
     :.OR. ( PARDYN(3,NAMECODE) .LE. 0 ) ) THEN
*
*      No default exists. Return a general error which will cause the
*      search-path to continue.
*
         STATUS = SUBPAR__ERROR
         CALL EMS_SETC( 'NAME', PARKEY(NAMECODE) )
         CALL EMS_REP( 'SUP_HDSDYN1',
     :   'SUBPAR: No dynamic default value for parameter ^NAME',
     :    STATUS )

      ELSE IF ( PARDYN(3,NAMECODE) .GE. 20 ) THEN
*
*      The default is the name of an HDS structure. Get a locator to
*      it.
*
         CALL SUBPAR_GETHDS ( NAMECODE, CHARLIST(PARDYN(1,NAMECODE)),
     :     ACCESS, LOC, STATUS )

      ELSE
*
*      Find the number of default values
*
         COUNT = PARDYN(2,NAMECODE) - PARDYN(1,NAMECODE) + 1
*
*      Invent the character-form of the data-type
*
         TYPE = PARDYN(3,NAMECODE)
         HDSTYPE = POSTYPES(TYPE)
         FIRST = PARDYN(1,NAMECODE)

*      Make CHAR types size dependent
         IF ( HDSTYPE .EQ. '_CHAR*' ) THEN
            IF ( COUNT .EQ. 1 ) THEN
               SLEN = MAX( 132, CHR_LEN( CHARLIST( FIRST ) ) )
            ELSE
               SLEN = SUBPAR__STRLEN
            END IF
            CALL CHR_ITOC( SLEN, HDSTYPE(7:), I )
         END IF
*
*      Create storage to hold the data and return a locator to it
*
         IF ( COUNT .GT. 1 ) THEN
            NDIMS = 1
         ELSE
            NDIMS = 0
         ENDIF
         CALL SUBPAR_CRINT ( NAMECODE, HDSTYPE, NDIMS, COUNT, LOC,
     :     STATUS )
*
*     Put the data into it
*
         FIRST = PARDYN(1,NAMECODE)

         IF ( TYPE .EQ. SUBPAR__REAL ) THEN
            CALL DAT_PUTR ( LOC, NDIMS, COUNT, REALLIST(FIRST), STATUS )
         ELSE IF ( TYPE .EQ. SUBPAR__CHAR ) THEN
            CALL DAT_PUTC ( LOC, NDIMS, COUNT, CHARLIST(FIRST), STATUS )
         ELSE IF ( TYPE .EQ. SUBPAR__INTEGER ) THEN
            CALL DAT_PUTI ( LOC, NDIMS, COUNT, INTLIST(FIRST), STATUS )
         ELSE IF ( TYPE .EQ. SUBPAR__INT64 ) THEN
            CALL DAT_PUTI ( LOC, NDIMS, COUNT, INT64LIST(FIRST),
     :        STATUS )
         ELSE IF ( TYPE .EQ. SUBPAR__DOUBLE ) THEN
            CALL DAT_PUTD ( LOC, NDIMS, COUNT, DOUBLELIST(FIRST),
     :        STATUS )
         ELSE IF ( TYPE .EQ. SUBPAR__LOGICAL ) THEN
            CALL DAT_PUTL ( LOC, NDIMS, COUNT, LOGLIST(FIRST), STATUS )
         ENDIF

      ENDIF

      END
