      SUBROUTINE HDRLIST( ISTAT )
*+
* Name:
*    HDRLIST

*  Purpose:
*     List all the header items in a named source.

*  Description:
*     This routine is part of the IMG example suite. It accesses an
*     existing image with a known header information source (such as
*     'FITS'). It then lists the names of all the items in the named
*     source and their values.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     PWD: Peter Draper (Starlink)

*  History:
*     03-JUN-1998 (PWD):
*         Original Version

*  Notes:
*     As this handles the special FITS header items 'COMMENT', 'HISTORY'
*     and ' ', it is a little more complex than the documentation in
*     SUN/160 suggests.
*
*     The PAR routines are described in SUN/114.

*-

*  Local Variables:
      CHARACTER * ( 30 ) SOURCE, ITEM
      CHARACTER * ( 80 ) VALUE
      LOGICAL DONEB, DONEC, DONEH, LIST
*.

*  Get the name of the source to read the header items from.
      CALL PAR_GET0C( 'SOURCE', SOURCE, ISTAT )

*  See how many items are present (this also accesses the image).
      CALL HDR_NUMB( 'IN', SOURCE, '*', N, ISTAT )
      IF ( N .GT. 0 ) THEN

*  Get the names of the items one-by-one. Then read the value
*  of the item. This will fail for FITS multiple items unless
*  we take special action.
         DONEH = .FALSE.
         DONEC = .FALSE.
         DONEB = .FALSE.
         DO 1 I = 1, N

*  Get the name of the I'th header item.
            CALL HDR_NAME( 'IN', SOURCE, I, ITEM, ISTAT )

*  Check if this is a special item.
            IF ( ITEM .EQ. 'HISTORY' .OR. ITEM .EQ. 'COMMENT' .OR.
     :           ITEM .EQ. ' ' ) THEN

*  One of the FITS specials, do we still need to list all the
*  occurrences?
               LIST = .FALSE.
               IF ( ITEM .EQ. 'HISTORY' .AND. .NOT. DONEH ) THEN
                  DONEH = .TRUE.
                  LIST = .TRUE.
               ELSE IF ( ITEM .EQ. 'COMMENT' .AND. .NOT. DONEC ) THEN
                  DONEC = .TRUE.
                  LIST = .TRUE.
               ELSE IF ( ITEM .EQ. ' ' .AND. .NOT. DONEB ) THEN
                  DONEB = .TRUE.
                  LIST = .TRUE.
               END IF
               IF ( LIST ) THEN

*  This special needs all its components listing.
                  CALL HDR_NUMB( 'IN', SOURCE, ITEM, NCOMP, ISTAT )
                  DO 2 J = 1, NCOMP
                     CALL HDR_INC( 'IN', SOURCE, ITEM, J, VALUE,
     :                             ISTAT )
                     CALL MSG_SETC( 'ITEM', ITEM )
                     CALL MSG_SETC( 'VALUE', VALUE )
                     CALL MSG_OUT( ' ', '^ITEM = ^VALUE', ISTAT )
 2                CONTINUE
               END IF
            ELSE

*  It's just an ordinary header item, so get it's value.
               CALL HDR_INC( 'IN', SOURCE, ITEM, 1, VALUE, ISTAT )

*  And write it out.
               CALL MSG_SETC( 'ITEM', ITEM )
               CALL MSG_SETC( 'VALUE', VALUE )
               CALL MSG_OUT( ' ', '^ITEM = ^VALUE', ISTAT )
            END IF
 1       CONTINUE
      ELSE

*  The number of items in the source is zero.
         CALL MSG_OUT( ' ', 'Header source doesn''t exist or '//
     :                 'contains no valid items', ISTAT )
      END IF

*  Free the input image.
      CALL IMG_FREE( 'IN', ISTAT )
      END
* $Id$
