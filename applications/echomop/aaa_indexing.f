      SUBROUTINE AAA_INDEXING
*+
*  Name:
*     ECHOMOP - AAA_INDEXING

*  Purpose:
*     Read a private format AAA index file specifying categories.

*  Description:
*     This routine reads the AAA index file which consists of lines in the
*     following forms (in ASCII):
*
*     000 Category-name
*         specifies the name of a major category grouping
*
*     nnn Category-name
*         where nnn are decimal digits, specifies the name of a subcategory
*
*     nnn =PHYS-nnnn
*         where nnn,nnnn are decimal digits, specifies the equivalence of
*         an AAA category (nnn) with a Physical Review category reference
*         number (nnnn).
*
*    The categories are subsequently used by the archiving procedure in order
*    to allow the user to provide some information on the type of data
*    frames which are being processed. The AAA category number as specified
*    in this index is stored in the archived frame header in the field
*    called AAA_CODE.

*  Invocation:
*     CALL AAA_INDEXING

*  Arguments:
*     None.

*  Authors:
*     DJM: Dave Mills (University College London)
*     BLY: Martin Bly (Starlink, RAL)
*     NG: Norman Gray (Starlink, Glasgow)

*  History:
*     23 Apr 1991 (DJM )
*        Created
*     02-MAR-1998 (BLY):
*        Corrected internal READ to use I4 not I.
*     05-JUL-1999 (NG)
*        Removed READONLY control from OPEN (unsupported in g77)

*-

*  Type Definitions:
      IMPLICIT NONE

*  Include Files:
      INCLUDE 'AAA_COMMON.INC'

*  Local Variables:
      INTEGER LUN     ! Logical unit number.
      INTEGER ISTAT   ! Temporary status return.
      INTEGER INDEX   ! Count of records.
      INTEGER XLEN

      CHARACTER*255 XLATE
*.
      ISTAT = 0

*   If not already read the AAA index then.
      IF ( .NOT. OPENED_AAA ) THEN

*     Grab a free logical unit number.
         CALL ECH_ACCESS_OBJECT( 'AAA-FILE-LUN', 'CREATE',
     :        'LUN', 0, 0, LUN, 0, 1, 0, ' ', ISTAT )

*     Open the file using standard FORTRAN.
         XLATE = 'AAA_CATEGORIES:'
         CALL ECH_PARSE_ENV( XLATE, XLEN )
         OPEN ( UNIT = LUN, FILE = XLATE( :XLEN ), STATUS = 'OLD',
     :           IOSTAT = ISTAT )

*     Initialise counters for indices.
         AAA_NCREFS = 0
         AAA_MAIN_CATS = 0
         AAA_MINOR_CATS = 0
         OPENED_AAA = .TRUE.

*     Loop through file.
         DO WHILE ( ISTAT .EQ. 0 )
            READ ( LUN, 1000, IOSTAT = ISTAT ) INDEX, CATEGORY
            IF ( ISTAT .NE. 0 ) THEN
                CLOSE( LUN )

            ELSE

*           If record specifies cross reference to Physical Review id then.
*           Decode cross reference and store it.
               IF ( CATEGORY( 1 : 6 ) .EQ. '=PHYS-' ) THEN
                  AAA_NCREFS = AAA_NCREFS + 1
                  READ ( CATEGORY( 7 : 10 ), '( I4 )' )
     :                 AAA_CREF_PHYS( AAA_NCREFS )
                  AAA_CREF_AAA( AAA_NCREFS ) = INDEX

*           Else if number is zero (denoting a new major category) the.
*           Increment catgory counter.
               ELSE IF ( INDEX .EQ. 0 ) THEN
                  AAA_MAIN_CATS = AAA_MAIN_CATS + 1
                  AAA_MAINCAT_DESCRIP( AAA_MAIN_CATS ) = CATEGORY

*           Else save category and number.
               ELSE
                  AAA_MINOR_CATS = AAA_MINOR_CATS + 1
                  AAA_NUMBERS( AAA_MINOR_CATS ) = INDEX
                  AAA_MINOR_CLASS( AAA_MINOR_CATS ) = AAA_MAIN_CATS
                  AAA_MINORCAT_DESCRIP( AAA_MINOR_CATS ) = CATEGORY
               ENDIF
            ENDIF
         END DO
      ENDIF

 1000 FORMAT ( I3, 1X, A )

      END
