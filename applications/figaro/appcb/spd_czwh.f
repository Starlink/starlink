      SUBROUTINE SPD_CZWH( FILE, FU, NCOMP, LINENA, LABFRE, COMPTY,
     :   NPARA, MASKL, MASKR, STATUS )
*+
*  Name:
*     SPD_CZWH

*  Purpose:
*     List result components (_DOUBLE).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_CZWH( FILE, FU, NCOMP, LINENA, LABFRE, COMPTY, NPARA,
*        MASKL, MASKR, STATUS )

*  Description:
*     This routine lists the contents of the spectral component indexed
*     result extension vectors in the Specdre Extension. The output
*     formats are adjusted for _DOUBLE laboratory frequencies. The mask
*     intervals are written only as _REAL.

*  Arguments:
*     FILE = LOGICAL (Given)
*        True if an ASCII file is to be used, has been opened
*        successfully and is identified by the Fortran unit number FU.
*     FU = INTEGER (Given)
*        The Fortran unit number of the output ASCII file. This is
*        unused if FILE is false.
*     NCOMP = INTEGER (Given)
*        The length of the vectors to be listed.
*     LINENA( NCOMP ) = CHARACTER * ( * ) (Given)
*        The character vector for the second column.
*     LABFRE( NCOMP ) = DOUBLE PRECISION (Given)
*        The numeric vector for the third column.
*     COMPTY( NCOMP ) = CHARACTER * ( * ) (Given)
*        The character vector for the fourth column.
*     NPARA( NCOMP ) = INTEGER (Given)
*        The integer vector for the fifth column.
*     MASKL( NCOMP ) = REAL (Given)
*        The left bound of the range of spectroscopic values to which
*        any component pertains.
*     MASKR( NCOMP ) = REAL (Given)
*        The right bound of the range of spectroscopic values to which
*        any component pertains.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     11 Mar 1992 (hme):
*        Original version.
*     24 Nov 1994 (hme):
*        Renamed from SPAAR.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Standard PRIMDAT constants

*  Arguments Given:
      LOGICAL FILE
      INTEGER FU
      INTEGER NCOMP
      CHARACTER * ( * ) LINENA( NCOMP )
      DOUBLE PRECISION  LABFRE( NCOMP )
      CHARACTER * ( * ) COMPTY( NCOMP )
      INTEGER NPARA( NCOMP )
      REAL MASKL( NCOMP )
      REAL MASKR( NCOMP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop index
      CHARACTER * ( 78 ) SHORT   ! Short string to write to screen

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Formats. A bad lab freq is signalled by asterisks. But bad mask
*  bounds are not intercepted.
 101  FORMAT ( ' ', I3, ' ', A32, ' ', G22.16E2, ' ', A32,
     :         ' ', I4, ' ', G13.7E2, ' ', G13.7E2 )
 103  FORMAT ( ' ', I3, ' ', A32, ' ******** bad ********* ', A32,
     :         ' ', I4, ' ', G13.7E2, ' ', G13.7E2 )
 201  FORMAT ( ' ', I3, ' ', A8,  ' ', G22.16E2, ' ', A8,
     :         ' ', I4, ' ', G12.6E2, ' ', G12.6E2 )
 202  FORMAT ( A )
 203  FORMAT ( ' ', I3, ' ', A8,  ' ******** bad ********* ', A8,
     :         ' ', I4, ' ', G12.6E2, ' ', G12.6E2 )

*  If list goes to ASCII file.
      IF ( FILE ) THEN

*     Table header.
         WRITE ( FU, 202 ) '   # line name                        ' //
     :      'lab freq.              ' //
     :      'comp. type                      npara ' //
     :      'mask from     to'

*     Table rows.
         DO 1 I = 1, NCOMP
            IF ( LABFRE(I) .EQ. VAL__BADD ) THEN
               WRITE ( FU, 103 )
     :            I, LINENA(I), COMPTY(I), NPARA(I), MASKL(I), MASKR(I)
            ELSE
               WRITE ( FU, 101 )
     :            I, LINENA(I), LABFRE(I), COMPTY(I), NPARA(I),
     :            MASKL(I), MASKR(I)
            END IF
 1       CONTINUE

*  Else (list goes to screen etc.)
      ELSE

*     Table header.
         SHORT = '   # line     lab freq.              ' //
     :      'type    npara mask from    to'
         CALL MSG_OUT( 'SPD_CZWH_LIST', SHORT, STATUS )

*     Table rows.
         DO 2 I = 1, NCOMP
            IF ( LABFRE(I) .EQ. VAL__BADD ) THEN
               WRITE ( SHORT, 203 )
     :            I, LINENA(I), COMPTY(I), NPARA(I), MASKL(I), MASKR(I)
            ELSE
               WRITE ( SHORT, 201 )
     :            I, LINENA(I), LABFRE(I), COMPTY(I), NPARA(I),
     :            MASKL(I), MASKR(I)
            END IF
            CALL MSG_OUT( 'SPD_CZWH_LIST', SHORT, STATUS )
 2       CONTINUE
      END IF

*  Return.
      END
