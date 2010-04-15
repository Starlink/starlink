      SUBROUTINE FV4_OPF321( UNIT, FILE, IFAIL )
*+
*  Name:
*     FV4_OPF321

*  Purpose:
*     Open a Specx spectra file with format version 3, 2, or 1.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL FV4_OPF321( UNIT, FILE, IFAIL )

*  Description:
*     This routine opens a file for Specx that contains spectra in one
*     of the old VAX binary formats of versions 3, 2, or 1. This is just
*     an OPEN statement. It is tucked away in this routine because the
*     record length has to be specified in 4-byte words on Digital
*     machines (VAX, mips, Alpha), but in bytes on other machines
*     (Sun4).
*
*     The file is openend:
*        STATUS = 'OLD'       file must exist,
*        READONLY             minimum privilege to access file required,
*        ACCESS = 'DIRECT'    direct access
*        FORM = 'UNFORMATTED' binary data
*        RECL = 64 words or   on VAX, mips, Alpha
*        RECL = 256 bytes     on Sun4

*  Arguments:
*     UNIT = INTEGER (Given)
*        The number of a free unit, on which the file should be opened.
*     FILE = CHARACTER * ( * ) (Given)
*        The file name to be used by the OPEN statement.
*     IFAIL = INTEGER (Returned)
*        The IOSTAT value returned by the OPEN statement.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     rp:  Rachael Padman (MRAO)
*     {enter_new_authors_here}

*  History:
*     03 Dec 1993 (hme):
*        Original version.
*     15 Jan 1994 (rp):
*        Replace unformatted direct access open with call to UOPENUF
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      INTEGER UNIT
      CHARACTER * ( * ) FILE

*  Arguments Returned:
      INTEGER IFAIL

*.

*     Called with READONLY parameter = .TRUE.
      CALL UOPENUF ( UNIT, FILE, 'OLD', 64, .TRUE., IFAIL)

      END
