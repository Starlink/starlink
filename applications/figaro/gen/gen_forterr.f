      SUBROUTINE GEN_FORTERR( IOS, SYMB, ERROR )
*+
*  Name:
*     GEN_FORTERR

*  Purpose:
*     Given a Fortran error code, returns an error string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GEN_FORTERR( IOS, SYMB, ERROR )

*  Description:
*     GEN_FORTERR returns a character string saying that a Fortran error
*     has occured. The codes given to this routine are those returned,
*     for example, by an I/O statement containing an IOSTAT=variable
*     specification.
*
*     Since the error codes depend on the machine and the Fortran
*     implementation, this routine returns an unspecific error string.
*     It is really a dummy routine and exists only for backwards
*     compatibility.

*  Arguments:
*     IOS = INTEGER (Given)
*        The Fortran error code.
*     SYMB = LOGICAL (Given)
*        Ignored.
*     ERROR = CHARACTER * ( * ) (Returned)
*        The character string describing the error.  The maximum length
*        of message returned by this routine is 62 characters.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     19 Jul 1995 (hme):
*        Original version.
*     11 Feb 2005 (timj):
*        g77 requires a format of Iw rather than just I
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      LOGICAL SYMB
      INTEGER IOS

*  Arguments Returned:
      CHARACTER * ( * ) ERROR

*.

      WRITE( ERROR,
     :   '( ''A Fortran error occured: IOSTAT = '', I4 )' ) IOS

      END
