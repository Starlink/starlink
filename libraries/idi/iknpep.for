************************************************************************

      SUBROUTINE IKNPEP( PARAM, SLEN, STRING, STATUS )

*+
*  Name:
*     IKNPEP
*
*  Purpose:
*     Perform the Ikon specific work for the IDI routine IIEPEP.
*
*  Invocation:
*     CALL IKNPEP( PARAM, SLEN, STRING, STATUS )
*
*  Description:
*     Routine not implemented.
*
*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        Parameter name
*     SLEN = INTEGER (Given)
*        Length of input buffer
*     STRING = CHARACTER * ( * ) (Given)
*        Buffer for string parameter
*     STATUS = INTEGER (Returned)
*        The global status.
*
*  Authors:
*     NE: Nick Eaton (Durham University)
*
*  History:
*     5 Dec 1990 (NE):
*        Original version.
*-
      
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'IDI_ERR'

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      INTEGER SLEN
      CHARACTER * ( * ) STRING

*  Status:
      INTEGER STATUS
*.

*   Subroutine not implemented
      STATUS = IDI__NOTIM

      END

