*+
*  Name:
*     KPG_ASTCMN

*  Purpose:
*     Define functional accessor/setter interface to KPG_AST common block

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Subroutines

*  Description:
*     This file contains a set of functions that enable access to the
*     KPG_AST common block from outside of the KPG library. Routines
*     are only added on demand.

*  Implementation Details:
*     The following routines are available:
*       KPG1_SETASTLN( LN ) - Set the ASTLN integer
*       KPG1_SETASTGRP( GRP ) - Set the ASTGRP identifier
*       KPG1_SETASTGSP( CHAR ) - Set ASTGSP character
*       KPG1_GETASTNPS()
*       KPG1_GETASTING()
*       KPG1_GETASTOUG()

*  Notes:
*     Since the routines are only 3 lines long, they are all in 
*     a single file with minimal comment wrapper. Types of the argument
*     for SET methods match the type in the common block.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1-OCT-2004 (TIMJ):
*        Original version. Add enough for NDFPACK:WCSSHOW
*     14-SEP-2005 (TIMJ):
*        Add 3 new get accessors
*     {enter_further_changes_here}

*-

      SUBROUTINE KPG1_SETASTLN( LN )
      IMPLICIT NONE
      INCLUDE 'DAT_PAR'
      INCLUDE 'KPG_AST'
      INTEGER LN

      ASTLN = LN
      END

      SUBROUTINE KPG1_SETASTGRP( IGRP )
      IMPLICIT NONE
      INCLUDE 'DAT_PAR'
      INCLUDE 'KPG_AST'
      INTEGER IGRP

      ASTGRP = IGRP
      END

      SUBROUTINE KPG1_SETASTGSP( GSP )
      IMPLICIT NONE
      INCLUDE 'DAT_PAR'
      INCLUDE 'KPG_AST'
      CHARACTER*(1) GSP

      ASTGSP = GSP
      END

      INTEGER FUNCTION KPG1_GETASTNPS ()
      IMPLICIT NONE
      INCLUDE 'DAT_PAR'
      INCLUDE 'KPG_AST'

      KPG1_GETASTNPS = ASTNPS
      END

      INTEGER FUNCTION KPG1_GETASTOUG ()
      IMPLICIT NONE
      INCLUDE 'DAT_PAR'
      INCLUDE 'KPG_AST'

      KPG1_GETASTOUG = ASTOUG
      END

      INTEGER FUNCTION KPG1_GETASTING ()
      IMPLICIT NONE
      INCLUDE 'DAT_PAR'
      INCLUDE 'KPG_AST'

      KPG1_GETASTING = ASTING
      END

