      BLOCK DATA PGP1_BLK
*+
*  Name:
*     PGP1_BLK

*  Purpose:
*     PGP Block Data Initialisation

*  Description:
*     Initialise the PGPGO Common Block so that implicit activation
*     of PGP can be done.

*  Authors:
*     DLT: David Terrett (Starlink, RAL)

*  History:
*     28-JAN-1992 (DLT):  
*        Original.
*-

*  Type Definitions:
      IMPLICIT NONE

*    Global variables:
      INCLUDE 'pgpgo_cmn'                  ! PGP Initialisation Switch

*    Global data:
      DATA PGPSLP /.TRUE./
*.

      END
