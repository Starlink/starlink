      BLOCK DATA SGS1_BLK
*+
*  Name:
*     SGS1_BLK

*  Purpose:
*     SGS Block Data Initialisation

*  Description:
*     Initialise the SGSGO Common Block so that implicit activation
*     of SGS can be done.

*  Authors:
*     SLW: Sid Wright (UCL)
*     DLT: David Terrett (Starlink, RAL)

*  History:
*     18-APR-1983 (SLW):  
*        Original.
*     13-JAN-1992 (DLT):
*        Change name to SGS1_BLK
*        Reformat comments
*        Convert code to upper case
*-

*  Type Definitions:
      IMPLICIT NONE

*    Global variables:
      INCLUDE 'sgsgo_cmn'                  ! SGS Initialisation Switch

*    Global data:
      DATA SGSSLP /.TRUE./
*.

      END
