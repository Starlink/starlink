      BLOCK DATA GKS1_BLK
*+
*  Name:
*     GKS1_BLK

*  Purpose:
*     Initialise the GKSGO Common Block so that implicit activation
*     of GKS can be done.     

*  Language:
*     Starlink Fortran 77

*  Authors:
*     SLW: Sid Wright (UCL)
*     DLT: David Terrett (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     18-APR-1983 (SLW):
*        Original Version.
*     09-JAN-1992 (DLT):
*        Reformated comments
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global variables:
      INCLUDE 'gksgo_cmn'                     ! GKS Initialisation Switch

*  Global data:
      DATA GKSSLP /.TRUE./
*.
      END
