*+
*  Name:
*     VMS_DAT_PAR

*  Purpose:
*     Replace the true DAT_PAR include file on VMS systems.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     Dummy global constants include file.

*  Description:
*     This file is a Fortran 77 include file which contains only
*     comments. It is used (on VMS systems only) to replace the true
*     DAT_PAR include file because the constants normally defined in
*     DAT_PAR are instead defined in SAE_PAR (via an intermediate file
*     called SAE_FIX). This is done to preserve a historically
*     anomalous arrangement on VMS systems.

*  Notes:
*     It is recommended that both the SAE_PAR and DAT_PAR include files
*     are used in source code whenever DAT_PAR itself is required. This
*     will ensure compatibility with both VMS and non-VMS versions of
*     HDS.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JAN-1993 (RFWS):
*        Original version.
*     {enter_changes_here}

*-

*.
