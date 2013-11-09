*+
*  Name:
*     {package}_CMN

*  Purpose:
*     Define {package}_ subroutine package initialisation flag

*  Language:
*     Starlink Fortran

*  Type of Module:
*     Global variables include file.

*  Description:
*     This file defines one global variable which defines the initialisation
*     state of the {package} subroutine group. This variable should be
*     defined to be .FALSE. on image startup by using an associated BLOCK
*     DATA module.

*  Prior Requirements:
*     None.

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Authors:
*     {author_identifier}: {authors_name} ({affiliation})
*     {enter_new_authors_here}

*  History:
*     {date} ({author_identifier}):
*        Original version.
*     {enter_changes_here}

*-

*  Global Constants:
      CHARACTER*{pack_prop_name_len}		{package}_PROP
        PARAMETER		( {package}_PROP = '{pack_prop_name}' )

*  Global Variables:
      LOGICAL			{package}_INIT		! Initialised?

*  Word aligned values      
      COMMON /{package}_CMN/          {package}_INIT

*  Save common block contents
      SAVE /{package}_CMN/
*.
