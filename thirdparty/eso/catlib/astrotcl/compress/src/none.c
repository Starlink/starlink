/*+
************************************************************************
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
*
* (c) 1995				(c) 1995.
* National Research Council		Conseil national de recherches
* Ottawa, Canada, K1A 0R6 		Ottawa, Canada, K1A 0R6
* All rights reserved			Tous droits reserves
* 					
* NRC disclaims any warranties,		Le CNRC denie toute garantie
* expressed, implied, or statu-		enoncee, implicite ou legale,
* tory, of any kind with respect	de quelque nature que se soit,
* to the software, including		concernant le logiciel, y com-
* without limitation any war-		pris sans restriction toute
* ranty of merchantability or		garantie de valeur marchande
* fitness for a particular pur-		ou de pertinence pour un usage
* pose.  NRC shall not be liable	particulier.  Le CNRC ne
* in any event for any damages,		pourra en aucun cas etre tenu
* whether direct or indirect,		responsable de tout dommage,
* special or general, consequen-	direct ou indirect, particul-
* tial or incidental, arising		ier ou general, accessoire ou
* from the use of the software.		fortuit, resultant de l'utili-
* 					sation du logiciel.
*
************************************************************************
*
*   Module Name:	press/src/none.c
*
*   Purpose:
*	Whatever
*
*   Routines:
*	type	routine		: Brief description.
*	type	routine		: Brief description.
*	type	routine		: Brief description.
*
*   Date		: Nov 16, 1995
*
*   SCCS data		: @(#)
*	Module Name	: none.c
*	Version Number	: 1.1
*	Release Number	: 1
*	Last Updated	: 03/07/96
*
*   Programmer		: Norm Hill
*
*   Modification History:
*
****  C A N A D I A N   A S T R O N O M Y   D A T A   C E N T R E  *****
************************************************************************
-*/

#include "gen_types.h"

#include "press.h"
#include "local_press.h"



/*+
************************************************************************
*
*   Function:	name
*
*   Purpose:
*	Statement of purpose.
*
*   Values Returned:
*	type	name		: Meaning	*** delete if void function ***
*	type	name		: Meaning	*** delete if void function ***
*
*   References:
*	*** delete if not applicable ***
*
************************************************************************
-*/

int	none_comp
(
    pfi		char_in,	/* (in)  Function to get data.		*/
    pfi		char_out	/* (in)  Function to write data.	*/
)
{
    byte	buffer[4096];
    int		bytes_left;


    while ( ( bytes_left = char_in( buffer, sizeof( buffer ) ) ) != PR_E_EOI )
    {
	PR_CHECK( char_out( buffer, bytes_left ) );
    }

    return( PR_SUCCESS );
}

/*+
************************************************************************
*
*   Function:	name
*
*   Purpose:
*	Statement of purpose.
*
*   Values Returned:
*	type	name		: Meaning	*** delete if void function ***
*	type	name		: Meaning	*** delete if void function ***
*
*   References:
*	*** delete if not applicable ***
*
************************************************************************
-*/

int	none_uncomp
(
    pfi		char_in,	/* (in)  Function to get data.		*/
    pfi		char_out	/* (in)  Function to write data.	*/
)
{
    byte	buffer[4096];
    int		bytes_left;


    while ( ( bytes_left = char_in( buffer, sizeof( buffer ) ) ) != PR_E_EOI )
    {
	PR_CHECK( char_out( buffer, bytes_left ) );
    }

    return( PR_SUCCESS );
}
