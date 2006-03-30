/*+
 *   Name:
 *      gaiaUtils

 *   Purpose:
 *      Utility routines for GAIA.

 *   Language:
 *      C

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      29-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <string.h>

#include <gaiaUtils.h>
#include <sae_par.h>
#include <ems.h>
#include <ems_par.h>

/*
 *  Name:
 *     gaiaUtilsErrMessage
 *
 *  Purpose:
 *     Copy the current EMS error message into a dynamic string for returning. 
 *     Make sure status is set before calling this and release the memory used
 *     for the string when complete. On return status is reset to SAI__OK and 
 *     the error stack is empty.
 */
char *gaiaUtilsErrMessage()
{
    char *result_copy = NULL;
    char *result_message = NULL;
    char buffer[EMS__SZMSG+1];
    char param[EMS__SZPAR+1];
    int buffer_length = 0;
    int mess_length = 0;
    int param_length = 0;
    int status_copy;

    /* Need to get error message in one pass. Since we don't know how
     * big this is going to be we need to realloc memory used */

    /* Recover error status from EMS */
    emsStat( &status_copy );

    /* Loop until all error has been read */
    while ( status_copy != SAI__OK ) {

        /* Load error message and start reading it */
        emsEload( param, &param_length, buffer, &buffer_length, &status_copy );

        /* If all read then stop */
        if ( status_copy != SAI__OK ) {

            /* Else initialise or increase the result buffer to make room for
             * new part, plus 2 for newline and null. */
            mess_length += ( buffer_length + 2 );
            result_copy = (char *) realloc( (void *) result_message,
                                            (size_t) mess_length );
            if ( result_copy == NULL ) {
                
                /* Realloc failed so use the fragment already recovered in
                 * result_message */
                status_copy = SAI__OK;
            }
            else {
                /* Realloc suceeded */
                result_message = result_copy;
                
                /* Add a newline to end of string to wrap last line, unless
                 * this is first, in which initialise it */
                if ( mess_length == ( buffer_length + 2 ) ) {
                    result_message[0] = '\0';
                }
                else {
                    strcat( result_message, "\n" );
                }
                
                /* Concatenate buffer into result string */
                strncat( result_message, buffer, buffer_length );
            }
        }
    }
    return result_message;
}
