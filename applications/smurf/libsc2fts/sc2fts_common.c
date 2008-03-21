/*
*+
*  Name:
*     sc2fts_common.c

*  Purpose:
*     common functions used by sc2fts_entry

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     see sc2fts_common.h


*  Description:
*
*

*  Authors:
*     B.Zhang (UoL)

*  History :
*     2008-03-18 (BZ):
*        Create a implementation for FTS-2

*  Copyright:
*     Copyright (C) 2005-2006 Particle Physics and Astronomy Research
*     Council. University of British Columbia. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/
/* standard C includes */
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

/* STARLINK includes */
#include "par.h"


/* SC2FTS includes */
#include "sc2fts_common.h"

/* test if an operation is listed in operation list 
   RETURN (int): 0 if an operation or parameter is listed in the operation/parameter list; 
                 -1, otherwise;
*/
int issupported(const char *op, const char *opslist)
{
   const char delimiters[] = " ,;";
   char  *token, *cp, *cp_t;
   int   nret = -1;

   cp = strdup(opslist);
   cp_t = cp;
   while(cp_t != NULL)
   {
      token = strsep(&cp_t, delimiters);
      if(strlen(token) != 0)
      {
          if((nret = strcasecmp(op, token)) == 0) break;
      }
   };

   free(cp);

   return nret;
}

/* get the value of a keyword from the parameter list */
char* getvalue(const char* keyword, const char *parslist)
{
   int i;
   char *keyword_t, *value_t, *value_f, *kv_pair; 
   char *cp, *cp_t;

   value_f = NULL;

   cp = strdup(parslist);
   cp_t = cp;
   while(cp_t != NULL)
   {
      kv_pair = strsep(&cp_t, " ,;");

      /* if the string is not empty, retrieve the value for the keyword */
      if(strlen(kv_pair) != 0)
      {
         keyword_t = strtok(kv_pair, "=");
         value_t = strtok(NULL, "=");
         if(strcasecmp(keyword_t, keyword) == 0) 
         {
            value_f = value_t;
            break; 
         }
      }
   }
   free(cp);
   return value_f;
}

/* get the parameters for an operation defined by opID from parameter list */
int getpars(const char* pars_fun, const char *parslist, char *pars)
{
   int nret;
   char  *cp, *cp_t;
   char  *keyword_t, *value_t = NULL;

   *pars = '\0';
   cp = strdup(pars_fun);
   cp_t = cp;
   nret = 0;
   while(cp_t != NULL)
   {
      keyword_t = strsep(&cp_t, " ");
      
      if(strlen(keyword_t) != 0)
      {
          value_t = getvalue(keyword_t, parslist);
          if(value_t == NULL) 
          {
              nret = -1;
              break;
          }
          strcat(pars, getvalue(keyword_t, parslist));
          strcat(pars, ":");
      }
   };

   free(cp);

   return nret;
}
