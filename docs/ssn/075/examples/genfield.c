/*
   Name:
      genfield
   Purpose:
      Generate a catalogue correspnding to a fake star field.
   Language:
      C.
   Type of Module:
      Main function.
   Arguments:
      First command line argument
         Central Right Ascension of the fake star field (a string in
         decimal degrees).
      Second command line argument
         Central Declination of the fake star field (a string in
         decimal degrees).
      Third command line argument
         Radius of the fake star field (a string in minutes of arc).

      The program returns a status, coded as follows:
         0 - success,
         1 - failure (bad input arguments).
   Description:
      Generate a catalogue correspnding to a fake star field.  The central
      coordinates and radius of the field are specified.  The same star
      pattern is always generated, centred and scaled till the specified
      field.  The star-list generated is written to standard output as
      a tab-separated table (TST).
   Algorithm:
      Initialise the status to zero.
      If the number of comman line arguments is 3 then
        Attempt to decode the Right Ascension, Declination and radius.
        If ok then
          Compute the scale factor for scaling the tabulated positions
          to fill the field.
          Write the TST header.
          Write the column details.
          For every star
            Compute the output coordinates.
            Write the output line.
          end for
        else
          Set the status.
          Write a message.
        end if
      else
        Set the status.
        Write a message.
      end if
   Copyright:
      Copyright (C) 2000 Central Laboratory of the Research Councils.
   Authors:
      ACD: A C Davenhall (Edinburgh)
   History:
      22/2/00 (ACD): Original version.
      11/4/00 (ACD): First stable version.
                                                                       */

#include <stdio.h>
#include <string.h>

main(argc, argv)
   int    argc;
   char **argv;
{  int status;        /* Return status. */

   int maxstars = 8;  /* Number of stars listed.  */

   static double ralist[8]  =                      /* Right Ascension.  */
     {16.0, -19.0, -10.0, -4.0, 0.0, 4.0, -10.0, 9.0};
   static double declist[8] =                      /* Declination.      */
     {30.0, -23.0, 25.0, 3.0, 0.0, -2.0, -4.0, -29.0};
   static float  maglist[8] =                      /* Magnitude.        */
     {0.58, 0.12, 1.64, 2.21, 1.70, 1.79, 3.38, 4.13};

   int star;   /* Loop counter for the current star.     */
   int seqno;  /* Sequence number for the current star.  */

   double tabrad = 30.0; /* Radius of tabulated star field.             */
   double scale;         /* Scale factor for calculating star coords.   */

   double racen;         /* Central R.A. of the star field (degrees).   */
   double deccen;        /*    "    Dec. "   "   "     "   (   "   ).   */
   double radius;        /* Radius of the star field (minutes of arc).  */

   double rastar;        /* R.A. of the current star (degrees).   */
   double decstar;       /* Dec. "   "     "     "   (   "   ).   */


/*
   Initialise the status to zero.  */

    status = 0;


/* 
   Check that exactly 3 command line arguments have been given.  */

    if(argc == 4)
    {

/*
      Attempt to decode the R.A. and Dec. and proceed if ok.  */

       racen = 999;
       ++argv;
       sscanf(*argv,"%lf", &racen);

       deccen = 999;
       ++argv;
       sscanf(*argv,"%lf", &deccen);

       radius = 999;
       ++argv;
       sscanf(*argv,"%lf", &radius);

/*     printf(" args: %f %f %f \n", racen, deccen, radius);  */

       if(racen < 999  &&  deccen < 999  &&  radius < 999)
       {

/*
         Compute the scale factor for scaling the tabulated positions
         to fill the field of view.  */

          scale = radius / (tabrad * 60.0);

/*
         Write the TST header.  */

          printf("Example Star Field. \n\n");

/*
         Write the column details.  */

          printf("#column-units:    \tDEGREES \tDEGREES \tMagnitudes \n");
          printf("#column-types:   CHAR*8 \tDOUBLE \tDOUBLE \tREAL \n");
          printf("#column-formats: A8 \tF12.6 \tF12.6 \tF6.2 \n\n");

          printf("Id \tRA \tDEC \tmag \n");
          printf("--\t--\t---\t---\n");

/*
         Loop writing the values for each star.  */

          for(star=0; star < maxstars; star++)
          {  seqno = star + 1;

/*
            Compute the output coordinates.  */

             rastar = racen + (ralist[star] * scale);
             decstar = deccen + (declist[star] * scale);

/*
            Write the output line.  */

             printf("Star %d \t", seqno);
             printf("%f \t", rastar);
             printf("%f \t", decstar);
             printf("%f \n", maglist[star]);
          }
       }
       else
       {  status = 1;
          printf("Invalid arguments given.\n");
       }
    }
    else
    {  status = 1;
       printf("Wrong number of arguments given (3 required).\n");
    }

/*
   Set the return status.  */

    return status;
}
