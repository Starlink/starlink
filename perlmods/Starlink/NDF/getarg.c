void getarg_(int* iarg, char* arg, int arglen) 
{
/* 
 *+

   Purpose:
     Returns the string corresponding the iarg'th argument

   Language:
     ANSI C with Perl extensions

   Description:
     This routine reads the perl variables $0 (program name) and @ARGV
     (array of supplied arguments) and returns the requested argument
     in the string arg. Makes use of perl C library calls to access the
     perl variables. If iarg is 0 then the program name is returned.

   Arguments:
     iarg  = Pointer to int (Given)
        This is the requested argument number 
     arg   = Char (Returned)
        The iarg'th argument. If this is zero then the program name is
        returned. This array is filled with blanks to size arglen so that
        it emulates a Fortran string.
     arglen = int (Given)
        Size of the character array

   Notes:
     - Replaces the getarg_ routine found in standard Fortran libraries.
     - getarg_ is only called when an NDF is opened with UPDATE or NEW mode
       and a history component exists.

   Implementation Status:
     Seems to run on Solaris, Digital Unix and Linux even though
     Solaris and linux (libf2c) contain there own versions of getarg_
     which I thought would generate clashes.

     This is a routine that replaces the internal fortran getarg_ subroutine
     This is done for two reasons: 

       1) getarg_ produces a segmentation violation when used 
       from Fortran libraries that have a C main (such as the NDF
       history calls - called from ndf1_gtarg.c)

       2) This version knows about perl $0 and @ARGV and uses these
       variables to return the necessary arguments descriptions. This
       means that the History component is updated correctly.

   Authors:
     Tim Jenness (TimJ)
   
   History:
     14-JUN-1997 (TimJ):
       Original version

   */

  int  i;             /* Loop counter */
  AV*  argv;          /* Array containing perl arguments */
  SV** val;           /* Pointer to member of array from fetch */
  SV*  name;          /* Pointer to name of perl binary */
  char * perl_string; /* Pointer to perl string */

  /* If iarg is zero then we need to return perl $0 */

  if (*iarg == 0) {

    /* There should always be a name */

    name = perl_get_sv("0", FALSE);

  } else {

    /* If iarg is greater than zero then we need to return @ARGV[iarg-1] */
    /* I can get these arguments from perl as AV*s */

    argv = (AV*) perl_get_av("ARGV", FALSE);

    /* Now fetch the ith-1 arguments */

    val = av_fetch(argv, *iarg - 1, 0);

    /* Copy this to my SV in order to simplify the code */
    /* Making sure that I have something to copy first */

    if (val == NULL) {
      name = NULL;
    } else {
      name = *val;
    }
  }

  if (name == NULL) {

    /* Simply fill the strings with blanks */
    for (i = 0; i <= arglen; i++) {
      *(arg + i) = ' ';  
    }

  } else {

    /* Copy this string into a char * */
    perl_string = (char *) SvPV(name, PL_na);
    
    (void)strcpy((char *) arg, (char *) perl_string);
    
    /* This string probably contains a null so run through and fill the
       remainder of the string with blanks */

    i = (int) strlen(arg); /* Find the position of the null character */

    if (i > arglen) {return;} /* If string is too large */

    while(i < arglen) {         /* Change to spaces to end of string */
      *(arg + i)=' ';
      i++;
    }

  }

}
