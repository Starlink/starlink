var whole, exact;
var matchedNames = new Array();
var matchedWeights;
var unmatchedNames = new Array();
var base = "";

/* Called when the go button is pressed
   ----------------------------------- */
function go() {
   var domain, test0, test1, test2, crit, phrase, form, resdoc, what, 
       checked, i, j, remove, add, name, re, nt, tests, nm, weight, value;

/* Set up search criteria required below. */
   var crita = { phrase: null,          // A criterion which searches string targets only
                  flags: 1,
                  tests: null };
                   
   var critb = { phrase: null,          // A criterion which searches all sub-target string 
                  flags: 4,             //  values within the parent target.
                  tests: [{ re: null, crit: crita}] };
                   
   var critc = { phrase: null,          // A criterion which searches all sub-target names
                  flags: 2,             //  within the parent target only.
                  tests: null }; 
   
/* Initialise commonlly used values. */
   form = document.forms[0];
   
/* Prepare the form for another search by giving keyboard focus to
   the text box. */  
   form.PHRASE.focus();

/* Get the search phrase from the form. If it is blank, report an error
   and skip. Otherwise, split the phrase into an array of words. */
   phrase = form.PHRASE.value;
   if( phrase.search( /\S+/ ) == -1 ) {
      alert( "No search string given!" );     
   } else {
      phrase = phrase.split(/\s+/);
      
/* See which routines are to be searched. */
      value = radioCheck( form, "DOMAIN" );
      if( value == "ALL" ) {
         domain = routines;
      } else if( value == "SEL" ) {
         domain = matchedNames;
      } else if( value == "UNSEL" ) {
         domain = unmatchedNames;
      }   
      
/* See if part-words can to be matched or only whole words, and if any 
   sub-set of the supplied words can be matched or only the exact phrase. */
      value = radioCheck( form, "MATCH" );
      if( value == "ANY" ) {
         exact = 0;
         whole = 0;
      } else if( value == "EXACT" ) {
         exact = 1;
         whole = 1;
      } else if( value == "WHOLE" ) {
         exact = 0;
         whole = 1;
      }   
      
/* Build the crierion for search routines. The top-level criterion checks
   the contents (i.e. flag=4) of all components which match any of the
   tests stored in test0, test1 or test2. Test0 checks the simple string
   components (Purpose, Description, Name). Test1 checks the names of the
   components within the Argument component. Test2 checks the strings
   components of the Argument component. Loop round each checked form
   button, assigning test1 and test2 if the relevant buttons are pressed,
   and the regular expression needed for test0. */
      re = ""
      checked = checkCheck( form, "WHAT" );
      for( i = 0; i < checked.length; i++ ) {
         what = checked[i];

         if( what == "ANAM" ) {
            test1= {re: /^argument/i, crit: critc };
         
         } else if( what == "ADES" ){
            test2= {re: /^argument/i, crit: critb };
        
         } else {
            if( re == "" ) {
               re += what;
            } else {
               re += "|"+what;
            }         
         }
      }
 
/* If required, construct test0 */  
      if( re != "" ) test0 = {re: new RegExp( "^("+re+")$", "i" ), crit: crita };
   
/* Now construct the top-level criterion. */
      tests = new Array();
      nt = 0;
      if( test0 != null ) tests[nt++] = test0;
      if( test1 != null ) tests[nt++] = test1;
      if( test2 != null ) tests[nt++] = test2;
         
      crit = {  phrase: phrase, flags: 4, tests: tests };
   
/* We now have all the information needed to perform the search. Check
   that there are some routines to search. If not, warn the user. Otherwise,
   do the search. */
      if( domain.length == 0 ) {
         alert( "There are no routines to search." );
      } else {    
         weight = find( domain, crit, false );

/* Get an array holding the matching routine names */
         match = new Array();
         nm = 0;
         for( name in weight ) match[nm++] = name;
   
/* Warn the user and leave any current results unchanged if no matching 
   routines were found.  */
         if( nm == 0 ) {
            alert( "No routines were found matching the given criteria." );
         } else {
          
/* Save an array of currently matched routines, using the method
   specified by the user for combining the new matching routines with any
   previous matched routines . */
            value = radioCheck( form, "METHOD" );
            if( value == "REPLACE" ) {
               matchedNames = match;
               matchedWeights = weight;
   
            } else if( value == "REMOVE" ) {
               for( j = 0; j < matchedNames.length; j++ ) {
                  name = matchedNames[j];
                  remove = false;
                  for( i = 0; i < match.length; i++ ) {
                     if( name == match[i] ) {
                        remove = true;
                        match.splice( i, 1 );
                        break;
                     }
                  }
                  if( remove ) {
                     matchedNames.splice( j, 1 );
                     delete matchedWeights[name];
                     j--;
                  }
               }
   
            } else if( value == "ADD" ) {
               for( j = 0; j < match.length; j++ ) {
                  name = match[j];
                  add = true;
                  for( i = 0; i < matchedNames.length; i++ ) {
                     if( name == matchedNames[i] ) {
                        add = false;
                        if( matchedWeights[name] < weight[name] ) {
                           matchedWeights[name] = weight[name];
                        }
                        break;
                     }
                  }
                  if( add ) {
                     matchedNames.push( name );
                     matchedWeights[name] = weight[name];
                  }
               }
            }   

/* Sort the current results. */
            sortResults( false );

/* Display the results. */
            displayResults();

/* Save an array of unmatched routines. */
            match = deepCopy( matchedNames );
            unmatchedNames = new Array();
            for( name in routines ) {
              add = true;
              for( i = 0; i < match.length; i++ ) {
                 if( name == match[i] ) {
                    add = false;
                    match.splice( i, 1 );
                    break;
                 }
              }
              if( add ) unmatchedNames.push( name );
            }
         }
      }
   }   
}

function find( routines, crit, alpha ) {
/* 
*  Name:
*     find

*  Purpose:
*     Search for routines which match a specified search criterion,
*     returning an ordered list of matching routine names.

*  Invocation:
*     return = find( routines, crit, alpha );

*  Arguments:
*     routines
*        An object with a property for each known routine. The property
*        name is equal to the routine name, and the property value is 
*        a searchable "target" containing the prologue information for
*        the routine.
*     crit
*        An object (a "criterion") holding the criteria for choosing 
*        matching routines.
*     alpha
*        If true, the returned names are sorted alphabetically.
*        Otherwise, they are sorted in order of decreasing significance
*        (=weight) of the match.

*  Returned Value:
*        An object containing a property for each matching routine. The
*        property names are the routine names, and the property values are
*        the weights of the matches.

*  Notes:
*     - A "target" is a searchable entity. It is either a string, or an
*     object containing other targets ("sub-targets"). Each sub-target is
*     then a property (i.e. a component) of the parent target, and as such 
*     will have a name within the parent target.
*
*     - A "criterion" is an object which defines how a target is to be
*     searched. A criterion is always associated with a target. A
*     criterion has 3 properties:
*        phrase: An array of words to be searched for. The way in which a
*    	    match between a given test string and the phrase is determined
*	    is controlled by the global variables "exact" and "whole". If
*           null, the phrase defaults to the phrase in the criterion
*           associated with any parent target.
*        flags: An integer indicating how the associated target should be
*           searched. If null, the flags value defaults to the value in the 
*           criterion associated with any parent target. The value should be 
*           the sum of one or more of the following values:   
*              1 - If the target is a string, search the string for the
*                  phrase.
*              2 - If the target is an object, search the list of
*                  property names (i.e. sub-target names) for any which
*                  match the phrase.
*              4 - If the target is an object, search selected sub-targets
*                  using specified criteria, as indicated by "tests".
*        tests: An array of "test" objects (described below), which
*           selects the sub-targets to be searched when "flags" contains 
*           "4". If this array is null, or of zero length, no sub-targets
*           are searched.
*
*     - A "test" is an object which defines a particular group of
*     sub-targets within a parent target. It has two properties:
*        re: A RegExp (regular expression) defining the names of the
*            sub-targets within the group. Any sub-target within the
*            associated parent target which has a name which matches
*            this regular expression is a member of the group. If this is
*            null, all sub-targets within the parent target are included 
*            in the group.
*        crit: A "criterion" to associate with each sub-target in the
*            group. This defines how the sub-targets should be searched.
*            If this is null, the criterion associated with the parent
*            target is used.
*/
   var names, nmatch, phrase, flags, weight;

/* Create an object in which to store the weights. */
   weight = new Object();

/* Extract the search phrase and flags from the supplied criterion. */
   phrase = crit.phrase;
   flags = crit.flags;

/* Search each routine, noting the weight of any matches found. */
   for( name in routines ) { 
      w = searchTarget( routines[name], crit, phrase, flags );
      if( w > 0.0 ) weight[name] = w;
   }

   return weight;

}

function searchTarget( target, crit, phrase, flags ){
/* 
*  Name:
*     searchTarget

*  Purpose:
*     Search a target using a specified criterion, returning the total
*     weight of any matches found at any level within the target.

*  Invocation:
*     return = searchTarget( target, crit, phrase, flags );

*  Arguments:
*     target 
*        A "target" to be searched. 
*     crit
*        A "criterion" specifying how to search the target. If a null
*        value is suplied, a default criterion is formed using the 
*        "phrase" and "flags" variables, and a null "tests" array.
*     phrase
*        A default phrase. This is used if the phrase property of "crit"
*        is null.
*     flags
*        Default flags. This is used if the flags property of "crit"
*        is null.

*  Returned Value:
*        A numerical value indicating the signficance (weight) of all
*        matches found using the supplied target and criterion. Zero is 
*        returned if no matches are found.

*  Notes:
*     - The "Notes" section for function "find" contains descriptions of
*     "targets", "criterion", etc.

*/
   var re, scrit, ntests, test, i, name, tests, flag1, flag2, flag4, ret;

/* Initialize the returned weight */
   ret = 0;

/* Get the criterion properties to use. */
   if( crit != null ) {
      if( crit.phrase != null ) phrase = crit.phrase;
      if( crit.flags != null ) flags = crit.flags;
      if( crit.tests != null ) tests = crit.tests;
   }

/* Extract individual flags. */
   flag1 = ( ( flags & 1 ) != 0 );
   flag2 = ( ( flags & 2 ) != 0 );
   flag4 = ( ( flags & 4 ) != 0 );

/* If the supplied target is a string, and flag 1 is set, search it. */
   if( typeof target == "string" ) {
     if( flag1 ) {
        ret += searchFor( target, phrase );
     }

/* If the supplied target is not a string, it is assumed to be an object */
   } else {

/* If flag 2 is set, test each sub-target name against the supplied
   phrase. */
      if( flag2 ) {
         for( name in target ) ret += searchFor( name, phrase );
      }

/* If flag 4 is set, and some sub-groups are defined, test all required 
   groups of sub-targets against their associated criterion. */
      if( flag4 && tests != null ) {
         ntests = tests.length;
         for( i = 0; i < ntests; i++ ) {
            test = tests[i];

/* Get the criterion to associate with each sub-target in this group. */
            if( test.crit != null ) {
               scrit = test.crit;
            } else {
               scrit = crit;
            }

/* If this test has a regular expression, find all sub-targets with names
   which match the regular expression, and search them by calling this
   function recursively. */
            if( test.re != null ) {
               re = test.re;
               for( name in target ) {
                  if( name.search(re) != -1 ) {
                     ret += searchTarget( target[name], scrit, phrase, flags );
                  }
               }

/* If this test has no regular expression, search all sub-targets by calling 
   this function recursively. */
            } else {
               for( name in target ) {
                  ret += searchTarget( target[name], scrit, phrase, flags );
               }
            }
         }
      }
   }

   return ret;
}

function searchFor( text, phrase ){
/* 
*  Name:
*     searchFor

*  Purpose:
*     Search a string for a given phrase, or any sub-phrase, returning
*     a weight indicating the significance of the match.

*  Invocation:
*     return = searchFor( text, phrase );

*  Arguments:
*     text 
*        The string to be searched.
*     phrase 
*        An array holding the words in the phrase top be searched for.

*  Returned Value:
*        A numerical value indicating the signficance (weight) of any
*        match found. Zero is returned if no matches are found.

*/
   var w;

/* If the text is to be searched for the exact phrase... */
   if( exact ) {
      w = searchText( text, phrase );

/* If the text is to be searched for any sub-phrase... */
   } else {
      w = searchSubText( text, phrase );
   }

   return w;
}

  
/* Search for a given list of adjacent words within a given test string, 
   returning zero if no match is found, or a weight equal to the square 
   of the number of words if a match is found.
   --------------------------------------------------------------------- */
function searchText( teststr, words ) {
   var pattern, re, i, nw, reta;

/*Save the number of words in the search phrase. */
   nw = words.length;
 
/* Create a regular expression for matching the supplied words. If only whole words 
   can be matched, ensure that the first word starts at a word boundary, the last word
   finishes at a word boundary, and there is at least one non-word character between 
   each pair of words. */
   if( whole ) {
      re = "\\b";
      for( i = 0; i < nw; i++ ) {
         if( i != 0 ) re += "\\W+";
         re += words[i];
      }
     re += "\\b";

/* If part words can be matched, just look for the given words, optionally with zero or more 
   arbitrary character between them. */
   } else {
      re = "";
      for( i = 0; i < nw; i++ ) {
         if( i > 0 ) re += ".*";
         re += words[i];
      }  
   }
       
   pattern = new RegExp( re, "i" );

/* Check the supplied test string against the above regular expression. If it
   matches return a weight equal to the square of the number of matching 
   words (this is to give a greater weight per word when matching long strings).*/
   if( teststr.search(pattern) != -1 ) {
      reta = nw*nw; 
   } else {
      reta = 0;
   }

/*  Return the weight. */
   return reta;
}

/* Searchs for any sub-string of array words within string teststr, and 
   return a weight for all matches. Matches of longer sub-strings have 
   more weight per word than shorter sub-strings. Thus checking the test
   string "Here is a good routine" for words "Here bad good routine" 
   will return the sum of the following weights:
         1 for the single matching word "Here"
         1 for the single matching word "good"
         1 for the single matching word "routine"
         2 for each of the two words in the matching phrase 
            "good routine" (giving 4 in total).

     This equals a total returned weight of 7 (=1+1+1+2+2)
  ------------------------------------------------------------------- */
function searchSubText( teststr, words ) {
   var i, ret, nph, pl;
   ret = 0;   

/* Search for the exact phrase. */
   ret = searchText( teststr, words );

/* Set the next phrase length. */
   pl = words.length - 1;

/* Set the number of phrases with this phrase length. */
   nph = 2;

/* Loop round searching for shorter sub-phrases each time. */
   while( pl > 0 ) {

/* Search for sub-phrases of the current length. */
      for( i = 0; i < nph; i++ ) {
         newwords = words.slice( i, i + pl );
         ret += searchText( teststr, newwords );
     }

/* Shorten the current sub-phrase length, and increase the number of
   sub-phrases to check. */
      pl += -1;
      nph++;

   }
  return ret;
}

/* Returns a string describing a criterion. 
   --------------------------- */
function discrit( crit, ind ){
   var last, nind, i, ret = "{ phrase: ";
   
   if( crit.phrase != null ) {
      ret += "\""+crit.phrase+"\",\n";
   } else {
      ret += "null,\n";
   }
   
   ret += ind + "  flags: ";
   if( crit.flags != null ) {
      ret += crit.flags+",\n";
   } else {
      ret += "null,\n";
   }
   
   ret += ind + "  tests: ";
   if( crit.tests != null ) {
      ret += "[ ";
      last = crit.tests.length - 1;
      for( i = 0; i <= last; i++ ) {
         if( i > 0 ) ret += ind + "           ";;
         ret += "{ re: ";
         if( crit.tests[i].re != null ) {
            ret += crit.tests[i].re+",\n";
         } else {
            ret += "null,\n";
         }
         
         ret += ind+"              crit: ";
         if( crit.tests[i].crit != null ) {
            nind = ind + "                    ";
            ret += discrit( crit.tests[i].crit, nind );
         } else {
            ret += "null";
         }
         if( i != last ){
            ret += " },\n ";
         } else {
            ret += " } ";
         }
     }
      
      ret += "] }";
   } else {
      ret += "null }";
   }
  
   return ret;
}


function deepCopy( a ){
   var ret = new Object();
   for( n in a ) {
      if( typeof a[n] == "object" ){
         ret[n] = deepCopy( a[n] );
      } else {
         ret[n] = a[n];
      }
   }
   return ret;
}

function sortResults( alpha ){
   if( alpha ) {
      matchedNames.sort();
   } else {
      matchedNames.sort(function(a,b) {
                    if( matchedWeights[a] > matchedWeights[b] ) {
                       return -1;
                    } else if( matchedWeights[a] < matchedWeights[b] ) {
                       return 1;
                    } else if( a < b ) {
                       return -1;
                    } else if( a > b ) {
                       return 1;
                    } else {
                       return 0;
                    }                       
                 });
   }
}

/* Returns the value of the checked button in a group of radio 
   buttons. 
   ----------------------------------------------------------- */
function radioCheck( form, name ) {
   var i, el = form[name];
   for( i = 0; i < el.length; i++ ) {
      if( el[i].checked ) {  
         return el[i].value;   
      }
   }
}

/* Returns an array holding the values of the checked buttons in a 
   group of check buttons. 
   ---------------------------------------------------------------*/
function checkCheck( form, name ) {
   var i, ret = new Array(), el = form[name];
   for( i = 0; i < el.length; i++ ) {
      if( el[i].checked ) {  
         ret.push( el[i].value );   
      }
   }
   return ret;
}

/* Display the results. 
   -------------------- */
function displayResults(){
   var doc, i, name;

   doc = parent.RESULTS.document;

   if( matchedNames.length == 0 ) {
      doc.writeln( '&nbsp;<p><b><center>No routines are currently selected.</center></b><p>' );
   } else {
      doc.writeln( '&nbsp;<p><b><center>Currently selected routines:</center></b><p>' );
      doc.writeln( '<table cellpadding="1" cellspacing="14" >' );

      for( i = 0; i < matchedNames.length; i++ ){
         name = matchedNames[i];
         doc.writeln( '<tr><td align="left" valign="top">' );
         doc.writeln( '<a href="'+routines[name].URL+'" target=MAIN>'+name+':</A></TD>' );
         doc.writeln( '<td align="left" valign="top">' );
         doc.writeln( routines[name].purpose );
   
         doc.write( '(&nbsp;<a href="javascript:void 1;" onMouseOver="status=\'The relevance of the routine.\';return true;" ' );
         doc.writeln( 'onMouseOut="status=\'\';return true;">'+matchedWeights[name]+'</a>&nbsp;)' );
   
         doc.write( '(&nbsp;<a href="javascript:parent.frames[0].removeName(\''+name+'\');" ' );
         doc.write( 'onMouseOver="status=\'Click to remove this routine from the results list.\';return true;" ' );
         doc.writeln( 'onMouseOut="status=\'\';return true;"><em>remove</em></a>&nbsp;)' );
   
         doc.writeln( '</td></tr>' );
      }
      doc.writeln( '</table>' );
   }

   doc.close();
}


function removeName(name) {

/* Search for the supplied name in the list of matching names. */
   for( i = 0; i < matchedNames.length; i++ ) {
      if( matchedNames[i] == name ) {

/* Remove the supplied name and its weight from the lists of matched
   names and weights, and add its name to the list of unmatched names. */
         matchedNames.splice( i, 1 );
         delete matchedWeights[name];
         unmatchedNames.push( name );
         break;
      }
   }

/* Display the new lists. */
   displayResults();
}
