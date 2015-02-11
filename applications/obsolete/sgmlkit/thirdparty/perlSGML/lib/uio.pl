##---------------------------------------------------------------------------##
##  File:
##	@(#) uio.pl 1.2 97/03/11 12:29:46 @(#)
##  Author:
##	Earl Hood	ehood@medusa.acs.uci.edu
##  Description:
##	This package contains subroutines for user input routines and
##	screen output routines.
##---------------------------------------------------------------------------##
##  Copyright (C) 1994-1997	Earl Hood, ehood@medusa.acs.uci.edu
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
##---------------------------------------------------------------------------##

if (!$uio'NoDate) {
    # date.pl is needed for get_later_date()
    require 'date.pl' || warn "Unable to require date.pl\n";
}

package uio;

##---------------------------------------------------------------------------##
##  The following routines are defined in this package.  All routines
##  work off of STDOUT.
##	sub main'ask_question		-- ask yes/no question
##	sub main'do_num_menu		-- autonumber menu items & print
##	sub main'flush			-- flush filehandle
##	sub main'get_char_choice	-- get char that is in list
##	sub main'get_later_date		-- Get date later than some date
##	sub main'get_lc_string		-- get string converted to lowercase
##	sub main'get_string		-- get string
##	sub main'pause			-- pause until <CR> hit
##	sub main'print_error		-- print error message
##	sub main'print_info		-- print information message
##	sub main'print_menu		-- print a menu to screen
##	sub main'print_note		-- print note message
##	sub main'print_warning		-- print warning message
##	sub main'prompt_user		-- prompt user for string
##	sub main'prompt_user_def	-- prompt_user() with default value
##
##  Private routines.
##	sub handler			-- private SIGCONT handler
##	sub print_message 		-- print typed message to screen
##	sub print_txt			-- print text and cache it
##	sub reset_handler		-- unregister handler()
##	sub set_handler			-- register handler()
##
##  Notes:
##	For the pause() and prompt_user*() routines, a signal handler
##	is registered to handle the SIGCONT signal.  This enables the
##	routines to redisplay the prompt after the program has been
##	suspended and continued while prompting the user for input.
##	The handler has to redisplay the text, therefore the mentioned
##	routines use the print_txt() for caching the prompt text.
##
##	When the signal handler is registered, any existing handler for
##	SIGCONT is stored and is called first before redisplaying the
##	cached text.  Uio's signal handler can be disabled if the
##	variable $uio'use_handler is set to zero.  By default, the
##	value is one.  More inforamtion is below in the various
##	subroutine descriptions.
##---------------------------------------------------------------------------##

			    ##------------------##
			    ## Global variables ##
			    ##------------------##
##---------------------------------------------------------------------------

@LastText = ();		## Cached text
$use_handler = 1;	## Flag to use private CONT signal handler
$term_io_bug = 0;       ## Flag if bogus reads must be done when
                        ##      consective suspend/foregrounds are done
$ext_sigcont;		## External CONT handler

##---------------------------------------------------------------------------

		    ##---------------------------------##
		    ## Private subroutine declerations ##
		    ##---------------------------------##
##---------------------------------------------------------------------------
##	print_txt() prints @_ to STDOUT.  It also makes a copy of @_ to
##	an array called @LastText.  Any outside package can access it
##	using uio'LastText.  This is useful for redisplaying text
##	after the program has been suspended and continued.
##
sub print_txt {
    print STDOUT @_;  @LastText = @_;
}

##---------------------------------------------------------------------------
##	This routine is used by print_{error, info, note, warning}
##	routines to output an informitive message to STDOUT.
##
sub print_message {
    local($type, $mesg) = ($_[0], join("", @_[1 .. $#_]));
    local($head) = sprintf("    %9s", "$type  ");
    local($indent) = " " x 13;
    local($i, $dashes, @array);

    @array = split(/\n/, $mesg);
    foreach (@array) { $i = length($_)  if length($_) > $i; }
    $dashes = " " x 4 . "-" x ($i+10);
    $mesg = join("\n", @array);
    $* = 1;
    $mesg =~ s/\n/\n$indent/g;
    $mesg = $head . $mesg;
    print STDOUT "$dashes\n", "$mesg\n", "$dashes\n";
    $* = 0;
}

##---------------------------------------------------------------------------
##	handler() is the private signal handler for the uio package.
##	It's used to when SIGCONT is caught for redisplay cached text.
##	The external handler is called if defined.
##
sub handler {
    if ($ext_sigcont ne 'IGNORE' && $ext_sigcont ne 'DEFAULT' &&
	$ext_sigcont ne "") { &$ext_sigcont(@_); }
    print STDOUT @LastText;
    &'flush(STDOUT);
    push(@Redo, 1)  if $term_io_bug;
}

##---------------------------------------------------------------------------
##	set_handler() sets the private CONT signal handler.
##
sub set_handler {
    return unless $use_handler;
    $ext_sigcont = $SIG{'CONT'};
    $SIG{'CONT'} = "uio'handler";
}

##---------------------------------------------------------------------------
##	reset_handler() restores the external handler
##
sub reset_handler {
    return unless $use_handler;
    $SIG{'CONT'} = $ext_sigcont;
}

##---------------------------------------------------------------------------

		    ##--------------------------------##
		    ## Public subroutine declerations ##
		    ##--------------------------------##
##---------------------------------------------------------------------------
##	print_menu() a tabular list of items in a "menu" type format.  The
##	list is printed in column major order.
##	Parameters:
##	    $title	: The title of the menu if not null.
##	    $cols	: Number of columns in menu.
##	    $box	: Boolean flag for drawing a box around menu.
##	    $sep	: Separator string between columns.
##	    @menu_items	: Array of menu items.
##	In box mode the title appears above the box.  Care must be taken
##	in the number of columns chosen when menu items string lengths get
##	long.  This routine assumes there is plenty of screen space to
##	print out the menu.
##
sub main'print_menu {
    local($title, $cols, $box, $sep, @menu_items) = @_;
    local($i, $j, $indent, $sp, $fmt, $lside, $rside);

    $sp = " ";  $indent = $sp x 8;
    foreach (@menu_items) { $i = length($_)  if length($_) > $i; }
    $fmt = "%-${i}s";
    $dashes = "-" x (($cols-1)*length($sep) + $cols*$i + 2);
    $lines = ($#menu_items+1)/$cols;
    $lines = ($lines == int($lines) ? $lines : int($lines)+1);
    print STDOUT "$indent$title\n"  if $title ne "";	## Print $title
    print STDOUT "$indent $dashes\n"  if $box;		## Print top of box
    $lside = ($box ? "| " : "  ");
    $rside = ($box ? " |\n" : "  \n");
    for ($i=0; $i < $lines; $i++) {
	print STDOUT $indent, $lside;
	for ($j=0; $j < $cols; $j++) {
	    print STDOUT sprintf($fmt, $menu_items[$i + $j*$lines]);
	    print STDOUT $sep  if $j < $cols-1;
	}
	print STDOUT $rside;
    }
    print STDOUT "$indent $dashes\n" if $box;
}
##---------------------------------------------------------------------------
##	do_num_menu() automatically generates a numbered list of items
##	for selection.  The routine returns the string item that is
##	selected.  This routine also appends a "Other" option and
##	if selected, will prompt for a user defined string.
##
sub main'do_num_menu {
    local($title, $prompt, $isother, $otherprompt, $otherdef, $cols,
	  @array) = @_;
    local(@menu_items) = @array;
    local($i, $tmp);

    ## Prepend numbers to items ##
    for ($i=0; $i <= $#menu_items; $i++) {
        $menu_items[$i] = $i+1 . " = \"$menu_items[$i]\"";
    }
    ## Add other selection if needed ##
    push(@menu_items, $i+1 . " = Other")  if $isother;
    ## Print menu ##
    print STDOUT "\n$title:\n\n";
    &'print_menu("", $cols, 0, " " x 5, @menu_items);
    ## Get selection ##
    while (1) {
	$tmp = &'prompt_user_def("\n$prompt", "1", 0, 0);
	last  if ($tmp >= 1 && $tmp <= $#menu_items+1);
	&'print_error("Invalid selection -- $tmp");
    }
    ## Determine selection ##
    if ($isother && $tmp == $#menu_items+1) {
        $tmp = &'prompt_user_def("  Other: $otherprompt", $otherdef, 1, 0);
    }
    else { $tmp = $array[$tmp-1]; }
    $tmp;
}
##---------------------------------------------------------------------------
## 	print_*() routines to print a message of type '*' to the screen.
##
sub main'print_error { &print_message("ERROR", @_); }
sub main'print_info { &print_message("Info:", @_); }
sub main'print_note { &print_message("Note:", @_); }
sub main'print_warning { &print_message("WARNING", @_); }
##---------------------------------------------------------------------------
##	Get string from STDIN and lowercase it.
##	called "get_lc_string".
##
sub main'get_lc_string {
    local($tmp);
    $tmp = <STDIN>;
    if ($term_io_bug) { while (shift(@Redo)) { $tmp = <STDIN>; } }
    chop $tmp;
    $tmp =~ tr/A-Z/a-z/;
    $tmp;
}
##---------------------------------------------------------------------------
##	Get string from STDIN.
##
sub main'get_string {
    local($tmp);
    $tmp = <STDIN>;
    if ($term_io_bug) { while (shift(@Redo)) { $tmp = <STDIN>; } }
    chop $tmp;
    $tmp;
}
##---------------------------------------------------------------------------
##	Pause output until CR is hit.
##
sub main'pause {
    &set_handler();
    local($tmp);
    &print_txt("\nHit <CR> to continue ... ");
    $tmp = <STDIN>;
    if ($term_io_bug) { while (shift(@Redo)) { $tmp = <STDIN>; } }
    &reset_handler();
}
##---------------------------------------------------------------------------
##	Prompt user for a string of input.  This routine appends a "->"
##	to the prompt string.  All leading and trailing whitespaces are
##	removed from the input string before returned.
##
sub main'prompt_user {
    &set_handler();
    local($prompt, $nonnull, $return) = @_;
    local($tmp);
    $prompt .= "\n" if $return;
    $prompt .= " -> ";
    while (1) {
	&print_txt($prompt);
	$tmp = &'get_string();
	&'print_error("Entry cannot be blank."), next
	    if $nonnull && $tmp =~ /^\s*$/;
	$tmp =~ s/^\s*(.*[^\s])\s*$/\1/;  ## strip beginning/trailing spaces
	last;
    }
    &reset_handler();
    $tmp;
}
##---------------------------------------------------------------------------
##	Like prompt_user() but a default choice is given.
##
sub main'prompt_user_def {
    &set_handler();
    local($prompt, $default, $nonnull, $return) = @_;
    local($tmp);
    $prompt .= " (\"$default\")";
    $prompt .= "\n" if $return;
    $prompt .= " -> ";
    while(1) {
	&print_txt($prompt);
	$tmp = &'get_string();
	$tmp = $default  if $tmp =~ m/^\s*$/;
	&'print_error("Entry cannot be blank."), next
	    if $nonnull && $tmp =~ /^\s*$/;
	$tmp =~ s/^\s*(.*[^\s])\s*$/\1/;
	last;
    }
    &reset_handler();
    $tmp;
}
##---------------------------------------------------------------------------
##	ask_question() asks a yes/no question.  $default signifies the
##	default response: 1 = yes, 0 = no.  The default response and
##	a '?' are appended to the prompt.
##
sub main'ask_question {
    &set_handler();
    local($prompt, $default) = @_;
    local($tmp, $ret);
    if ($default) { $prompt .= " (\"y\")"; }
    else { $prompt .= " (\"n\")"; };
    while(1) {
	&print_txt("$prompt?  ");
	$tmp = &'get_lc_string();
	$ret = 1, last  if ($tmp =~ /^\s*$/ && $default);
	$ret = 0, last  if ($tmp =~ /^\s*$/ && !$default);
	$ret = 1, last  if ($tmp =~ /^\s*y\s*$/ || $tmp =~ /^\s*yes*\s*$/);
	$ret = 0, last  if ($tmp =~ /^\s*n\s*$/ || $tmp =~ /^\s*no*\s*$/);
    }
    &reset_handler();
    $ret;
}
##---------------------------------------------------------------------------
##	get_char_choice() gets a single character response.  Valid
##	character responses are specified by @chars.  A default can
##	be specified by $default, and it is the choice if CR is hit.
##	All whitespaces (except \r and \n) and non-specified chars
##	are ignored.  This function returns with choice when a valid
##	character is hit.  Character choices are case-insensitive.
##
##	The terminal is put into raw mode for get_char_choice() to
##	perform its task.  Because of this, SIGINT, SIGQUIT, and
##	SIGTERM are temporarily ignored until a valid character is
##	hit.  The original signal handlers are restored before the
##	functions returns.
##
sub main'get_char_choice {
    local($default, @chars) = @_;
    local($in, $return);
    local($choices) = join(" ", @chars);
    $choices =~ tr/A-Z/a-z/;
    local($sigint, $sigquit, $sigterm, $sigtstp) =
	($SIG{'INT'}, $SIG{'QUIT'}, $SIG{'TERM'}, $SIG{'TSTP'});

    &'flush(STDOUT);
    $SIG{'INT'} = 'IGNORE'; $SIG{'QUIT'} = 'IGNORE'; $SIG{'TERM'} = 'IGNORE';
    $SIG{'TSTP'} = 'IGNORE';
    system("stty raw -echo");	## Set terminal in raw mode
    while (1) {
        $in = getc(STDIN);
        $return = $default, last if $default ne "" &&
				    ($in eq "\r" || $in eq "\n");
	next if $in =~ /\s/;
        $in =~ tr/A-Z/a-z/;
	$in =~ s/([\[\]\(\)\.\^\{\}\$\*\?\+\\\|])/\\\1/g;
        $return = $in, last if $choices =~ /$in/;
    }
    system("stty -raw echo");	## Restore terminal
    print STDOUT "$return\n";	## Output selection
    $SIG{'INT'} = $sigint;	## Restore signal handler
    $SIG{'QUIT'} = $sigquit;	## Restore signal handler
    $SIG{'TERM'} = $sigterm;	## Restore signal handler
    $SIG{'TSTP'} = $sigtstp;	## Restore signal handler
    $return;
}
##---------------------------------------------------------------------------
##	flush() flushes the buffer of passed in filehandle.  Copied from
##	"flush.pl".
##
sub main'flush {
    local($old) = select(shift);
    $| = 1;
    print "";
    $| = 0;
    select($old);
}
##---------------------------------------------------------------------------
##	get_later_date() retrieves from STDIN a date later than the
##	julian date $ljdate.  $prompt is the user prompt and $default
##	is the default response.  This routine uses the julian date
##	routines defined in "date.pl".  "date.pl" must be required
##	somewhere in the Perl program for this routine to work.
##
sub main'get_later_date {
    local($prompt, $default, $ljdate) = @_;
    local($tmp, $i, $m, $d, $y, $wd);
    while (1) {
	$tmp = &'prompt_user_def($prompt, $default, 1, 0);
	if ($tmp !~ m!^(\d{1,2})[-/](\d{1,2})[-/](\d{2}|\d{4})$!) {
	    &'print_error("Incorrect date specification -- $tmp\n",
	    	"Valid date formats: 02-28-94, 4/1/94, 12-25-1994.");
	}
	else {
	    $i = &'jday($1, $2, ($3 > 1900 ? $3 : "19".$3));
	    last  if $i > $ljdate;
	    ($m, $d, $y, $wd) = &'jdate($ljdate);
	    &'print_error("Date must be later than $m/$d/$y.");
	}
    }
    $tmp;
}
##---------------------------------------------------------------------------
1;  ## end package
