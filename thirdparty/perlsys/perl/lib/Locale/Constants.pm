#
# Locale::Constants - defined constants for identifying codesets
#
# $Id$
#

package Locale::Constants;
use strict;

require Exporter;

#-----------------------------------------------------------------------
#	Public Global Variables
#-----------------------------------------------------------------------
use vars qw($VERSION @ISA @EXPORT);
$VERSION   = sprintf("%d.%02d", q$Revision$ =~ /(\d+)\.(\d+)/);
@ISA	= qw(Exporter);
@EXPORT = qw(LOCALE_CODE_ALPHA_2 LOCALE_CODE_ALPHA_3 LOCALE_CODE_NUMERIC
		LOCALE_CODE_DEFAULT);

#-----------------------------------------------------------------------
#	Constants
#-----------------------------------------------------------------------
use constant LOCALE_CODE_ALPHA_2 => 1;
use constant LOCALE_CODE_ALPHA_3 => 2;
use constant LOCALE_CODE_NUMERIC => 3;

use constant LOCALE_CODE_DEFAULT => LOCALE_CODE_ALPHA_2;

1;

