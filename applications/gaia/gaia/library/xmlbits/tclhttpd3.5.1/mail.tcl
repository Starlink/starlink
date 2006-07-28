# mail.tcl
# mail support using tcllib's smtp and mime packages
#
# The /mail URL is registered as a direct url that maps to procedures
# in this file that begin with Mail.
#
# Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::mail 1.0

# No useful default, but we define procedures so the vanilla server
# can start up.

if {[info exists Config(mail)] && ($Config(mail) != {})} {
    set Mail(server) $Config(mail)
}

package require smtp
package require mime

# Mail_Send
#	Send email to recipients using tcllib's smtp client.
#
# Arguments:
#	recipients	list of email addresses to which to send the mail
#	subject	subject line for email
#	from	email address of email sender
#	type	mime type of body ("text/plain" for normal email)
#	body	the body of the email
#
# Results:
#	return the tcllib smtp client's return, {} if successful else:
#	A list indicating which recipients were unacceptable to the SMTP server.
#	Each element of the list is another list, containing the address,
#	an SMTP error code, and a textual diagnostic.
#	
# Side Effects:
#	Email is sent to each of the recipients

proc Mail_Send {recipients subject from type body} {
    global Mail

    set token [mime::initialize -canonical $type -string $body]
    mime::setheader $token Subject $subject

    set result [smtp::sendmessage $token \
		    -recipients $recipients \
		    -originator $from \
		    -servers $Mail(server)]
    mime::finalize $token
    return $result
}

# Mail_Result
#	convert tcllib smtp server result to a table suitable for display

proc Mail_Result {mail_err} {
    set html ""
    if {$result != {}} {
	set html <table>
	foreach el $result {
	    append html <tr>
	    foreach {addr errcode diagnostic} $el {
		append html <th> [protect_text $addr] </th>
		append html <td> [protect_text $errcode] </td>
		append html <td> [protect_text $diagnostic] </td>
	    }
	    append html </tr>
	}
	append html </table>
    }
    return $html
}

proc MailSend {recipients subject from type body} {
    set result [Mail_Send $recipients $subject $from $type $body]
    if {$result == ""} {
	set html "<b>Thank You!</b><br>Mailed report to <b>[protect_text $recipients]</b>"
    } else {
	return [Mail_Result $result]
    }
}

proc Mail_Url {dir} {
    Direct_Url $dir Mail
}

proc Mail/bugreport {email errorInfo args} {
    global Httpd
    set html "<pre>"
    foreach {name value} $args {
	if {([string compare $name "env"] == 0) && 
		([catch {array set X $value}] == 0)} {
	    # add this later
	    continue
	} else {
	    append html "$name: $value\n"
	}
    }
    append html  $Httpd(server)\n
    append html [protect_text $errorInfo]

    if {[info exist X]} {
	append html "\n\nEnvironment:\n"
	foreach n [lsort [array names X]] {
	    append html "  $n: $X($n)\n"
	}
    }
    append html "</pre>"
    if {$email == ""} {
	set email [Httpd_Webmaster]
    }
    MailSend $email "$Httpd(name):$Httpd(port) error" "" text/html $html
}

# If your form action is /mail/forminfo, then this procedure
# sends the results to the address specified by "sendto"
proc Mail/forminfo {sendto subject href label args} {
    set from ""
    foreach {name value} $args {
	# If the value has unbalanced braces, we will do base64
	# encoding to avoid a huge jumble of backslashes and
	# a long line that will not survive the email transport
	set blob [list $value]
	if {[regsub -all \\\\ $blob {} _] > 0} {
	    append message "[list Data64 $name] \\\n"
	    append message [list [base64::encode $value]]\n
	} else {
	    append message [list Data $name $value]\n
	}
	if {[string compare $name "email"] == 0} {
	    set from $value
	}
    }
    set html [MailSend $sendto $subject $from text/plain $message]
    if {[string length $href]} {
	if {[string length $label] == 0} {
	    set label Back
	}
	append html "<p><a href=\"$href\">$label</a>"
    }
    return $html
}

# This form is designed to be embedded into a page
# that handles form data.

proc Mail_FormInfo {} {
    global page
    set html {<!-- Mail_FormInfo -->}
    if {[info exist page(query)]} {
	array set q $page(query)
	if {[info exist q(sendto)] && [info exist q(subject)]} {
	    eval {Mail/forminfo $q(sendto) $q(subject) {} {}} $page(query)
	    set html {<!-- Mail_FormInfo sent email -->}
	}
    }
    return $html
}

# Older version of mail/forminfo
proc Mail/formdata {email subject args} {
    foreach {name value} $args {
	append message "$name: $value\n"
    }
    MailSend $email $subject {} text/plain $message
}
