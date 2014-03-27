# Import smtplib for the actual sending function
from __future__ import division, absolute_import, print_function
import smtplib
import email

me =  'cole@ccdc.cam.ac.uk'
you = 'cole@ccdc.cam.ac.uk'

msg = email.message_from_string("Testing a mail")

msg['Subject'] = 'Test'
msg['From'] = me
msg['To'] = you

# Send the message via our own SMTP server, but don't include the
# envelope header.
s = smtplib.SMTP('jeeves.ccdc.cam.ac.uk')

s.sendmail(me, [you], msg.as_string())
s.quit()