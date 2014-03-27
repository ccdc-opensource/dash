from __future__ import division, print_function
import imaplib
import time
import email.message
import getpass

def open_connection(verbose=False):
    # Connect to the server
    print("at connection creation")
    connection = imaplib.IMAP4_SSL('mail01.ccdc.cam.ac.uk')
    # Login to our account
    print("login", getpass.getuser(), getpass.getpass())
    connection.login(getpass.getuser(), getpass.getpass())
    return connection

new_message = email.message.Message()
new_message.set_unixfrom('cole')
new_message['Subject'] = 'subject goes here'
new_message['From'] = 'cole@ccdc.cam.ac.uk'
new_message['To'] = 'cole@ccdc.cam.ac.uk'
new_message.set_payload('This is the body of the message.\n')

c = open_connection()
try:
    c.append('License Drafts', '', imaplib.Time2Internaldate(time.time()), str(new_message))
finally:
    try:
        c.close()
    except:
        pass
    c.logout()



