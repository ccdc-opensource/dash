"""
Read DASH spreadsheet and filter out superfluous data
"""
import string
import urllib
import urllib2
import base64
import sys
import email
import smtplib
import time

# You'll need to have this installed in your python from http://pypi.python.org/pypi/xlrd
import xlrd
# You'll need to have this installed in your python from http://pypi.python.org/pypi/xlwt
import xlwt
# You'll need to have this installed in your python from http://pypi.python.org/pypi/xlutils
import xlutils

# Change this to the xls file that contains the customer data
spreadsheet_name = "example.xls"

##
test_email_account = 'cole@ccdc.cam.ac.uk'
# Changing the above line to this will mean you get e-mail sent to the accounts as specified in the spreadsheet
# test_email_account = None

# These should be fine
smtp_server = 'jeeves.ccdc.cam.ac.uk'
generation_url =  "http://intranet.ccdc.cam.ac.uk/~bardwell/web_licence_database/dash/dash_lic/validate.php"
license_text_url = "http://intranet.ccdc.cam.ac.uk/~bardwell/web_licence_database/dash/dash_lic/licence_file.txt"

def get_email(org):
    if test_email_account != None:
        return  test_email_account
    else:
        return org.email


def generate_licences(organisations):
    generate_licences_implementation(organisations)
    return


###########################################################################################
import types
def cell_to_string(value):
        if type(value) == types.FloatType:
            return str(value)
        else:
            return value.encode('utf8')
        

class LicenseDatum:
    def __init__(self, sheet, row):
        if sheet != None and row != None:
            v = sheet.cell(row,6).value
            if type(v) == types.FloatType:
                self.host_id = str(int(v))
            else:
                self.host_id = v.encode('utf8')
        
            self.version = cell_to_string(sheet.cell(row,7).value)
            self.expiry  = cell_to_string(sheet.cell(row,8).value)
        else:
            self.host_id = "ABCDABCD"
            self.version = "3.2"
            self.expiry = "30/9/2011"
        
        self.key     = "Unassigned"
        
    def __repr__(self):
        return "\"%s\",\"%s\",\"%s\",\"%s\"" % ( self.host_id, self.version, self.expiry, self.key)

         
class Organisation:
    def __init__(self, sheet = None, row = None):
        if sheet != None and row != None:
            self.code       = str(sheet.cell(row,0).value)
            self.agreement_number       = sheet.cell(row,1).value.encode('utf8')
            self.country                = sheet.cell(row,2).value.encode('utf8')
            self.organisation           = sheet.cell(row,3).value.encode('utf8')
            self.type                   = sheet.cell(row,4).value.encode('utf8')
            self.nlic                   = sheet.cell(row,5).value
            self.licence_data           = [ LicenseDatum(sheet,row) ]
            self.site_contact_title     = sheet.cell(row,9).value.encode('utf8')
            self.site_contact_firstname = sheet.cell(row,10).value.encode('utf8')
            self.site_contact_surname   = sheet.cell(row,11).value.encode('utf8')
            self.email                  = sheet.cell(row,12).value.encode('utf8')
        else:
            self.code       = "1111"
            self.agreement_number       = "D/0000/0000"
            self.country                = "Outer Mongolia"
            self.organisation           = "Mickey Mouse University"
            self.type                   = "Academic"
            self.nlic                   = 10
            self.licence_data           = [ LicenseDatum(sheet,row) ]
            self.site_contact_title     = "Dr"
            self.site_contact_firstname = "Bob"
            self.site_contact_surname   = "Monkey"
            self.email                  = test_email_account
        
    def add_row(self, sheet,row):
        self.licence_data.append(LicenseDatum(sheet,row))

    def prune_to_best_licences(self):
        # cut down to only licences from last year
        # also - remove licenses containing the text (old)

        current = self.licence_data
        most_recent = []
        self.licence_data = []
        for l in current:
             if l.expiry == "30/9/2011" and string.find(l.host_id,"(old)") == -1:
                 most_recent.append(l)
        self.licence_data = most_recent

    def __repr__(self):
        rep = ""
        for datum in self.licence_data:
            rep += "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",%s,\"%s\",\"%s\",\"%s\",\"%s\"\n" % \
            ( self.code,self.agreement_number,
              self.country,self.organisation,self.type,self.nlic,
              datum,self.site_contact_title,self.site_contact_firstname,
              self.site_contact_surname,self.email)
        return rep[:-1]

def read_worksheet(worksheet_name):
    wb = xlrd.open_workbook(worksheet_name)
    
    organisations = []

    header = None
    for s in wb.sheets(): # There should only be one sheet
        current_org = None
        for row in range(s.nrows):
            if row == 0:
                header = Organisation(s,row) # its the header, lets ignore
                pass
            
            sval = s.cell(row,1).value

            # New Organisation
            if len(sval) > 0:
                if current_org != None:
                    organisations.append(current_org)
                current_org = Organisation(s,row)
            else:
                current_org.add_row(s,row)

        if current_org != None:
            organisations.append(current_org)

    return organisations

def save_organisations(organisations, out):
    for org in organisations:
        if len(org.licence_data) > 0:
            out.write(str(org)+"\n")

def prune_organisations(organisations):
    new_orgs = []
    for org in organisations:
        org.prune_to_best_licences()
        if len(org.licence_data) > 0 and org.code != "0" and org.code != "1":
            new_orgs.append(org)
    return new_orgs

def create_dash_post_data(hostid,
                     version, 
                     institution,
                     department,
                     city,
                     country,
                     title, 
                     firstname,
                     surname,
                     email,
                     agreement_number):

    values = {}
    if len(hostid) < 8:
       values['licencetype'] = "site"
       values['nodenumber'] = ""
       values['sitenumber'] = str(hostid)
    else:
       values['licencetype'] = "node"
       values['nodenumber'] = str(hostid)
       values['sitenumber'] = ""

    values['expirydate'] = "absolute"
    values['expiryyear'] = 2012
    values['expirymonth'] = 9
    values['expiryday'] = 30
    values['relativeexpirydate'] = 60
    values['version'] = version
    values['confirm'] = 1
    values['personId'] = "auto"
    values['institution'] = institution
    values['newinstitution'] = institution
    values['department'] = department
    values['newdepartment'] = department
    values['city'] = city
    values['newcity'] = city
    values['country'] = country
    values['newcountry'] = country
    values['title'] = title
    values['firstName'] = firstname
    values['lastName'] = surname
    values['email'] = email
    
    values['initial'] = "LST"
    # If we change here, we can influence what we see in the licence db
    values['comment'] = "Agreement Number " + agreement_number + " Host " + str(hostid) + " Live Generation 2011"
    return values

def create_dash_licence(hostid,
                     version, 
                     institution,
                     department,
                     city,
                     country,
                     title, 
                     firstname,
                     surname,
                     email,
                     agreement_number):

    username = "dash_lic"
    password = "dashlic04"
    values = create_dash_post_data(hostid,
                     version, 
                     institution,
                     department,
                     city,
                     country,
                     title, 
                     firstname,
                     surname,
                     email,
                     agreement_number)

    data = urllib.urlencode(values)
    request = urllib2.Request(generation_url,data)
    
    base64string = base64.encodestring(
                '%s:%s' % (username, password))[:-1]
    authheader =  "Basic %s" % base64string
    
    request.add_header("Authorization", authheader)

    # This request fires off the license generator
    response = urllib2.urlopen(request)
    first_page_content = response.read()
    

        
    # Rather crude error checking!!
    if first_page_content.find("ERROR") != -1:
        sys.stderr.write( "Generation Failure for " + email + " " + str(version) + " " + str(hostid) + "\n")
        return None

    if first_page_content.find("Could not insert") != -1:
        sys.stderr.write( "Table insertion failure for " + email + " " + str(version) + " " + str(hostid) + "\n")
        return None

    if first_page_content.find("A record already exists for this email") != -1:
        sys.stderr.write( "Personal details mismatch for " + email + " " + str(version) + " " + str(hostid) + "\n")
        return None
    
    if first_page_content.find('Cannot open file "licence_file.txt"') != -1:
        sys.stderr.write( "Unable to create a license due to encoder failure " + email + " " + str(version) + " " + str(hostid) + "\n")
        return None

    if first_page_content.find("Here is the licence key:") == -1: # We expect successful generations to have this
        sys.stderr.write( "Generation Failure for " + email + " " + str(version) + " " + str(hostid) + "\n")
        sys.stderr.writelines(first_page_content)

        return None
        
    # This request reads back licence_file.txt from the web server which was created in the previous
    # request
    request = urllib2.Request(license_text_url)
    request.add_header("Authorization", authheader)
    try:
        response = urllib2.urlopen(request)
        return response.read()
    except urllib2.HTTPError: # This ususally means file not found, which means the first request failed
        sys.stderr.write("Other failure\n")
        sys.stderr.writelines(first_page_content)
        return None

def generate_licences_implementation(organisations):
    for org in organisations:
        for licence in org.licence_data:
            email_address = org.email
            licence_key = create_dash_licence(
                licence.host_id,
                licence.version,
                org.organisation,
                "Unspecified", # We dont know department so set to Unspecified
                "Unspecified", # The city is also unavailable from the spreadsheet
                org.country,
                org.site_contact_title,
                org.site_contact_firstname,
                org.site_contact_surname,
                email_address,
                org.agreement_number
            )
            
            if licence_key == None:
                sys.stderr.write("Failed for %s with host id %s" % (org.agreement_number,licence.host_id) + "\n")
            else:
                print "Success %s with host id %s : key returned - %s" % (org.agreement_number,licence.host_id, licence_key)
            licence.key = licence_key
            time.sleep(0.5)

            
def write_email_japan(org, out):
    
    import codecs
    f = codecs.open("japan_template_header.unic",encoding='utf-16')
    body_text = u""
    for l in f:
        body_text += l

    body_text += u"\n\nDear Customer (Organisation: %s, Agreement:  %s)" % (org.organisation,org.agreement_number)
    body_text += u"""
 
Attached key(s) are valid until 30 September 2013. 
These key(s) are for: DASH 3.0 - 3.2.   

For CSD users: These keys will not work for DASH 3.3 which is distributed with the CSD system.

"""
    for licence in org.licence_data:
        if licence.key != None:
            body_text += "     Key (for version %s) for " % (licence.version)

            if len(licence.host_id) == 4:
                body_text += "Site Id "
            else:
                body_text += "Host Id "

            body_text += "%s: %s \n" % (licence.host_id,licence.key)

    f = codecs.open("japan_template_footer.unic",encoding='utf-16')
    body_text += u"\n\n"
    for l in f:
        body_text += l

    me = 'admin@ccdc.cam.ac.uk'
    you = get_email(org)
    
    return create_email_draft(me,you,'DASH Licence Keys for the next calendar year',body_text)
    

def write_email_row(org, out):
    #draft = email.message_from_string("")
    title = "Dr"
    if len(org.site_contact_title) > 0:
        title = org.site_contact_title

    body_text = "Dear " + title + " " + org.site_contact_surname +",\n\n"
    body_text +="Here are your DASH license key(s) for 2013:\n\n"
    for licence in org.licence_data:
        if licence.key != None:
            body_text += "     DASH Key (for version %s) for " % (licence.version)

            if len(licence.host_id) == 4:
                body_text += "Site Id "
            else:
                body_text += "Host Id "

            body_text += "%s: %s \n" % (licence.host_id,licence.key)

    body_text += "\nThis licence key expires on 30 September 2013. Before the licence expires we will send a replacement\n"
    body_text += "\n\nBest Wishes\n\nLaura Petley\nCCDC"

    me = 'admin@ccdc.cam.ac.uk'
    you = get_email(org)
    
    #draft.set_unixfrom('admin')
    #draft['Subject'] = 'DASH Licence Keys for the next calendar year'
    #draft['From'] = me
    #draft['To'] = you
    #draft.set_payload(body_text)
    
    
    return create_email_draft(me,you,'DASH Licence Keys for the next calendar year',body_text)
    
def create_email_draft(sender, recipient, subject, body):
    from smtplib import SMTP
    from email.MIMEText import MIMEText
    from email.Header import Header
    from email.Utils import parseaddr, formataddr

    """create an email.

    All arguments should be Unicode strings (plain ASCII works as well).

    Only the real name part of sender and recipient addresses may contain
    non-ASCII characters.

    The email will be properly MIME encoded and delivered though SMTP to
    localhost port 25.  This is easy to change if you want something different.

    The charset of the email will be the first one out of US-ASCII, ISO-8859-1
    and UTF-8 that can represent all the characters occurring in the email.
    """

    # Header class is smart enough to try US-ASCII, then the charset we
    # provide, then fall back to UTF-8.
    header_charset = 'ISO-8859-1'

    # We must choose the body charset manually
    for body_charset in 'US-ASCII', 'ISO-8859-1', 'UTF-8':
        try:
            body.encode(body_charset)
        except UnicodeError:
            pass
        else:
            break

    # Split real name (which is optional) and email address parts
    sender_name, sender_addr = parseaddr(sender)
    recipient_name, recipient_addr = parseaddr(recipient)

    # We must always pass Unicode strings to Header, otherwise it will
    # use RFC 2047 encoding even on plain ASCII strings.
    sender_name = str(Header(unicode(sender_name), header_charset))
    recipient_name = str(Header(unicode(recipient_name), header_charset))

    # Make sure email addresses do not contain non-ASCII characters
    sender_addr = sender_addr.encode('ascii')
    recipient_addr = recipient_addr.encode('ascii')

    # Create the message ('plain' stands for Content-Type: text/plain)
    msg = MIMEText(body.encode(body_charset), 'plain', body_charset)
    msg['From'] = formataddr((sender_name, sender_addr))
    msg['To'] = formataddr((recipient_name, recipient_addr))
    msg['Subject'] = Header(unicode(subject), header_charset)
    return msg

def write_email( org, out, do_send = 1): 
    draft = None
    if org.country == "Japan":
        draft = write_email_japan(org, out)
    else:
        draft = write_email_row(org, out)

    out.write( draft.as_string() +"\n\n\n")
    
    if do_send == 1:
        you = get_email(org)
        me = 'admin@ccdc.cam.ac.uk'
        if len(you) > 0:
            # Now - lets send this via SMTP
            s = smtplib.SMTP(smtp_server)
            s.sendmail(me, [you], draft.as_string())
            s.quit()
        else:
            sys.stderr.write("Failed to email for %s - no email\n" % ( org.agreement_number ) )

def write_emails(organisations,out):
    for org in organisations:
        count = 0
        for licence in org.licence_data:
            if licence.key != None: count += 1

        if count > 0:
            write_email(org,out,1)

#def write_japanese_emails(organisations,out):
#    for org in organisations:
#        if org.country == "Japan":
#            count = 0
#            for licence in org.licence_data:
#                if licence.key != None: count += 1
#
#            if count > 0:
#                write_email(org,out,0)

#def write_japanese_spreadsheet(organisations,out):
#    for org in organisations:
#        if len(org.licence_data) > 0 and org.country == "Japan":
#            out.write(str(org) + "\n")

#def write_spreadsheet_for_hiromi(organisations,out):
#    
#    def site_line(org):
#        return 'Site licence key is: %s for site id %s' % ( org.licence_data[0].key, org.licence_data[0].host_id)
#        
#    def node_line(org):
#        ret = "Key for "
#        for lic in org.licence_data:
#            v = "%s is %s," % (lic.host_id,lic.key)  
#            ret = ret + v
#
#        # chop off last comma
#        return ret[:-1]
#        
#    out.write('"Agreement#",Org,Name,e-mail,"Site licence or ** installations","S/N and Keys"\n')
#    for org in organisations:
#        if len(org.licence_data) > 0 and org.country == "Japan":
#            if len(org.licence_data) > 1 or len(org.licence_data[0].host_id) > 5:
#                key_line = node_line(org)
#                key_info = str(int(org.nlic)) + " installations"
#            else:
#                key_line = site_line(org)
#                key_info = "unlimited"
#            
# 
#            name =  org.site_contact_title + ' ' + org.site_contact_firstname + ' ' + org.site_contact_surname
#
#            out.write('"%s","%s","%s","%s","%s","%s"\n' % ( org.agreement_number,org.organisation,name,org.email,key_info,key_line ) )
            

# Filter out the junk

#sys.stderr = open("Errors.txt","w")

#raw_organisations = read_worksheet(spreadsheet_name)

#organisations = prune_organisations(raw_organisations)

# Run the licence generation proces
# generation process for the pruned organisations

#generate_licences(organisations)

# Write out a csv containing the organisations who are going to get keys
#orgout = open("pruned_organisations.csv","wb")
#save_organisations(organisations,orgout)

# write out all the emails - currently to a text file and send via jeeves 

organisations = [ Organisation() ]

out = open("emails.txt","wb")
write_emails(organisations,out)

# Write out a CSV file with Japanese license keys
#out2 = open("japan.csv","wb")
#write_spreadsheet_for_hiromi(organisations,out2)

# out3 = open("japan_emails.txt","wb")
# write_japanese_emails(organisations,out3)
