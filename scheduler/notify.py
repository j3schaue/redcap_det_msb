#!/usr/bin/python

import smtplib
import time

time.sleep(300)

sender = ''
receivers = ['']

message = """From: STATS LEAD <email@domain.edu>

Subject: Subject of email

Body of email
"""

try:
   smtpObj = smtplib.SMTP('hostsmtp.northwestern.edu', 25)
   smtpObj.sendmail(sender, receivers, message)         
   print("Successfully sent email")
except SMTPException:
   print("Error: unable to send email")
