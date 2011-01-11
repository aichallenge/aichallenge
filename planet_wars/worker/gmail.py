import smtplib
from server_info import server_info

# Uses Gmail to send an email message. This function also work with Google Apps
# (Gmail for your domain) accounts.
#   username: the full email address of the sender (ex: sunflower@gmail.com).
#   password: the gmail password for the sender.
#   recipients: a list of addresses to send the message to. Can also be a
#               single email address.
#   subject: the subject line of the email
#   body: the actual content of the email
#   full_name: the full name of the sender. If this is omitted or has length
#              zero, then the name of the sender will just be the sender's
#              email address.
def send_gmail(username, password, recipients, subject, body, full_name):
  if isinstance(recipients, list):
    recipients = [r for r in recipients if r.find("@") >= 0]
  print recipients
  try:
    if full_name is not None and len(full_name) > 0:
      from_line = full_name + " <" + username + ">"
    else:
      from_line = username  
    message = "From: " + from_line + "\n" + \
      "Subject: " + subject + "\n" + \
      "\n" + body
    smtp_server = smtplib.SMTP("smtp.gmail.com", 587)
    smtp_server.ehlo()
    smtp_server.starttls()
    smtp_server.ehlo()
    smtp_server.login(username, password)
    smtp_server.sendmail(username, recipients, message)
    smtp_server.quit()
    return True
  except Exception, inst:
    return False

# Sends an email message using the email account specified in the
# server_info.txt file. If the message is successfully sent, returns True.
# Otherwise, returns False.
def send(recipients, subject, body):
  mail_username = server_info["mail_username"]
  mail_password = server_info["mail_password"]
  mail_name = server_info["mail_name"]
  return send_gmail(mail_username, \
                    mail_password, \
                    recipients, \
                    subject, \
                    body, \
                    mail_name)

def main():
  print "result: " + \
    str(send("youraddress@yourdomain.com", "Test mail message", "Test!"))

if __name__ == "__main__":
  main()
