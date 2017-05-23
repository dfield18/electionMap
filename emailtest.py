import smtplib


server = smtplib.SMTP('smtp.gmail.com', 587)

server.ehlo()
server.starttls()
server.ehlo()

server.login("davidcmfield@gmail.com", "Eaglesspurs18")
msg = "\nHello!"
server.sendmail("davidcmfield@gmail.com", "davidcharlesfield@gmail.com", "test email")

print("done")
