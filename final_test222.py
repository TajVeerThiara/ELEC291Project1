import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math
import time
import serial
from matplotlib import style
from twilio.rest import TwilioRestClient
import pyttsx
import smtplib

style.use("ggplot")
engine = pyttsx.init()
volume = engine.getProperty('volume')
engine.setProperty('volume', volume+45.0)
voices = engine.getProperty('voices')
for voice in voices:
   engine.setProperty('voice', voice.id)
   
# configure the serial port
ser = serial.Serial(
 port='COM3',
 baudrate=115200,
 parity=serial.PARITY_NONE,
 stopbits=serial.STOPBITS_TWO,
 bytesize=serial.EIGHTBITS
)
ser.isOpen()
server = smtplib.SMTP('smtp.gmail.com', 587)
engine.say('Please enter the password')
engine.runAndWait()

server.ehlo()
server.starttls()
server.ehlo()

#Next, log in to the server
server.login("tajveer.thiara", "sherlock721")



xsize=500

def data_gen():
    flag=0
    flag2=0
    flag3=0
    t = data_gen.t
    while True:
       t+=1 
       strin = ser.readline()
       val=float(strin)
       if val>30 and flag2==0:
           from twilio.rest import TwilioRestClient
           client = TwilioRestClient("AC9a955ac6f078b3f0cd056cb0eee7f6e8", "e28e35a52a8ae098c4d566911f0e6222")
           client.messages.create(to="+16047169633", from_="+17786538398", 
                       body="Reflow process started!")


           engine.say('The process has started!')
           engine.runAndWait()
           flag2=1
       elif val>135 and flag3==0:
           engine.say('Entering Preheat Stage')
           engine.runAndWait()
           flag3=1
       elif val>207 and flag==0:
            from twilio.rest import TwilioRestClient
            client = TwilioRestClient("AC9a955ac6f078b3f0cd056cb0eee7f6e8", "e28e35a52a8ae098c4d566911f0e6222")
            client.messages.create(to="+16047169633", from_="+17786538398", 
                       body="Open the oven after 30s")
            engine.say('Entering reflow stage')
            engine.say('Open the oven door after thirty seconds to cool')
            engine.runAndWait()
            flag=1
            
       elif t==300:
        msg = "/nReflow Process Completed"
        fig.savefig("filename.png")
        server.sendmail("tajveer.thiara@gmail.com", "tajveer.thiara@gmail.com", msg)
       yield t, val

def run(data):
    # update the data
    t,y = data
    if t>-1:
        xdata.append(t)
        ydata.append(y)
        if t>xsize: # Scroll to the left.
            ax.set_xlim(t-xsize, t)
            temp_text.set_position( (t-xsize, 250 + 20))
            time_text.set_position( (t-xsize, 0))
        line.set_data(xdata, ydata)
        temp_text.set_text("Current Temperature = " + str(y) + "C")
        time_text.set_text("Time = " + str(t) + "s")
    return line,

def on_close_figure(event):
    sys.exit(0)

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], color = 'green', lw=2)
ax.set_ylim(0, 250)
ax.set_xlim(0, xsize)
ax.set_xlabel('Time(seconds)')
ax.set_ylabel('Temperature(C)')
ax.set_title('Reflow Soldering Profile')
temp_text = ax.text(0, 250 + 20,'', fontsize = 12)
time_text = ax.text(0,0,'', fontsize = 12)

ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines to appear when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=1000, repeat=False)
plt.show()
