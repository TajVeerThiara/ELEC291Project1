import matplotlib
matplotlib.use('TkAgg')
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.figure import Figure
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib import style
import sys, time, math
import serial
import tkinter as tk
from tkinter import ttk


from twilio.rest import TwilioRestClient
import os
import smtplib
import pyttsx
from email import encoders
from email.mime.base import MIMEBase
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText  # Added
from email.mime.image import MIMEImage

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


#Don't Start Graph when we open the program
chartLoad = False
onStart = True

#Dimensions of the graph
xsize = 300
ysize = 300

# Check the serial port
try:
    ser = serial.Serial( port= 'COM5', baudrate=115200, parity=serial.PARITY_NONE, stopbits=serial.STOPBITS_TWO, bytesize=serial.EIGHTBITS)
except:
    print("Error Opening Port")
    sys.exit()
    
#Controls Chart
def loadChart(run):
    global chartLoad, onStart

    if run == 'start':
        chartLoad = True
        onStart = False

    elif run == 'stop':
        chartLoad = False
        
#Data generation and update data functions
def data_gen():
    global chartLoad
    flag=0
    flag2=0
    t = data_gen.t
    while True:
        val = float(ser.readline())
        if chartLoad:
            t+=1
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
    global chartLoad
    # update the data
    if chartLoad == True:
        t,y = data
        if t>-1:
            xdata.append(t)
            ydata.append(y)
            if t>xsize: # Scroll to the left.
               ax.set_xlim(t-xsize, t)
               temp_text.set_position((t-xsize, ysize + 40)) #adjusts text
               time_text.set_position((t-xsize,0))
            line.set_data(xdata, ydata)
            temp_text.set_text("Current Temperature = " + str(y) + "°C")#updates text
            time_text.set_text("Time = " + str(t) + "s")
        return line,

data_gen.t = -1
fig = Figure()
ax = fig.add_subplot(111)
line, = ax.plot([], [], color = 'green', lw=2)

ax.set_ylim(0, ysize)
ax.set_xlim(0, xsize)
ax.set_title('Reflow Soldering Profile')
ax.set_xlabel('Time (s)')
ax.set_ylabel('Temperature (°C)')
temp_text = ax.text(0, ysize + 40, '',fontsize=12)
time_text = ax.text(0,0, '', fontsize= 12)
#ax.grid()
xdata, ydata = [], []

#Tkinter Window            
class Application(tk.Tk):
    def __init__(self, *args, **kwargs):
        tk.Tk.__init__(self, *args, **kwargs)
        #Frame
        container = tk.Frame(self)
        container.pack(side= "top")
        
        container2 = tk.Frame(self)
        container2.pack(side= "top")

        tk.Label(container, text = "Status:",font = ("Helvetica", 15)).grid(row = 0, column = 0, sticky= "w")
        self.statusLabel = tk.Label(container, text ="Press 'Start Monitor' to begin", font = ("Helvetica",15), fg = 'blue')
        self.statusLabel.grid(row = 0, column = 1, sticky = "w")
   
        tk.Button(container2, text ="Start Monitoring",
                  command=lambda: loadChart('start')).pack(side = "left", padx = 5)
        tk.Button(container2, text ="Pause Monitoring",
                  command=lambda: loadChart('stop')).pack(side = "right", padx =5)
        
        #Puts the graph on the window
        self.show_graph()
        self.update_status()
        
    def show_graph(self):
        canvas = FigureCanvasTkAgg(fig, self)
        canvas.show()
        canvas.get_tk_widget().pack(side=tk.BOTTOM, fill=tk.BOTH, expand=True)
        
        toolbar = NavigationToolbar2TkAgg(canvas, self)
        toolbar.update()
        canvas._tkcanvas.pack(side=tk.TOP, fill=tk.BOTH, expand=True)
        
    def update_status(self):
        global chartLoard, onStart 
        if chartLoad:
            self.statusLabel.configure(text = "Monitoring Temperature", fg = 'green')
        elif chartLoad == False and onStart == False:
            self.statusLabel.configure(text = "Paused", fg = 'red')
        self.after(500, self.update_status)

                
def main():
    app = Application()
    app.title("Reflow Soldering Profile")
    #Doesnt work if interval = 1000
    ani = animation.FuncAnimation(fig, run, data_gen, blit = False,
                                  interval = 1000, repeat = False)
    app.mainloop()#Enters into a loop
    

if __name__ == '__main__':
    main()
