import sys
sys.path.append('lib')

import awsiot

import pigpio

import evdev
from evdev import ecodes

from select import select

import time
import json
import argparse


maxBrightness   = 1000
deltaBrightness =  50

brightnessKey = "brightness"

led_pins = [ 9, 11, 10, 22 ]

def clampTo(min, max, val):
    if min != None and val < min:
        val = min
    if max != None and val > max:
        val = max

    return val

def getDevices():
    devices = {}
    for f in evdev.list_devices():
        d = evdev.InputDevice(f)

        tag  = None
        name = d.name.lower()
        
        if name == "soc:knob":
            tag = "knob"

        if name == "soc:keypad" or "keyboard" in name:
            tag = "button"    

        if tag:
            devices[d.fd] = { "tag":tag, "dev":d }

    return devices

def updateState(s, tag, e):
    brightness = s[brightnessKey]
    
    # handle rotation events
    if tag == "knob" and e.type == 2:
        brightness += e.value * deltaBrightness

    # handle button down events
    if tag == "button" and e.type == 1 and e.value == 1:

        # e.code tells us which button was pushed
        
        # custom button toggles state
        if e.code == 256:
            if brightness < maxBrightness * 0.05:
                brightness = maxBrightness
            else:
                brightness = 0

        if e.code == ecodes.KEY_UP:
            brightness += deltaBrightness        

        if e.code == ecodes.KEY_DOWN:
            brightness -= deltaBrightness        
            
        if e.code == ecodes.KEY_LEFT:
            brightness = 0

        if e.code == ecodes.KEY_RIGHT:
            brightness = maxBrightness

    brightness = clampTo(0, maxBrightness, brightness)
            
    if brightness != s[brightnessKey]:
        s[brightnessKey] = brightness

def setBrightness(p, bot, s):
    if brightnessKey in s:
        b = s.get(brightnessKey, 0)

        f = b / (maxBrightness + 0.0)

        for pin in led_pins:
            p.set_PWM_dutycycle(pin, 255 * f)
        
        awsiot.reportState(bot, s)

def connectToAWS(config, r):
    aiot = awsiot.shadowClient(config)

    connected = aiot.connect()
    if not connected:
        return None
        
    bot = aiot.createShadow('toy-light')
    if not bot:
        return None

    updateCB = lambda s: r.update(s)

    # track remote changes but don't report them as processed
    cbs = awsiot.updateCallbacks(bot, updateCB, None)
    cbs.enable()
    
    return bot
        
# main starts here..

parser = argparse.ArgumentParser(description = 'AWS IoT light demo.')
parser.add_argument('config', help = 'directory containing configuration files')
parser.add_argument('--hasLED', dest='hasLED', action='store_true')

args = parser.parse_args()

devices = getDevices()

bot = None

localState    = { brightnessKey: 0 }
remoteState   = {  }
sentState     = {  }

idleTime = 0.05
remote_lockout_period = 1
update_lockout_period = 0.05

gpio = None
if args.hasLED:
  gpio = pigpio.pi()

remote_lockout_expire = 0
update_lockout_expire = 0

while True:
    if bot == None:
        bot = connectToAWS(args.config, remoteState)
    
    r, w, x = select(devices, [], [], idleTime)

    oldState = localState.copy()
    for fd in r:
        d = devices[fd]
        for e in d["dev"].read():
            updateState(localState, d["tag"], e)

    t = time.time()

    if localState != oldState:
        remote_lockout_expire = t + remote_lockout_period
        
    if t > update_lockout_expire and sentState != localState:
        awsiot.pushState(bot, localState)
        sentState = localState.copy()
        update_lockout_expire = t + update_lockout_period

    if t > remote_lockout_expire:
        localState.update(remoteState)
        sentState = localState.copy()

    if gpio and bot:
        setBrightness(gpio, bot, localState)
