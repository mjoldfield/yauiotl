
import math
import time

def echoTo(file, payload):
    try:
        f = open(file, 'w')
        f.write(payload)
        f.close()
    except IOError:
        print "Failed to write %s to %s" % (payload, file)

def clampTo(min, max, val):
    if min != None and val < min:
        val = min
    if max != None and val > max:
        val = max

    return val


class led:
    def __init__(self, pin):
        echoTo('/sys/class/gpio/export', str(pin))
        time.sleep(0.1)

        self.pin     = pin
        self.pinPath = '/sys/class/gpio/gpio' + str(pin) + '/'
                        
        echoTo(self.pinPath + 'direction', 'out')

    def __del__(self):
        if (self.pin):
            echoTo('/sys/class/gpio/unexport', str(self.pin))
                
    def setState(self, state):
        echoTo(self.pinPath + 'value', str(state))

    def hello(self):
        state = 1
        for i in range(1,5):
            self.setState(state)
            state = 1 - state
            time.sleep(0.25)

# The code below should work given a functional PWM device but
# two problems seem to exist with the current implementation on
# the Raspberry Pi: all the permissions only allow root to write
# stuff; unexporting the device breaks it.
#
# For now, a reasonable workaround is to run things as non-root,
# hacking the permissions so that we can write to ...pwmchipM/pwmN,
# but not ...pwmchipM itself. This implies that someone else has
# to export the device for us.
class pwm:
    def __init__(self, pwmchip, pwm, period):
        self.pwmchip = pwmchip
        self.pwm     = pwm

        self.pwmchipPath  = '/sys/class/pwm/pwmchip%d/'       % (pwmchip)
        self.pwmPath      = '/sys/class/pwm/pwmchip%d/pwm%d/' % (pwmchip, pwm)

        echoTo(self.pwmchipPath + 'export', str(pwm))
        time.sleep(0.1)

        self.tick = 1e-9 # in ns from spec in device tree

        self.setPeriod(period)
        self.setDutyCycle(0)
        self.setEnable(1)

    def __del__(self):
        if hasattr(self, 'pwm'):
            self.setDutyCycle(0)
            self.setEnable(0)
            echoTo(self.pwmchipPath + 'unexport', str(self.pwm))

    def setPeriod(self, t):
        count = clampTo(0, None, int(t / self.tick))
        self.period = count
        echoTo(self.pwmPath + 'period',     str(self.period))
            
    def setDutyCycle(self, x):
        on_time = clampTo(0, self.period, int(self.period * x))
        echoTo(self.pwmPath + 'duty_cycle', str(on_time))

    def setEnable(self, x):
        echoTo(self.pwmPath + 'enable',    str(x))

    def hello(self):
        ncycles  = 2.0
        nsteps   = 1000
        duration = 1.5 # in seconds
        for i in xrange(0,nsteps):
            theta = ncycles * math.pi * i / nsteps 
            x = math.fabs(math.sin(theta))
            self.setDutyCycle(x)
            time.sleep(duration / nsteps)
    

