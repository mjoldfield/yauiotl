'''
/*
 * Copyright 2010-2016 Amazon.com, Inc. or its affiliates. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License").
 * You may not use this file except in compliance with the License.
 * A copy of the License is located at
 *
 *  http://aws.amazon.com/apache2.0
 *
 * or in the "license" file accompanying this file. This file is distributed
 * on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
 * express or implied. See the License for the specific language governing
 * permissions and limitations under the License.
 */
 '''

from AWSIoTPythonSDK.MQTTLib import AWSIoTMQTTShadowClient

import logging
import glob
import sys
import json
import time
import platform
import os

def uniqglob(path):
    matches = glob.glob(path)

    if len(matches) == 1:
        return matches[0]

    sys.exit("Globbing for %s didn't get a unique match: %s" % (path, str(matches)))

class shadowClient:
    def __init__(self, certDir, logLevel=logging.WARNING, clientID=None):

        if clientID == None:
            clientID = platform.node() + '.' + str(os.getpid())
        
        self.clientID = clientID

    	self.client = AWSIoTMQTTShadowClient(clientID)

        self.configureLogging(logLevel)

        self.configureConnection(certDir)

    def configureConnection(self, certDir):

        configPath = uniqglob(certDir + '*config.json')
        with open(configPath) as json_data:
            endpoint = json.load(json_data)

            port = 8883

            if 'host' in endpoint:
                host = endpoint['host']
            else:
                sys.exit("host not defined in %s" % (configPath))

            if 'port' in endpoint:
                port = int(endpoint['port'])

            self.client.configureEndpoint(host, port)
                
        rootCAPath = uniqglob(certDir + '*rootCA*')
        prvKeyPath = uniqglob(certDir + '*.private.key*')
        certPath   = uniqglob(certDir + '*.cert.*')
        
        self.client.configureCredentials(rootCAPath, prvKeyPath, certPath)
    
        self.client.configureAutoReconnectBackoffTime(1, 32, 20)
        self.client.configureConnectDisconnectTimeout(10)  # 10 sec
        self.client.configureMQTTOperationTimeout(5)  # 5 sec

    def configureLogging(self, logLevel):
        logger = logging.getLogger('AWSIoTPythonSDK.core')
        logger.setLevel(logLevel)

        formatter     = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

        streamHandler = logging.StreamHandler()
        streamHandler.setFormatter(formatter)
        logger.addHandler(streamHandler)
         
    def connect(self):
        return self.client.connect()        

    def createShadow(self, name):
        isPersistent = True # for better performance (?!)
        return self.client.createShadowHandlerWithName(name, isPersistent)
    
class updateCallbacks:
	def __init__(self, shadow, updater, report=True):
	    self.shadow  = shadow
            self.updater = updater
            self.report  = report
            self.logger  = logging.getLogger('AWSIoTPythonSDK.core')

        def enable(self):
            self.shadow.shadowGet(self.cbInitial, 5)
            self.shadow.shadowRegisterDeltaCallback(self.cbDelta)

        def disable(self):
            self.shadow.shadowUnregisterDeltaCallback()

	def cbDelta(self, payload, responseStatus, token):

	    payloadDict = json.loads(payload)
	    message = json.dumps(payloadDict["state"])
            self.logger.info('received delta message:' + message)

            state = payloadDict["state"]
                
            self.updater(state)

            #if self.report:
            #    reportState(self.shadow, state)

            if self.report:
                newPayload = '{"state":{"reported":' + message + '}}'
                self.shadow.shadowUpdate(newPayload, None, 5)

        def cbInitial(self, payload, responseStatus, token):

	    payloadDict = json.loads(payload)
	    message = json.dumps(payloadDict["state"]["desired"])
            self.logger.info('received initial message:' + message)
            
            state = payloadDict["state"]["desired"]
            
            self.updater(state)

            #if self.report:
            #    reportState(self.shadow, state)

            if self.report:
                newPayload = '{"state":{"reported":' + message + '}}'
                self.shadow.shadowUpdate(newPayload, None, 5)


def pushState(shadow, state):    
    payload = { "state": { "desired": state } }
    
    cb = lambda payload, response, token: None
    shadow.shadowUpdate(json.dumps(payload), cb, 10)
    

def reportState(shadow, state):
    payload = { "state": { "reported": state } }
    
    cb = lambda payload, response, token: None
    shadow.shadowUpdate(json.dumps(payload), cb, 10)
