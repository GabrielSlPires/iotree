import serial.tools.list_ports
from datetime import datetime
import time
import os
import sys
import re

iotree_regex = r'^[0-9]+,([0-9]+\.[0-9]+,){4}[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}'
pneumatron_regex = r'^(-?[0-9]+,){5}(([0-9]+\.[0-9]+,){4}v3|,([0-9]+\.[0-9]+),,,v1),[0-9]{4}(-[0-9]+){2} [0-9]+(:[0-9]+){2}'
iotree_file = "database"
pneumatron_file = "pneumatron_database"

def com_ports():
    ports_name = serial.tools.list_ports.comports()
    ports = []

    for port, desc, hwid in sorted(ports_name):
        ports.append(port)
    
    #ports.remove('/dev/ttyAMA0')
    return(ports)

ports = com_ports()
print(f"\nReading ports: {ports}\n")
ser = {port: serial.Serial(port, 115200, timeout=0.01) for port in ports}

for port in ports:
    ser[port].setRTS(False)
    ser[port].setDTR(False)
    time.sleep(0.5)
    ser[port].reset_input_buffer()

try: 
 while True:
    print("\rWaiting       ", end="")
    for port in ports:
        message = ser[port].readline()
        if len(message) > 2:
            measure = message.decode()
            now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            line = f'{measure[:-1]},{now}\n'

            #match regex to discover device
            if bool(re.search(iotree_regex, line)):
               file = iotree_file
            if bool(re.search(pneumatron_regex, line)):
               file = pneumatron_file
            
            #write message in file
            try:
             with open(f'data/{file}.csv', 'a') as f:
                 f.write(line)
                 f.close()
             print("\rReceiving data", end="")
            except Exception:
             pass

    new_com_port = not any([ser[port].inWaiting() for port in ports] + [ports == com_ports()])
    if new_com_port:
        print("\nNew COM Port - Restart script")
        time.sleep(1)
        os.execv(sys.executable,["python3"] + [sys.argv[0]])
     
except Exception:
 print("\nError found - Restart script")
 time.sleep(1)
 os.execv(sys.executable,["python3"] + [sys.argv[0]])
