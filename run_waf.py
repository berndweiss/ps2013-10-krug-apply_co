#!/usr/bin/env python
import subprocess
import os
import re

## This script's purpose is to manage multiple waf calls. 
## Unfortunately, whenever I start working on another machine,
## waf requires me to run "waf configure". So, this little script
## also checks, if I've changed my computer. The name of the most
## recent computer is documented in my config file... 


## Open config file
f = open('config', 'r')
filecontent = f.read()
f.close()

## Extract computer name
oldname = re.findall("computername : .*", filecontent)
## extract list element
oldname = re.sub("^.* :", "", oldname[0]) 
## Strip white space
oldname = oldname.strip()
print "--------------------------------------------------------------"
print "You are currently working on " + oldname + "."
print "You used to work on " + os.environ["COMPUTERNAME"] + "."
print "--------------------------------------------------------------"

## Check if I am still on the same computer ("else")
if oldname != os.environ["COMPUTERNAME"]:
   ## Generate waf call which includes a build argument
   waf_call = "python waf configure build"
   #subprocess.call("python waf configure build", cwd='src/tex')
   #subprocess.call("python waf", cwd='src/tex')
   filecontent2 = re.sub("computername : .*", "computername : " \
                         + os.environ["COMPUTERNAME"], filecontent)
   f = open('config', 'w')
   f.write(filecontent2)
   f.close()     		
else:
   waf_call = "python waf build"
   
## The actual waf call   
subprocess.call(waf_call, cwd='src/tex')
