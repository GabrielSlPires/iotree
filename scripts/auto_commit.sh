#!/bin/sh

cd /home/pi/Documents/iotree/data
git pull
git add .
timestamp() {
  date +"at %H:%M:%S on %d/%m/%Y"
}
git commit -m "database auto-commit $(timestamp)"
git push