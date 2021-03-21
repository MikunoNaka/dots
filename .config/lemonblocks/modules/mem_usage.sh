#!/bin/bash

mem=$(free -m | grep Mem: | awk '{print$3 / $2 * 100}')
printf " MEM: %.0f%% " $mem
