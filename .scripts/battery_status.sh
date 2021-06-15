#!/bin/sh
acpi -b | awk '{print substr($4, 0, length($2) + 1) ", " substr($3, 0, length($3) - 1)}'
