#!/bin/bash -e

dup cl-base64 -Uftp.b9.com -D/home/ftp/cl-base64 -C"/home/kevin/bin/remove-old-versions cl-base64" -su $*

