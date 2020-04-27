#!/bin/sh
# Encrypt the file
gpg --symmetric --cipher-algo AES256 GCS_AUTH_FILE.json
gpg --symmetric --cipher-algo AES256 cd26ed5dc626f11802a652e81d02762e_data.colec.fbf@gmail.com
gpg --symmetric --cipher-algo AES256 credentials