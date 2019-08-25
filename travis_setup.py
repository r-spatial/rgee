#!/usr/bin/env python

import os

# Get key
key = os.environ['EE_CREDENTIALS']

# Build line
line = '{"refresh_token": "%s"}' % key

# Create directory
os.makedirs(os.path.expanduser('~/.config/earthengine/'))

# Write line to file
with open(os.path.expanduser('~/.config/earthengine/credentials'), 'w') as dst:
   dst.write(line)
