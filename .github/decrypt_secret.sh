#!/bin/sh

# Decrypt the file
mkdir -p $HOME/.config/earthengine/data.colec.fbf/

# --batch to prevent interactive command --yes to assume "yes" for questions
gpg --quiet --batch --yes --decrypt --passphrase="$RGEE_SECRET_PASSPHRASE" \
--output $HOME/.config/earthengine/data.colec.fbf/GCS_AUTH_FILE.json \
./tests/credentials/GCS_AUTH_FILE.json.gpg

gpg --quiet --batch --yes --decrypt --passphrase="$RGEE_SECRET_PASSPHRASE" \
--output $HOME/.config/earthengine/data.colec.fbf/cd26ed5dc626f11802a652e81d02762e_data.colec.fbf@gmail.com ./tests/credentials/cd26ed5dc626f11802a652e81d02762e_data.colec.fbf@gmail.com.gpg

gpg --quiet --batch --yes --decrypt --passphrase="$RGEE_SECRET_PASSPHRASE" \
--output $HOME/.config/earthengine/data.colec.fbf/credentials ./tests/credentials/credentials.gpg

echo "NA, NA, NA" > $HOME/.config/earthengine/rgee_sessioninfo.txt 