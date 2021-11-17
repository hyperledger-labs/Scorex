#!/bin/bash
# setting up gpg2 for reading passphrase from parameters
# via https://github.com/beautiful-scala/scalastyle/blob/master/.github/workflows/release.yml#L16
# from https://github.com/olafurpg/sbt-ci-release/issues/95

# setup gpg
mkdir ~/.gnupg && chmod 700 ~/.gnupg
echo use-agent >> ~/.gnupg/gpg.conf
echo pinentry-mode loopback >> ~/.gnupg/gpg.conf
echo allow-loopback-pinentry >> ~/.gnupg/gpg-agent.conf
chmod 600 ~/.gnupg/*
echo RELOADAGENT | gpg-connect-agent

# decode key
# private key should be previously exported with:
# gpg --export-secret-keys [id] | base64 | pbcopy
# and stored as github repository secret under the following name (see env var name below)
printf "$GPG_SIGNING_KEY" | base64 --decode > ~/.gnupg/private.key

# import key
gpg --no-tty --batch --yes --import ~/.gnupg/private.key
