#!/bin/sh
#https://gist.githubusercontent.com/nicferrier/2277987/raw/53fd0e91911f919e944f8ee0a194782f161e18e3/gistfile1.sh
#because we don't want ssh keys everywhere, and we want to pre-build the latest version
REPOSRC=$1
LOCALREPO=$2

# We do it this way so that we can abstract if from just git later on
LOCALREPO_VC_DIR=$LOCALREPO/.git

if [ ! -d $LOCALREPO_VC_DIR ]
then
    git clone $REPOSRC $LOCALREPO
else
    cd $LOCALREPO
    git pull $REPOSRC
fi

# End
