FROM capitalmatch/haskell-desktop

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y curl gedit

RUN curl -sL https://deb.nodesource.com/setup | bash - \
    && apt-get install -y nodejs

RUN add-apt-repository -y ppa:webupd8team/sublime-text-2 && \
    apt-get update && \
    apt-get install sublime-text

WORKDIR /home/dockerx

ADD exercises exercises

RUN cd exercises/js && \
    npm install jsverify && \
    npm install underscore

RUN cd exercises/hsmoney && \
    cabal install --only-dependencies

RUN chown -R dockerx.dockerx exercises

