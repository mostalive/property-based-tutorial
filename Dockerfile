FROM capitalmatch/haskell-desktop

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y curl gedit

RUN curl -sL https://deb.nodesource.com/setup | bash - \
    && apt-get install -y nodejs

RUN add-apt-repository -y ppa:webupd8team/sublime-text-2 && \
    apt-get update && \
    apt-get install sublime-text

CMD ["/bin/bash"]


