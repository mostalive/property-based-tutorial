FROM capitalmatch/haskell-desktop

RUN DEBIAN_FRONTEND=noninteractive apt-get install -y curl gedit

RUN curl -sL https://deb.nodesource.com/setup | bash - \
    && apt-get install -y nodejs

CMD ["/bin/bash"]


