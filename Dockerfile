FROM haskell:latest

RUN useradd -ms /bin/bash invenuser
WORKDIR /app
COPY . /app

RUN chown -R invenuser:invenuser /app
USER invenuser

ENV PATH="/home/invenuser/.local/bin:${PATH}"

RUN stack setup && stack build
RUN stack install --install-ghc

EXPOSE 4200

CMD . inven serve
