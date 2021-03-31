FROM alpine:latest

WORKDIR /app
VOLUME /app

RUN apk add --update gcc g++ curl make cmake git openssl-dev mysql mysql-client erlang erlang-dev unixodbc unixodbc-dev erlang-odbc sqsh && rm -f /var/cache/apk/*
RUN set -xe \
    && curl -fSL -o rebar3 "https://s3.amazonaws.com/rebar3-nightly/rebar3" \
    && chmod +x ./rebar3 \
    && ./rebar3 local install \
    && rm ./rebar3
ENV PATH="/root/.cache/rebar3/bin:${PATH}"

# These lines moved to the end allow us to rebuild image quickly after only these files were modified.
COPY startup.sh /startup.sh
COPY my.cnf /etc/mysql/my.cnf
COPY TaskOrganizerDB.sql /TaskOrganizerDB.sql

ENTRYPOINT ["/startup.sh"]
