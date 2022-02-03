FROM fpco/stack-build:lts-16.31 AS builder

RUN mkdir /opt/build
WORKDIR /opt/build
COPY . /opt/build
RUN make
RUN cp "$(stack path --local-install-root)/bin/chesskell" /opt/build/chesskell

FROM ubuntu:22.04

RUN apt-get update && apt-get install -y netbase

RUN mkdir -p /opt/chesskell
WORKDIR /opt/chesskell
COPY --from=builder /opt/build/chesskell /opt/chesskell
RUN mkdir /opt/chesskell/cheskell
COPY --from=builder /opt/build/cheskell /opt/chesskell/cheskell

ENV PORT=8080
EXPOSE 8080

ENTRYPOINT ["/opt/chesskell/chesskell"]
