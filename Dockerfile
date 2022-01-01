FROM fpco/stack-build:lts-6.12 AS builder

RUN mkdir /opt/build
WORKDIR /opt/build
COPY . /opt/build
RUN make

ENV PORT=8080
EXPOSE 8080

ENTRYPOINT ["/opt/build/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/bin/chesskell"]
