FROM fpco/stack-build:lts-6.12 AS builder
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && make

FROM nginx:1.21.5
RUN apt-get update && apt-get install -y netbase

RUN mkdir -p /opt/chesskell
RUN mkdir /opt/chesskell/cheskell
WORKDIR /opt/chesskell
COPY --from=builder /opt/build/.stack-work/install/x86_64-linux/lts-6.12/7.10.3/bin/chesskell .
COPY cheskell cheskell

ENV PORT=8080
ENTRYPOINT ["/opt/chesskell/chesskell"]
