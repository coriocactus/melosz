#!/usr/bin/env zsh

set -e

docker run -d --name redis-test -p 5268:6379 -v redis-test-data:/data \
  redis:latest redis-server \
  --appendonly yes \
  --appendfsync everysec \
  --save 240 1 \
  --save 120 10 \
  --save 60 10000 \
  --auto-aof-rewrite-percentage 100 \
  --auto-aof-rewrite-min-size 64mb
