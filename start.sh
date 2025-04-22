#!/usr/bin/env zsh

set -e

docker run -d --name melosz-redis -p 7480:6379 -v melosz-data:/data \
  redis:latest redis-server \
  --appendonly yes \
  --appendfsync everysec \
  --save 240 1 \
  --save 120 10 \
  --save 60 10000 \
  --auto-aof-rewrite-percentage 100 \
  --auto-aof-rewrite-min-size 64mb
