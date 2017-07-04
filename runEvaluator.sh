#!/usr/bin/env bash
docker build -t evaluator .
docker run   -t --rm --name evaluator -p 127.0.0.1:8080:8080 evaluator $1 $2 $3 $4
