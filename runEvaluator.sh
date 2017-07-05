#!/usr/bin/env bash
docker build -t evaluator .
docker run   -t --rm --name evaluator evaluator $1 $2 $3 $4
