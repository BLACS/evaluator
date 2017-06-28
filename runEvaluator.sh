#!/usr/bin/env bash
docker build -t evaluator .
docker run   -t --rm --name evaluator evaluator
