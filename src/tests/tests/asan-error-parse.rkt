#lang racket

(require "../../collects/seashell/backend/asan-error-parse.rkt")

(display (asan-rewrite (port->bytes)))

